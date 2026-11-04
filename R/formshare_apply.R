# Applying a filled cleaning log to FormShare.
#
# cleaningtools applies a cleaning log to a copy of the raw data
# (create_clean_data). With FormShare the corrections are made where the data
# lives: each one through FormShare's cleaning API, as an assistant who may
# clean the form, with its reason. FormShare's audit log keeps the old value,
# the new one, who and why, so the cleaning can be reviewed from the server
# itself (fs_review_cleaning). The whole log is checked first, and nothing is
# written while it has a problem.

FS_REASON_MAX_LENGTH <- 1000

# The rowuuids of `uuids` that exist in a table, asked in chunks
fs_existing_uuids <- function(connection, repository, table, uuids) {
  uuids <- unique(uuids[!is.na(uuids)])
  found <- character(0)
  for (chunk in split(uuids, ceiling(seq_along(uuids) / 500))) {
    sql <- paste0(
      "SELECT `rowuuid` FROM ", sql_name(table),
      " WHERE `rowuuid` IN (", paste(sql_text(chunk), collapse = ", "), ")"
    )
    rows <- connection$execute(repository, sql)
    if (nrow(rows) > 0) {
      found <- c(found, as.character(rows$rowuuid))
    }
  }
  found
}

# The kind of each entry: change_response, blank_response, remove_survey,
# no_action, or NA when the change type is none of the values given
fs_entry_kind <- function(change_type, change_response_value, blank_response_value,
                          no_change_value, remove_survey_value) {
  dplyr::case_when(
    change_type %in% change_response_value ~ "change_response",
    change_type %in% blank_response_value ~ "blank_response",
    change_type %in% remove_survey_value ~ "remove_survey",
    change_type %in% no_change_value ~ "no_action",
    TRUE ~ NA_character_
  )
}

#' Check a cleaning log against a FormShare repository
#'
#' Before a cleaning log is applied, checks each entry against what FormShare
#' holds, as [review_cleaning_log()] does against a raw dataset. Possible
#' problems:
#' - na_in_change_type: the change type is empty
#' - unknown_change_type: the change type is none of the values given
#' - all_data_not_supported: a change to every row; make it in FormShare
#' - question_does_not_exist: not a question of the row's table
#' - question_is_read_only: one of FormShare's own columns
#' - question_is_encrypted: change it in FormShare's own cleaning grid
#' - question_is_a_key: it identifies the record; FormShare does not change it
#' - value_not_a_choice: a select's new value is not one of its choices
#' - uuid_does_not_exist: no row with that rowuuid in its table
#' - remove_survey_not_a_submission: only a row of maintable is a submission
#'
#' @param connection A logged-in FormShare connection
#' @param repository The repository
#' @param cleaning_log The cleaning log (data frame)
#' @param uuid_column Column of the log with the row's uuid (its rowuuid)
#' @param question_column Column of the log with the question
#' @param change_type_column Column of the log with the change type
#' @param new_value_column Column of the log with the new value
#' @param table_column Column of the log with the row's table, if it has one.
#' Without it, each question's table is found from the form.
#' @param change_response_value Value(s) of the change type that change an answer
#' @param blank_response_value Value(s) of the change type that empty an answer
#' @param no_change_value Value(s) of the change type that change nothing
#' @param remove_survey_value Value(s) of the change type that remove a submission
#' @param survey The form's questions ([fs_survey()]), read if not given
#' @param choices The form's choices ([fs_choices()]), read if not given
#' @return A data frame with one row per problem: the problem, then the log's
#' columns. No rows when the log can be applied.
#' @export
fs_check_cleaning_log <- function(connection, repository, cleaning_log,
                                  uuid_column = "uuid",
                                  question_column = "question",
                                  change_type_column = "change_type",
                                  new_value_column = "new_value",
                                  table_column = "table",
                                  change_response_value = "change_response",
                                  blank_response_value = "blank_response",
                                  no_change_value = c("no_action", "no_change"),
                                  remove_survey_value = "remove_survey",
                                  survey = NULL,
                                  choices = NULL) {
  needed <- c(uuid_column, question_column, change_type_column, new_value_column)
  if (!all(needed %in% names(cleaning_log))) {
    stop(glue::glue(
      "The cleaning log has no column {glue::glue_collapse(needed[!needed %in% names(cleaning_log)], ', ')}"
    ))
  }
  if (is.null(survey)) {
    survey <- fs_survey(connection, repository)
  }
  if (is.null(choices)) {
    choices <- fs_choices(connection, repository)
  }

  log <- as.data.frame(cleaning_log)
  uuid <- as.character(log[[uuid_column]])
  question <- trimws(as.character(log[[question_column]]))
  new_value <- trimws(as.character(log[[new_value_column]]))
  kind <- fs_entry_kind(
    log[[change_type_column]], change_response_value, blank_response_value,
    no_change_value, remove_survey_value
  )
  table <- if (table_column %in% names(log)) as.character(log[[table_column]]) else rep(NA_character_, nrow(log))
  found_table <- survey$table[match(question, survey$name)]
  table <- dplyr::coalesce(table, found_table)
  edits <- kind %in% c("change_response", "blank_response")

  problem <- rep(NA_character_, nrow(log))
  flag <- function(problem, condition, name) {
    ifelse(is.na(problem) & condition, name, problem)
  }
  problem <- flag(problem, is.na(log[[change_type_column]]), "na_in_change_type")
  problem <- flag(problem, is.na(kind), "unknown_change_type")
  problem <- flag(problem, edits & uuid %in% c("all", "all_data"), "all_data_not_supported")
  problem <- flag(problem, edits & fs_is_control_column(question), "question_is_read_only")
  in_survey <- paste(question, table) %in% paste(survey$name, survey$table)
  problem <- flag(problem, edits & !in_survey, "question_does_not_exist")
  encrypted <- survey$encrypted[match(paste(question, table), paste(survey$name, survey$table))]
  problem <- flag(problem, edits & in_survey & encrypted %in% TRUE, "question_is_encrypted")
  key <- survey$key[match(paste(question, table), paste(survey$name, survey$table))]
  problem <- flag(problem, edits & in_survey & key %in% TRUE, "question_is_a_key")

  # a select's new value must be one (or, for a select multiple, each) of its choices
  type <- survey$type[match(paste(question, table), paste(survey$name, survey$table))]
  list_name <- survey$list_name[match(paste(question, table), paste(survey$name, survey$table))]
  not_a_choice <- vapply(seq_len(nrow(log)), function(i) {
    if (kind[i] %in% "change_response" && !is.na(type[i]) && !is.na(list_name[i])) {
      codes <- choices$name[choices$list_name == list_name[i]]
      values <- if (startsWith(type[i], "select_multiple")) {
        strsplit(new_value[i], "\\s+")[[1]]
      } else {
        new_value[i]
      }
      return(any(!values %in% codes))
    }
    FALSE
  }, logical(1))
  problem <- flag(problem, not_a_choice, "value_not_a_choice")
  problem <- flag(problem, kind %in% "remove_survey" & !is.na(table) & table != "maintable",
                  "remove_survey_not_a_submission")

  # the rows: each must exist in its table (a removal names a submission)
  table[kind %in% "remove_survey" & is.na(table)] <- "maintable"
  to_find <- (edits | kind %in% "remove_survey") & is.na(problem) & !is.na(table)
  for (a_table in unique(table[to_find])) {
    in_table <- to_find & table == a_table
    existing <- fs_existing_uuids(connection, repository, a_table, uuid[in_table])
    problem <- flag(problem, in_table & !uuid %in% existing, "uuid_does_not_exist")
  }

  problems <- cbind(problem = problem, log, stringsAsFactors = FALSE)[!is.na(problem), , drop = FALSE]
  rownames(problems) <- NULL
  if (nrow(problems) == 0) {
    message("No problems in the cleaning log")
  } else {
    message(glue::glue("The cleaning log has {nrow(problems)} problems, see the output table"))
  }
  problems
}

#' Apply a cleaning log to FormShare
#'
#' Makes the corrections of a filled cleaning log in FormShare, through its
#' cleaning API, instead of in a copy of the data ([create_clean_data()]). Each
#' change is made as the assistant whose API key the connection holds (who
#' needs permission to clean the form), and FormShare's audit log keeps the old
#' value, the new one, who and the reason.
#'
#' - The whole log is checked first ([fs_check_cleaning_log()]): while it has a
#'   problem, nothing is written.
#' - `dry_run` is TRUE by default: the result says what would be done. Run it
#'   again with `dry_run = FALSE` to write.
#' - A change_response sets the answer to the new value; a blank_response
#'   empties it; a no_action changes nothing.
#' - A remove_survey is only listed by default, to remove in FormShare. With
#'   `remove_survey = "delete"` the submission is deleted (the connection's
#'   account must own or administer the project); with "move_to_logs" it is
#'   moved to the logs, for the assistant `assistant` to fix.
#'
#' @inheritParams fs_check_cleaning_log
#' @param reason The reason recorded with each change, followed by the entry's
#' issue when the log has an `issue` column: "Cleaning log: outlier (log
#' distribution)"
#' @param reason_column A column of the log with each entry's own reason. An
#' empty one falls back to `reason`.
#' @param dry_run TRUE (default) to only say what would be done
#' @param remove_survey What to do with a remove_survey: "report" (default),
#' "delete" or "move_to_logs"
#' @param assistant With "move_to_logs": the ID of the assistant in charge of
#' the log entries (`connection$get_assistants(repository)` lists them)
#' @param assistant_uuid Or that assistant's uuid, for example a global
#' assistant's
#' @return A data frame, one row per entry: uuid, table, question, change_type,
#' new_value, status, message, reason, reason_kept
#' @export
#' @examplesIf interactive()
#' connection <- fs_connect("https://formshare.org", "my_user", "my_key", "my_secret",
#'   assistant_api_key = "a_cleaning_assistant_key"
#' )
#' filled_log <- read.csv("cleaning_log_filled.csv")
#' fs_apply_cleaning_log(connection, "FS_xxxx", filled_log) # what would be done
#' fs_apply_cleaning_log(connection, "FS_xxxx", filled_log, dry_run = FALSE)
fs_apply_cleaning_log <- function(connection, repository, cleaning_log,
                                  uuid_column = "uuid",
                                  question_column = "question",
                                  change_type_column = "change_type",
                                  new_value_column = "new_value",
                                  table_column = "table",
                                  change_response_value = "change_response",
                                  blank_response_value = "blank_response",
                                  no_change_value = c("no_action", "no_change"),
                                  remove_survey_value = "remove_survey",
                                  reason = "Cleaning log",
                                  reason_column = NULL,
                                  dry_run = TRUE,
                                  remove_survey = c("report", "delete", "move_to_logs"),
                                  assistant = NULL,
                                  assistant_uuid = NULL,
                                  survey = NULL,
                                  choices = NULL) {
  remove_survey <- match.arg(remove_survey)
  if (remove_survey == "move_to_logs" && (is.null(assistant) || assistant == "") &&
    (is.null(assistant_uuid) || assistant_uuid == "")) {
    stop("Moving submissions to the logs needs the assistant in charge of them (assistant or assistant_uuid)")
  }
  if (is.null(survey)) {
    survey <- fs_survey(connection, repository)
  }
  if (is.null(choices)) {
    choices <- fs_choices(connection, repository)
  }

  log <- as.data.frame(cleaning_log)
  uuid <- as.character(log[[uuid_column]])
  question <- trimws(as.character(log[[question_column]]))
  new_value <- trimws(as.character(log[[new_value_column]]))
  kind <- fs_entry_kind(
    log[[change_type_column]], change_response_value, blank_response_value,
    no_change_value, remove_survey_value
  )
  table <- if (table_column %in% names(log)) as.character(log[[table_column]]) else rep(NA_character_, nrow(log))
  table <- dplyr::coalesce(table, survey$table[match(question, survey$name)])
  table[kind %in% "remove_survey" & is.na(table)] <- "maintable"

  # each entry's reason: its own, or the default with the entry's issue
  reasons <- rep(reason, nrow(log))
  if ("issue" %in% names(log)) {
    has_issue <- !is.na(log$issue) & log$issue != ""
    reasons[has_issue] <- paste0(reason, ": ", log$issue[has_issue])
  }
  if (!is.null(reason_column)) {
    own <- as.character(log[[reason_column]])
    reasons <- ifelse(!is.na(own) & own != "", own, reasons)
  }
  reasons <- substr(reasons, 1, FS_REASON_MAX_LENGTH)

  report <- data.frame(
    uuid = uuid, table = table, question = question,
    change_type = as.character(log[[change_type_column]]), new_value = new_value,
    status = NA_character_, message = NA_character_, reason = reasons, reason_kept = NA,
    stringsAsFactors = FALSE
  )

  problems <- fs_check_cleaning_log(
    connection, repository, log,
    uuid_column = uuid_column, question_column = question_column,
    change_type_column = change_type_column, new_value_column = new_value_column,
    table_column = table_column, change_response_value = change_response_value,
    blank_response_value = blank_response_value, no_change_value = no_change_value,
    remove_survey_value = remove_survey_value, survey = survey, choices = choices
  )
  if (nrow(problems) > 0) {
    # The problems by entry: the log's row as it was given
    keys <- paste(uuid, question, log[[change_type_column]])
    problem_keys <- paste(
      problems[[uuid_column]], trimws(problems[[question_column]]), problems[[change_type_column]]
    )
    report$status <- ifelse(keys %in% problem_keys, "problem", "not sent")
    report$message <- problems$problem[match(keys, problem_keys)]
    warning(glue::glue(
      "The cleaning log has {nrow(problems)} problems: nothing was written. See the status and message columns."
    ))
    return(report)
  }

  for (i in seq_len(nrow(log))) {
    if (kind[i] == "no_action") {
      report$status[i] <- "no action"
      next
    }
    if (kind[i] == "remove_survey") {
      if (remove_survey == "report") {
        report$status[i] <- "to remove in FormShare"
        report$message[i] <- "Delete it, or move it to the logs, on the form's Submissions page"
        next
      }
      if (dry_run) {
        report$status[i] <- "dry run"
        report$message[i] <- if (remove_survey == "delete") "would delete the submission" else "would move the submission to the logs"
        next
      }
      outcome <- tryCatch(
        {
          connection$delete_submission(
            repository, uuid[i],
            reason = reasons[i],
            move_to_logs = remove_survey == "move_to_logs",
            assistant = assistant,
            assistant_uuid = assistant_uuid
          )
          NULL
        },
        error = function(e) conditionMessage(e)
      )
      report$status[i] <- if (is.null(outcome)) {
        if (remove_survey == "delete") "deleted" else "moved to the logs"
      } else {
        "refused"
      }
      report$message[i] <- outcome %||% NA_character_
      next
    }
    value <- if (kind[i] == "blank_response") NA else new_value[i]
    if (dry_run) {
      report$status[i] <- "dry run"
      report$message[i] <- if (is.na(value)) {
        glue::glue("would empty {question[i]}")
      } else {
        glue::glue("would set {question[i]} to {value}")
      }
      next
    }
    answer <- tryCatch(
      connection$update_record(uuid[i], stats::setNames(list(value), question[i]), reason = reasons[i]),
      error = function(e) e
    )
    if (inherits(answer, "error")) {
      report$status[i] <- "refused"
      report$message[i] <- conditionMessage(answer)
    } else if (question[i] %in% unlist(answer$ignored)) {
      report$status[i] <- "ignored"
      report$message[i] <- "Encrypted: change it in FormShare's own cleaning grid"
    } else {
      report$status[i] <- "applied"
      report$reason_kept[i] <- isTRUE(answer$reason_kept)
    }
  }

  counts <- table(report$status)
  message(glue::glue_collapse(paste(names(counts), counts, sep = ": "), sep = ", "))
  report
}

`%||%` <- function(x, y) if (is.null(x)) y else x
