# Raising FormShare's Data Quality flags from the checks.
#
# The checks of cleaningtools write logs. With FormShare's Data Quality
# plugin, what they found can also be flags on the submissions, which lower
# each one's Data Quality Score where the whole team sees it: the Data
# quality page, Clean data and the assistants' phones. The flags go through
# the plugin's flags API (the FormShare R package's set_flags), from a source
# of their own, so FormShare's own checks keep theirs.

FS_FLAG_SEVERITIES <- c("critical", "major", "minor")

# A check's name for the flags API, from a log's check_id or issue: at most 64
# letters, digits or _ . : -
fs_check_code <- function(text) {
  code <- tolower(gsub("[^A-Za-z0-9_.:-]+", "_", trimws(as.character(text))))
  code <- gsub("^_+|_+$", "", code)
  code[is.na(text)] <- NA_character_
  substr(code, 1, 64)
}

# The logs among what the checks return: the list's data frames but the
# checked dataset, or one log
fs_logs_of <- function(checks, dataset_name) {
  if (is.data.frame(checks)) {
    return(list(cleaning_log = checks))
  }
  if (!is.list(checks)) {
    stop("checks must be what the checks return (a list) or a log (a data frame)")
  }
  logs <- checks[names(checks) != dataset_name]
  logs[vapply(logs, is.data.frame, TRUE)]
}

# A log's entries as flags: rowuuid, check, severity, message, fields, value
fs_flags_of_log <- function(log, log_name, severity, uuid_column, sm_separator) {
  if (!uuid_column %in% names(log)) {
    stop("The log ", log_name, " has no ", uuid_column, " column")
  }
  column <- function(name) {
    if (name %in% names(log)) as.character(log[[name]]) else rep(NA_character_, nrow(log))
  }
  issue <- column("issue")
  question <- column("question")
  check <- fs_check_code(ifelse(is.na(column("check_id")), issue, column("check_id")))
  check[is.na(check) | check == ""] <- fs_check_code(log_name)

  # The question a flag is about: a select multiple's option column
  # (crops.maize) is its question
  at <- regexpr(sm_separator, question, fixed = TRUE)
  base <- ifelse(!is.na(at) & at > 1, substr(question, 1, at - 1), question)
  fields <- ifelse(!is.na(base) & grepl("^[A-Za-z0-9_]{1,64}$", base), base, NA_character_)

  wanted <- column("severity")
  given <- if (is.null(names(severity))) {
    rep(severity[1], nrow(log))
  } else if (log_name %in% names(severity)) {
    rep(severity[[log_name]], nrow(log))
  } else {
    unname(ifelse(check %in% names(severity), severity[check],
      ifelse(issue %in% names(severity), severity[issue], NA_character_)
    ))
  }
  data.frame(
    rowuuid = as.character(log[[uuid_column]]),
    check = check,
    severity = tolower(ifelse(is.na(wanted) | wanted == "", given, wanted)),
    message = substr(ifelse(is.na(issue) | trimws(issue) == "", paste("Found by", log_name), issue), 1, 1000),
    fields = fields,
    value = substr(column("old_value"), 1, 1000),
    log = log_name,
    stringsAsFactors = FALSE
  )
}

#' Raise FormShare's Data Quality flags from the checks
#'
#' Posts what the checks found as Data Quality flags in FormShare, through the
#' flags API of its Data Quality plugin. FormShare scores each submission
#' again: its Data Quality Score is 100 minus the weights of its open flags,
#' from every source (critical 40, major 15 and minor 5, unless the server
#' weighs them otherwise). The team sees the flags where FormShare shows them,
#' on the Data quality page and in Clean data.
#'
#' Every submission checked is posted: one with entries gets a flag for each,
#' and one without gets none, which resolves the flags this `source` raised
#' there before. So after the cleaning, the same checks run again resolve the
#' flags of what was fixed, and the scores go back up. Posting the same flags
#' twice changes nothing, and a flag a supervisor dismissed stays dismissed.
#'
#' An entry about a whole column (uuid "all", such as a possible personal
#' question) is not about a submission, and is left out. So is an entry that
#' repeats another's check on the same question for the same submission.
#'
#' @param connection A connection from [fs_connect()], of an account that owns,
#'   administers or edits the project
#' @param repository The repository (its form_schema)
#' @param checks What the checks return (the list with the checked dataset and
#'   their logs), or one log, such as the combined cleaning log
#' @param severity The flags' severity: critical, major or minor. One for all,
#'   or one for each check, named by its log (`potential_outliers`), its
#'   check_id or its issue. A log's own `severity` column comes first.
#' @param source Who raises the flags, up to 64 letters, digits or `_ . : -`.
#'   Give each script its own: a post replaces only its source's flags.
#' @param dataset_name The element of `checks` with the checked dataset
#' @param dataset_uuid_column The dataset's column with the rowuuid
#' @param cleaning_log_uuid_column The logs' column with the rowuuid
#' @param checked The submissions checked, if not those of the checked dataset
#'   (or, for a log alone, those of the log)
#' @param sm_separator The separator of a select multiple's option columns
#' @param dry_run TRUE to return the flags without posting them
#'
#' @return A list: `flags`, the flags (rowuuid, check, severity, message,
#'   fields, value, and the log they come from), `checked`, and after a post
#'   the server's answer: `records`, `added`, `updated`, `resolved`, `gone` and
#'   `scores` (rowuuid, score).
#' @export
#'
#' @examples
#' \dontrun{
#' connection <- fs_connect("https://formshare.org", "my_user", "my_key", "my_secret")
#' households <- fs_read_data(connection, "FS_xxxx")
#' survey <- fs_survey(connection, "FS_xxxx")
#' choices <- fs_choices(connection, "FS_xxxx")
#' checks <- households |>
#'   check_outliers(uuid_column = "rowuuid", kobo_survey = survey, kobo_choices = choices) |>
#'   check_soft_duplicates(
#'     kobo_survey = survey, uuid_column = "rowuuid", group_column = "_submitted_by"
#'   )
#' fs_flag_checks(connection, "FS_xxxx", checks,
#'   severity = c(potential_outliers = "major", soft_duplicate_log = "minor")
#' )
#' }
fs_flag_checks <- function(connection, repository, checks,
                           severity = "major",
                           source = "script:cleaningtools",
                           dataset_name = "checked_dataset",
                           dataset_uuid_column = "rowuuid",
                           cleaning_log_uuid_column = "uuid",
                           checked = NULL,
                           sm_separator = ".",
                           dry_run = FALSE) {
  if (length(severity) == 0 || !all(tolower(severity) %in% FS_FLAG_SEVERITIES)) {
    stop("severity must be critical, major or minor")
  }
  logs <- fs_logs_of(checks, dataset_name)
  if (is.null(checked) && is.list(checks) && !is.data.frame(checks) &&
    is.data.frame(checks[[dataset_name]])) {
    if (!dataset_uuid_column %in% names(checks[[dataset_name]])) {
      stop("The checked dataset has no ", dataset_uuid_column, " column")
    }
    checked <- as.character(checks[[dataset_name]][[dataset_uuid_column]])
  }

  flags <- do.call(rbind, c(
    list(data.frame(
      rowuuid = character(0), check = character(0), severity = character(0),
      message = character(0), fields = character(0), value = character(0),
      log = character(0)
    )),
    lapply(names(logs), function(name) {
      if (nrow(logs[[name]]) == 0) {
        return(NULL)
      }
      fs_flags_of_log(logs[[name]], name, severity, cleaning_log_uuid_column, sm_separator)
    })
  ))
  whole_column <- is.na(flags$rowuuid) | flags$rowuuid == "all"
  if (any(whole_column)) {
    message(sum(whole_column), " entries about a whole column (uuid \"all\") are not about a submission: left out")
  }
  flags <- flags[!whole_column, , drop = FALSE]
  if (any(is.na(flags$severity))) {
    stop(
      "No severity for these checks: ",
      paste(unique(flags$log[is.na(flags$severity)]), collapse = ", "),
      ". Name them in severity, or give one severity for all"
    )
  }
  key <- paste(flags$rowuuid, flags$check, flags$fields, sep = "\r")
  if (any(duplicated(key))) {
    message(sum(duplicated(key)), " entries repeat another's check on the same question: left out")
    flags <- flags[!duplicated(key), , drop = FALSE]
  }
  rownames(flags) <- NULL
  if (is.null(checked)) {
    checked <- unique(flags$rowuuid)
  }
  checked <- unique(as.character(checked[!is.na(checked)]))

  # Flags go on submissions: a row of a repeat, or a submission deleted since,
  # would have the whole post refused
  posted <- unique(c(checked, flags$rowuuid))
  unknown <- setdiff(posted, fs_existing_uuids(connection, repository, "maintable", posted))
  if (length(unknown) > 0) {
    stop(
      "These uuids are not submissions of ", repository, ": ",
      paste(utils::head(unknown, 20), collapse = ", "), if (length(unknown) > 20) " ...",
      ". A flag goes on the submission's rowuuid, in maintable, not on a row of a repeat"
    )
  }

  if (dry_run) {
    message(
      "dry run: ", nrow(flags), " flags on ", length(unique(flags$rowuuid)),
      " submissions, of ", length(checked), " checked"
    )
    return(list(flags = flags, checked = checked))
  }
  answer <- connection$set_flags(repository,
    flags[, c("rowuuid", "check", "severity", "message", "fields", "value")],
    checked = checked, source = source
  )
  message(
    "Flags posted for ", answer$records, " submissions: ", answer$added, " added, ",
    answer$updated, " updated, ", answer$resolved, " resolved"
  )
  c(list(flags = flags, checked = checked), answer)
}
