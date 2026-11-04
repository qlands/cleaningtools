# The FormShare layer, against the stand-in of helper-fake-formshare.R.

# --- what FormShare's columns are -------------------------------------------

test_that("FormShare's own columns are told from the questions", {
  expect_equal(
    fs_is_control_column(c("rowuuid", "surveyid", "_submitted_by", "_dqscore", "hh_id", "county")),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )
})

# --- reading -------------------------------------------------------------------

test_that("fs_survey builds a survey sheet from FormShare's dictionary", {
  survey <- fs_survey(fake_formshare(), "FS_1")
  expect_true(all(c("type", "name", "label", "table", "list_name", "xpath") %in% names(survey)))
  type_of <- stats::setNames(survey$type, survey$name)
  expect_equal(type_of[["county"]], "select_one county")
  expect_equal(type_of[["crops"]], "select_multiple crops")
  expect_equal(type_of[["members_n"]], "integer")
  expect_equal(type_of[["start"]], "start")
  # FormShare's own columns are not questions
  expect_false(any(c("surveyid", "_submitted_by", "rowuuid", "members_rowid") %in% survey$name))
  # a continuation's question belongs to the table it continues
  expect_equal(survey$table[survey$name == "notes"], "maintable")
  expect_equal(survey$table[survey$name == "mem_age"], "members")
  expect_true(survey$sensitive[survey$name == "phone"])
  expect_true(survey$encrypted[survey$name == "income"])
  expect_true(survey$key[survey$name == "hh_id"])
  expect_false(survey$key[survey$name == "county"])
  # a case link: a select of another form's cases, with no list of choices
  expect_equal(type_of[["school"]], "select_one")
  expect_equal(survey$references[survey$name == "school"], "FS_schools.maintable")
  expect_true(is.na(survey$list_name[survey$name == "school"]))
  expect_true(verify_valid_survey(survey))
})

test_that("fs_choices reads every list in one query", {
  connection <- fake_formshare()
  choices <- fs_choices(connection, "FS_1")
  expect_equal(names(choices), c("list_name", "name", "label"))
  expect_true(verify_valid_choices(choices))
  expect_equal(length(connection$seen$sql), 1)
  expect_match(connection$seen$sql, "FROM `lkpcounty` UNION ALL", fixed = TRUE)
  expect_match(connection$seen$sql, "`county_des` AS label", fixed = TRUE)
})

test_that("fs_read_data joins a split table and converts the date-times", {
  connection <- fake_formshare()
  data <- fs_read_data(connection, "FS_1")
  sql <- connection$seen$sql
  expect_match(sql, "SELECT t.*, c1.`notes` FROM `maintable` t", fixed = TRUE)
  expect_match(sql, "LEFT JOIN `maintable_ext1` c1 ON c1.`link_rowuuid` = t.`rowuuid`", fixed = TRUE)
  expect_s3_class(data$`_submitted_date`, "POSIXct")
  expect_s3_class(data$start, "POSIXct")
  expect_equal(data$hh_id, c("007", "008", "009"))
})

test_that("a table the repository does not have is refused", {
  expect_error(fs_read_data(fake_formshare(), "FS_1", table = "nope"), "is not a table")
})

test_that("fs_xlsform reads the form's sheets, named in lower case", {
  form <- fs_xlsform(fake_formshare(), "FS_1")
  expect_true(all(c("survey", "choices", "settings") %in% names(form)))
  expect_equal(form$survey$name, c("county", "county_other"))
})

test_that("fs_audit_list gives the list create_audit_list reads from a zip", {
  audits <- fs_audit_list(fake_formshare(), "FS_1")
  expect_equal(sort(names(audits)), c("r1", "r2"))
  expect_true(all(c("event", "node", "start", "end", "old-value", "change-reason") %in% names(audits$r1)))
  # it works with the original duration functions as it is
  data <- data.frame(rowuuid = c("r1", "r2"))
  with_duration <- add_duration_from_audit(data, uuid_column = "rowuuid", audit_list = audits)
  expect_equal(with_duration$duration_audit_sum_all_minutes, c(3, 2))
})

test_that("fs_audit_list gives an empty audit to a submission without one", {
  data <- data.frame(rowuuid = c("r1", "r3"))
  expect_warning(
    audits <- fs_audit_list(fake_formshare(), "FS_1", dataset = data),
    "1 submissions have no audit"
  )
  expect_equal(sort(names(audits)), c("r1", "r3"))
  expect_equal(nrow(audits$r3), 0)
})

test_that("fs_change_log reads the audit log in the cleaning log's format", {
  connection <- fake_formshare()
  changes <- fs_change_log(connection, "FS_1", since = "2026-11-03")
  expect_equal(names(changes), c("uuid", "table", "question", "old_value", "new_value",
                                 "change_type", "user", "date", "reason"))
  # FormShare's own columns are left out by default
  expect_false("_dqscore" %in% changes$question)
  expect_equal(changes$change_type[changes$uuid == "r2" & changes$question %in% "phone"], "blank_response")
  expect_equal(changes$change_type[changes$uuid == "r9"], "remove_survey")
  expect_s3_class(changes$date, "POSIXct")
  expect_match(connection$seen$sql, "`audit_action` <> 'INSERT'", fixed = TRUE)
  # compared as an instant, whatever the database's time zone
  since <- format(as.numeric(as.POSIXct("2026-11-03", tz = "UTC")), scientific = FALSE)
  expect_match(connection$seen$sql, paste0("UNIX_TIMESTAMP(`audit_date`) >= ", since), fixed = TRUE)
  expect_equal(changes$date[changes$uuid == "r9"], as.POSIXct("2026-11-03 12:00:00", tz = "UTC"))
})

# --- checking and applying a cleaning log ------------------------------------

test_log <- function() {
  data.frame(
    uuid = c("r1", "r2", "r3", "r1"),
    question = c("county", "phone", "members_n", NA),
    change_type = c("change_response", "blank_response", "change_response", "no_action"),
    new_value = c("2", NA, "4", NA),
    issue = c("wrong county", "personal data", "outlier (log distribution)", "duplicated hh_id"),
    stringsAsFactors = FALSE
  )
}

test_that("a cleaning log FormShare can apply has no problems", {
  problems <- suppressMessages(fs_check_cleaning_log(fake_formshare(), "FS_1", test_log()))
  expect_equal(nrow(problems), 0)
})

test_that("each kind of problem in a cleaning log is found", {
  log <- data.frame(
    uuid = c("r1", "r1", "r1", "r1", "r1", "r1", "nope", "all_data", "m1", "r1"),
    question = c("county", "crops", "unknown_q", "_dqscore", "income", "members_n", "county", "county",
                 "mem_age", "hh_id"),
    change_type = c("change_response", "change_response", "change_response", "change_response",
                    "change_response", "fix it", "change_response", "change_response", "remove_survey",
                    "blank_response"),
    new_value = c("9", "maize rice", "1", "100", "500", "4", "1", "1", NA, NA),
    table = c(NA, NA, NA, NA, NA, NA, NA, NA, "members", NA),
    stringsAsFactors = FALSE
  )
  problems <- suppressMessages(fs_check_cleaning_log(fake_formshare(), "FS_1", log))
  expect_equal(
    problems$problem,
    c("value_not_a_choice", "value_not_a_choice", "question_does_not_exist", "question_is_read_only",
      "question_is_encrypted", "unknown_change_type", "uuid_does_not_exist",
      "all_data_not_supported", "remove_survey_not_a_submission", "question_is_a_key")
  )
})

test_that("a dry run says what would be done and writes nothing", {
  connection <- fake_formshare()
  report <- suppressMessages(fs_apply_cleaning_log(connection, "FS_1", test_log()))
  expect_equal(report$status, c("dry run", "dry run", "dry run", "no action"))
  expect_equal(report$message[1], "would set county to 2")
  expect_equal(report$message[2], "would empty phone")
  expect_length(connection$seen$updates, 0)
})

test_that("applying writes each correction with its reason", {
  connection <- fake_formshare()
  report <- suppressMessages(fs_apply_cleaning_log(connection, "FS_1", test_log(), dry_run = FALSE))
  expect_equal(report$status, c("applied", "applied", "applied", "no action"))
  expect_true(all(report$reason_kept[1:3]))
  updates <- connection$seen$updates
  expect_length(updates, 3)
  expect_equal(updates[[1]]$values, list(county = "2"))
  expect_equal(updates[[1]]$reason, "Cleaning log: wrong county")
  expect_true(is.na(updates[[2]]$values$phone)) # a blank_response empties
  expect_equal(updates[[3]]$reason, "Cleaning log: outlier (log distribution)")
})

test_that("an entry's own reason is used when the log has a reason column", {
  connection <- fake_formshare()
  log <- test_log()
  log$why <- c("The household moved", "", NA, NA)
  suppressMessages(fs_apply_cleaning_log(connection, "FS_1", log, dry_run = FALSE, reason_column = "why"))
  expect_equal(connection$seen$updates[[1]]$reason, "The household moved")
  expect_equal(connection$seen$updates[[2]]$reason, "Cleaning log: personal data")
})

test_that("a refused or an encrypted correction is reported, and the rest goes on", {
  connection <- fake_formshare(update_answer = function(rowuuid, values, reason) {
    if (rowuuid == "r2") stop("This API key don't have permission to clean the form")
    if ("members_n" %in% names(values)) {
      return(list(status = "OK", ignored = list("members_n"), reason_kept = TRUE))
    }
    list(status = "OK", reason_kept = FALSE)
  })
  report <- suppressMessages(fs_apply_cleaning_log(connection, "FS_1", test_log(), dry_run = FALSE))
  expect_equal(report$status[1:3], c("applied", "refused", "ignored"))
  expect_match(report$message[2], "permission to clean")
  expect_false(report$reason_kept[1])
})

test_that("a cleaning log with a problem writes nothing", {
  connection <- fake_formshare()
  log <- test_log()
  log$new_value[1] <- "9" # not a county
  expect_warning(
    report <- suppressMessages(fs_apply_cleaning_log(connection, "FS_1", log, dry_run = FALSE)),
    "nothing was written"
  )
  expect_equal(report$status, c("problem", "not sent", "not sent", "not sent"))
  expect_equal(report$message[1], "value_not_a_choice")
  expect_length(connection$seen$updates, 0)
})

test_that("a remove_survey is reported unless asked, and then deleted or moved", {
  log <- data.frame(uuid = "r3", question = NA, change_type = "remove_survey", new_value = NA,
                    issue = "duplicate", stringsAsFactors = FALSE)
  connection <- fake_formshare()
  report <- suppressMessages(fs_apply_cleaning_log(connection, "FS_1", log, dry_run = FALSE))
  expect_equal(report$status, "to remove in FormShare")
  expect_length(connection$seen$deletions, 0)

  report <- suppressMessages(fs_apply_cleaning_log(connection, "FS_1", log, dry_run = FALSE,
                                                   remove_survey = "delete"))
  expect_equal(report$status, "deleted")
  expect_equal(connection$seen$deletions[[1]]$reason, "Cleaning log: duplicate")
  expect_false(connection$seen$deletions[[1]]$move_to_logs)

  expect_error(
    fs_apply_cleaning_log(connection, "FS_1", log, dry_run = FALSE, remove_survey = "move_to_logs"),
    "assistant_uuid"
  )
  report <- suppressMessages(fs_apply_cleaning_log(connection, "FS_1", log, dry_run = FALSE,
                                                   remove_survey = "move_to_logs",
                                                   assistant = "ana"))
  expect_equal(report$status, "moved to the logs")
  expect_true(connection$seen$deletions[[2]]$move_to_logs)
  expect_equal(connection$seen$deletions[[2]]$assistant, "ana")
})

# --- reviewing ---------------------------------------------------------------------

test_that("the review compares the log with what FormShare recorded", {
  log <- data.frame(
    uuid = c("r3", "r2", "r1", "r3"),
    question = c("members_n", "county", "hh_id", NA),
    change_type = c("change_response", "change_response", "change_response", "remove_survey"),
    new_value = c("4", "2", "001", NA),
    stringsAsFactors = FALSE
  )
  review <- fs_review_cleaning(fake_formshare(), "FS_1", log)
  comment_of <- function(uuid, question) {
    review$comment[review$uuid == uuid & review$question %in% question]
  }
  # "4" and "4.0" are the same number: no comment for r3's members_n
  expect_length(comment_of("r3", "members_n"), 0)
  expect_equal(comment_of("r2", "county"), "New value in cleaning log and value in FormShare not matching")
  expect_equal(comment_of("r1", "hh_id"), "Changes were not applied")
  expect_equal(comment_of("r3", NA), "This survey should be deleted but it was not deleted")
  # made outside the log: who, when and why are kept
  outside <- review[review$comment == "Entry missing in cleaning log", ]
  expect_equal(outside$uuid, "r2")
  expect_equal(outside$question, "phone")
  expect_equal(outside$user, "maria")
  expect_equal(outside$reason, "Asked by the household")
  expect_equal(review$comment[review$uuid == "r9"], "Survey removed but missing in the cleaning log")
})

test_that("a review with nothing to say has no rows", {
  log <- data.frame(uuid = "r3", question = "members_n", change_type = "change_response",
                    new_value = "4", stringsAsFactors = FALSE)
  changes <- fs_change_log(fake_formshare(), "FS_1")
  changes <- changes[changes$uuid == "r3", ]
  expect_equal(nrow(fs_review_cleaning(fake_formshare(), "FS_1", log, changes = changes)), 0)
})

# --- PII ---------------------------------------------------------------------------

test_that("fs_check_pii adds what FormShare marks sensitive or encrypted", {
  connection <- fake_formshare()
  survey <- fs_survey(connection, "FS_1")
  data <- fs_read_data(connection, "FS_1")
  result <- fs_check_pii(data, survey)
  pii <- result$potential_PII
  expect_equal(pii$issue[pii$question == "income"], "Encrypted in FormShare")
  # phone is found by its name and is not listed twice
  expect_equal(sum(pii$question == "phone"), 1)
})

# --- the original checks on FormShare's data ---------------------------------------

test_that("the original checks run on FormShare's data and combine into one log", {
  connection <- fake_formshare()
  data <- fs_read_data(connection, "FS_1")
  survey <- fs_survey(connection, "FS_1")
  log <- suppressMessages(capture.output(
    checks <- data |>
      check_duplicate(uuid_column = "rowuuid") |>
      check_value(uuid_column = "rowuuid", values_to_look = c(99, 999)) |>
      check_logical(
        uuid_column = "rowuuid",
        check_to_perform = "members_n > 20",
        columns_to_clean = "members_n",
        description = "a very large household"
      ) |>
      create_combined_log() |>
      add_info_to_cleaning_log(
        dataset_uuid_column = "rowuuid",
        information_to_add = c("_submitted_by", "_submitted_date")
      )
  ))
  cleaning_log <- checks$cleaning_log
  expect_equal(cleaning_log$uuid, "r3")
  expect_equal(cleaning_log$question, "members_n")
  expect_equal(cleaning_log$`_submitted_by`, "ben")
  # and the log can go back to FormShare
  cleaning_log$change_type <- "change_response"
  cleaning_log$new_value <- "4"
  report <- suppressMessages(fs_apply_cleaning_log(connection, "FS_1", cleaning_log, survey = survey))
  expect_equal(report$status, "dry run")
})

# --- select multiples as one column per option --------------------------------------

test_that("fs_expand_select_multiple adds a column per option, NA when unanswered", {
  connection <- fake_formshare()
  survey <- fs_survey(connection, "FS_1")
  choices <- fs_choices(connection, "FS_1")
  data <- data.frame(rowuuid = c("r1", "r2", "r3"), crops = c("maize beans", "maize", NA))
  expanded <- fs_expand_select_multiple(data, survey, choices)
  expect_equal(expanded$crops.maize, c(TRUE, TRUE, NA))
  expect_equal(expanded$crops.beans, c(TRUE, FALSE, NA))
  # the answer itself is kept, for corrections
  expect_equal(expanded$crops, data$crops)
})

# --- the template ------------------------------------------------------------------

test_that("the FormShare template is valid R and is copied", {
  script <- system.file("formshare_cleaning_template", "cleaning_template.R", package = "cleaningtools")
  expect_true(file.exists(script))
  expect_no_error(parse(script))
  folder <- file.path(tempfile(), "cleaning")
  create_formshare_cleaning_template(folder)
  expect_true(file.exists(file.path(folder, "cleaning_template.R")))
  expect_true(file.exists(file.path(folder, "inputs", "check_list.csv")))
})
