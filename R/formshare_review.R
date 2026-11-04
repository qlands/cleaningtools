# Reviewing the cleaning against what FormShare recorded.
#
# cleaningtools reviews a cleaning by rebuilding the changes from two copies of
# the data, raw and clean (review_cleaning). FormShare keeps one copy and
# records each change as it is made, with who, when and why: the review reads
# that record and compares it with the cleaning log.

# Two answers as FormShare and a cleaning log write them: the same text, the
# same number ("2" and "2.0"), the same logical ("TRUE" and "1"), or both empty
fs_same_value <- function(a, b) {
  a <- as.character(a)
  b <- as.character(b)
  both_empty <- is.na(a) & is.na(b)
  same_text <- !is.na(a) & !is.na(b) & a == b
  number_a <- suppressWarnings(as.numeric(a))
  number_b <- suppressWarnings(as.numeric(b))
  same_number <- !is.na(number_a) & !is.na(number_b) & number_a == number_b
  truth <- function(x) dplyr::case_when(x %in% c("TRUE", "true", "1") ~ "T", x %in% c("FALSE", "false", "0") ~ "F")
  same_logical <- !is.na(truth(a)) & truth(a) == truth(b) & !is.na(truth(b))
  both_empty | same_text | same_number | same_logical
}

# The rows of one kind of discrepancy; a column given one value is repeated
# for every row, and none is a frame of no rows
review_rows <- function(uuid, question, change_type, log_new_value, formshare_new_value,
                        comment, user = NA_character_, date = NA, reason = NA_character_) {
  n <- length(uuid)
  data.frame(
    uuid = as.character(uuid),
    question = rep(as.character(question), length.out = n),
    change_type = rep(as.character(change_type), length.out = n),
    log_new_value = rep(as.character(log_new_value), length.out = n),
    formshare_new_value = rep(as.character(formshare_new_value), length.out = n),
    comment = rep(comment, length.out = n),
    user = rep(as.character(user), length.out = n),
    date = rep(as.POSIXct(date, tz = "UTC"), length.out = n),
    reason = rep(as.character(reason), length.out = n),
    stringsAsFactors = FALSE
  )
}

#' Review a cleaning against FormShare's record of changes
#'
#' Compares a cleaning log with the changes FormShare recorded
#' ([fs_change_log()]), as [review_cleaning()] compares it with a raw and a
#' clean dataset. Possible comments:
#' - Changes were not applied: no change of that answer was recorded
#' - New value in cleaning log and value in FormShare not matching
#' - This survey should be deleted but it was not deleted
#' - Entry missing in cleaning log: an answer was changed without an entry
#' - Survey removed but missing in the cleaning log
#'
#' A change made outside the log names who made it, when and why.
#'
#' @inheritParams fs_check_cleaning_log
#' @param since Only changes on or after this date-time count (for example
#' when the cleaning started), all by default
#' @param changes The changes, if already read with [fs_change_log()]
#' @return A data frame of the discrepancies: uuid, question, change_type,
#' log_new_value, formshare_new_value, comment, user, date, reason. No rows
#' when the log and FormShare agree.
#' @export
fs_review_cleaning <- function(connection, repository, cleaning_log,
                               uuid_column = "uuid",
                               question_column = "question",
                               change_type_column = "change_type",
                               new_value_column = "new_value",
                               change_response_value = "change_response",
                               blank_response_value = "blank_response",
                               no_change_value = c("no_action", "no_change"),
                               remove_survey_value = "remove_survey",
                               since = NULL,
                               changes = NULL) {
  if (is.null(changes)) {
    changes <- fs_change_log(connection, repository, since = since)
  }
  log <- as.data.frame(cleaning_log)
  entries <- data.frame(
    uuid = as.character(log[[uuid_column]]),
    question = trimws(as.character(log[[question_column]])),
    change_type = as.character(log[[change_type_column]]),
    new_value = trimws(as.character(log[[new_value_column]])),
    kind = fs_entry_kind(
      log[[change_type_column]], change_response_value, blank_response_value,
      no_change_value, remove_survey_value
    ),
    stringsAsFactors = FALSE
  )
  edits <- entries[entries$kind %in% c("change_response", "blank_response"), ]
  removals <- entries[entries$kind %in% "remove_survey", ]

  changed <- changes[changes$change_type %in% c("change_response", "blank_response"), ]
  # the last change of each answer is what FormShare holds now
  changed <- changed[order(changed$date), ]
  latest <- changed[!duplicated(paste(changed$uuid, changed$question), fromLast = TRUE), ]
  removed <- changes[changes$change_type %in% "remove_survey", ]

  issues <- list()

  # each edit of the log: recorded, and with the value it asked for
  at <- match(paste(edits$uuid, edits$question), paste(latest$uuid, latest$question))
  expected <- ifelse(edits$kind == "blank_response", NA_character_, edits$new_value)
  not_applied <- is.na(at)
  mismatch <- !not_applied & !fs_same_value(expected, latest$new_value[at])
  issues$not_applied <- review_rows(
    edits$uuid[not_applied], edits$question[not_applied], edits$change_type[not_applied],
    expected[not_applied], NA_character_, "Changes were not applied"
  )
  issues$mismatch <- review_rows(
    edits$uuid[mismatch], edits$question[mismatch], edits$change_type[mismatch],
    expected[mismatch], latest$new_value[at][mismatch],
    "New value in cleaning log and value in FormShare not matching",
    latest$user[at][mismatch], latest$date[at][mismatch], latest$reason[at][mismatch]
  )

  # each removal of the log: a deletion recorded
  not_removed <- !removals$uuid %in% removed$uuid
  issues$not_removed <- review_rows(
    removals$uuid[not_removed], NA_character_, removals$change_type[not_removed],
    NA_character_, NA_character_, "This survey should be deleted but it was not deleted"
  )

  # each change made outside the log
  outside <- !paste(latest$uuid, latest$question) %in% paste(edits$uuid, edits$question)
  issues$outside <- review_rows(
    latest$uuid[outside], latest$question[outside], latest$change_type[outside],
    NA_character_, latest$new_value[outside], "Entry missing in cleaning log",
    latest$user[outside], latest$date[outside], latest$reason[outside]
  )
  removed_outside <- removed[!removed$uuid %in% removals$uuid & !duplicated(removed$uuid), ]
  issues$removed_outside <- review_rows(
    removed_outside$uuid, NA_character_, removed_outside$change_type,
    NA_character_, NA_character_, "Survey removed but missing in the cleaning log",
    removed_outside$user, removed_outside$date, removed_outside$reason
  )

  review <- dplyr::bind_rows(issues)
  rownames(review) <- NULL
  review
}
