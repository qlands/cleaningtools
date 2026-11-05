# The fixes made to the original functions on the FormShare branch.

# --- create_clean_data keeps each column's type -----------------------------

test_that("a clean dataset keeps the raw dataset's column types", {
  raw <- data.frame(
    uuid = c("u1", "u2", "u3"),
    hh_id = c("007", "010", "011"), # an identifier: text, leading zeros kept
    members = c(4L, 6L, 3L),
    income = c(100.5, 200, 300),
    owns_land = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  log <- data.frame(
    uuid = c("u1", "u2", "u3"),
    question = c("members", "owns_land", "income"),
    change_type = c("change_response", "change_response", "blank_response"),
    new_value = c("5", "1", NA)
  )
  clean <- create_clean_data(
    raw_dataset = raw, raw_data_uuid_column = "uuid", cleaning_log = log,
    cleaning_log_question_column = "question",
    cleaning_log_new_value_column = "new_value",
    cleaning_log_change_type_column = "change_type"
  )
  expect_equal(clean$hh_id, c("007", "010", "011"))
  expect_type(clean$members, "integer")
  expect_equal(clean$members, c(5L, 6L, 3L))
  expect_equal(clean$owns_land, c(TRUE, TRUE, TRUE)) # "1" read as TRUE
  expect_equal(clean$income, c(100.5, 200, NA))
})

test_that("a fraction in an integer column makes it a double, not a cut", {
  raw <- data.frame(uuid = c("u1", "u2"), members = c(4L, 6L))
  log <- data.frame(uuid = "u1", question = "members", change_type = "change_response", new_value = "4.5")
  clean <- create_clean_data(
    raw_dataset = raw, raw_data_uuid_column = "uuid", cleaning_log = log,
    cleaning_log_question_column = "question",
    cleaning_log_new_value_column = "new_value",
    cleaning_log_change_type_column = "change_type"
  )
  expect_equal(clean$members, c(4.5, 6))
})

test_that("a value that cannot take the column's type is kept as text, with a warning", {
  raw <- data.frame(uuid = c("u1", "u2"), members = c(4L, 6L))
  log <- data.frame(uuid = "u1", question = "members", change_type = "change_response", new_value = "four")
  expect_warning(
    clean <- create_clean_data(
      raw_dataset = raw, raw_data_uuid_column = "uuid", cleaning_log = log,
      cleaning_log_question_column = "question",
      cleaning_log_new_value_column = "new_value",
      cleaning_log_change_type_column = "change_type"
    ),
    "members keeps text values"
  )
  expect_equal(clean$members, c("four", "6"))
})

test_that("an all_data row changes the whole column", {
  raw <- data.frame(uuid = c("u1", "u2"), unit = c("kg", "lb"))
  log <- data.frame(uuid = "all_data", question = "unit", change_type = "change_response", new_value = "kg")
  clean <- create_clean_data(
    raw_dataset = raw, raw_data_uuid_column = "uuid", cleaning_log = log,
    cleaning_log_question_column = "question",
    cleaning_log_new_value_column = "new_value",
    cleaning_log_change_type_column = "change_type"
  )
  expect_equal(clean$unit, c("kg", "kg"))
})

# --- add_duration reads FormShare's date-times ------------------------------

test_that("add_duration reads FormShare's date-times, across midnight", {
  data <- data.frame(
    uuid = c("a", "b"),
    start = c("2026-11-03 23:50:00", "2026-11-04 09:00:00"),
    end = c("2026-11-04 00:35:30", "2026-11-04 09:42:00")
  )
  out <- add_duration(data)
  expect_equal(out$duration, c(45.5, 42))
  expect_equal(as.numeric(out$days_diff), c(1, 0))
  expect_equal(out$start_date, c("2026-11-03", "2026-11-04"))
})

test_that("add_duration reads POSIXct date-times", {
  data <- data.frame(
    uuid = "a",
    start = as.POSIXct("2026-11-04 09:00:00", tz = "UTC"),
    end = as.POSIXct("2026-11-04 10:15:00", tz = "UTC")
  )
  expect_equal(add_duration(data)$duration, 75)
})

# --- check_outliers leaves the platforms' own columns alone -----------------

test_that("check_outliers does not check FormShare's control columns", {
  set.seed(1)
  data <- data.frame(
    uuid = paste0("u", 1:50),
    `_cellxpos` = c(rep(1, 49), 1000),
    `_dqscore` = c(rep(100, 49), 0),
    income = c(rep(100, 49), 100000),
    check.names = FALSE
  )
  outliers <- suppressMessages(capture.output(
    result <- check_outliers(data, uuid_column = "uuid")
  ))
  checked <- unique(result$potential_outliers$question)
  expect_true("income" %in% checked)
  expect_false(any(c("_cellxpos", "_dqscore") %in% checked))
})

# --- check_fcs goes into the combined log -----------------------------------

test_that("check_fcs given the pipeline's list adds a log in the cleaning log's format", {
  data <- data.frame(
    uuid = c("u1", "u2"),
    c1 = c(7, 3), c2 = c(7, 2), c3 = c(7, 1), c4 = c(7, 0),
    c5 = c(7, 5), c6 = c(7, 6), c7 = c(7, 4), c8 = c(7, 2)
  )
  result <- suppressWarnings(check_fcs(
    list(checked_dataset = data),
    uuid_column = "uuid",
    cereals_column = "c1", pulses_column = "c2", dairy_column = "c3", meat_column = "c4",
    vegetables_column = "c5", fruits_column = "c6", oil_column = "c7", sugar_column = "c8"
  ))
  expect_named(result, c("checked_dataset", "fcs_log"))
  expect_equal(names(result$fcs_log), c("uuid", "question", "old_value", "issue"))
  expect_equal(unique(result$fcs_log$uuid), "u1")
  expect_equal(nrow(result$fcs_log), 8)
  combined <- create_combined_log(result)
  expect_equal(nrow(combined$cleaning_log), 8)
})

# --- check_soft_duplicates compares within a group ---------------------------

test_that("check_soft_duplicates can compare each survey within its group only", {
  # the questions only: the record's uuid is not a question of the form
  survey <- data.frame(
    type = c("select_one yn", "select_one yn", "integer"),
    name = c("q1", "q2", "q3")
  )
  data <- data.frame(
    uuid = c("a1", "a2", "b1", "b2", "c1"),
    enumerator = c("ana", "ana", "ben", "ben", "carl"),
    q1 = c("yes", "yes", "yes", "no", "yes"),
    q2 = c("no", "no", "no", "yes", "no"),
    q3 = c("4", "4", "4", "9", "4")
  )
  result <- check_soft_duplicates(
    data,
    kobo_survey = survey, uuid_column = "uuid", threshold = 0,
    return_all_results = TRUE, group_column = "enumerator"
  )[["soft_duplicate_log"]]
  # carl has one survey: nothing to compare with, left out
  expect_setequal(result$uuid, c("a1", "a2", "b1", "b2"))
  # a1 and a2 are the same within ana's surveys; b1 is compared only with b2
  expect_equal(result$id_most_similar_survey[result$uuid == "a1"], "a2")
  expect_equal(result$id_most_similar_survey[result$uuid == "b1"], "b2")
  expect_true(all(result$number_different_columns[result$group == "ana"] == 0))
  expect_true(all(result$number_different_columns[result$group == "ben"] > 0))
})

# --- create_xlsx_cleaning_log's dropdowns and an entry with no question ------

test_that("the dropdowns pass over an entry that names no question", {
  survey <- data.frame(type = c("select_one yn", "integer"), name = c("q1", "q2"))
  choices <- data.frame(list_name = "yn", name = c("yes", "no"), label = c("Yes", "No"))
  log <- list(
    checked_dataset = data.frame(uuid = c("a1", "a2"), q1 = c("yes", "no"), q2 = c(4, 5)),
    cleaning_log = data.frame(
      uuid = c("a1", "a2"),
      question = c("q1", NA), # a2 is a soft duplicate: the whole survey
      old_value = c("yes", NA),
      issue = c("To confirm", "Less than 7 differents options"),
      change_type = NA_character_,
      new_value = NA_character_,
      check_binding = c("q1 ~/~ a1", "NA ~/~ a2")
    )
  )
  # it used to stop with "missing value where TRUE/FALSE needed"
  workbook <- create_xlsx_cleaning_log(
    log,
    use_dropdown = TRUE, kobo_survey = survey, kobo_choices = choices
  )
  rules <- workbook$worksheets[[which(names(workbook) == "cleaning_log")]]$dataValidationsLst
  new_value <- openxlsx::int2col(which(names(log$cleaning_log) == "new_value"))
  # q1's new value offers its choices; the soft duplicate's offers nothing
  expect_true(any(grepl(paste0("<xm:sqref>", new_value, "2:", new_value, "2<"), rules)))
  expect_false(any(grepl(paste0("<xm:sqref>", new_value, "3:"), rules)))
})
