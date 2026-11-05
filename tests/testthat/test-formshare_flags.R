# fs_flag_checks against the stand-in connection (helper-fake-formshare.R):
# what the checks found becomes Data Quality flags in FormShare.

test_that("what the checks found becomes flags, and every checked submission is posted", {
  fake <- fake_formshare()
  checks <- list(
    checked_dataset = FAKE_HOUSEHOLDS,
    potential_outliers = data.frame(
      uuid = "r3", question = "members_n", old_value = "40",
      issue = "Value seems different from interquartile range"
    ),
    soft_duplicate_log = data.frame(
      uuid = c("r1", "r2"), issue = "Less than 7 differents options", num_cols_not_NA = 9
    ),
    potential_PII = data.frame(uuid = "all", question = "phone", old_value = NA, issue = "Potential PII")
  )
  messages <- capture_messages(
    result <- fs_flag_checks(fake, "FS_x", checks,
      severity = c(potential_outliers = "major", soft_duplicate_log = "minor"),
      source = "script:test"
    )
  )
  expect_match(messages, "whole column", all = FALSE)
  post <- fake$seen$flag_posts[[1]]
  expect_equal(post$repository, "FS_x")
  expect_equal(post$source, "script:test")
  # the submissions with no entry go too: their earlier flags are resolved
  expect_setequal(post$checked, c("r1", "r2", "r3"))
  expect_equal(post$flags$rowuuid, c("r3", "r1", "r2"))
  expect_equal(post$flags$check[1], "value_seems_different_from_interquartile_range")
  expect_equal(post$flags$severity, c("major", "minor", "minor"))
  expect_equal(post$flags$fields, c("members_n", NA, NA))
  expect_equal(post$flags$value[1], "40")
  expect_equal(result$scores$score[result$scores$rowuuid == "r3"], 85)
})

test_that("a select multiple's option column is flagged as its question", {
  fake <- fake_formshare()
  log <- data.frame(uuid = "r1", question = "crops.maize", old_value = "1", issue = "Odd crop")
  result <- suppressMessages(fs_flag_checks(fake, "FS_x", log))
  expect_equal(result$flags$fields, "crops")
  # a log alone: the submissions it names are the ones checked
  expect_equal(fake$seen$flag_posts[[1]]$checked, "r1")
})

test_that("a logical check's id names its flags, and its severity", {
  fake <- fake_formshare()
  checks <- list(
    checked_dataset = FAKE_HOUSEHOLDS,
    logical_all = data.frame(
      uuid = "r3", question = "members_n", old_value = "40",
      issue = "More than 20 members", check_id = "many_members"
    )
  )
  result <- suppressMessages(fs_flag_checks(fake, "FS_x", checks, severity = c(many_members = "critical")))
  expect_equal(result$flags$check, "many_members")
  expect_equal(result$flags$severity, "critical")
})

test_that("an entry that is not a submission stops before anything is posted", {
  fake <- fake_formshare()
  log <- data.frame(uuid = "m1", question = "mem_age", old_value = "130", issue = "Too old")
  expect_error(suppressMessages(fs_flag_checks(fake, "FS_x", log)), "not submissions of FS_x: m1")
  expect_length(fake$seen$flag_posts, 0)
})

test_that("a check with no severity, or a severity that is none, is refused", {
  fake <- fake_formshare()
  checks <- list(
    checked_dataset = FAKE_HOUSEHOLDS,
    potential_outliers = data.frame(uuid = "r3", question = "members_n", issue = "Odd"),
    duplicate_log = data.frame(uuid = "r2", question = "hh_id", issue = "Duplicated")
  )
  expect_error(
    fs_flag_checks(fake, "FS_x", checks, severity = c(potential_outliers = "major")),
    "No severity for these checks: duplicate_log"
  )
  expect_error(fs_flag_checks(fake, "FS_x", checks, severity = "serious"), "critical, major or minor")
  expect_length(fake$seen$flag_posts, 0)
})

test_that("a dry run posts nothing, and a repeated entry is flagged once", {
  fake <- fake_formshare()
  checks <- list(
    checked_dataset = FAKE_HOUSEHOLDS,
    a_log = data.frame(uuid = c("r1", "r1"), question = "county", issue = "Odd county"),
    b_log = data.frame(uuid = "r1", question = "county", issue = "Odd county")
  )
  messages <- capture_messages(result <- fs_flag_checks(fake, "FS_x", checks, dry_run = TRUE))
  expect_match(messages, "repeat another's check", all = FALSE)
  expect_equal(nrow(result$flags), 1)
  expect_setequal(result$checked, c("r1", "r2", "r3"))
  expect_length(fake$seen$flag_posts, 0)
})

test_that("checks that find nothing post every checked submission clean", {
  fake <- fake_formshare()
  checks <- list(
    checked_dataset = FAKE_HOUSEHOLDS,
    potential_outliers = data.frame(uuid = character(0), question = character(0), issue = character(0))
  )
  suppressMessages(fs_flag_checks(fake, "FS_x", checks))
  post <- fake$seen$flag_posts[[1]]
  expect_equal(nrow(post$flags), 0)
  expect_setequal(post$checked, c("r1", "r2", "r3"))
})
