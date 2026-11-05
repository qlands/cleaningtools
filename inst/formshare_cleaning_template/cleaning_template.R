# Cleaning a FormShare form with cleaningtools
#
# The data, its questions and choices, and the ODK audits are read from
# FormShare as they are now. The corrections of the filled cleaning log are
# made in FormShare, each with its reason, and the review reads what FormShare
# recorded.
#
# The account: your API key and secret are in your FormShare profile.
# Writing corrections uses the API key of an assistant who may clean the form.
# Keep them out of the script, for example in ~/.Renviron:
#   FORMSHARE_URL=https://formshare.org
#   FORMSHARE_USER=my_user
#   FORMSHARE_API_KEY=...
#   FORMSHARE_API_SECRET=...
#   FORMSHARE_CLEANER_KEY=...

library(cleaningtools)

connection <- fs_connect(
  Sys.getenv("FORMSHARE_URL"),
  Sys.getenv("FORMSHARE_USER"),
  Sys.getenv("FORMSHARE_API_KEY"),
  Sys.getenv("FORMSHARE_API_SECRET"),
  assistant_api_key = Sys.getenv("FORMSHARE_CLEANER_KEY")
)

# The form's repository: its form_schema in connection$get_repositories()
repository <- "FS_xxxxxxxx_xxxx_xxxx_xxxx_xxxxxxxxxxxx"

# Reading ----------------------------------------------------------------------
raw_dataset <- fs_read_data(connection, repository) # the main record; a repeat: table = "..."
survey <- fs_survey(connection, repository) # the questions, as a survey sheet
choices <- fs_choices(connection, repository) # the choices, as a choices sheet

# Performing checks ------------------------------------------------------------
# A record is named by rowuuid, FormShare's row identifier, which the cleaning
# API takes back. _submitted_by is the assistant who sent it.
checks_logs <- raw_dataset |>
  check_duplicate(uuid_column = "rowuuid") |>
  check_soft_duplicates(
    kobo_survey = survey,
    uuid_column = "rowuuid",
    idnk_value = "dont_know",
    group_column = "_submitted_by" # each interview against the same assistant's others
  ) |>
  fs_check_pii(survey = survey) |>
  check_outliers(uuid_column = "rowuuid", kobo_survey = survey, kobo_choices = choices) |>
  check_value(uuid_column = "rowuuid")

## Duration
### From the form's start and end
checks_logs$checked_dataset <- checks_logs$checked_dataset |>
  add_duration(uuid_column = "rowuuid")

### Or from the ODK audit FormShare keeps, for a form with an audit question
# audits <- fs_audit_list(connection, repository, dataset = raw_dataset)
# checks_logs$checked_dataset <- checks_logs$checked_dataset |>
#   add_duration_from_audit(uuid_column = "rowuuid", audit_list = audits)

## Percentage missing, over the form's questions
types_in_form <- unique(stringr::word(survey$type, 1))
checks_logs$checked_dataset <- checks_logs$checked_dataset |>
  add_percentage_missing(
    kobo_survey = survey[survey$table == "maintable", ],
    type_to_include = intersect(c("integer", "date", "text", "select_one", "select_multiple"), types_in_form)
  )

## Others: the text questions that specify an "other"
text_oth <- survey$name[survey$type == "text" & survey$table == "maintable" &
  grepl("other", survey$name)]

## Logical checks
logical_list <- read.csv("inputs/check_list.csv")

checks_logs <- checks_logs |>
  check_duration(uuid_column = "rowuuid", column_to_check = "duration") |>
  check_percentage_missing(uuid_column = "rowuuid") |>
  check_others(uuid_column = "rowuuid", columns_to_check = text_oth) |>
  check_logical_with_list(
    uuid_column = "rowuuid",
    list_of_check = logical_list,
    check_id_column = "check_id",
    check_to_perform_column = "check_to_perform",
    columns_to_clean_column = "columns_to_clean",
    description_column = "description"
  )

# Flagging in FormShare ----------------------------------------------------------
# With FormShare's Data Quality plugin, what the checks found becomes flags on
# the submissions, and each one's Data Quality Score drops by the weights of
# its flags, where the team sees it. Run it again after the cleaning: the flags
# of what was fixed are resolved. It needs an account that owns, administers or
# edits the project.
flagged <- fs_flag_checks(connection, repository, checks_logs,
  severity = "minor" # or one for each check: c(potential_outliers = "major", ...)
)

# Creating the cleaning log ----------------------------------------------------
cleaning_log <- checks_logs |>
  create_combined_log() |>
  add_info_to_cleaning_log(
    dataset_uuid_column = "rowuuid",
    information_to_add = c("_submitted_by", "_submitted_date")
  )

cleaning_log |>
  create_xlsx_cleaning_log(
    use_dropdown = TRUE,
    kobo_survey = survey,
    kobo_choices = choices,
    output_path = "outputs/cleaning_log.xlsx"
  )

# Applying the filled cleaning log in FormShare --------------------------------
# The field team fills change_type and new_value (and, if you add one, a
# column with each entry's reason).
filled_cleaning_log <- readxl::read_excel("outputs/cleaning_log_filled.xlsx", sheet = "cleaning_log")

## Every entry checked against FormShare first
fs_check_cleaning_log(connection, repository, filled_cleaning_log, survey = survey, choices = choices)

## A dry run: what would be done
started <- Sys.time()
report <- fs_apply_cleaning_log(connection, repository, filled_cleaning_log,
  survey = survey, choices = choices
)

## The corrections, in FormShare. A remove_survey is listed, to remove on the
## form's Submissions page, unless remove_survey = "delete" or "move_to_logs".
report <- fs_apply_cleaning_log(connection, repository, filled_cleaning_log,
  survey = survey, choices = choices, dry_run = FALSE
)
write.csv(report, "outputs/applied_in_formshare.csv", row.names = FALSE)

# Review of the cleaning -------------------------------------------------------
## What FormShare recorded, against the log
cleaning_review <- fs_review_cleaning(connection, repository, filled_cleaning_log, since = started)
write.csv(cleaning_review, "outputs/cleaning_review.csv", row.names = FALSE)

## The others, against the form's relevance: the form itself from FormShare
form <- fs_xlsform(connection, repository)
clean_data <- fs_read_data(connection, repository) |>
  fs_expand_select_multiple(survey, choices)
others_review <- review_others(clean_data, uuid_column = "rowuuid", kobo_survey = form$survey)

## The sampling frame, if you have one
# sampling_frame <- read.csv("inputs/sampling_frame.csv")
# sampling_frame_review <- review_sample_frame_with_dataset(
#   sample_frame = sampling_frame,
#   sampling_frame_strata_column = "Neighbourhood",
#   sampling_frame_target_survey_column = "Total.no.of.HH",
#   clean_dataset = clean_data,
#   clean_dataset_strata_column = "neighbourhood",
#   consent_column = "consent",
#   consent_yes_value = "yes"
# )
