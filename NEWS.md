# cleaningtools 0.1.0 (FormShare)

This version adds FormShare to IMPACT Initiatives' cleaningtools 0.0.0.9003.

## FormShare

* `fs_connect()` logs in to a FormShare server with the FormShare R package.
* `fs_read_data()` reads a table of a form's repository as it is now, joining
  the continuation tables of a table FormShare split, with dates and
  date-times as R's types.
* `fs_survey()` and `fs_choices()` build, from FormShare's dictionary and
  lookups, the survey and choices sheets the functions taking `kobo_survey`
  and `kobo_choices` read. `fs_xlsform()` downloads and reads the form itself,
  for relevance and constraints.
* `fs_audit_list()` reads the ODK audits FormShare keeps into the list
  `create_audit_list()` builds from a zip.
* `fs_change_log()` reads FormShare's record of changes in the cleaning log's
  format.
* `fs_check_cleaning_log()` checks a filled cleaning log against FormShare,
  and `fs_apply_cleaning_log()` makes its corrections there through the
  cleaning API, each with its reason: a dry run first, nothing written while
  the log has a problem, a remove_survey listed unless asked to delete or move
  it to the logs.
* `fs_review_cleaning()` reviews the cleaning against FormShare's record of
  changes.
* `fs_check_pii()` adds the questions FormShare marks as sensitive or
  encrypted to `check_pii()`'s.
* `fs_expand_select_multiple()` adds a column per option of a select multiple,
  for the functions that read Kobo's.
* `fs_is_control_column()` tells FormShare's own columns from the questions.
* `create_formshare_cleaning_template()` copies a template of the whole
  workflow.

## Changes to the original functions

* `check_soft_duplicates()` gains `group_column`, to compare each survey only
  with its group's (for example the same enumerator's).
* `check_fcs()` given a list adds a log in the cleaning log's format, for
  `create_combined_log()`. Given a data frame it returns one, as before. Its
  message no longer has a typo.
* `create_clean_data()` keeps each column's type instead of guessing it again,
  so an identifier "007" stays "007"; a value that cannot take the column's
  type keeps it as text, with a warning. It accepts the `all_data` rows it
  applies.
* `add_duration()` reads date-times as FormShare gives them (POSIXct, or
  "2026-11-04 10:30:00"), as a real time difference.
* `check_outliers()` leaves out columns starting with an underscore, as
  FormShare's own do.
* The Excel cleaning log's colours come from a fixed palette instead of
  randomcoloR, which needed V8.
* `create_xlsx_cleaning_log()` with `use_dropdown = TRUE` no longer stops at
  an entry that names no question, such as a soft duplicate.
* The package depends on R 4.1, whose pipe (`|>`) its code uses; it declared
  R 4.0, on which it cannot be installed.
