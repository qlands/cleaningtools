# A stand-in for a FormShare connection (the FormShare R package's
# FormShare object): it answers get_tables, get_fields, execute,
# update_record, delete_submission and get_xlsform from a small fake
# repository, and keeps what it was asked, so the fs_* functions are tested
# without a server.
#
# The repository: a household form whose main table is split in two
# (maintable, maintable_ext1), a repeat (members), two lists of choices
# (lkpcounty, lkpcrops), a select multiple (crops), an ODK audit and a change
# log.

fake_fields <- function(...) {
  rows <- list(...)
  data.frame(
    field_name = vapply(rows, `[[`, "", "name"),
    field_type = vapply(rows, function(r) r$type %||% "varchar(255)", ""),
    field_desc = vapply(rows, function(r) r$desc %||% r$name, ""),
    field_key = vapply(rows, function(r) isTRUE(r$key), TRUE),
    field_rtable = vapply(rows, function(r) r$rtable %||% "NA", ""),
    field_rfield = vapply(rows, function(r) r$rfield %||% "NA", ""),
    field_rlookup = vapply(rows, function(r) !is.null(r$rtable) && startsWith(r$rtable, "lkp"), TRUE),
    field_rfilter = "NA",
    field_sensitive = vapply(rows, function(r) isTRUE(r$sensitive), TRUE),
    field_encrypted = vapply(rows, function(r) isTRUE(r$encrypted), TRUE),
    field_generated = FALSE,
    field_odktype = vapply(rows, function(r) r$odk %||% "NA", ""),
    field_xmlcode = vapply(rows, function(r) r$xpath %||% "NA", ""),
    stringsAsFactors = FALSE
  )
}

FAKE_TABLES <- data.frame(
  table_name = c(
    "maintable", "maintable_ext1", "members", "lkpcounty", "lkpcrops",
    "maintable_msel_crops", "odk_audit", "audit_log"
  ),
  table_desc = "a table",
  table_type = c(
    "DATA TABLE", "DATA TABLE", "DATA TABLE", "LOOKUP TABLE", "LOOKUP TABLE",
    "MULTISELECT TABLE", "AUDIT TABLE", "CHANGE LOG TABLE"
  ),
  table_continues = c(NA, "maintable", NA, NA, NA, NA, NA, NA),
  stringsAsFactors = FALSE
)

FAKE_FIELDS <- list(
  maintable = fake_fields(
    list(name = "surveyid"),
    list(name = "_submitted_by"),
    list(name = "_submitted_date", type = "datetime"),
    list(name = "_dqscore", type = "decimal(10,0)"),
    list(name = "hh_id", odk = "text", key = TRUE, xpath = "hh/hh_id"),
    list(name = "county", odk = "select one", rtable = "lkpcounty", rfield = "county_cod", xpath = "hh/county"),
    list(name = "crops", odk = "select all that apply", xpath = "hh/crops"),
    list(name = "members_n", odk = "integer", type = "int(9)", xpath = "hh/members_n"),
    list(name = "school", odk = "select one", rtable = "FS_schools.maintable", rfield = "rowuuid",
         xpath = "hh/school"),
    list(name = "phone", odk = "text", sensitive = TRUE, xpath = "hh/phone"),
    list(name = "income", odk = "decimal", type = "decimal(10,3)", encrypted = TRUE, xpath = "hh/income"),
    list(name = "start", odk = "start", type = "datetime"),
    list(name = "end", odk = "end", type = "datetime"),
    list(name = "rowuuid")
  ),
  maintable_ext1 = fake_fields(
    list(name = "rowuuid"),
    list(name = "link_rowuuid"),
    list(name = "notes", odk = "text", xpath = "hh/notes")
  ),
  members = fake_fields(
    list(name = "hh_id", rtable = "maintable", rfield = "hh_id", key = TRUE),
    list(name = "members_rowid", key = TRUE),
    list(name = "mem_age", odk = "integer", type = "int(9)", xpath = "hh/members/mem_age"),
    list(name = "rowuuid")
  ),
  lkpcounty = fake_fields(
    list(name = "county_cod", key = TRUE), list(name = "county_des"), list(name = "rowuuid")
  ),
  lkpcrops = fake_fields(
    list(name = "crops_cod", key = TRUE), list(name = "crops_des"), list(name = "rowuuid")
  ),
  maintable_msel_crops = fake_fields(
    list(name = "hh_id", rtable = "maintable", rfield = "hh_id", key = TRUE),
    list(name = "crops", rtable = "lkpcrops", rfield = "crops_cod", key = TRUE),
    list(name = "rowuuid")
  ),
  audit_log = fake_fields(
    list(name = "audit_id"), list(name = "audit_date", type = "timestamp(6)"),
    list(name = "audit_action"), list(name = "audit_user"), list(name = "audit_table"),
    list(name = "audit_column"), list(name = "audit_key"), list(name = "audit_oldvalue"),
    list(name = "audit_newvalue"), list(name = "audit_insdeldata"), list(name = "audit_reason")
  )
)

FAKE_HOUSEHOLDS <- data.frame(
  surveyid = c("s1", "s2", "s3"),
  `_submitted_by` = c("ana", "ana", "ben"),
  `_submitted_date` = c("2026-11-01 10:00:00", "2026-11-01 11:00:00", "2026-11-02 09:00:00"),
  `_dqscore` = c(100, 90, 100),
  hh_id = c("007", "008", "009"),
  county = c("1", "2", "1"),
  crops = c("maize beans", "maize", "beans"),
  members_n = c(4, 6, 40),
  phone = c("555-1", "555-2", "555-3"),
  income = c("fsenc:x", "fsenc:y", "fsenc:z"),
  start = c("2026-11-01 09:30:00", "2026-11-01 10:20:00", "2026-11-02 08:15:00"),
  end = c("2026-11-01 10:00:00", "2026-11-01 11:00:00", "2026-11-02 09:00:00"),
  rowuuid = c("r1", "r2", "r3"),
  notes = c("ok", NA, "check"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

FAKE_CHOICES <- data.frame(
  list_name = c("county", "county", "crops", "crops"),
  name = c("1", "2", "maize", "beans"),
  label = c("North", "South", "Maize", "Beans"),
  stringsAsFactors = FALSE
)

FAKE_AUDIT <- data.frame(
  fs_uuid = c("r1", "r1", "r1", "r2", "r2"),
  event = c("form start", "question", "question", "form start", "question"),
  node = c(NA, "/data/hh/hh_id", "/data/hh/members_n", NA, "/data/hh/hh_id"),
  start = c(1000, 1000, 61000, 5000, 5000),
  end = c(NA, 61000, 181000, NA, 125000),
  latitude = NA, longitude = NA, accuracy = NA,
  old_value = NA, new_value = NA, user = NA, change_reason = NA,
  stringsAsFactors = FALSE
)

FAKE_CHANGES <- data.frame(
  uuid = c("r3", "r2", "r1", "r2", "r9"),
  table = "maintable",
  question = c("members_n", "county", "_dqscore", "phone", NA),
  old_value = c("40", "2", "100", "555-2", NA),
  new_value = c("4.0", "1", "90", NA, NA),
  action = c("UPDATE", "UPDATE", "UPDATE", "UPDATE", "DELETE"),
  user = c("cleaner", "cleaner", "dq", "maria", "carlos"),
  date = c("2026-11-03 10:00:00", "2026-11-03 10:05:00", "2026-11-03 10:06:00",
           "2026-11-03 11:00:00", "2026-11-03 12:00:00"),
  reason = c("Cleaning log: outlier", "Cleaning log: wrong county", NA, "Asked by the household", "Duplicate"),
  stringsAsFactors = FALSE
)

# The connection: a list of functions sharing an environment that records
# what they were asked
fake_formshare <- function(update_answer = function(rowuuid, values, reason) {
                             list(status = "OK", message = "Update completed", reason_kept = TRUE)
                           }) {
  seen <- new.env()
  seen$sql <- character(0)
  seen$updates <- list()
  seen$deletions <- list()
  connection <- list(
    seen = seen,
    get_tables = function(repository) FAKE_TABLES,
    get_fields = function(repository, table) {
      fields <- FAKE_FIELDS[[table]]
      if (is.null(fields)) data.frame() else fields
    },
    execute = function(repository, sql, async = FALSE, ...) {
      seen$sql <- c(seen$sql, sql)
      if (grepl("FROM `odk_audit`", sql, fixed = TRUE)) {
        return(FAKE_AUDIT)
      }
      if (grepl("FROM `audit_log`", sql, fixed = TRUE)) {
        changes <- FAKE_CHANGES
        if (grepl("`audit_action` <> 'INSERT'", sql, fixed = TRUE)) {
          changes <- changes[changes$action != "INSERT", ]
        }
        changes$epoch <- as.numeric(as.POSIXct(changes$date, tz = "UTC"))
        changes$date <- NULL
        return(changes)
      }
      if (grepl("UNION ALL", sql, fixed = TRUE) || grepl("AS list_name", sql, fixed = TRUE)) {
        return(FAKE_CHOICES)
      }
      if (grepl("SELECT `rowuuid` FROM", sql, fixed = TRUE)) {
        table <- sub("^SELECT `rowuuid` FROM `([^`]+)`.*$", "\\1", sql)
        asked <- regmatches(sql, gregexpr("'[^']*'", sql))[[1]]
        asked <- gsub("'", "", asked)
        known <- if (table == "maintable") FAKE_HOUSEHOLDS$rowuuid else c("m1", "m2")
        return(data.frame(rowuuid = intersect(asked, known)))
      }
      if (grepl("FROM `maintable` t", sql, fixed = TRUE)) {
        return(FAKE_HOUSEHOLDS)
      }
      stop("the fake server cannot answer: ", sql)
    },
    update_record = function(rowuuid, values, reason = NULL, assistant_api_key = NULL) {
      seen$updates[[length(seen$updates) + 1]] <- list(rowuuid = rowuuid, values = values, reason = reason)
      update_answer(rowuuid, values, reason)
    },
    delete_submission = function(repository, rowuuid, reason = NULL, move_to_logs = FALSE,
                                 assistant = NULL, assistant_uuid = NULL) {
      seen$deletions[[length(seen$deletions) + 1]] <- list(
        rowuuid = rowuuid, reason = reason, move_to_logs = move_to_logs,
        assistant = assistant, assistant_uuid = assistant_uuid
      )
      invisible(TRUE)
    },
    get_xlsform = function(repository, destfile = NULL) {
      path <- tempfile(fileext = ".xlsx")
      openxlsx::write.xlsx(
        list(
          survey = data.frame(type = c("select_one county", "text"), name = c("county", "county_other"),
                              relevant = c(NA, "${county} = 'other'")),
          choices = data.frame(list_name = "county", name = c("1", "other"), label = c("North", "Other")),
          Settings = data.frame(form_id = "household")
        ),
        path
      )
      path
    }
  )
  connection
}

`%||%` <- function(x, y) if (is.null(x)) y else x
