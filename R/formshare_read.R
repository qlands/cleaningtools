# Reading a FormShare repository into the shapes cleaningtools works with.
#
# A FormShare form has a repository: a MySQL schema with a table for the main
# record (maintable), one for each repeat, a lookup table for each list of
# choices (lkp<list>) and a table for each select multiple
# (<table>_msel_<question>). FormShare also keeps each submission's ODK audit
# (odk_audit) and every change made to the data (audit_log). These functions
# read them through the FormShare R package, live, instead of an export:
# what the checks see is what the server holds.

#' Connect to FormShare
#'
#' Creates a connection with the FormShare R package and logs in. Reading uses
#' your account; writing corrections ([fs_apply_cleaning_log()]) uses the API
#' key of an assistant who may clean the form.
#'
#' @param server_url The FormShare server, for example "https://formshare.org"
#' @param user_id Your user ID in FormShare
#' @param api_key Your API key
#' @param api_secret Your API secret
#' @param assistant_api_key The API key of an assistant who may clean the form,
#' used to write corrections (optional)
#'
#' @return A logged-in `FormShare` connection
#' @export
#' @examplesIf interactive()
#' connection <- fs_connect("https://formshare.org", "my_user", "my_key", "my_secret")
fs_connect <- function(server_url, user_id, api_key, api_secret, assistant_api_key = "") {
  connection <- FormShare::FormShare$new(
    server_url = server_url,
    user_id = user_id,
    api_key = api_key,
    api_secret = api_secret,
    assistant_api_key = assistant_api_key
  )
  if (!isTRUE(connection$login())) {
    stop("Could not log in to ", server_url, ": check the API key and secret")
  }
  connection
}

# FormShare's own columns in a data table: not questions of the form, and not
# for anyone to edit. They all start with an underscore, or are one of these.
FS_ROW_COLUMNS <- c("surveyid", "originid", "rowuuid", "root_rowuuid", "parent_rowuuid", "link_rowuuid")

#' FormShare's own columns
#'
#' Tells which of the given column names are FormShare's own (the row's
#' identifiers, who submitted it and when, its location, its data quality
#' score...) rather than questions of the form. They start with an underscore,
#' or are one of `surveyid`, `originid`, `rowuuid`, `root_rowuuid`,
#' `parent_rowuuid`, `link_rowuuid`.
#'
#' @param names Column names
#' @return A logical vector, TRUE for FormShare's own columns
#' @export
#' @examples
#' fs_is_control_column(c("rowuuid", "_submitted_by", "age"))
fs_is_control_column <- function(names) {
  startsWith(names, "_") | names %in% FS_ROW_COLUMNS
}

# A table or column name in SQL, in backticks: FormShare accepts names that
# are reserved words ("select", "from").
sql_name <- function(name) {
  paste0("`", gsub("`", "``", name, fixed = TRUE), "`")
}

# A text value in SQL. MySQL reads a backslash as an escape in a string, so it
# is doubled along with the quote.
sql_text <- function(value) {
  value <- gsub("\\", "\\\\", value, fixed = TRUE)
  paste0("'", gsub("'", "''", value, fixed = TRUE), "'")
}

# The answer of a listing, empty or not, as a data frame
as_listing <- function(x) {
  if (is.null(x) || length(x) == 0) data.frame() else as.data.frame(x)
}

#' Read a table of a FormShare repository
#'
#' Reads the rows of a table (the main record by default, or a repeat) as a
#' data frame. A table split by FormShare into continuations
#' (maintable_ext1...) is read as one, joined row by row. Dates and date-times
#' are converted to R's types.
#'
#' The row's identifier is `rowuuid`: it is what a cleaning log names and what
#' FormShare's cleaning API takes. `_submitted_by` is the assistant who sent the
#' submission, and `_submitted_date` when.
#'
#' @param connection A logged-in FormShare connection ([fs_connect()])
#' @param repository The repository (its schema, `form_schema` in
#' `connection$get_repositories()`)
#' @param table The table, "maintable" by default
#' @param join_continuations TRUE (default) to join the table's continuations
#' @param async TRUE to run the query in the background on the server, for a
#' large table
#' @return A data frame
#' @export
#' @examplesIf interactive()
#' connection <- fs_connect("https://formshare.org", "my_user", "my_key", "my_secret")
#' households <- fs_read_data(connection, "FS_xxxx")
#' members <- fs_read_data(connection, "FS_xxxx", table = "members")
fs_read_data <- function(connection, repository, table = "maintable",
                         join_continuations = TRUE, async = FALSE) {
  fields <- as_listing(connection$get_fields(repository, table))
  if (nrow(fields) == 0) {
    stop(glue::glue("{table} is not a table of {repository}"))
  }
  sql <- glue::glue("SELECT t.* FROM {sql_name(table)} t")
  if (join_continuations) {
    tables <- as_listing(connection$get_tables(repository))
    continuations <- if ("table_continues" %in% names(tables)) {
      tables$table_name[!is.na(tables$table_continues) & tables$table_continues == table]
    } else {
      character(0)
    }
    taken <- fields$field_name
    for (i in seq_along(continuations)) {
      continuation <- continuations[i]
      extra <- as_listing(connection$get_fields(repository, continuation))
      extra <- extra[!fs_is_control_column(extra$field_name) & !extra$field_name %in% taken, ]
      alias <- paste0("c", i)
      if (nrow(extra) > 0) {
        sql <- sub(
          " FROM ",
          paste0(", ", paste0(alias, ".", sql_name(extra$field_name), collapse = ", "), " FROM "),
          sql,
          fixed = TRUE
        )
      }
      sql <- paste0(
        sql, " LEFT JOIN ", sql_name(continuation), " ", alias,
        " ON ", alias, ".`link_rowuuid` = t.`rowuuid`"
      )
      taken <- c(taken, extra$field_name)
      fields <- dplyr::bind_rows(fields, extra)
    }
  }
  data <- connection$execute(repository, sql, async = async)
  fs_convert_types(data, fields)
}

# Dates and date-times as R's types, by the column's type in FormShare. Older
# servers send a DECIMAL as text: it is made a number.
fs_convert_types <- function(data, fields) {
  if (nrow(data) == 0) {
    return(data)
  }
  types <- stats::setNames(fields$field_type, fields$field_name)
  for (column in intersect(names(data), names(types))) {
    type <- tolower(sub("\\(.*$", "", types[[column]]))
    if (type == "datetime") {
      data[[column]] <- as.POSIXct(data[[column]], tz = "UTC")
    } else if (type == "date") {
      data[[column]] <- as.Date(data[[column]])
    } else if (type %in% c("decimal", "double", "float") && is.character(data[[column]])) {
      data[[column]] <- suppressWarnings(as.numeric(data[[column]]))
    }
  }
  data
}

# The question types of FormShare's dictionary, as an XLSForm names them
fs_xlsform_type <- function(odk_type, list_name) {
  odk_type <- tolower(odk_type)
  dplyr::case_when(
    # a select with no lookup names cases of another form (a case link)
    odk_type == "select one" & is.na(list_name) ~ "select_one",
    odk_type == "select all that apply" & is.na(list_name) ~ "select_multiple",
    odk_type == "select one" ~ paste("select_one", list_name),
    odk_type == "select all that apply" ~ paste("select_multiple", list_name),
    odk_type == "datetime" ~ "dateTime",
    TRUE ~ gsub(" ", "_", odk_type)
  )
}

#' The questions of a FormShare repository, as an XLSForm's survey sheet
#'
#' Builds, from FormShare's dictionary, a data frame with the columns the
#' functions taking a `kobo_survey` read: `type` ("integer", "select_one
#' county"...) and `name`, with `label`. It adds what FormShare knows of each
#' question: its `table` (the main record or a repeat), its `list_name`, its
#' `xpath` in the form, and whether it is marked `sensitive` or `encrypted`.
#' Only questions are listed, not FormShare's own columns.
#'
#' The dictionary has no relevance or constraint: for those, read the form
#' itself with [fs_xlsform()].
#'
#' @param connection A logged-in FormShare connection
#' @param repository The repository
#' A question that links to the cases of another form (a follow-up's case
#' link) is a `select_one` with no `list_name`; `references` names the table
#' it points to. `key` marks the questions that identify a record, which
#' FormShare does not change.
#'
#' @return A data frame: type, name, label, table, list_name, xpath, sensitive,
#' encrypted, key, references, odk_type
#' @export
fs_survey <- function(connection, repository) {
  tables <- as_listing(connection$get_tables(repository))
  data_tables <- tables$table_name[tables$table_type == "DATA TABLE"]
  continues <- if ("table_continues" %in% names(tables)) {
    stats::setNames(tables$table_continues, tables$table_name)
  } else {
    stats::setNames(rep(NA_character_, nrow(tables)), tables$table_name)
  }
  msel_tables <- tables$table_name[tables$table_type == "MULTISELECT TABLE"]

  rows <- list()
  for (table in data_tables) {
    fields <- as_listing(connection$get_fields(repository, table))
    if (nrow(fields) == 0 || !"field_odktype" %in% names(fields)) {
      next
    }
    questions <- fields[!is.na(fields$field_odktype) &
      !fields$field_odktype %in% c("NA", "") &
      !fs_is_control_column(fields$field_name), ]
    if (nrow(questions) == 0) {
      next
    }
    # a continuation's questions belong to the table it continues
    owner <- if (!is.na(continues[[table]])) continues[[table]] else table
    list_name <- ifelse(
      !is.na(questions$field_rtable) & startsWith(questions$field_rtable, "lkp"),
      sub("^lkp", "", questions$field_rtable),
      NA_character_
    )
    # a select multiple's list is the lookup of its <table>_msel_<question>
    for (i in which(tolower(questions$field_odktype) == "select all that apply")) {
      msel <- paste0(table, "_msel_", questions$field_name[i])
      if (!msel %in% msel_tables) {
        msel <- paste0(owner, "_msel_", questions$field_name[i])
      }
      if (msel %in% msel_tables) {
        msel_fields <- as_listing(connection$get_fields(repository, msel))
        lookup <- msel_fields$field_rtable[msel_fields$field_name == questions$field_name[i]]
        if (length(lookup) == 1 && !is.na(lookup) && startsWith(lookup, "lkp")) {
          list_name[i] <- sub("^lkp", "", lookup)
        }
      }
    }
    rows[[table]] <- data.frame(
      type = fs_xlsform_type(questions$field_odktype, list_name),
      name = questions$field_name,
      label = questions$field_desc,
      table = owner,
      list_name = list_name,
      xpath = questions$field_xmlcode,
      sensitive = as.logical(questions$field_sensitive),
      encrypted = if ("field_encrypted" %in% names(questions)) as.logical(questions$field_encrypted) else FALSE,
      key = as.logical(questions$field_key),
      references = ifelse(
        !is.na(questions$field_rtable) & questions$field_rtable != "NA" & !startsWith(questions$field_rtable, "lkp"),
        questions$field_rtable,
        NA_character_
      ),
      odk_type = questions$field_odktype,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) {
    return(data.frame(
      type = character(0), name = character(0), label = character(0),
      table = character(0), list_name = character(0), xpath = character(0),
      sensitive = logical(0), encrypted = logical(0), key = logical(0),
      references = character(0), odk_type = character(0)
    ))
  }
  survey <- dplyr::bind_rows(rows)
  rownames(survey) <- NULL
  survey
}

#' The choices of a FormShare repository, as an XLSForm's choices sheet
#'
#' Reads every list of choices from FormShare's lookup tables, in one query:
#' `list_name`, `name` (the code stored in the data) and `label`. With
#' [fs_survey()], it gives the functions that take a `kobo_choices` what they
#' read, for example the dropdowns of [create_xlsx_cleaning_log()].
#'
#' @param connection A logged-in FormShare connection
#' @param repository The repository
#' @return A data frame: list_name, name, label
#' @export
fs_choices <- function(connection, repository) {
  tables <- as_listing(connection$get_tables(repository))
  lookups <- tables$table_name[tables$table_type == "LOOKUP TABLE"]
  if (length(lookups) == 0) {
    return(data.frame(list_name = character(0), name = character(0), label = character(0)))
  }
  # lkp<list> holds <list>_cod, the code, and <list>_des, its label
  lists <- sub("^lkp", "", lookups)
  sql <- paste(
    glue::glue(
      "SELECT {sql_text(lists)} AS list_name, CAST({sql_name(paste0(lists, '_cod'))} AS CHAR) AS name, ",
      "{sql_name(paste0(lists, '_des'))} AS label FROM {sql_name(lookups)}"
    ),
    collapse = " UNION ALL "
  )
  choices <- connection$execute(repository, sql)
  if (nrow(choices) == 0) {
    return(data.frame(list_name = character(0), name = character(0), label = character(0)))
  }
  choices[, c("list_name", "name", "label")]
}

#' The XLSForm of a FormShare repository
#'
#' Downloads the form from FormShare and reads its sheets. For a form merged
#' into several versions, the newest, which has every question the repository
#' keeps. Use it where the dictionary is not enough: relevance (as
#' [review_others()] and [create_logic_for_other()] read it), constraints,
#' labels in each language.
#'
#' @param connection A logged-in FormShare connection
#' @param repository The repository
#' @return A list of data frames, one per sheet, named in lower case: survey,
#' choices, settings...
#' @export
fs_xlsform <- function(connection, repository) {
  path <- connection$get_xlsform(repository)
  on.exit(unlink(path), add = TRUE)
  sheets <- readxl::excel_sheets(path)
  form <- lapply(sheets, function(sheet) {
    as.data.frame(readxl::read_excel(path, sheet = sheet, col_types = "text"))
  })
  stats::setNames(form, tolower(sheets))
}

#' Read the ODK audits of a FormShare repository
#'
#' FormShare keeps the audit Collect sends with each submission of a form that
#' has an `audit` question. This reads them into the list
#' [create_audit_list()] builds from a zip of audit.csv files: one data frame
#' per submission, named by its uuid (the row's `rowuuid`), with ODK's columns
#' (event, node, start, end, ...). [add_duration_from_audit()] and the other
#' functions taking an `audit_list` use it as it is.
#'
#' @param connection A logged-in FormShare connection
#' @param repository The repository
#' @param dataset Optional: the data the audits are for. Only its submissions
#' are kept, and one without an audit gets an empty data frame.
#' @param uuid_column The uuid column of `dataset`, "rowuuid" by default
#' @param async TRUE to read in the background on the server
#' @return A named list of data frames
#' @export
fs_audit_list <- function(connection, repository, dataset = NULL, uuid_column = "rowuuid",
                          async = FALSE) {
  tables <- as_listing(connection$get_tables(repository))
  if (!"odk_audit" %in% tables$table_name) {
    stop(glue::glue("{repository} has no ODK audit: its form has no audit question"))
  }
  sql <- paste(
    "SELECT m.`rowuuid` AS fs_uuid, a.`event`, a.`node`, a.`start`, a.`end`,",
    "a.`latitude`, a.`longitude`, a.`accuracy`, a.`old_value`, a.`new_value`,",
    "a.`user`, a.`change_reason`",
    "FROM `odk_audit` a JOIN `maintable` m ON m.`surveyid` = a.`submission_id`",
    "ORDER BY m.`rowuuid`, a.`event_order`"
  )
  events <- connection$execute(repository, sql, async = async)
  # ODK's own names, as in audit.csv
  columns <- c("event", "node", "start", "end", "latitude", "longitude", "accuracy",
               "old-value", "new-value", "user", "change-reason")
  if (nrow(events) == 0) {
    audits <- list()
    empty <- as.data.frame(stats::setNames(replicate(length(columns), character(0), simplify = FALSE), columns))
  } else {
    names(events) <- c("fs_uuid", columns)
    events$node[is.na(events$node)] <- ""
    audits <- split(events[, columns], events$fs_uuid)
    audits <- lapply(audits, function(x) {
      rownames(x) <- NULL
      x
    })
    empty <- events[0, columns]
  }
  if (!is.null(dataset)) {
    if (!uuid_column %in% names(dataset)) {
      stop(glue::glue("The variable {uuid_column} cannot be identified in the dataset provided."))
    }
    wanted <- as.character(dataset[[uuid_column]])
    audits <- audits[names(audits) %in% wanted]
    missing <- wanted[!wanted %in% names(audits)]
    if (length(missing) > 0) {
      warning(glue::glue("{length(missing)} submissions have no audit. They were added as empty dataframes."))
      audits <- c(audits, stats::setNames(rep(list(empty), length(missing)), missing))
    }
  }
  audits
}

#' The changes made to a FormShare repository
#'
#' FormShare records every change to the data in the repository's audit log:
#' each answer edited (with its old and new value), each submission deleted,
#' who did it, when and, where given, why. This reads it in the cleaning log's
#' format, so that what was actually done can be compared with what a
#' cleaning log asked for ([fs_review_cleaning()]).
#'
#' @param connection A logged-in FormShare connection
#' @param repository The repository
#' @param since Optional: only changes on or after this moment: a POSIXct (for
#' example `Sys.time()` when the cleaning started), or text such as
#' "2026-11-04 10:30:00", read as UTC
#' @param include_control TRUE to keep changes to FormShare's own columns
#' (for example the data quality score), FALSE by default
#' @param include_inserts TRUE to keep the rows added (a submission arriving),
#' FALSE by default: a review of the cleaning needs the edits and deletions
#' @param async TRUE to read in the background on the server
#' @return A data frame: uuid, table, question, old_value, new_value,
#' change_type ("change_response", "blank_response", "remove_survey" or
#' "added_survey"), user, date, reason
#' @export
fs_change_log <- function(connection, repository, since = NULL, include_control = FALSE,
                          include_inserts = FALSE, async = FALSE) {
  fields <- as_listing(connection$get_fields(repository, "audit_log"))
  if (nrow(fields) == 0) {
    stop(glue::glue("{repository} has no change log"))
  }
  # The reason is kept by repositories whose audit triggers name it
  reason <- if ("audit_reason" %in% fields$field_name) "`audit_reason`" else "NULL"
  # audit_date is read and compared as an instant (UNIX_TIMESTAMP): the
  # database's session may be in another time zone than R, and its wall time
  # would then shift every comparison by the difference
  sql <- paste0(
    "SELECT `audit_key` AS `uuid`, `audit_table` AS `table`, `audit_column` AS `question`, ",
    "`audit_oldvalue` AS `old_value`, `audit_newvalue` AS `new_value`, `audit_action` AS `action`, ",
    "`audit_user` AS `user`, UNIX_TIMESTAMP(`audit_date`) AS `epoch`, ", reason, " AS `reason` FROM `audit_log`"
  )
  conditions <- character(0)
  if (!include_inserts) {
    conditions <- c(conditions, "`audit_action` <> 'INSERT'")
  }
  if (!is.null(since)) {
    conditions <- c(conditions, paste0(
      "UNIX_TIMESTAMP(`audit_date`) >= ", format(as.numeric(as.POSIXct(since, tz = "UTC")), scientific = FALSE)
    ))
  }
  if (length(conditions) > 0) {
    sql <- paste0(sql, " WHERE ", paste(conditions, collapse = " AND "))
  }
  sql <- paste0(sql, " ORDER BY `audit_date`")
  changes <- connection$execute(repository, sql, async = async)
  columns <- c("uuid", "table", "question", "old_value", "new_value", "change_type", "user", "date", "reason")
  if (nrow(changes) == 0) {
    return(as.data.frame(stats::setNames(replicate(length(columns), character(0), simplify = FALSE), columns)))
  }
  changes$change_type <- dplyr::case_when(
    changes$action == "DELETE" ~ "remove_survey",
    changes$action == "INSERT" ~ "added_survey",
    is.na(changes$new_value) ~ "blank_response",
    TRUE ~ "change_response"
  )
  if (!include_control) {
    changes <- changes[is.na(changes$question) | !fs_is_control_column(changes$question), ]
  }
  changes$date <- as.POSIXct(as.numeric(changes$epoch), origin = "1970-01-01", tz = "UTC")
  rownames(changes) <- NULL
  changes[, columns]
}
