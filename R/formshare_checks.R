# Checks that know what FormShare knows of a form.

#' Check for potential PII, with what FormShare marks
#'
#' Runs [check_pii()], which finds personal data by the column's name, and adds
#' the questions FormShare itself marks as sensitive or encrypted: the form's
#' author already said those hold personal data.
#'
#' @param dataset dataset to be check as a dataframe or a list with the
#' dataframe stored as "checked_dataset"
#' @param survey The form's questions, from [fs_survey()]
#' @param uuid_column uuid column in the dataset. Default is "rowuuid",
#' FormShare's row identifier.
#' @param element_name If the input is a list, the name of the element that
#' holds the dataset
#' @param words_to_look Extra words that may name personal data
#' @return The input with a potential_PII log, as [check_pii()] returns it
#' @export
fs_check_pii <- function(dataset,
                         survey,
                         uuid_column = "rowuuid",
                         element_name = "checked_dataset",
                         words_to_look = NULL) {
  result <- check_pii(
    dataset,
    element_name = element_name,
    uuid_column = uuid_column,
    words_to_look = words_to_look
  )
  data <- if (is.data.frame(dataset)) dataset else dataset[[element_name]]
  marked <- survey[(survey$sensitive %in% TRUE | survey$encrypted %in% TRUE) &
    survey$name %in% names(data), ]
  if (nrow(marked) > 0) {
    from_formshare <- tibble::tibble(
      uuid = "all",
      question = marked$name,
      issue = ifelse(marked$encrypted %in% TRUE, "Encrypted in FormShare", "Marked sensitive in FormShare")
    )
    found <- result$potential_PII
    from_formshare <- from_formshare[!from_formshare$question %in% found$question, ]
    result$potential_PII <- dplyr::bind_rows(found, from_formshare)
  }
  result
}

#' Select multiple answers as one column per option
#'
#' FormShare keeps a select multiple's answer in one column, the codes chosen
#' separated by spaces ("maize beans"), and keeps the options in a table of
#' their own. Kobo's exports add a column per option instead (crops.maize,
#' crops.beans), which some functions read: [review_others()],
#' [create_logic_for_other()], [recreate_parent_column()]. This adds those
#' columns, TRUE or FALSE (NA when the question was not answered), from the
#' form's choices.
#'
#' They are for reading: a correction of a select multiple goes to its own
#' column, the codes separated by spaces, which FormShare applies to the options
#' too.
#'
#' @param dataset Data read with [fs_read_data()]
#' @param survey The form's questions, from [fs_survey()]
#' @param choices The form's choices, from [fs_choices()]
#' @param sm_separator Separator between the question and the option. The
#' default is "."
#' @return The dataset with a column per option of each select multiple
#' @export
fs_expand_select_multiple <- function(dataset, survey, choices, sm_separator = ".") {
  multiple <- survey[startsWith(survey$type, "select_multiple") & survey$name %in% names(dataset), ]
  for (i in seq_len(nrow(multiple))) {
    question <- multiple$name[i]
    answered <- !is.na(dataset[[question]])
    chosen <- strsplit(ifelse(answered, as.character(dataset[[question]]), ""), "\\s+")
    for (code in choices$name[choices$list_name %in% multiple$list_name[i]]) {
      column <- paste0(question, sm_separator, code)
      if (column %in% names(dataset)) {
        next
      }
      dataset[[column]] <- ifelse(answered, vapply(chosen, function(x) code %in% x, logical(1)), NA)
    }
  }
  dataset
}
