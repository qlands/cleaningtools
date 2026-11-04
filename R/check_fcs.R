#' FCS component checks
#'
#' Flags the records whose eight food consumption answers are all the same
#' (every group eaten on the same number of days).
#'
#' Given a data frame, it returns a data frame of the flagged records, as it
#' always has. Given the list the other checks pass along (with the dataset as
#' "checked_dataset"), it adds a log in the cleaning log's format (uuid,
#' question, old_value, issue) under `log_name`, so that it goes into
#' [create_combined_log()] with the rest.
#'
#' @param dataset dataset to be check as a dataframe or a list with the dataframe stored as "checked_dataset"
#' @param uuid_column uuid column in the dataset. Default is uuid.
#' @param cereals_column Variable name for- In the last 7 days, on how many days did your household eat cereals, grains, roots and tubers, including wild roots?
#' @param pulses_column Variable name for-  In the last 7 days, on how many days did your household eat any beans / legumes, pulses or nuts?
#' @param dairy_column Variable name for- In the last 7 days, on how many days did your household drink milk or eat other dairy products?
#' @param meat_column  Variable name for-  In the last 7 days, on how many days did your household eat meat, fish or eggs?
#' @param vegetables_column Variable name for- In the last 7 days, on how many days did your household eat vegetables or leaves, including all wild vegetables and leaves?
#' @param fruits_column Variable name for- In the last 7 days, on how many days did your household eat fruits, including all wild fruits?
#' @param oil_column Variable name for- In the last 7 days, on how many days did your household eat oil, fat, or butter?
#' @param sugar_column Variable name for- In the last 7 days, on how many days did your household eat sugar or sugary foods?
#' @param log_name name of the log when the input is a list. Default is "fcs_log".
#' @return the flagged records as a data frame, or with a list as input, the
#' list with the log added
#' @export

check_fcs <- function(dataset,
                      uuid_column = "uuid",
                      cereals_column = "fs_fcs_cerealgrainroottuber",
                      pulses_column = "fs_fcs_beansnuts",
                      dairy_column = "fs_fcs_dairy",
                      meat_column = "fs_fcs_meatfishegg",
                      vegetables_column = "fs_fcs_vegetableleave",
                      fruits_column = "fs_fcs_fruit",
                      oil_column = "fs_fcs_fat",
                      sugar_column = "fs_fcs_sugar",
                      log_name = "fcs_log") {
  input_is_list <- is.list(dataset) & !is.data.frame(dataset)
  if (input_is_list) {
    if (!"checked_dataset" %in% names(dataset)) {
      stop("Cannot identify the dataset in the list")
    }
    data <- dataset[["checked_dataset"]]
  } else {
    data <- dataset
  }

  fcs_cols <- c(
    cereals_column, pulses_column, dairy_column, meat_column, vegetables_column,
    fruits_column, oil_column, sugar_column
  )

  issue <- "All the values of the food consumption variables are the same"
  check_df <- data %>%
    dplyr::filter(pmax(!!!rlang::syms(fcs_cols), na.rm = T) == pmin(!!!rlang::syms(fcs_cols), na.rm = T)) %>%
    dplyr::select(!!rlang::sym(uuid_column), dplyr::all_of(fcs_cols)) %>%
    dplyr::mutate(
      issue = issue
    ) ## filtering only observation where all the values are same. Didn't use if_all to consider NA.

  warring_ma <- paste0("Potential issue:: There are ", nrow(check_df), " observations where all the variables of food consumption score are the same.Check result.")

  if (nrow(check_df) > 0) {
    warning(warring_ma)
  }

  if (nrow(check_df) == 0) {
    print("No issue found")
  }

  if (!input_is_list) {
    return(check_df)
  }

  dataset[[log_name]] <- check_df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(fcs_cols), as.character)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(fcs_cols),
      names_to = "question",
      values_to = "old_value"
    ) %>%
    dplyr::rename(uuid = !!rlang::sym(uuid_column)) %>%
    dplyr::mutate(uuid = as.character(uuid)) %>%
    dplyr::select(uuid, question, old_value, issue)
  dataset
}
