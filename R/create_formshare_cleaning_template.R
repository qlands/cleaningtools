#' Create a project folder with a FormShare cleaning template
#'
#' Copies a template for cleaning a FormShare form: a script that reads the
#' form from FormShare, runs the checks, writes the cleaning log, applies the
#' filled log in FormShare and reviews the cleaning.
#'
#' @param folder_path Path for the new folder
#' @param ... Extra information collected from the RStudio wizard
#'
#' @return The folder, with a copy of the template
#' @export
#'
#' @examples
#' \dontrun{
#' create_formshare_cleaning_template("path/to/folder")
#' }
create_formshare_cleaning_template <- function(folder_path, ...) {
  from <- system.file("formshare_cleaning_template", package = "cleaningtools")
  fs::dir_copy(from, folder_path, overwrite = FALSE)
  invisible(folder_path)
}
