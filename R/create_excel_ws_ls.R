#' Create an Excel File and Worksheet Tibble
#'
#' Creates a tibble of information on all Excel (".xls" or ".xlsx") files found
#' in a provided folder path.
#'
#' @title .create_excel_ws_ls
#' @author Daniel Germony \email{daniel.r.germony.civ@@mail.mil}
#'
#' @param folder_path Required. A path to the folder with Excel files you want
#'   data on.
#' @param recurse Optional. If \code{TRUE} recurse fully, if a positive number
#'   the number of levels to recurse. Set to \code{TRUE} by default.
#'
#' @return Returns a tibble with the following columns of data on all ".xls" or
#'   ".xlsx" files found at the provided path: \enumerate{ \item \code{path}: A
#'   full path to each Excel file found at the provided folder path. \item
#'   \code{folder_path}: The folder path the Excel file was found in. \item
#'   \code{file_name}: The Excel file name (with extention) found in the folder.
#'   \item \code{worksheet_title}: The title of each worksheet in the Excel
#'   file.}
#' @family dstCSDR Import Excel Data Functions
#' @seealso [.import_excel_ws_ls()] for how to import worksheets identfied in
#'   the tibble created with this function.
#' @seealso [.grab_cell()] pulls the value out of an individual Excel cell and
#'   reurns it to R.
#' @export
.create_excel_ws_ls <- function(folder_path, recurse = TRUE) {
  excel_ws_ls <- folder_path %>%
    fs::dir_ls(regexp = "[.]xls[x]$", recurse = recurse) %>%
    purrr::map(readxl::excel_sheets) %>%
    tibble::enframe() %>%
    tidyr::unnest(cols = c(.data$value)) %>%
    dplyr::mutate(file_name = stringr::str_remove_all(
      .data$name,
      paste0(folder_path, "/"))) %>%
    dplyr::mutate(folder_path = stringr::str_remove_all(
      .data$name,
      .data$file_name)) %>%
    dplyr::select(path = .data$name,
                  folder_path,
                  .data$file_name,
                  worksheet_title = .data$value)

  return(excel_ws_ls)
}
