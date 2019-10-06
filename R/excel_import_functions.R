#' Create an Excel File and Worksheet Table
#'
#' Creates a tibble of information on all Excel (".xls[x]") files at the
#' provided path.
#'
#' @title .create_excel_ws_ls
#' @author Daniel Germony \email{daniel.r.germony.civ@@mail.mil}
#'
#' @param folder_path Required. A path to the folder with Excel files you want
#'   data on.
#' @param recurse Optional. If \code{TRUE} recurse fully, if a positive number
#'   the number of levels to recurse. Set to \code{TRUE} by default.
#'
#' @return Returns a tibble with the following columns of data on all ".xls[x]"
#'   files found at the provided path: \enumerate{ \item path: A full path to
#'   each Excel file found at the provided folder path. \item folder_path: The
#'   folder path the Excel file was found in. \item file_name: The Excel file
#'   name (with extention) found in the folder. \item worksheet_title: The title
#'   of each worksheet in the Excel file.}
#' @export
.create_excel_ws_ls <- function(folder_path, recurse = TRUE) {
  excel_ws_ls <- folder_path %>%
    fs::dir_ls(regexp = "[.]xls[x]$", recurse = recurse) %>%
    purrr::map(readxl::excel_sheets) %>%
    tibble::enframe() %>%
    tidyr::unnest(cols = c(.data$value)) %>%
    dplyr::mutate(file_name = stringr::str_remove_all(.data$name,
                                                      paste0(folder_path, "/"))) %>%
    dplyr::mutate(folder_path = stringr::str_remove_all(.data$name, .data$file_name)) %>%
    dplyr::select(path = .data$name,
                  folder_path,
                  .data$file_name,
                  worksheet_title = .data$value)

  return(excel_ws_ls)
}

#' Import Worksheets From Given excel_ws_ls Object
#'
#' Imports all worksheets listed in a excel_ws_ls object to a single tibble.
#' Each column in the source worksheets should be of identical names and types.
#'
#' @title .import_excel_ws_ls
#' @author Daniel Germony \email{daniel.r.germony.civ@@mail.mil}
#'
#' @param excel_ws_ls Required. A tibble object created (preferablly) using
#'   \code{.create_excel_ws_ls} which has a column of file paths and worksheet
#'   titles that will be imported.
#'
#' @return Returns a tibble with each worksheet listed in the \code{excel_ws_ls}
#'   object combined into one tibble. A column for \code{source_file_name} and
#'   \code{source_file_worksheet} are also added to keep track of where the data
#'   came from.
#' @export
.import_excel_ws_ls <- function(excel_ws_ls) {
  for (i in seq_len(nrow(excel_ws_ls))) {
    if (i != 1) {
      combined_worksheets <-
        rbind(combined_worksheets,
              (readxl::read_excel(
                path = excel_ws_ls$path[i],
                sheet = excel_ws_ls$worksheet_title[i]) %>%
                 dplyr::mutate(
                   source_file_name = excel_ws_ls$file_name[i],
                   source_file_worksheet = excel_ws_ls$worksheet_title[i])))
    }
    if (i == 1) {
      combined_worksheets <- (
        readxl::read_excel(
          path = excel_ws_ls$path[i],
          sheet = excel_ws_ls$worksheet_title[i]) %>%
          dplyr::mutate(
            source_file_name = excel_ws_ls$file_name[i],
            source_file_worksheet = excel_ws_ls$worksheet_title[i]))
    }
  }
  return(combined_worksheets)
}
