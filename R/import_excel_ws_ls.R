#' Import Worksheets From a \code{excel_ws_ls} Object
#'
#' Imports all worksheets listed in a \code{excel_ws_ls} object to a single
#' tibble. Each column in the source worksheets should be of identical names and
#' types.
#'
#' @title .import_excel_ws_ls
#' @author Daniel Germony \email{daniel.r.germony.civ@@mail.mil}
#'
#' @param excel_ws_ls Required. A tibble object created (preferablly) using
#'   \code{\link[dstCSDR:dot-create_excel_ws_ls]{.create_excel_ws_ls}} which has
#'   a column of file paths and worksheet titles that will be imported.
#' @param skip Optional. Taken from and passed to
#'   \code{\link[readxl:read_excel]{read_excel}}: Minimum number of rows to skip
#'   before reading anything, be it column names or data. Leading empty rows are
#'   automatically skipped, so this is a lower bound. \code{0} by default.
#' @param col_names Optional. Taken from and passed to
#'   \code{\link[readxl:read_excel]{read_excel}}: \code{TRUE} to use the first
#'   row as column names, \code{FALSE} to get default names, or a character
#'   vector giving a name for each column. \code{TRUE} by default.
#' @param range Optional. Taken from and passed to
#'   \code{\link[readxl:read_excel]{read_excel}}:
#'   A cell range to read from, as described in
#'   \code{\link[readxl:cell-specification]{cell specification}}. Includes
#'   typical Excel ranges like "B3:D87", possibly including the sheet name like
#'   "Budget!B2:G14", and more. Interpreted strictly, even if the range forces
#'   the inclusion of leading or trailing empty rows or columns.
#'
#' @return Returns a tibble with each worksheet listed in the \code{excel_ws_ls}
#'   object combined into one tibble. A column with \code{source_file_name} and
#'   \code{source_file_worksheet} are also added to keep track of where the data
#'   came from.
#' @family dstCSDR Import Excel Data Functions
#' @seealso [.create_excel_ws_ls()] to create a tibble of files and worksheet
#'   names.
#' @seealso [.grab_cell()] pulls the value out of an individual Excel cell and
#'   reurns it to R.
#' @export
.import_excel_ws_ls <- function(excel_ws_ls, skip = 0, col_names = TRUE, range = NULL) {
  for (i in seq_len(nrow(excel_ws_ls))) {
    if (i != 1) {
      combined_worksheets <-
        rbind(combined_worksheets,
              (readxl::read_excel(
                path = excel_ws_ls$path[i],
                sheet = excel_ws_ls$worksheet_title[i],
                col_names = col_names,
                skip = skip,
                range = NULL,
                .name_repair = "universal") %>%
                 dplyr::mutate(
                   source_file_name = excel_ws_ls$file_name[i],
                   source_file_worksheet = excel_ws_ls$worksheet_title[i])))
    }
    if (i == 1) {
      combined_worksheets <- (
        readxl::read_excel(
          path = excel_ws_ls$path[i],
          sheet = excel_ws_ls$worksheet_title[i],
          col_names = col_names,
          skip = skip,
          range = NULL,
          .name_repair = "universal") %>%
          dplyr::mutate(
            source_file_name = excel_ws_ls$file_name[i],
            source_file_worksheet = excel_ws_ls$worksheet_title[i]))
    }
  }
  return(combined_worksheets)
}
