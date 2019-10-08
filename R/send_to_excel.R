#' Send to Excel
#'
#' A simple wrapper function used to send a single object to a temp excel file.
#'
#' @param x Required. The (presumably) table object to send to Excel.
#'
#' @return Opens an Excel file with the object on the first worksheet.
#' @export
.send_to_excel <- function(x) {
  openxlsx::openXL(writexl::write_xlsx(x))
}
