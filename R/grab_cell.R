#' Get Excel Cell Value
#'
#' A function which pulls the value out of an individual Excel cell and reurns
#' it to R.
#'
#' @param path Required. The full path to an Excel file.
#' @param cell_range Required. The cell range where you want to pull data form.
#'   Must be in the "A1" style.
#'
#' @return Returns the value from the Excel file as an "NA" if the cell was
#'   blank or as a character otherwise
#' @family dstCSDR Import Excel Data Functions
#' @seealso [.create_excel_ws_ls()] to create a tibble of files and worksheet
#'   names.
#' @seealso [.import_excel_ws_ls()] for how to import worksheets identfied in
#'   the tibble created with this function.
#' @export
.grab_cell <- function(path, cell_range) {
  cell_value <- readxl::read_excel(
    path = path,
    col_names = FALSE,
    range = c(cell_range),
    .name_repair = "minimal"
  )

  # Return an NA value if the cell in Excel was blank
  if (nrow(cell_value) == 0)
    return(NA)

  # If the Excel cell had a date, it needs special attention to get it type
  # coerced into a char that looks like a date.
  if (lubridate::is.POSIXct(cell_value[[1]])) {
    cell_value[[1]] <- as.character(cell_value[[1]])
  }

  # Everything other than NA and dates can get coerced and returned as a char
  return(as.character(cell_value))
}
