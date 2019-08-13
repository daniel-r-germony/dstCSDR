#' Import a CSDR 1921 from Excel into a Tibble
#'
#' Takes a CSDR 1921 Excel worksheet and imports it to a tibble.
#'
#' @title import_CSDR_1921
#' @author Daniel Germony \email{daniel.r.germony.civ@@mail.mil}
#' @param path Required. Path to the xls/xlsx file which contains the CSDR
#'    1921. Note the Excel file must have the CSDR 1921 data on the first
#'    worksheet (remove any coversheets prior to using this script) and cannot
#'    have columns or rows moved/changed from the standard from (i.e., the
#'    script assumes a cPet produced/compliant file is provided).
#' @return Returns a tibble with each WBS element as a row and traditional
#'    CSDR 1921 data as columns.
#' @export

import_CSDR_1921 <- function(path) {

  # Import the pipe! ----------------------------------------------------------
  `%>%` <- magrittr::`%>%`

  # Specify 1921 column types and mark that merged columns are skipped. -------

  CSDR_1921_col_types <- c(
    "text",
    "skip", # Because of merged cells
    "text",
    "skip", # Because of merged cells
    "skip", # Because of merged cells
    "skip", # Because of merged cells
    "skip", # Because of merged cells
    "numeric",
    "skip", # Because of merged cells
    "numeric",
    "skip", # Because of merged cells
    "numeric",
    "skip", # Because of merged cells
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  )

  # Provide column names for imported 1921 columns. ---------------------------
  CSDR_1921_col_names <- c(
    "WBS Code",                                    # "WBSElementID"
    "WBS Reporting Element",                       # "WBSElementName"
    "Number of Units to Date",                     # "QuantityToDate"
    "Costs Incurred To Date - Nonrecurring",       # "NonrecurringCostsToDate"
    "Costs Incurred To Date - Recurring",          # "RecurringCostsToDate"
    "Costs Incurred To Date - Total",              # "TotalCostsToDate"
    "Number of Units At Completion",               # "QuantityAtCompletion"
    "Costs Incurred At Completion - Nonrecurring", # "NonrecurringCostsAtCompletion"
    "Costs Incurred At Completion - Recurring",    # "RecurringCostsAtCompletion"
    "Costs Incurred At Completion - Total"         # "TotalCostsAtCompletion"
  )

  # Import non-metadata from the 1921. ----------------------------------------
  CSDR_1921 <-
    # Suppress import errors from the "22. REMARKS" row.
    suppressWarnings(
      readxl::read_excel(
        path = path,
        # WBS table anchor at cell 'B23'
        range = readxl::cell_limits(c(23, 2), c(NA, NA)),
        col_types = CSDR_1921_col_types,
        col_names = CSDR_1921_col_names
      ) %>%
        # Remove the "DD FORM 1921, MAY 2011" row.
        dplyr::slice(1:(dplyr::n() - 1))
    )

  return(CSDR_1921)

}
