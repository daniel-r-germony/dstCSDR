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
    "WBS_ELEMENT_CODE",
    "WBS_REPORTING_ELEMENTS",
    "NUMBER_OF_UNITS_TO_DATE",
    "NONRECURRING_COSTS_INCURRED_TO_DATE",
    "RECURRING_COSTS_INCURRED_TO_DATE",
    "TOTAL_COSTS_INCURRED_TO_DATE",
    "NUMBER_OF_UNITS_AT_COMPLETION",
    "NONRECURRING_COSTS_INCURRED_AT_COMPLETION",
    "RECURRING_COSTS_INCURRED_AT_COMPLETION",
    "TOTAL_COSTS_INCURRED_AT_COMPLETION"
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

  # Pull reported "22. REMARKS" into its own table. ---------------------------
  CSDR_1921_remarks <-
    CSDR_1921 %>% dplyr::slice(dplyr::n()) %>%
    dplyr::select(1) %>%
    dplyr::rename("22. REMARKS" = "WBS_ELEMENT_CODE")

  # Create a table with only reported data (i.e., no summary elements). -------
  CSDR_1921_reported_data <-
    CSDR_1921 %>%
    # Remove "Subtotal Cost" thru "Total Price" rows.
    dplyr::slice(1:(dplyr::n() - 15))

  # Create a table with only summary elements. --------------------------------
  CSDR_1921_summary_reporting_elements <-
    CSDR_1921 %>%
    # Remove "Subtotal Cost" thru "Total Price" rows.
    dplyr::slice((dplyr::n() - 10):dplyr::n() - 3) %>%
    dplyr::filter(!is.na(WBS_REPORTING_ELEMENTS)) %>%
    # Remove the columns without data
    dplyr::select(2, # WBS_REPORTING_ELEMENTS
                  6, # TOTAL_COSTS_INCURRED_TO_DATE
                  10 # TOTAL_COSTS_INCURRED_AT_COMPLETION
    ) %>%
    # Rename the first column to something more appropriate
    dplyr::rename("SUMMARY_REPORTING_ELEMENT" = "WBS_REPORTING_ELEMENTS")

  return(CSDR_1921)

}
