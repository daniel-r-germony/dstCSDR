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

import_cdsr_excel <- function(path) {

  # Import the pipe! ----------------------------------------------------------
  `%>%` <- magrittr::`%>%`

  # Specify 1921 column types and mark that merged columns are skipped. -------

  cdsr_excel_col_types <- c(
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
  cdsr_excel_col_names <- c(
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
  cdsr_excel <-
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

  # Import metadata from the 1921. --------------------------------------------

  grab_cell <- function(path, cell_range) {
    cell_value <- readxl::read_excel(
      path = path,
      col_names = FALSE,
      range = c(cell_range),
      .name_repair = "minimal"
    )

    return(cell_value)
  }


  csdr_excel_metadata  <- tibble::tibble(
    "Data Type"                            = "1921",
    "Data Version"                         = "2011",
    "Security Classification"              = "UNCLASSIFIED",
    "Major Program Name"                   = grab_cell(path, "H6"),
    "Major Program Phase/Milestone"        = "1",
    "Prime Mission Product"                = "1",
    "Reporting Organization Type"          = "1",
    "Organization Name"                    = "1",
    "Organization Address Line 1"          = "11",
    "Organization Address Line 2"          = "12",
    "Organization Address City"            = "1",
    "Organization Address State"           = "1",
    "Organization Address Zip"             = "1",
    "Division Name"                        = "1",
    "Division Address Line 1"              = "1",
    "Division Address Line 2"              = "1",
    "Division Address City"                = "1",
    "Division Address State"               = "1",
    "Division Address Zip"                 = "1",
    "Approved Plan Number"                 = "1",
    "Customer"                             = "1",
    "Contract Type"                        = "1",
    "Contract Price"                       = "1",
    "Contract Ceiling"                     = "1",
    "Contract No"                          = "1",
    "Latest Modification"                  = "1",
    "Solicitation No"                      = "1",
    "Contract Name"                        = "1",
    "Order/Lot No"                         = "1",
    "PoP Start Date"                       = "1",
    "PoP End Date"                         = "1",
    "Appropriation"                        = "1",
    "Report Cycle"                         = "1",
    "Submission Number"                    = "1",
    "Resubmission Number"                  = "1",
    "Report As Of"                         = "1",
    "Point of Contact Name"                = "1",
    "Department"                           = "1",
    "Telephone Number"                     = "1",
    "Email Address"                        = "1",
    "Date Prepared"                        = "1",
    "Subtotal To Date"                     = "1",
    "Subtotal At Completion"               = "1",
    "G&A To Date"                          = "1",
    "G&A At Completion"                    = "1",
    "Undistributed Budget At Completion"   = "1",
    "Management Reserve At Completion"     = "1",
    "FCCM To Date"                         = "1",
    "FCCM At Completion"                   = "1",
    "Fee To Date"                          = "1",
    "Fee At Completion"                    = "1",
    "Price To Date"                        = "1",
    "Price At Completion"                  = "1",
    "DD 1921 Remark"                       = "1"
   ) %>% t()

  return(cdsr_excel, csdr_excel_metadata)

}
