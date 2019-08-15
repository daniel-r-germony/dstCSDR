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
        col_types = cdsr_excel_col_types,
        col_names = cdsr_excel_col_names
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

    if (nrow(cell_value) == 0) return(NA)

    return(cell_value)
  }


  csdr_excel_metadata  <- tibble::tibble(
    "Data Type"                            = "1921",
    "Data Version"                         = "2011",
    "Security Classification"              = "UNCLASSIFIED",
    "Major Program Name"                   = grab_cell(path, "H6"), # Need to fix so that a list is imported
    "Major Program Phase/Milestone"        = "TBD", # Need to fix so that a list is imported
    "Prime Mission Product"                = grab_cell(path, "H9"),
    "Reporting Organization Type"          = "TBD", # Need to fix so that a list is imported
    "Organization Name"                    = grab_cell(path, "O9"),
    "Organization Address Line 1"          = grab_cell(path, "H6"),
    "Organization Address Line 2"          = grab_cell(path, "H6"),
    "Organization Address City"            = grab_cell(path, "H6"),
    "Organization Address State"           = grab_cell(path, "H6"),
    "Organization Address Zip"             = grab_cell(path, "H6"),
    "Division Name"                        = grab_cell(path, "Q9"),
    "Division Address Line 1"              = grab_cell(path, "H6"),
    "Division Address Line 2"              = grab_cell(path, "H6"),
    "Division Address City"                = grab_cell(path, "H6"),
    "Division Address State"               = grab_cell(path, "H6"),
    "Division Address Zip"                 = grab_cell(path, "H6"),
    "Approved Plan Number"                 = grab_cell(path, "S9"),
    "Customer"                             = grab_cell(path, "B13"),
    "Contract Type"                        = grab_cell(path, "F12"),
    "Contract Price"                       = grab_cell(path, "H12"),
    "Contract Ceiling"                     = grab_cell(path, "I13"),
    "Contract No"                          = grab_cell(path, "M12"),
    "Latest Modification"                  = grab_cell(path, "M13"),
    "Solicitation No"                      = grab_cell(path, "P12"),
    "Contract Name"                        = grab_cell(path, "P13"),
    "Order/Lot No"                         = grab_cell(path, "R12"),
    "PoP Start Date"                       = grab_cell(path, "F15"),
    "PoP End Date"                         = grab_cell(path, "F16"),
    "Appropriation"                        = "TBD", # Need to fix so that a list is imported
    "Report Cycle"                         = "TBD", # Need to fix so that a list is imported
    "Submission Number"                    = grab_cell(path, "O15"),
    "Resubmission Number"                  = grab_cell(path, "Q16"),
    "Report As Of"                         = grab_cell(path, "R15"),
    "Point of Contact Name"                = grab_cell(path, "B19"),
    "Department"                           = grab_cell(path, "I19"),
    "Telephone Number"                     = grab_cell(path, "M19"),
    "Email Address"                        = grab_cell(path, "P19"),
    "Date Prepared"                        = grab_cell(path, "R19")
   ) %>% t()

  return(cdsr_excel, csdr_excel_metadata)

}
