#' Import a CSDR CDSR (DD Form 1921) from Excel into a Tibble
#'
#' Takes a CSDR CDSR (DD Form 1921) Excel worksheet and imports it to three a
#' tibbles.
#'
#' @title import_cdsr_excel
#' @author Daniel Germony \email{daniel.r.germony.civ@@mail.mil}
#' @param path Required. Path to the xls/xlsx file which contains the CSDR
#'    1921. Note the Excel file must have the CSDR 1921 data on the first
#'    worksheet (remove any coversheets prior to using this script) and cannot
#'    have columns or rows moved/changed from the standard from (i.e., the
#'    script assumes a cPet produced/compliant file is provided).
#' @return Returns a list of three tibbles. The first tibble includes all of
#'    the submission's metadata in two columns. The second tibble includes each
#'    WBS element as a row and the repoted data as columns. The third tibble
#'    includes the summary reporting elements (the rows at the bottom of a
#'    CDSR with summary cost data and no WBS number).
#' @export

import_cdsr_excel <- function(path) {

  # Create a function to help get data from individual Excel cells. -----------
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

  # Import metadata and create an object out of it. ---------------------------
  metadata  <- tibble::tibble(
    "Security Classification"              = .grab_cell(path, "G2"),
    "Major Program Name"                   = .grab_cell(path, "H6"),
    "Major Program Phase/Milestone"        = stringr::str_c(sep = ", ",
      if (is.na(.grab_cell(path, "B8"))) NULL else "Pre-A",
      if (is.na(.grab_cell(path, "B9"))) NULL else "A",
      if (is.na(.grab_cell(path, "D8"))) NULL else "B",
      if (is.na(.grab_cell(path, "D9"))) NULL else "C-LRIP",
      if (is.na(.grab_cell(path, "F8"))) NULL else "C-FRP",
      if (is.na(.grab_cell(path, "F9"))) NULL else "O&S"),
    "Prime Mission Product"                = .grab_cell(path, "H9"),
    "Reporting Organization Type"          = stringr::str_c(sep = ", ",
      if (is.na(.grab_cell(path, "I8"))) NULL else "PRIME / ASSOCIATE CONTRACTOR",
      if (is.na(.grab_cell(path, "K9"))) NULL else "DIRECT-REPORTING SUBCONTRACTOR",
      if (is.na(.grab_cell(path, "M8"))) NULL else "GOVERNMENT"),
    "Organization Name & Address"          = .grab_cell(path, "O9"),
    "Division Name  & Address"             = .grab_cell(path, "Q9"),
    "Approved Plan Number"                 = .grab_cell(path, "S9"),
    "Customer"                             = .grab_cell(path, "B13"),
    "Contract Type"                        = .grab_cell(path, "F12"),
    "Contract Price"                       = .grab_cell(path, "H12"),
    "Contract Ceiling"                     = .grab_cell(path, "I13"),
    "Contract No"                          = .grab_cell(path, "M12"),
    "Latest Modification"                  = .grab_cell(path, "M13"),
    "Solicitation No"                      = .grab_cell(path, "P12"),
    "Contract Name"                        = .grab_cell(path, "P13"),
    "Task Order/Deliver Order/Lot Number"  = .grab_cell(path, "R12"),
    "Period of Performance Start Date"     = .grab_cell(path, "F15"),
    "Period of Performance End Date"       = .grab_cell(path, "F16"),
    "Appropriation"                        = stringr::str_c(sep = ", ",
      if (is.na(.grab_cell(path, "I8"))) NULL else "RDT&E",
      if (is.na(.grab_cell(path, "K9"))) NULL else "PROCUREMENT",
      if (is.na(.grab_cell(path, "M8"))) NULL else "O&M"),
    "Report Cycle"                         = stringr::str_c(sep = ", ",
      if (is.na(.grab_cell(path, "M15"))) NULL else "INITIAL",
      if (is.na(.grab_cell(path, "M16"))) NULL else "INTERIM",
      if (is.na(.grab_cell(path, "M17"))) NULL else "FINAL"),
    "Submission Number"                    = .grab_cell(path, "O15"),
    "Resubmission Number"                  = .grab_cell(path, "Q16"),
    "Report As Of"                         = .grab_cell(path, "R15"),
    "Point of Contact Name"                = .grab_cell(path, "B19"),
    "Department"                           = .grab_cell(path, "I19"),
    "Telephone Number"                     = .grab_cell(path, "M19"),
    "Email Address"                        = .grab_cell(path, "P19"),
    "Date Prepared"                        = .grab_cell(path, "R19")
  ) %>% tidyr::pivot_longer(
    tidyr::everything(),
    names_to = "metadata_field",
    values_to = "repoted_value")

  # Specify CDSR column types and mark that merged columns are skipped. -------
  .cdsr_excel_col_types <- c(
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

  # Provide column names for imported CDSR columns. ---------------------------
  .cdsr_excel_col_names <- c(
    "WBS Element Code",                            # "WBSElementID"
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

  cdsr_data <-
    # Suppress import errors from the "22. REMARKS" row.
    suppressWarnings(
      readxl::read_excel(
        path = path,
        # WBS table anchor at cell 'B23'
        range = readxl::cell_limits(c(23, 2), c(NA, NA)),
        col_types = .cdsr_excel_col_types,
        col_names = .cdsr_excel_col_names
      ) %>%
        # Remove the "DD FORM 1921, MAY 2011" row.
        dplyr::slice(1:(dplyr::n() - 1))
    )

  # Pull "22. REMARKS" and add it to the metadata tibble. ---------------------
  remark <- cdsr_data %>%
    dplyr::slice(dplyr::n()) %>%
    dplyr::select(1) %>%
    dplyr::rename("Remarks" = "WBS Element Code") %>%
    tidyr::pivot_longer(tidyr::everything(),
                        names_to = "metadata_field",
                        values_to = "repoted_value")

  metadata <- metadata %>%
    tibble::add_row("metadata_field" = remark$'metadata_field',
                    "repoted_value"  = remark$'repoted_value')

  # Pull the summary elements and add them to their own tibble. ---------------

  summary_reporting_elements <- cdsr_data %>%
    dplyr::filter(is.na(.data$`WBS Element Code`)) %>%
    dplyr::select(
      .data$`WBS Reporting Element`,
      .data$`Costs Incurred To Date - Total`,
      .data$`Costs Incurred At Completion - Total`) %>%
    dplyr::rename(
      `Summary Reporting Element` = .data$`WBS Reporting Element`) %>%
    dplyr::filter(!is.na(.data$`Summary Reporting Element`))

  # Remove the remark and summary element rows from the reported data. --------

  cdsr_data <- cdsr_data %>%
    dplyr::slice(1:(dplyr::n()-15))

  return(list(metadata = metadata,
              reported_data = cdsr_data,
              summary_reporting_elements = summary_reporting_elements))

}