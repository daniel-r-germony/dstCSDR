#' Import a CSDR FCHR (DD Form 1921-1) from Excel into a Tibble
#'
#' Takes a CSDR Functional Cost-Hour Report (FCHR) (DD Form 1921-1) Excel
#' worksheet and imports it to a set of tibbles.
#'
#' @title import_fchr_excel
#' @author Daniel Germony \email{daniel.r.germony.civ@@mail.mil}
#' @param path Required. Path to the xls/xlsx file which contains the CSDR
#'    FCHR 1921-1. Note the Excel file must have FCHR data on the first
#'    worksheet (remove any coversheets prior to using this script) and cannot
#'    have columns or rows moved/changed from the standard from (i.e., the
#'    script assumes a cPet produced/compliant file is provided).
#' @return Returns a list of two tibbles. The first tibble includes all of the
#'    submission's metadata in two columns. The second tibble includes each
#'    worksheet from the FCHR combined into one long table where each row
#'    is a Functional Data Element and the columns include the cost & hours
#'    data. Remarks by WBS and a few other columns are included so that the
#'    FCHR data is a proper rectangular table.
#' @export
#' @examples
#' # Import the example FCHR.
#' import_fchr_excel(dstCSDR_files("Example_FCHR_1921-1.xls"))
#'
#' \dontrun{
#'
#' # Import a FCHR from a fake user desktop by directly passing the path.
#' import_fchr_excel("C:/Users/john.doe/Desktop/DD Form 1921-1 submission.xls")
#'
#' # Import a FCHR from a fake user desktop by passing a path object.
#' fchr_path <- "C:/Users/john.doe/Desktop/DD Form 1921-1 submission.xls"
#' import_fchr_excel(fchr_path)
#'
#' }
import_fchr_excel <- function(path) {

  # Import metadata and create an object out of it. ---------------------------
  metadata  <- tibble::tibble(
    "Security Classification"              = dstCSDR::.grab_cell(path, "G2"),
    "Major Program Name"                   = dstCSDR::.grab_cell(path, "F6"),
    "Major Program Phase/Milestone"        = stringr::str_c(sep = ", ",
      if (is.na(dstCSDR::.grab_cell(path, "B8"))) NULL else "Pre-A",
      if (is.na(dstCSDR::.grab_cell(path, "B9"))) NULL else "A",
      if (is.na(dstCSDR::.grab_cell(path, "D8"))) NULL else "B",
      if (is.na(dstCSDR::.grab_cell(path, "D9"))) NULL else "C-LRIP",
      if (is.na(dstCSDR::.grab_cell(path, "F8"))) NULL else "C-FRP",
      if (is.na(dstCSDR::.grab_cell(path, "F9"))) NULL else "O&S"),
    "Prime Mission Product"                = dstCSDR::.grab_cell(path, "H9"),
    "Reporting Organization Type"          = stringr::str_c(sep = ", ",
      if (is.na(dstCSDR::.grab_cell(path, "I8"))) NULL else "PRIME / ASSOCIATE CONTRACTOR",
      if (is.na(dstCSDR::.grab_cell(path, "K9"))) NULL else "DIRECT-REPORTING SUBCONTRACTOR",
      if (is.na(dstCSDR::.grab_cell(path, "M8"))) NULL else "GOVERNMENT"),
    "Organization Name & Address"          = dstCSDR::.grab_cell(path, "O9"),
    "Division Name  & Address"             = dstCSDR::.grab_cell(path, "R9"),
    "Approved Plan Number"                 = dstCSDR::.grab_cell(path, "T9"),
    "Customer"                             = dstCSDR::.grab_cell(path, "B12"),
    "Contract No"                          = dstCSDR::.grab_cell(path, "M12"),
    "Latest Modification"                  = dstCSDR::.grab_cell(path, "M13"),
    "Solicitation No"                      = dstCSDR::.grab_cell(path, "P12"),
    "Contract Name"                        = dstCSDR::.grab_cell(path, "P13"),
    "Task Order/Deliver Order/Lot Number"  = dstCSDR::.grab_cell(path, "S12"),
    "Period of Performance Start Date"     = dstCSDR::.grab_cell(path, "F15"),
    "Period of Performance End Date"       = dstCSDR::.grab_cell(path, "F16"),
    "Report Cycle"                         = stringr::str_c(sep = ", ",
      if (is.na(dstCSDR::.grab_cell(path, "K15"))) NULL else "INITIAL",
      if (is.na(dstCSDR::.grab_cell(path, "K16"))) NULL else "INTERIM",
      if (is.na(dstCSDR::.grab_cell(path, "K17"))) NULL else "FINAL"),
    "Submission Number"                    = dstCSDR::.grab_cell(path, "M15"),
    "Resubmission Number"                  = dstCSDR::.grab_cell(path, "P16"),
    "Report As Of"                         = dstCSDR::.grab_cell(path, "S15"),
    "Point of Contact Name"                = dstCSDR::.grab_cell(path, "B19"),
    "Department"                           = dstCSDR::.grab_cell(path, "G19"),
    "Telephone Number"                     = dstCSDR::.grab_cell(path, "K19"),
    "Email Address"                        = dstCSDR::.grab_cell(path, "O19"),
    "Date Prepared"                        = dstCSDR::.grab_cell(path, "S19"),
    "Appropriation"                        = stringr::str_c(sep = ", ",
      if (is.na(dstCSDR::.grab_cell(path, "P21"))) NULL else "RDT&E",
      if (is.na(dstCSDR::.grab_cell(path, "P22"))) NULL else "PROCUREMENT",
      if (is.na(dstCSDR::.grab_cell(path, "P23"))) NULL else "O&M")
  ) %>% tidyr::pivot_longer(
    tidyr::everything(),
    names_to = "metadata_field",
    values_to = "repoted_value")

  # Specify FCHR column types and mark that merged columns are skipped. -------
  .fchr_col_types <- c(
    "text",    # Col B
    "skip",    # Because of merged cells
    "skip",    # Because of merged cells
    "skip",    # Because of merged cells
    "skip",    # Because of merged cells
    "skip",    # Because of merged cells
    "skip",    # Because of merged cells
    "skip",    # Because of merged cells
    "skip",    # Because of merged cells
    "numeric", # Col K
    "skip",    # Because of merged cells
    "numeric", # Col M
    "skip",    # Because of merged cells
    "numeric", # Col O
    "numeric", # Col P
    "skip",    # Because of merged cells
    "numeric", # Col R
    "numeric"  # Col S
  )

  # Provide column names for imported FCHR columns. ---------------------------
  .fchr_col_names <- c(
    "Functional Data Element",                               # LOOKUP
    "Costs and Hours Incurred To Date - Nonrecurring",       # "NonrecurringCostsToDate"
    "Costs and Hours Incurred To Date - Recurring",          # "RecurringCostsToDate"
    "Costs and Hours Incurred To Date - Total",              # "TotalCostsToDate"
    "Costs and Hours Incurred At Completion - Nonrecurring", # "NonrecurringCostsAtCompletion"
    "Costs and Hours Incurred At Completion - Recurring",    # "RecurringCostsAtCompletion"
    "Costs and Hours Incurred At Completion - Total"         # "TotalCostsAtCompletion"
  )

  # Import all FCHR worksheets and combine into a tibble. ---------------------
  fchr_data <-
    path %>%
    readxl::excel_sheets() %>%
    purrr::set_names() %>%
    purrr::map_df(~suppressWarnings(
      readxl::read_excel(
        path = path,
        sheet = .x,
        range = readxl::cell_limits(c(26, 2), c(51, 19)),
        col_types = .fchr_col_types,
        col_names = .fchr_col_names
      )),
      .id = "sheet") %>%
    dplyr::rename("source_worksheet_title" = .data$sheet)

  # Remove Functional Category rows then add them as a column. ----------------
  fchr_data <-
    fchr_data %>%
    dplyr::filter(!(
      .data$`Functional Data Element` %in% c(
        "ENGINEERING",
        "MANUFACTURING OPERATIONS",
        "MATERIALS",
        "OTHER COSTS",
        "SUMMARY"
      )
    ))

  fchr_data <-
    fchr_data %>% dplyr::mutate(
      `Functional Category` = dplyr::case_when(
        .data$`Functional Data Element` == "(1) DIRECT ENGINEERING LABOR HOURS" ~ "Engineering",
        .data$`Functional Data Element` == "(2) DIRECT ENGINEERING LABOR DOLLARS" ~ "Engineering",
        .data$`Functional Data Element` == "(3) ENGINEERING OVERHEAD DOLLARS" ~ "Engineering",
        .data$`Functional Data Element` == "(4) TOTAL ENGINEERING DOLLARS" ~ "Summary",
        .data$`Functional Data Element` == "(5) DIRECT TOOLING LABOR HOURS" ~ "Tooling",
        .data$`Functional Data Element` == "(6) DIRECT TOOLING LABOR DOLLARS" ~ "Tooling",
        .data$`Functional Data Element` == "(7) DIRECT TOOLING & EQUIPMENT DOLLARS" ~ "Tooling",
        .data$`Functional Data Element` == "(8) DIRECT QUALITY CONTROL LABOR HOURS" ~ "Quality Control",
        .data$`Functional Data Element` == "(9) DIRECT QUALITY CONTROL LABOR DOLLARS" ~ "Quality Control",
        .data$`Functional Data Element` == "(10) DIRECT MANUFACTURING LABOR HOURS" ~ "Manufacturing",
        .data$`Functional Data Element` == "(11) DIRECT MANUFACTURING LABOR DOLLARS" ~ "Manufacturing",
        .data$`Functional Data Element` == "(12) MANUFACTURING OPERATIONS OVERHEAD DOLLARS (Including Tooling and Quality Control)" ~ "Manufacturing Operations",
        .data$`Functional Data Element` == "(13) TOTAL MANUFACTURING OPERATIONS DOLLARS (Sum of rows 6, 7, 9, 11, and 12)" ~ "Summary",
        .data$`Functional Data Element` == "(14) RAW MATERIAL DOLLARS" ~ "Material",
        .data$`Functional Data Element` == "(15) PURCHASED PARTS DOLLARS" ~ "Material",
        .data$`Functional Data Element` == "(16) PURCHASED EQUIPMENT DOLLARS" ~ "Material",
        .data$`Functional Data Element` == "(17) MATERIAL HANDLING OVERHEAD DOLLARS" ~ "Material",
        .data$`Functional Data Element` == "(18) TOTAL DIRECT-REPORTING SUBCONTRACTOR DOLLARS" ~ "Material",
        .data$`Functional Data Element` == "(19) TOTAL MATERIAL DOLLARS" ~ "Summary",
        .data$`Functional Data Element` == "(20) OTHER COSTS NOT SHOWN ELSEWHERE (Specify in Remarks)" ~ "Other",
        .data$`Functional Data Element` == "(21) TOTAL COST (Direct and Overhead)" ~ "Summary",
        TRUE ~ "REMOVE"
      )
    ) %>%
    dplyr::filter(.data$`Functional Category` != "REMOVE")

  fchr_data$`Functional Category` <-
    fchr_data$`Functional Category` %>%
    forcats::as_factor()

  # Add a column to mark unit of measure. -------------------------------------
  fchr_data <-
    fchr_data %>% dplyr::mutate(
      `Unit of Measure` = dplyr::case_when(
        .data$`Functional Data Element` == "(1) DIRECT ENGINEERING LABOR HOURS" ~ "Hours",
        .data$`Functional Data Element` == "(2) DIRECT ENGINEERING LABOR DOLLARS" ~ "TY $K",
        .data$`Functional Data Element` == "(3) ENGINEERING OVERHEAD DOLLARS" ~ "TY $K",
        .data$`Functional Data Element` == "(4) TOTAL ENGINEERING DOLLARS" ~ "TY $K",
        .data$`Functional Data Element` == "(5) DIRECT TOOLING LABOR HOURS" ~ "Hours",
        .data$`Functional Data Element` == "(6) DIRECT TOOLING LABOR DOLLARS" ~ "TY $K",
        .data$`Functional Data Element` == "(7) DIRECT TOOLING & EQUIPMENT DOLLARS" ~ "TY $K",
        .data$`Functional Data Element` == "(8) DIRECT QUALITY CONTROL LABOR HOURS" ~ "Hours",
        .data$`Functional Data Element` == "(9) DIRECT QUALITY CONTROL LABOR DOLLARS" ~ "TY $K",
        .data$`Functional Data Element` == "(10) DIRECT MANUFACTURING LABOR HOURS" ~ "Hours",
        .data$`Functional Data Element` == "(11) DIRECT MANUFACTURING LABOR DOLLARS" ~ "TY $K",
        .data$`Functional Data Element` == "(12) MANUFACTURING OPERATIONS OVERHEAD DOLLARS (Including Tooling and Quality Control)" ~ "TY $K",
        .data$`Functional Data Element` == "(13) TOTAL MANUFACTURING OPERATIONS DOLLARS (Sum of rows 6, 7, 9, 11, and 12)" ~ "TY $K",
        .data$`Functional Data Element` == "(14) RAW MATERIAL DOLLARS" ~ "TY $K",
        .data$`Functional Data Element` == "(15) PURCHASED PARTS DOLLARS" ~ "TY $K",
        .data$`Functional Data Element` == "(16) PURCHASED EQUIPMENT DOLLARS" ~ "TY $K",
        .data$`Functional Data Element` == "(17) MATERIAL HANDLING OVERHEAD DOLLARS" ~ "TY $K",
        .data$`Functional Data Element` == "(18) TOTAL DIRECT-REPORTING SUBCONTRACTOR DOLLARS" ~ "TY $K",
        .data$`Functional Data Element` == "(19) TOTAL MATERIAL DOLLARS" ~ "TY $K",
        .data$`Functional Data Element` == "(20) OTHER COSTS NOT SHOWN ELSEWHERE (Specify in Remarks)" ~ "TY $K",
        .data$`Functional Data Element` == "(21) TOTAL COST (Direct and Overhead)" ~ "TY $K"
      )
    )

  fchr_data$`Unit of Measure` <-
    fchr_data$`Unit of Measure` %>%
    forcats::as_factor()

  # Create the cell_to_tibble function. ---------------------------------------
  # Here we create a function to grab additional data from each worksheet and
  # add the data to a tibble along with the source worksheet it came from.
  # Later we will use the source worksheet to left_join the additional data
  # into the main 1921-1 tibble.
  .cell_to_tibble <- function(path, range, col_type, col_name) {
    temp_df <- path %>%
      readxl::excel_sheets() %>%
      purrr::set_names() %>%
      purrr::map_df(~ suppressWarnings(
        readxl::read_excel(
          path = path,
          sheet = .x,
          range = range,
          col_types = col_type,
          col_names = col_name
        )
      ),
      .id = "sheet")  %>%
      dplyr::rename("source_worksheet_title" = .data$sheet)

    return(temp_df)
  }

  # Use cell_to_tibble to grab additional data elements from the 1921-1. ------
  `WBS Element Code` <- .cell_to_tibble(path, "B21", "text", "WBS Element Code")
  `WBS Reporting Element` <- .cell_to_tibble(path, "G21", "text", "WBS Reporting Element")
  `Number of Units to Date` <- .cell_to_tibble(path, "K22", "numeric", "Number of Units to Date")
  `Number of Units At Completion` <- .cell_to_tibble(path, "M22", "numeric", "Number of Units At Completion")
  Remarks <- .cell_to_tibble(path, "B53", "text", "Remarks")

  # Add the additional elements as columns in the 1921-1 tibble. --------------
  fchr_data <-
    fchr_data %>%
    dplyr::left_join(`WBS Element Code`, by = "source_worksheet_title") %>%
    dplyr::left_join(`WBS Reporting Element`, by = "source_worksheet_title") %>%
    dplyr::left_join(`Number of Units to Date`, by = "source_worksheet_title") %>%
    dplyr::left_join(`Number of Units At Completion`, by = "source_worksheet_title") %>%
    dplyr::left_join(Remarks, by = "source_worksheet_title")

  fchr_data$`WBS Element Code` <-
    fchr_data$`WBS Element Code` %>%
    forcats::as_factor()

  fchr_data$`WBS Reporting Element` <-
    fchr_data$`WBS Reporting Element` %>%
    forcats::as_factor()

  fchr_data$`Functional Data Element` <-
    fchr_data$`Functional Data Element` %>%
    forcats::as_factor()

  # Reorder the columns before returning. -------------------------------------
  fchr_data <-
    fchr_data %>%
    dplyr::select(
      "WBS Element Code",                                      # "WBSElementID"
      "WBS Reporting Element",                                 # "WBSElementName"
      "Functional Category",                                   # "FunctionalCategory"
      "Functional Data Element",                               # "StandardCategory"
      "Unit of Measure",
      "Costs and Hours Incurred To Date - Nonrecurring",       # "NonrecurringCostsToDate"
      "Costs and Hours Incurred To Date - Recurring",          # "RecurringCostsToDate"
      "Costs and Hours Incurred To Date - Total",              # "TotalCostsToDate"
      "Costs and Hours Incurred At Completion - Nonrecurring", # "NonrecurringCostsAtCompletion"
      "Costs and Hours Incurred At Completion - Recurring",    # "RecurringCostsAtCompletion"
      "Costs and Hours Incurred At Completion - Total",        # "TotalCostsAtCompletion"
      "Number of Units to Date",                               # "QuantityToDate"
      "Number of Units At Completion",                         # "QuantityAtCompletion"
      "Remarks"                                                # "WBSElementRemark"
    )

  return(list(metadata = metadata,
              reported_data = fchr_data))

}

