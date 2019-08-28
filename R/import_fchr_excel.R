#' Import a CSDR FCHR (DD Form 1921-1) from Excel into a Tibble
#'
#' Takes a CSDR Functional Cost-Hour Report (FCHR) (DD Form 1921-1) Excel
#' worksheet and imports it to a tibble.
#'
#' @title import_fchr_excel
#' @author Daniel Germony \email{daniel.r.germony.civ@@mail.mil}
#' @param path Required. Path to the xls/xlsx file which contains the CSDR
#'    FCHR 1921-1. Note the Excel file must have FCHR data on the first
#'    worksheet (remove any coversheets prior to usingthis script) and cannot
#'    have columns or rows moved/changed from the standard from (i.e., the
#'    script assumes a cPet produced/compliant file is provided).
#' @return Returns a list of two tibbles. The first tibble includes each WBS
#'    element as a row and the repoted data as columns. The second tibble
#'    includes all of the submission's metadata in two columns.
#' @export

import_fchr_excel <- function(path) {
  
  # Import the pipe! ----------------------------------------------------------
  `%>%` <- magrittr::`%>%`
  
  # Create a function to help get data from individual Excel cells. -----------
  grab_cell <- function(path, cell_range) {
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
    "Security Classification"              = grab_cell(path, "G2"),
    "Major Program Name"                   = grab_cell(path, "F6"),
    "Major Program Phase/Milestone"        = stringr::str_c(sep = ", ",
      if (is.na(grab_cell(path, "B8"))) NULL else "Pre-A",
      if (is.na(grab_cell(path, "B9"))) NULL else "A",
      if (is.na(grab_cell(path, "D8"))) NULL else "B",
      if (is.na(grab_cell(path, "D9"))) NULL else "C-LRIP",
      if (is.na(grab_cell(path, "F8"))) NULL else "C-FRP",
      if (is.na(grab_cell(path, "F9"))) NULL else "O&S"),
    "Prime Mission Product"                = grab_cell(path, "H9"),
    "Reporting Organization Type"          = stringr::str_c(sep = ", ",
      if (is.na(grab_cell(path, "I8"))) NULL else "PRIME / ASSOCIATE CONTRACTOR",
      if (is.na(grab_cell(path, "K9"))) NULL else "DIRECT-REPORTING SUBCONTRACTOR",
      if (is.na(grab_cell(path, "M8"))) NULL else "GOVERNMENT"),
    "Organization Name & Address"          = grab_cell(path, "O9"),
    "Division Name  & Address"             = grab_cell(path, "R9"),
    "Approved Plan Number"                 = grab_cell(path, "T9"),
    "Customer"                             = grab_cell(path, "B12"),
    "Contract No"                          = grab_cell(path, "M12"),
    "Latest Modification"                  = grab_cell(path, "M13"),
    "Solicitation No"                      = grab_cell(path, "P12"),
    "Contract Name"                        = grab_cell(path, "P13"),
    "Task Order/Deliver Order/Lot Number"  = grab_cell(path, "S12"),
    "Period of Performance Start Date"     = grab_cell(path, "F15"),
    "Period of Performance End Date"       = grab_cell(path, "F16"),
    "Report Cycle"                         = stringr::str_c(sep = ", ",
      if (is.na(grab_cell(path, "K15"))) NULL else "INITIAL",
      if (is.na(grab_cell(path, "K16"))) NULL else "INTERIM",
      if (is.na(grab_cell(path, "K17"))) NULL else "FINAL"),
    "Submission Number"                    = grab_cell(path, "M15"),
    "Resubmission Number"                  = grab_cell(path, "P16"),
    "Report As Of"                         = grab_cell(path, "S15"),
    "Point of Contact Name"                = grab_cell(path, "B19"),
    "Department"                           = grab_cell(path, "G19"),
    "Telephone Number"                     = grab_cell(path, "K19"),
    "Email Address"                        = grab_cell(path, "O19"),
    "Date Prepared"                        = grab_cell(path, "S19"),
    "Appropriation"                        = stringr::str_c(sep = ", ",
      if (is.na(grab_cell(path, "P21"))) NULL else "RDT&E",
      if (is.na(grab_cell(path, "P22"))) NULL else "PROCUREMENT",
      if (is.na(grab_cell(path, "P23"))) NULL else "O&M")
  ) %>% tidyr::pivot_longer(
    everything(),
    names_to = "metadata_field",
    values_to = "repoted_value")
  
  # Specify FCHR column types and mark that merged columns are skipped. -------
  fchr_col_types <- c(
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
  fchr_col_names <- c(
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
        col_types = fchr_col_types,
        col_names = fchr_col_names
      )),
      .id = "sheet") %>%
    dplyr::rename("WBS Element Code" = sheet)
  
  # Remove Functional Category rows then add them as a column. ----------------
  fchr_data <-
    fchr_data %>%
    dplyr::filter(!(
      `Functional Data Element` %in% c(
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
        `Functional Data Element` == "(1) DIRECT ENGINEERING LABOR HOURS" ~ "Engineering",
        `Functional Data Element` == "(2) DIRECT ENGINEERING LABOR DOLLARS" ~ "Engineering",
        `Functional Data Element` == "(3) ENGINEERING OVERHEAD DOLLARS" ~ "Engineering",
        `Functional Data Element` == "(4) TOTAL ENGINEERING DOLLARS" ~ "Summary",
        `Functional Data Element` == "(5) DIRECT TOOLING LABOR HOURS" ~ "Tooling",
        `Functional Data Element` == "(6) DIRECT TOOLING LABOR DOLLARS" ~ "Tooling",
        `Functional Data Element` == "(7) DIRECT TOOLING & EQUIPMENT DOLLARS" ~ "Tooling",
        `Functional Data Element` == "(8) DIRECT QUALITY CONTROL LABOR HOURS" ~ "Quality Control",
        `Functional Data Element` == "(9) DIRECT QUALITY CONTROL LABOR DOLLARS" ~ "Quality Control",
        `Functional Data Element` == "(10) DIRECT MANUFACTURING LABOR HOURS" ~ "Manufacturing",
        `Functional Data Element` == "(11) DIRECT MANUFACTURING LABOR DOLLARS" ~ "Manufacturing",
        `Functional Data Element` == "(12) MANUFACTURING OPERATIONS OVERHEAD DOLLARS (Including Tooling and Quality Control)" ~ "Manufacturing Operations",
        `Functional Data Element` == "(13) TOTAL MANUFACTURING OPERATIONS DOLLARS (Sum of rows 6, 7, 9, 11, and 12)" ~ "Summary",
        `Functional Data Element` == "(14) RAW MATERIAL DOLLARS" ~ "Material",
        `Functional Data Element` == "(15) PURCHASED PARTS DOLLARS" ~ "Material",
        `Functional Data Element` == "(16) PURCHASED EQUIPMENT DOLLARS" ~ "Material",
        `Functional Data Element` == "(17) MATERIAL HANDLING OVERHEAD DOLLARS" ~ "Material",
        `Functional Data Element` == "(18) TOTAL DIRECT-REPORTING SUBCONTRACTOR DOLLARS" ~ "Material",
        `Functional Data Element` == "(19) TOTAL MATERIAL DOLLARS" ~ "Summary",
        `Functional Data Element` == "(20) OTHER COSTS NOT SHOWN ELSEWHERE (Specify in Remarks)" ~ "Other",
        `Functional Data Element` == "(21) TOTAL COST (Direct and Overhead)" ~ "Summary",
        TRUE ~ "REMOVE"
      )
    ) %>%
    dplyr::filter(`Functional Category` != "REMOVE")
  
  fchr_data$`Functional Category` <-
    fchr_data$`Functional Category` %>%
    forcats::as_factor()
  
  # Add a column to mark unit of measure. -------------------------------------
  fchr_data <-
    fchr_data %>% dplyr::mutate(
      `Unit of Measure` = dplyr::case_when(
        `Functional Data Element` == "(1) DIRECT ENGINEERING LABOR HOURS" ~ "Hours",
        `Functional Data Element` == "(2) DIRECT ENGINEERING LABOR DOLLARS" ~ "TY $K",
        `Functional Data Element` == "(3) ENGINEERING OVERHEAD DOLLARS" ~ "TY $K",
        `Functional Data Element` == "(4) TOTAL ENGINEERING DOLLARS" ~ "TY $K",
        `Functional Data Element` == "(5) DIRECT TOOLING LABOR HOURS" ~ "Hours",
        `Functional Data Element` == "(6) DIRECT TOOLING LABOR DOLLARS" ~ "TY $K",
        `Functional Data Element` == "(7) DIRECT TOOLING & EQUIPMENT DOLLARS" ~ "TY $K",
        `Functional Data Element` == "(8) DIRECT QUALITY CONTROL LABOR HOURS" ~ "Hours",
        `Functional Data Element` == "(9) DIRECT QUALITY CONTROL LABOR DOLLARS" ~ "TY $K",
        `Functional Data Element` == "(10) DIRECT MANUFACTURING LABOR HOURS" ~ "Hours",
        `Functional Data Element` == "(11) DIRECT MANUFACTURING LABOR DOLLARS" ~ "TY $K",
        `Functional Data Element` == "(12) MANUFACTURING OPERATIONS OVERHEAD DOLLARS (Including Tooling and Quality Control)" ~ "TY $K",
        `Functional Data Element` == "(13) TOTAL MANUFACTURING OPERATIONS DOLLARS (Sum of rows 6, 7, 9, 11, and 12)" ~ "TY $K",
        `Functional Data Element` == "(14) RAW MATERIAL DOLLARS" ~ "TY $K",
        `Functional Data Element` == "(15) PURCHASED PARTS DOLLARS" ~ "TY $K",
        `Functional Data Element` == "(16) PURCHASED EQUIPMENT DOLLARS" ~ "TY $K",
        `Functional Data Element` == "(17) MATERIAL HANDLING OVERHEAD DOLLARS" ~ "TY $K",
        `Functional Data Element` == "(18) TOTAL DIRECT-REPORTING SUBCONTRACTOR DOLLARS" ~ "TY $K",
        `Functional Data Element` == "(19) TOTAL MATERIAL DOLLARS" ~ "TY $K",
        `Functional Data Element` == "(20) OTHER COSTS NOT SHOWN ELSEWHERE (Specify in Remarks)" ~ "TY $K",
        `Functional Data Element` == "(21) TOTAL COST (Direct and Overhead)" ~ "TY $K"
      )
    )
  
  fchr_data$`Unit of Measure` <-
    fchr_data$`Unit of Measure` %>%
    forcats::as_factor()
  
  # # Create the cell_to_tibble function. ---------------------------------------
  # # Here we create a function to grab additional data from each worksheet and
  # # add the data to a tibble along with the source worksheet it came from.
  # # Later we will use the source worksheet to left_join the additional data
  # # into the main 1921-1 tibble.
  # cell_to_tibble <- function(path, range, col_type, col_name) {
  #   temp_df <- path %>%
  #     readxl::excel_sheets() %>%
  #     purrr::set_names() %>%
  #     purrr::map_df(~ suppressWarnings(
  #       readxl::read_excel(
  #         path = path,
  #         sheet = .x,
  #         range = range,
  #         col_types = col_type,
  #         col_names = col_name
  #       )
  #     ),
  #     .id = "sheet")  %>%
  #     dplyr::rename("source_worksheet_title" = sheet)
  #
  #   return(temp_df)
  # }
  #
  # # Use cell_to_tibble to grab additional data elements from the 1921-1. ------
  # `WBS Code` <- cell_to_tibble(path, "B21", "text", "WBS Code")
  # `WBS Reporting Element` <- cell_to_tibble(path, "G21", "text", "WBS Reporting Element")
  # `Number of Units to Date` <- cell_to_tibble(path, "K22", "numeric", "Number of Units to Date")
  # `Number of Units At Completion` <- cell_to_tibble(path, "M22", "numeric", "Number of Units At Completion")
  # Remarks <- cell_to_tibble(path, "B53", "text", "Remarks")
  #
  # # Add the additional elements as columns in the 1921-1 tibble. --------------
  # CSDR_1921_1 <-
  #   CSDR_1921_1 %>%
  #   dplyr::left_join(`WBS Code`, by = "source_worksheet_title") %>%
  #   dplyr::left_join(`WBS Reporting Element`, by = "source_worksheet_title") %>%
  #   dplyr::left_join(`Number of Units to Date`, by = "source_worksheet_title") %>%
  #   dplyr::left_join(`Number of Units At Completion`, by = "source_worksheet_title") %>%
  #   dplyr::left_join(Remarks, by = "source_worksheet_title")
  #
  # CSDR_1921_1$`WBS Code` <-
  #   CSDR_1921_1$`WBS Code` %>%
  #   forcats::as_factor()
  #
  # CSDR_1921_1$`WBS Reporting Element` <-
  #   CSDR_1921_1$`WBS Reporting Element` %>%
  #   forcats::as_factor()
  #
  # CSDR_1921_1$`Functional Data Element` <-
  #   CSDR_1921_1$`Functional Data Element` %>%
  #   forcats::as_factor()
  #
  # # Reorder the columns before returning. -------------------------------------
  # CSDR_1921_1 <-
  #   CSDR_1921_1 %>%
  #   dplyr::select(
  #     "WBS Code",                                              # "WBSElementID"
  #     "WBS Reporting Element",                                 # "WBSElementName"
  #     "Functional Category",                                   # "FunctionalCategory"
  #     "Functional Data Element",                               # "StandardCategory"
  #     "Unit of Measure",
  #     "Costs and Hours Incurred To Date - Nonrecurring",       # "NonrecurringCostsToDate"
  #     "Costs and Hours Incurred To Date - Recurring",          # "RecurringCostsToDate"
  #     "Costs and Hours Incurred To Date - Total",              # "TotalCostsToDate"
  #     "Costs and Hours Incurred At Completion - Nonrecurring", # "NonrecurringCostsAtCompletion"
  #     "Costs and Hours Incurred At Completion - Recurring",    # "RecurringCostsAtCompletion"
  #     "Costs and Hours Incurred At Completion - Total",        # "TotalCostsAtCompletion"
  #     "Number of Units to Date",                               # "QuantityToDate"
  #     "Number of Units At Completion",                         # "QuantityAtCompletion"
  #     "Remarks"                                                # "WBSElementRemark"
  #     # "source_worksheet_title"
  #   )
  #
  return(list(metadata = metadata, 
              reported_data = fchr_excel_data))
  
}

