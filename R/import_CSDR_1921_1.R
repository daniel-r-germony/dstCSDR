#' Import a CSDR 1921-1 from Excel into a Tibble
#'
#' Takes a CSDR 1921-1 Excel file and imports it to a single long tibble.
#'
#' @title import_CSDR_1921_1
#' @author Daniel Germony \email{daniel.r.germony.civ@@mail.mil}
#' @param path Required. Path to the xls/xlsx file which contains the CSDR
#'    1921-1 data. Note the Excel file must include  only CSDR 1921-1
#'    worksheets, one worksheet per WBS (remove any coversheets prior to using
#'    this script) and cannot have columns or rows moved/changed from the
#'    standard from (i.e., the script assumes a cPet produced/compliant file
#'    is provided).
#' @return Returns a tibble with each CSDR 1921-1 worksheet combined into one
#'    table and traditional  CSDR 1921-1 data as columns. A limited amount of
#'    transformations are done to make produce a proper table of data (e.g.,
#'    functional categories are converted from padded rows to a column).
#' @export

import_CSDR_1921_1 <- function(path) {

  # Import the pipe! ----------------------------------------------------------
  `%>%` <- magrittr::`%>%`

  # Specify 1921-1 column types and mark that merged columns are skipped. -----
  CSDR_1921_1_col_types <- c(
    "text",    # Col B
    "skip",    # Col C
    "skip",    # Col D
    "skip",    # Col E
    "skip",    # Col F
    "skip",    # Col G
    "skip",    # Col H
    "skip",    # Col I
    "skip",    # Col J
    "numeric", # Col K
    "skip",    # Col L
    "numeric", # Col M
    "skip",    # Col N
    "numeric", # Col O
    "numeric", # Col P
    "skip",    # Col Q
    "numeric", # Col R
    "numeric"  # Col S
  )

  # Provide column names for imported 1921-1 columns. -------------------------
  CSDR_1921_1_col_names <- c(
    "Functional Data Element",
    "Costs and Hours Incurred To Date - Nonrecurring",
    "Costs and Hours Incurred To Date - Recurring",
    "Costs and Hours Incurred To Date - Total",
    "Costs and Hours Incurred At Completion - Nonrecurring",
    "Costs and Hours Incurred At Completion - Recurring",
    "Costs and Hours Incurred At Completion - Total"
  )

  # Import all 1921-1 worksheets and combine into a tibble. -------------------
  CSDR_1921_1 <-
    path %>%
    readxl::excel_sheets() %>%
    purrr::set_names() %>%
    purrr::map_df(~suppressWarnings(
      readxl::read_excel(
        path = path,
        sheet = .x,
        range = readxl::cell_limits(c(26, 2), c(51, 19)),
        col_types = CSDR_1921_1_col_types,
        col_names = CSDR_1921_1_col_names
      )),
      .id = "sheet") %>%
    dplyr::rename("source_worksheet_title" = sheet)

  # Remove Functional Category rows then add them as a column. ----------------
  CSDR_1921_1 <-
    CSDR_1921_1 %>%
    dplyr::filter(!(
      `Functional Data Element` %in% c(
        "ENGINEERING",
        "MANUFACTURING OPERATIONS",
        "MATERIALS",
        "OTHER COSTS",
        "SUMMARY"
      )
    ))

  CSDR_1921_1 <-
    CSDR_1921_1 %>% dplyr::mutate(
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

  CSDR_1921_1$`Functional Category` <-
    CSDR_1921_1$`Functional Category` %>%
    forcats::as_factor()

  # Add a column to mark unit of measure. -------------------------------------
  CSDR_1921_1 <-
    CSDR_1921_1 %>% dplyr::mutate(
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

  CSDR_1921_1$`Unit of Measure` <-
    CSDR_1921_1$`Unit of Measure` %>%
    forcats::as_factor()
  # Create the cell_to_tibble function. ---------------------------------------
  # Here we create a function to grab additional data from each worksheet and
  # add the data to a tibble along with the source worksheet it came from.
  # Later we will use the source worksheet to left_join the additional data
  # into the main 1921-1 tibble.
  cell_to_tibble <- function(path, range, col_type, col_name) {
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
      dplyr::rename("source_worksheet_title" = sheet)

    return(temp_df)
  }

  # Use cell_to_tibble to grab additional data elements from the 1921-1. ------
  `WBS Code` <- cell_to_tibble(path, "B21", "text", "WBS Code")
  `WBS Reporting Element` <- cell_to_tibble(path, "G21", "text", "WBS Reporting Element")
  `Number of Units to Date` <- cell_to_tibble(path, "K22", "numeric", "Number of Units to Date")
  `Number of Units At Completion` <- cell_to_tibble(path, "M22", "numeric", "Number of Units At Completion")
  Remarks <- cell_to_tibble(path, "B53", "text", "Remarks")

  # Add the additional elements as columns in the 1921-1 tibble. --------------
  CSDR_1921_1 <-
    CSDR_1921_1 %>%
    dplyr::left_join(`WBS Code`, by = "source_worksheet_title") %>%
    dplyr::left_join(`WBS Reporting Element`, by = "source_worksheet_title") %>%
    dplyr::left_join(`Number of Units to Date`, by = "source_worksheet_title") %>%
    dplyr::left_join(`Number of Units At Completion`, by = "source_worksheet_title") %>%
    dplyr::left_join(Remarks, by = "source_worksheet_title")

  CSDR_1921_1$`WBS Code` <-
    CSDR_1921_1$`WBS Code` %>%
    forcats::as_factor()

  CSDR_1921_1$`WBS Reporting Element` <-
    CSDR_1921_1$`WBS Reporting Element` %>%
    forcats::as_factor()

  CSDR_1921_1$`Functional Data Element` <-
    CSDR_1921_1$`Functional Data Element` %>%
    forcats::as_factor()

  # Reorder the columns before returning. -------------------------------------
  CSDR_1921_1 <-
    CSDR_1921_1 %>%
    dplyr::select(
      "WBS Code",                                              # "WBSElementID"
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
      # "source_worksheet_title"
    )

  return(CSDR_1921_1)

}

