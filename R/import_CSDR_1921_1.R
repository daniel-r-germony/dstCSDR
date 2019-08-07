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
    "FUNCTIONAL DATA ELEMENTS",
    "A. NONRECURRING INCURRED TO DATE",
    "B. RECURRING INCURRED TO DATE",
    "C. TOTAL INCURRED TO DATE",
    "D. NONRECURRING INCURRED AT COMPLETION",
    "E. RECURRING INCURRED AT COMPLETION",
    "F. TOTAL INCURRED AT COMPLETION"
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
      `FUNCTIONAL DATA ELEMENTS` %in% c(
        "ENGINEERING",
        "MANUFACTURING OPERATIONS",
        "MATERIALS",
        "OTHER COSTS",
        "Summary"
      )
    ))

  CSDR_1921_1 <-
    CSDR_1921_1 %>% dplyr::mutate(
      "FUNCTIONAL CATEGORY" = dplyr::case_when(
        `FUNCTIONAL DATA ELEMENTS` == "(1) DIRECT ENGINEERING LABOR HOURS" ~ "ENGINEERING",
        `FUNCTIONAL DATA ELEMENTS` == "(2) DIRECT ENGINEERING LABOR DOLLARS" ~ "ENGINEERING",
        `FUNCTIONAL DATA ELEMENTS` == "(3) ENGINEERING OVERHEAD DOLLARS" ~ "ENGINEERING",
        `FUNCTIONAL DATA ELEMENTS` == "(4) TOTAL ENGINEERING DOLLARS" ~ "ENGINEERING",
        `FUNCTIONAL DATA ELEMENTS` == "(5) DIRECT TOOLING LABOR HOURS" ~ "MANUFACTURING OPERATIONS",
        `FUNCTIONAL DATA ELEMENTS` == "(6) DIRECT TOOLING LABOR DOLLARS" ~ "MANUFACTURING OPERATIONS",
        `FUNCTIONAL DATA ELEMENTS` == "(7) DIRECT TOOLING & EQUIPMENT DOLLARS" ~ "MANUFACTURING OPERATIONS",
        `FUNCTIONAL DATA ELEMENTS` == "(8) DIRECT QUALITY CONTROL LABOR HOURS" ~ "MANUFACTURING OPERATIONS",
        `FUNCTIONAL DATA ELEMENTS` == "(9) DIRECT QUALITY CONTROL LABOR DOLLARS" ~ "MANUFACTURING OPERATIONS",
        `FUNCTIONAL DATA ELEMENTS` == "(10) DIRECT MANUFACTURING LABOR HOURS" ~ "MANUFACTURING OPERATIONS",
        `FUNCTIONAL DATA ELEMENTS` == "(11) DIRECT MANUFACTURING LABOR DOLLARS" ~ "MANUFACTURING OPERATIONS",
        `FUNCTIONAL DATA ELEMENTS` == "(12) MANUFACTURING OPERATIONS OVERHEAD DOLLARS (Including Tooling and Quality Control)" ~ "MANUFACTURING OPERATIONS",
        `FUNCTIONAL DATA ELEMENTS` == "(13) TOTAL MANUFACTURING OPERATIONS DOLLARS (Sum of rows 6, 7, 9, 11, and 12)" ~ "MANUFACTURING OPERATIONS",
        `FUNCTIONAL DATA ELEMENTS` == "(14) RAW MATERIAL DOLLARS" ~ "MATERIALS",
        `FUNCTIONAL DATA ELEMENTS` == "(15) PURCHASED PARTS DOLLARS" ~ "MATERIALS",
        `FUNCTIONAL DATA ELEMENTS` == "(16) PURCHASED EQUIPMENT DOLLARS" ~ "MATERIALS",
        `FUNCTIONAL DATA ELEMENTS` == "(17) MATERIAL HANDLING OVERHEAD DOLLARS" ~ "MATERIALS",
        `FUNCTIONAL DATA ELEMENTS` == "(18) TOTAL DIRECT-REPORTING SUBCONTRACTOR DOLLARS" ~ "MATERIALS",
        `FUNCTIONAL DATA ELEMENTS` == "(19) TOTAL MATERIAL DOLLARS" ~ "MATERIALS",
        `FUNCTIONAL DATA ELEMENTS` == "(20) OTHER COSTS NOT SHOWN ELSEWHERE (Specify in Remarks)" ~ "OTHER COSTS",
        `FUNCTIONAL DATA ELEMENTS` == "(21) TOTAL COST (Direct and Overhead)" ~ "SUMMARY",
        TRUE ~ "REMOVE"
      )
    ) %>%
    dplyr::filter(`FUNCTIONAL CATEGORY` != "REMOVE")

  CSDR_1921_1$`FUNCTIONAL CATEGORY` <-
    CSDR_1921_1$`FUNCTIONAL CATEGORY` %>%
    forcats::as_factor()

  # Add a column to mark unit of measure. -------------------------------------
  CSDR_1921_1 <-
    CSDR_1921_1 %>% dplyr::mutate(
      "Unit of Measure" = dplyr::case_when(
        `FUNCTIONAL DATA ELEMENTS` == "(1) DIRECT ENGINEERING LABOR HOURS" ~ "Hours",
        `FUNCTIONAL DATA ELEMENTS` == "(2) DIRECT ENGINEERING LABOR DOLLARS" ~ "TY $K",
        `FUNCTIONAL DATA ELEMENTS` == "(3) ENGINEERING OVERHEAD DOLLARS" ~ "TY $K",
        `FUNCTIONAL DATA ELEMENTS` == "(4) TOTAL ENGINEERING DOLLARS" ~ "TY $K",
        `FUNCTIONAL DATA ELEMENTS` == "(5) DIRECT TOOLING LABOR HOURS" ~ "Hours",
        `FUNCTIONAL DATA ELEMENTS` == "(6) DIRECT TOOLING LABOR DOLLARS" ~ "TY $K",
        `FUNCTIONAL DATA ELEMENTS` == "(7) DIRECT TOOLING & EQUIPMENT DOLLARS" ~ "TY $K",
        `FUNCTIONAL DATA ELEMENTS` == "(8) DIRECT QUALITY CONTROL LABOR HOURS" ~ "Hours",
        `FUNCTIONAL DATA ELEMENTS` == "(9) DIRECT QUALITY CONTROL LABOR DOLLARS" ~ "TY $K",
        `FUNCTIONAL DATA ELEMENTS` == "(10) DIRECT MANUFACTURING LABOR HOURS" ~ "Hours",
        `FUNCTIONAL DATA ELEMENTS` == "(11) DIRECT MANUFACTURING LABOR DOLLARS" ~ "TY $K",
        `FUNCTIONAL DATA ELEMENTS` == "(12) MANUFACTURING OPERATIONS OVERHEAD DOLLARS (Including Tooling and Quality Control)" ~ "TY $K",
        `FUNCTIONAL DATA ELEMENTS` == "(13) TOTAL MANUFACTURING OPERATIONS DOLLARS (Sum of rows 6, 7, 9, 11, and 12)" ~ "TY $K",
        `FUNCTIONAL DATA ELEMENTS` == "(14) RAW MATERIAL DOLLARS" ~ "TY $K",
        `FUNCTIONAL DATA ELEMENTS` == "(15) PURCHASED PARTS DOLLARS" ~ "TY $K",
        `FUNCTIONAL DATA ELEMENTS` == "(16) PURCHASED EQUIPMENT DOLLARS" ~ "TY $K",
        `FUNCTIONAL DATA ELEMENTS` == "(17) MATERIAL HANDLING OVERHEAD DOLLARS" ~ "TY $K",
        `FUNCTIONAL DATA ELEMENTS` == "(18) TOTAL DIRECT-REPORTING SUBCONTRACTOR DOLLARS" ~ "TY $K",
        `FUNCTIONAL DATA ELEMENTS` == "(19) TOTAL MATERIAL DOLLARS" ~ "DOLLARS",
        `FUNCTIONAL DATA ELEMENTS` == "(20) OTHER COSTS NOT SHOWN ELSEWHERE (Specify in Remarks)" ~ "TY $K",
        `FUNCTIONAL DATA ELEMENTS` == "(21) TOTAL COST (Direct and Overhead)" ~ "TY $K"
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
  WBS_ELEMENT_CODE <- cell_to_tibble(path, "B21", "text", "18. WBS ELEMENT CODE")
  WBS_REPORTING_ELEMENT <- cell_to_tibble(path, "G21", "text", "19. WBS REPORTING ELEMENT")
  NUMBER_OF_UNITS_TO_DATE <- cell_to_tibble(path, "K22", "numeric", "20a. NUMBER OF UNITS TO DATE")
  NUMBER_OF_UNITS_AT_COMPLETION <- cell_to_tibble(path, "M22", "numeric", "20b. NUMBER OF UNITS AT COMPLETION")
  REMARKS <- cell_to_tibble(path, "B53", "text", "22. REMARKS")

  # Add the additional elements as columns in the 1921-1 tibble. --------------
  CSDR_1921_1 <-
    CSDR_1921_1 %>%
    dplyr::left_join(WBS_ELEMENT_CODE, by = "source_worksheet_title") %>%
    dplyr::left_join(WBS_REPORTING_ELEMENT, by = "source_worksheet_title") %>%
    dplyr::left_join(NUMBER_OF_UNITS_TO_DATE, by = "source_worksheet_title") %>%
    dplyr::left_join(NUMBER_OF_UNITS_AT_COMPLETION, by = "source_worksheet_title") %>%
    dplyr::left_join(REMARKS, by = "source_worksheet_title")

  CSDR_1921_1$`18. WBS ELEMENT CODE` <-
    CSDR_1921_1$`18. WBS ELEMENT CODE` %>%
    forcats::as_factor()

  CSDR_1921_1$`19. WBS REPORTING ELEMENT` <-
    CSDR_1921_1$`19. WBS REPORTING ELEMENT` %>%
    forcats::as_factor()

  CSDR_1921_1$`FUNCTIONAL DATA ELEMENTS` <-
    CSDR_1921_1$`FUNCTIONAL DATA ELEMENTS` %>%
    forcats::as_factor()

  # Reorder the columns before returning. -------------------------------------
  CSDR_1921_1 <-
    CSDR_1921_1 %>%
    dplyr::select(
      "18. WBS ELEMENT CODE",
      "19. WBS REPORTING ELEMENT",
      "FUNCTIONAL CATEGORY",
      "FUNCTIONAL DATA ELEMENTS",
      "Unit of Measure",
      "A. NONRECURRING INCURRED TO DATE",
      "B. RECURRING INCURRED TO DATE",
      "C. TOTAL INCURRED TO DATE",
      "D. NONRECURRING INCURRED AT COMPLETION",
      "E. RECURRING INCURRED AT COMPLETION",
      "F. TOTAL INCURRED AT COMPLETION",
      "20a. NUMBER OF UNITS TO DATE",
      "20b. NUMBER OF UNITS AT COMPLETION",
      "22. REMARKS",
      "source_worksheet_title"
    )

  return(CSDR_1921_1)

}
