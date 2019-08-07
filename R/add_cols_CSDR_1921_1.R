#' Add Supporting Columns to a CSDR 1921-1 Tibble
#'
#' Takes a CSDR 1921-1 tibble and adds additional columns to allow for
#' additional/easier sorting/filtering.
#'
#' @title add_cols_CSDR_1921_1
#' @author Daniel Germony \email{daniel.r.germony.civ@@mail.mil}
#' @param CSDR_1921_1 Required. A tibble of CSDR 1921-1 data that has been
#'   imported using the \code{import_CSDR_1921()} function.
#' @return Returns a modified tibble of the CSDR 1921-1 data with additional
#'   columns added.
#' @export

# @import magrittr, tidyr, forcats, dplyr

add_cols_CSDR_1921_1 <- function(CSDR_1921_1) {

  # Import the pipe! ----------------------------------------------------------
  `%>%` <- magrittr::`%>%`

  # Gather the 1921-1 data. ---------------------------------------------------
  CSDR_1921_1_plus <-
    CSDR_1921_1 %>%
    tidyr::gather(key = "TYPE",
                  value = "VALUE",
                  `A. NONRECURRING INCURRED TO DATE`:`F. TOTAL INCURRED AT COMPLETION`)

  CSDR_1921_1_plus$TYPE <-
    CSDR_1921_1_plus$TYPE %>%
    forcats::as_factor()

  # ADD "Recurring / Nonrecurring" column. ---------------------------
  CSDR_1921_1_plus <-
    CSDR_1921_1_plus %>%
    dplyr::mutate(
      "Recurring / Nonrecurring" = dplyr::case_when(
        TYPE == "A. NONRECURRING INCURRED TO DATE" ~ "Nonrecurring",
        TYPE == "B. RECURRING INCURRED TO DATE" ~ "Recurring",
        TYPE == "C. TOTAL INCURRED TO DATE" ~ "Total",
        TYPE == "D. NONRECURRING INCURRED AT COMPLETION" ~ "Nonrecurring",
        TYPE == "E. RECURRING INCURRED AT COMPLETION" ~ "Recurring",
        TYPE == "F. TOTAL INCURRED AT COMPLETION" ~ "Total",
        TRUE ~ "ERROR"
      )
    )

  CSDR_1921_1_plus$`Recurring / Nonrecurring` <-
    CSDR_1921_1_plus$`Recurring / Nonrecurring` %>%
    forcats::as_factor()

  # Add "To Date / At Completion" column. --------------------------------------
  CSDR_1921_1_plus <-
    CSDR_1921_1_plus %>%
    dplyr::mutate(
      "To Date / At Completion" = dplyr::case_when(
        TYPE == "A. NONRECURRING INCURRED TO DATE" ~ "To Date",
        TYPE == "B. RECURRING INCURRED TO DATE" ~ "To Date",
        TYPE == "C. TOTAL INCURRED TO DATE" ~ "To Date",
        TYPE == "D. NONRECURRING INCURRED AT COMPLETION" ~ "At Completion",
        TYPE == "E. RECURRING INCURRED AT COMPLETION" ~ "At Completion",
        TYPE == "F. TOTAL INCURRED AT COMPLETION" ~ "At Completion",
        TRUE ~ "ERROR"
      )
    )

  CSDR_1921_1_plus$`To Date / At Completion` <-
    CSDR_1921_1_plus$`To Date / At Completion` %>%
    forcats::as_factor()

  # Add "FUNCTIONAL DATA ELEMENT NUMBER" column. ------------------------------
  CSDR_1921_1_plus <-
    CSDR_1921_1_plus %>% dplyr::mutate(
      "FUNCTIONAL DATA ELEMENT NUMBER" = dplyr::case_when(
        `FUNCTIONAL DATA ELEMENTS` == "(1) DIRECT ENGINEERING LABOR HOURS" ~ 1L,
        `FUNCTIONAL DATA ELEMENTS` == "(2) DIRECT ENGINEERING LABOR DOLLARS" ~ 2L,
        `FUNCTIONAL DATA ELEMENTS` == "(3) ENGINEERING OVERHEAD DOLLARS" ~ 3L,
        `FUNCTIONAL DATA ELEMENTS` == "(4) TOTAL ENGINEERING DOLLARS" ~ 4L,
        `FUNCTIONAL DATA ELEMENTS` == "(5) DIRECT TOOLING LABOR HOURS" ~ 5L,
        `FUNCTIONAL DATA ELEMENTS` == "(6) DIRECT TOOLING LABOR DOLLARS" ~ 6L,
        `FUNCTIONAL DATA ELEMENTS` == "(7) DIRECT TOOLING & EQUIPMENT DOLLARS" ~ 7L,
        `FUNCTIONAL DATA ELEMENTS` == "(8) DIRECT QUALITY CONTROL LABOR HOURS" ~ 8L,
        `FUNCTIONAL DATA ELEMENTS` == "(9) DIRECT QUALITY CONTROL LABOR DOLLARS" ~ 9L,
        `FUNCTIONAL DATA ELEMENTS` == "(10) DIRECT MANUFACTURING LABOR HOURS" ~ 10L,
        `FUNCTIONAL DATA ELEMENTS` == "(11) DIRECT MANUFACTURING LABOR DOLLARS" ~ 11L,
        `FUNCTIONAL DATA ELEMENTS` == "(12) MANUFACTURING OPERATIONS OVERHEAD DOLLARS (Including Tooling and Quality Control)" ~ 12L,
        `FUNCTIONAL DATA ELEMENTS` == "(13) TOTAL MANUFACTURING OPERATIONS DOLLARS (Sum of rows 6, 7, 9, 11, and 12)" ~ 13L,
        `FUNCTIONAL DATA ELEMENTS` == "(14) RAW MATERIAL DOLLARS" ~ 14L,
        `FUNCTIONAL DATA ELEMENTS` == "(15) PURCHASED PARTS DOLLARS" ~ 15L,
        `FUNCTIONAL DATA ELEMENTS` == "(16) PURCHASED EQUIPMENT DOLLARS" ~ 16L,
        `FUNCTIONAL DATA ELEMENTS` == "(17) MATERIAL HANDLING OVERHEAD DOLLARS" ~ 17L,
        `FUNCTIONAL DATA ELEMENTS` == "(18) TOTAL DIRECT-REPORTING SUBCONTRACTOR DOLLARS" ~ 18L,
        `FUNCTIONAL DATA ELEMENTS` == "(19) TOTAL MATERIAL DOLLARS" ~ 19L,
        `FUNCTIONAL DATA ELEMENTS` == "(20) OTHER COSTS NOT SHOWN ELSEWHERE (Specify in Remarks)" ~ 20L,
        `FUNCTIONAL DATA ELEMENTS` == "(21) TOTAL COST (Direct and Overhead)" ~ 21L
      )
    )

  # Add "COST TYPE" column. ---------------------------------------------------
  CSDR_1921_1_plus <-
    CSDR_1921_1_plus %>% dplyr::mutate(
      "COST TYPE" = dplyr::case_when(
        # `FUNCTIONAL DATA ELEMENTS` == "(1) DIRECT ENGINEERING LABOR HOURS" ~ NA,
        `FUNCTIONAL DATA ELEMENTS` == "(2) DIRECT ENGINEERING LABOR DOLLARS" ~ "DIRECT",
        `FUNCTIONAL DATA ELEMENTS` == "(3) ENGINEERING OVERHEAD DOLLARS" ~ "OVERHEAD",
        `FUNCTIONAL DATA ELEMENTS` == "(4) TOTAL ENGINEERING DOLLARS" ~ "SUBTOTAL",
        # `FUNCTIONAL DATA ELEMENTS` == "(5) DIRECT TOOLING LABOR HOURS" ~ NA,
        `FUNCTIONAL DATA ELEMENTS` == "(6) DIRECT TOOLING LABOR DOLLARS" ~ "DIRECT",
        `FUNCTIONAL DATA ELEMENTS` == "(7) DIRECT TOOLING & EQUIPMENT DOLLARS" ~ "DIRECT",
        # `FUNCTIONAL DATA ELEMENTS` == "(8) DIRECT QUALITY CONTROL LABOR HOURS" ~ NA,
        `FUNCTIONAL DATA ELEMENTS` == "(9) DIRECT QUALITY CONTROL LABOR DOLLARS" ~ "DIRECT",
        # `FUNCTIONAL DATA ELEMENTS` == "(10) DIRECT MANUFACTURING LABOR HOURS" ~ NA,
        `FUNCTIONAL DATA ELEMENTS` == "(11) DIRECT MANUFACTURING LABOR DOLLARS" ~ "DIRECT",
        `FUNCTIONAL DATA ELEMENTS` == "(12) MANUFACTURING OPERATIONS OVERHEAD DOLLARS (Including Tooling and Quality Control)" ~ "OVERHEAD",
        `FUNCTIONAL DATA ELEMENTS` == "(13) TOTAL MANUFACTURING OPERATIONS DOLLARS (Sum of rows 6, 7, 9, 11, and 12)" ~ "SUBTOTAL",
        `FUNCTIONAL DATA ELEMENTS` == "(14) RAW MATERIAL DOLLARS" ~ "DIRECT",
        `FUNCTIONAL DATA ELEMENTS` == "(15) PURCHASED PARTS DOLLARS" ~ "DIRECT",
        `FUNCTIONAL DATA ELEMENTS` == "(16) PURCHASED EQUIPMENT DOLLARS" ~ "DIRECT",
        `FUNCTIONAL DATA ELEMENTS` == "(17) MATERIAL HANDLING OVERHEAD DOLLARS" ~ "OVERHEAD",
        `FUNCTIONAL DATA ELEMENTS` == "(18) TOTAL DIRECT-REPORTING SUBCONTRACTOR DOLLARS" ~ "DIRECT",
        `FUNCTIONAL DATA ELEMENTS` == "(19) TOTAL MATERIAL DOLLARS" ~ "SUBTOTAL",
        `FUNCTIONAL DATA ELEMENTS` == "(20) OTHER COSTS NOT SHOWN ELSEWHERE (Specify in Remarks)" ~ "OTHER DIRECT COSTS",
        `FUNCTIONAL DATA ELEMENTS` == "(21) TOTAL COST (Direct and Overhead)" ~ "TOTAL"
      )
    )
  CSDR_1921_1_plus$`COST TYPE` <-
    CSDR_1921_1_plus$`COST TYPE` %>%
    forcats::as_factor()

  # Add "COST TYPE" column. ---------------------------------------------------
  CSDR_1921_1_plus <-
    CSDR_1921_1_plus %>% dplyr::mutate(
      "SUB-FUCTIONAL CATIGORY" = dplyr::case_when(
        `FUNCTIONAL DATA ELEMENTS` == "(1) DIRECT ENGINEERING LABOR HOURS" ~ "ENGINEERING",
        `FUNCTIONAL DATA ELEMENTS` == "(2) DIRECT ENGINEERING LABOR DOLLARS" ~ "ENGINEERING",
        `FUNCTIONAL DATA ELEMENTS` == "(3) ENGINEERING OVERHEAD DOLLARS" ~ "ENGINEERING",
        `FUNCTIONAL DATA ELEMENTS` == "(4) TOTAL ENGINEERING DOLLARS" ~ "ENGINEERING",
        `FUNCTIONAL DATA ELEMENTS` == "(5) DIRECT TOOLING LABOR HOURS" ~ "TOOLING & EQUIPMENT",
        `FUNCTIONAL DATA ELEMENTS` == "(6) DIRECT TOOLING LABOR DOLLARS" ~ "TOOLING & EQUIPMENT",
        `FUNCTIONAL DATA ELEMENTS` == "(7) DIRECT TOOLING & EQUIPMENT DOLLARS" ~ "TOOLING & EQUIPMENT",
        `FUNCTIONAL DATA ELEMENTS` == "(8) DIRECT QUALITY CONTROL LABOR HOURS" ~ "QUALITY CONTROL",
        `FUNCTIONAL DATA ELEMENTS` == "(9) DIRECT QUALITY CONTROL LABOR DOLLARS" ~ "QUALITY CONTROL",
        `FUNCTIONAL DATA ELEMENTS` == "(10) DIRECT MANUFACTURING LABOR HOURS" ~ "MANUFACTURING",
        `FUNCTIONAL DATA ELEMENTS` == "(11) DIRECT MANUFACTURING LABOR DOLLARS" ~ "MANUFACTURING",
        `FUNCTIONAL DATA ELEMENTS` == "(12) MANUFACTURING OPERATIONS OVERHEAD DOLLARS (Including Tooling and Quality Control)" ~ "MANUFACTURING OPERATIONS",
        `FUNCTIONAL DATA ELEMENTS` == "(13) TOTAL MANUFACTURING OPERATIONS DOLLARS (Sum of rows 6, 7, 9, 11, and 12)" ~ "MANUFACTURING OPERATIONS",
        `FUNCTIONAL DATA ELEMENTS` == "(14) RAW MATERIAL DOLLARS" ~ "RAW MATERIAL",
        `FUNCTIONAL DATA ELEMENTS` == "(15) PURCHASED PARTS DOLLARS" ~ "PURCHASED PARTS",
        `FUNCTIONAL DATA ELEMENTS` == "(16) PURCHASED EQUIPMENT DOLLARS" ~ "PURCHASED EQUIPMENT",
        `FUNCTIONAL DATA ELEMENTS` == "(17) MATERIAL HANDLING OVERHEAD DOLLARS" ~ "MATERIAL HANDLING",
        `FUNCTIONAL DATA ELEMENTS` == "(18) TOTAL DIRECT-REPORTING SUBCONTRACTOR DOLLARS" ~ "DIRECT-REPORTING SUBCONTRACTOR",
        `FUNCTIONAL DATA ELEMENTS` == "(19) TOTAL MATERIAL DOLLARS" ~ "MATERIAL",
        `FUNCTIONAL DATA ELEMENTS` == "(20) OTHER COSTS NOT SHOWN ELSEWHERE (Specify in Remarks)" ~ "OTHER DIRECT COSTS",
        `FUNCTIONAL DATA ELEMENTS` == "(21) TOTAL COST (Direct and Overhead)" ~ "TOTAL"
      )
    )
  CSDR_1921_1_plus$`SUB-FUCTIONAL CATIGORY` <-
    CSDR_1921_1_plus$`SUB-FUCTIONAL CATIGORY` %>%
    forcats::as_factor()

  # Reorder columns before return. --------------------------------------------
  # Reorder the columns.
  CSDR_1921_1_plus <-
    CSDR_1921_1_plus %>%
    dplyr::select(
      "18. WBS ELEMENT CODE",
      "19. WBS REPORTING ELEMENT",
      "FUNCTIONAL CATEGORY",
      "SUB-FUCTIONAL CATIGORY",
      "Unit of Measure",
      "TYPE",
      "Recurring / Nonrecurring",
      "To Date / At Completion",
      "COST TYPE",
      "FUNCTIONAL DATA ELEMENTS",
      "FUNCTIONAL DATA ELEMENT NUMBER",
      "VALUE",
      "20a. NUMBER OF UNITS TO DATE",
      "20b. NUMBER OF UNITS AT COMPLETION",
      "22. REMARKS",
      "source_worksheet_title"
    ) %>%
    dplyr::arrange(`18. WBS ELEMENT CODE`,
                   `TYPE`,
                   `To Date / At Completion`,
                   `FUNCTIONAL DATA ELEMENTS`)

  return(CSDR_1921_1_plus)
}
