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

  # Add "Functional Element Number" column. ------------------------------
  CSDR_1921_1_plus <-
    CSDR_1921_1_plus %>% dplyr::mutate(
      "Functional Element Number" = dplyr::case_when(
        `Functional Element` == "(1) DIRECT ENGINEERING LABOR HOURS" ~ 1L,
        `Functional Element` == "(2) DIRECT ENGINEERING LABOR DOLLARS" ~ 2L,
        `Functional Element` == "(3) ENGINEERING OVERHEAD DOLLARS" ~ 3L,
        `Functional Element` == "(4) TOTAL ENGINEERING DOLLARS" ~ 4L,
        `Functional Element` == "(5) DIRECT TOOLING LABOR HOURS" ~ 5L,
        `Functional Element` == "(6) DIRECT TOOLING LABOR DOLLARS" ~ 6L,
        `Functional Element` == "(7) DIRECT TOOLING & EQUIPMENT DOLLARS" ~ 7L,
        `Functional Element` == "(8) DIRECT QUALITY CONTROL LABOR HOURS" ~ 8L,
        `Functional Element` == "(9) DIRECT QUALITY CONTROL LABOR DOLLARS" ~ 9L,
        `Functional Element` == "(10) DIRECT MANUFACTURING LABOR HOURS" ~ 10L,
        `Functional Element` == "(11) DIRECT MANUFACTURING LABOR DOLLARS" ~ 11L,
        `Functional Element` == "(12) MANUFACTURING OPERATIONS OVERHEAD DOLLARS (Including Tooling and Quality Control)" ~ 12L,
        `Functional Element` == "(13) TOTAL MANUFACTURING OPERATIONS DOLLARS (Sum of rows 6, 7, 9, 11, and 12)" ~ 13L,
        `Functional Element` == "(14) RAW MATERIAL DOLLARS" ~ 14L,
        `Functional Element` == "(15) PURCHASED PARTS DOLLARS" ~ 15L,
        `Functional Element` == "(16) PURCHASED EQUIPMENT DOLLARS" ~ 16L,
        `Functional Element` == "(17) MATERIAL HANDLING OVERHEAD DOLLARS" ~ 17L,
        `Functional Element` == "(18) TOTAL DIRECT-REPORTING SUBCONTRACTOR DOLLARS" ~ 18L,
        `Functional Element` == "(19) TOTAL MATERIAL DOLLARS" ~ 19L,
        `Functional Element` == "(20) OTHER COSTS NOT SHOWN ELSEWHERE (Specify in Remarks)" ~ 20L,
        `Functional Element` == "(21) TOTAL COST (Direct and Overhead)" ~ 21L
      )
    )

  # Add "Functional Element" column. ---------------------------------------------------
  CSDR_1921_1_plus <-
    CSDR_1921_1_plus %>% dplyr::mutate(
      "Functional Element_alt" = dplyr::case_when(
        `Functional Element` == "(1) DIRECT ENGINEERING LABOR HOURS" ~ "Direct Engineering Labor",
        `Functional Element` == "(2) DIRECT ENGINEERING LABOR DOLLARS" ~ "Direct Engineering Labor",
        `Functional Element` == "(3) ENGINEERING OVERHEAD DOLLARS" ~ "Engineering Overhead",
        `Functional Element` == "(4) TOTAL ENGINEERING DOLLARS" ~ "Summary",
        `Functional Element` == "(5) DIRECT TOOLING LABOR HOURS" ~ "Direct Tooling Labor",
        `Functional Element` == "(6) DIRECT TOOLING LABOR DOLLARS" ~ "Direct Tooling Labor",
        `Functional Element` == "(7) DIRECT TOOLING & EQUIPMENT DOLLARS" ~ "Direct Tooling & Equipment",
        `Functional Element` == "(8) DIRECT QUALITY CONTROL LABOR HOURS" ~ "Direct Quality Control Labor",
        `Functional Element` == "(9) DIRECT QUALITY CONTROL LABOR DOLLARS" ~ "Direct Quality Control Labor",
        `Functional Element` == "(10) DIRECT MANUFACTURING LABOR HOURS" ~ "Direct Manufacturing Labor",
        `Functional Element` == "(11) DIRECT MANUFACTURING LABOR DOLLARS" ~ "Direct Manufacturing Labor",
        `Functional Element` == "(12) MANUFACTURING OPERATIONS OVERHEAD DOLLARS (Including Tooling and Quality Control)" ~ "Manufacturing Operations Overhead",
        `Functional Element` == "(13) TOTAL MANUFACTURING OPERATIONS DOLLARS (Sum of rows 6, 7, 9, 11, and 12)" ~ "Summary",
        `Functional Element` == "(14) RAW MATERIAL DOLLARS" ~ "Raw Material",
        `Functional Element` == "(15) PURCHASED PARTS DOLLARS" ~ "Purchased Parts",
        `Functional Element` == "(16) PURCHASED EQUIPMENT DOLLARS" ~ "Purchased Equipment",
        `Functional Element` == "(17) MATERIAL HANDLING OVERHEAD DOLLARS" ~ "Material Handling/Overhead",
        `Functional Element` == "(18) TOTAL DIRECT-REPORTING SUBCONTRACTOR DOLLARS" ~ "Direct-Reporting Subcontractor",
        `Functional Element` == "(19) TOTAL MATERIAL DOLLARS" ~ "Summary",
        `Functional Element` == "(20) OTHER COSTS NOT SHOWN ELSEWHERE (Specify in Remarks)" ~ "Other Costs Not Shown Elsewhere",
        `Functional Element` == "(21) TOTAL COST (Direct and Overhead)" ~ "Summary"
      )
    )
  CSDR_1921_1_plus$`Functional Element_alt` <-
    CSDR_1921_1_plus$`Functional Element_alt` %>%
    forcats::as_factor()

  # Add "Functional Category" column. ---------------------------------------------------
  CSDR_1921_1_plus <-
    CSDR_1921_1_plus %>% dplyr::mutate(
      "Functional Category" = dplyr::case_when(
        `Functional Element` == "(1) DIRECT ENGINEERING LABOR HOURS" ~ "Engineering",
        `Functional Element` == "(2) DIRECT ENGINEERING LABOR DOLLARS" ~ "Engineering",
        `Functional Element` == "(3) ENGINEERING OVERHEAD DOLLARS" ~ "Engineering",
        `Functional Element` == "(4) TOTAL ENGINEERING DOLLARS" ~ "Engineering",
        `Functional Element` == "(5) DIRECT TOOLING LABOR HOURS" ~ "Tooling",
        `Functional Element` == "(6) DIRECT TOOLING LABOR DOLLARS" ~ "Tooling",
        `Functional Element` == "(7) DIRECT TOOLING & EQUIPMENT DOLLARS" ~ "Tooling",
        `Functional Element` == "(8) DIRECT QUALITY CONTROL LABOR HOURS" ~ "Quality Control",
        `Functional Element` == "(9) DIRECT QUALITY CONTROL LABOR DOLLARS" ~ "Quality Control",
        `Functional Element` == "(10) DIRECT MANUFACTURING LABOR HOURS" ~ "Manufacturing",
        `Functional Element` == "(11) DIRECT MANUFACTURING LABOR DOLLARS" ~ "Manufacturing",
        `Functional Element` == "(12) MANUFACTURING OPERATIONS OVERHEAD DOLLARS (Including Tooling and Quality Control)" ~ "Manufacturing Operations",
        `Functional Element` == "(13) TOTAL MANUFACTURING OPERATIONS DOLLARS (Sum of rows 6, 7, 9, 11, and 12)" ~ "Manufacturing & Manufacturing Operations",
        `Functional Element` == "(14) RAW MATERIAL DOLLARS" ~ "Material",
        `Functional Element` == "(15) PURCHASED PARTS DOLLARS" ~ "Material",
        `Functional Element` == "(16) PURCHASED EQUIPMENT DOLLARS" ~ "Material",
        `Functional Element` == "(17) MATERIAL HANDLING OVERHEAD DOLLARS" ~ "Material",
        `Functional Element` == "(18) TOTAL DIRECT-REPORTING SUBCONTRACTOR DOLLARS" ~ "Material",
        `Functional Element` == "(19) TOTAL MATERIAL DOLLARS" ~ "Material",
        `Functional Element` == "(20) OTHER COSTS NOT SHOWN ELSEWHERE (Specify in Remarks)" ~ "Other",
        `Functional Element` == "(21) TOTAL COST (Direct and Overhead)" ~ "Summary"
      )
    )
  CSDR_1921_1_plus$`Functional Category` <-
    CSDR_1921_1_plus$`Functional Category` %>%
    forcats::as_factor()

  # Reorder columns before return. --------------------------------------------
  # Reorder the columns.
  CSDR_1921_1_plus <-
    CSDR_1921_1_plus %>%
    dplyr::select(
      "18. WBS ELEMENT CODE",
      "19. WBS REPORTING ELEMENT",
      "Functional Category",
      "Unit of Measure",
      "TYPE",
      "Recurring / Nonrecurring",
      "To Date / At Completion",
      "Functional Element",
      "Functional Element_alt",
      "Functional Element Number",
      "VALUE",
      "20a. NUMBER OF UNITS TO DATE",
      "20b. NUMBER OF UNITS AT COMPLETION",
      "22. REMARKS",
      "source_worksheet_title"
    ) %>%
    dplyr::arrange(`18. WBS ELEMENT CODE`,
                   `TYPE`,
                   `To Date / At Completion`,
                   `Functional Element`)

  return(CSDR_1921_1_plus)
}
