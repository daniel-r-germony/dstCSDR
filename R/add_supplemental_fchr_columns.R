#' Add Supplemental Columns to a CDSR FCHR (DD Form 1921-1) Tibble
#'
#' Takes a CSDR Functional Cost-Hour Report (FCHR) (DD Form 1921-1) object and
#' adds supplemental columns to the to the \code{`reported_data`} tibble to
#' allow for additional/easier sorting/filtering.
#'
#' @title add_supplemental_fchr_columns
#' @author Daniel Germony \email{daniel.r.germony.civ@@mail.mil}
#' @param fchr_object Required. A list object of CSDR FCHR 1921-1 data that has
#'   been imported using the \code{import_cdsr_excel()} function.
#' @return Adds the following columns to the \code{`reported_data`} tibble:
#'   \enumerate{
#'     \item Functional Category
#'     \item Recurring / Nonrecurring
#'     \item To Date / At Completion
#'     \item Functional Element
#'     \item Functional Data Element Number
#'     }
#' @export

add_supplemental_fchr_columns <- function(fchr_object) {

  fchr_plus <- fchr_object

  # Gather the 1921-1 data. ---------------------------------------------------
  fchr_plus[["reported_data"]] <- fchr_object[["reported_data"]] %>%
    tidyr::gather(
      key = "Reported Data Field",
      value = "Reported Data Value",
      .data$`Costs and Hours Incurred To Date - Nonrecurring`:.data$`Costs and Hours Incurred At Completion - Total`
    )

  fchr_plus[["reported_data"]]$`Reported Data Field` <-
    fchr_plus[["reported_data"]]$`Reported Data Field` %>%
    forcats::as_factor()

  # Add Recurring / Nonrecurring" column. ---------------------------
  fchr_plus[["reported_data"]] <-
    fchr_plus[["reported_data"]] %>%
    dplyr::mutate(
      "Recurring / Nonrecurring" = dplyr::case_when(
        .data$`Reported Data Field` == "Costs and Hours Incurred To Date - Nonrecurring" ~ "Nonrecurring",
        .data$`Reported Data Field` == "Costs and Hours Incurred To Date - Recurring" ~ "Recurring",
        .data$`Reported Data Field` == "Costs and Hours Incurred To Date - Total" ~ "Total",
        .data$`Reported Data Field` == "Costs and Hours Incurred At Completion - Nonrecurring" ~ "Nonrecurring",
        .data$`Reported Data Field` == "Costs and Hours Incurred At Completion - Recurring" ~ "Recurring",
        .data$`Reported Data Field` == "Costs and Hours Incurred At Completion - Total" ~ "Total",
        TRUE ~ NA_character_
      )
    )

  fchr_plus[["reported_data"]]$`Recurring / Nonrecurring` <-
    fchr_plus[["reported_data"]]$`Recurring / Nonrecurring` %>%
    forcats::as_factor()

  # Add "To Date / At Completion" column. --------------------------------------
  fchr_plus[["reported_data"]] <-
    fchr_plus[["reported_data"]] %>%
    dplyr::mutate(
      "To Date / At Completion" = dplyr::case_when(
        .data$`Reported Data Field` == "Costs and Hours Incurred To Date - Nonrecurring" ~ "To Date",
        .data$`Reported Data Field` == "Costs and Hours Incurred To Date - Recurring" ~ "To Date",
        .data$`Reported Data Field` == "Costs and Hours Incurred To Date - Total" ~ "To Date",
        .data$`Reported Data Field` == "Costs and Hours Incurred At Completion - Nonrecurring" ~ "At Completion",
        .data$`Reported Data Field` == "Costs and Hours Incurred At Completion - Recurring" ~ "At Completion",
        .data$`Reported Data Field` == "Costs and Hours Incurred At Completion - Total" ~ "At Completion",
        TRUE ~ NA_character_
      )
    )

  fchr_plus[["reported_data"]]$`To Date / At Completion` <-
    fchr_plus[["reported_data"]]$`To Date / At Completion` %>%
    forcats::as_factor()

  # Add "Functional Data Element Number" column. ------------------------------
  fchr_plus[["reported_data"]] <-
    fchr_plus[["reported_data"]] %>% dplyr::mutate(
      "Functional Data Element Number" = dplyr::case_when(
        .data$`Functional Data Element` == "(1) DIRECT ENGINEERING LABOR HOURS" ~ "1",
        .data$`Functional Data Element` == "(2) DIRECT ENGINEERING LABOR DOLLARS" ~ "2",
        .data$`Functional Data Element` == "(3) ENGINEERING OVERHEAD DOLLARS" ~ "3",
        .data$`Functional Data Element` == "(4) TOTAL ENGINEERING DOLLARS" ~ "4",
        .data$`Functional Data Element` == "(5) DIRECT TOOLING LABOR HOURS" ~ "5",
        .data$`Functional Data Element` == "(6) DIRECT TOOLING LABOR DOLLARS" ~ "6",
        .data$`Functional Data Element` == "(7) DIRECT TOOLING & EQUIPMENT DOLLARS" ~ "7",
        .data$`Functional Data Element` == "(8) DIRECT QUALITY CONTROL LABOR HOURS" ~ "8",
        .data$`Functional Data Element` == "(9) DIRECT QUALITY CONTROL LABOR DOLLARS" ~ "9",
        .data$`Functional Data Element` == "(10) DIRECT MANUFACTURING LABOR HOURS" ~ "10",
        .data$`Functional Data Element` == "(11) DIRECT MANUFACTURING LABOR DOLLARS" ~ "11",
        .data$`Functional Data Element` == "(12) MANUFACTURING OPERATIONS OVERHEAD DOLLARS (Including Tooling and Quality Control)" ~ "12",
        .data$`Functional Data Element` == "(13) TOTAL MANUFACTURING OPERATIONS DOLLARS (Sum of rows 6, 7, 9, 11, and 12)" ~ "13",
        .data$`Functional Data Element` == "(14) RAW MATERIAL DOLLARS" ~ "14",
        .data$`Functional Data Element` == "(15) PURCHASED PARTS DOLLARS" ~ "15",
        .data$`Functional Data Element` == "(16) PURCHASED EQUIPMENT DOLLARS" ~ "16",
        .data$`Functional Data Element` == "(17) MATERIAL HANDLING OVERHEAD DOLLARS" ~ "17",
        .data$`Functional Data Element` == "(18) TOTAL DIRECT-REPORTING SUBCONTRACTOR DOLLARS" ~ "18",
        .data$`Functional Data Element` == "(19) TOTAL MATERIAL DOLLARS" ~ "19",
        .data$`Functional Data Element` == "(20) OTHER COSTS NOT SHOWN ELSEWHERE (Specify in Remarks)" ~ "20",
        .data$`Functional Data Element` == "(21) TOTAL COST (Direct and Overhead)" ~ "21",
        TRUE ~ NA_character_
      )
    )

  fchr_plus[["reported_data"]]$`Functional Data Element Number` <-
    fchr_plus[["reported_data"]]$`Functional Data Element Number` %>%
    forcats::as_factor()

  # Add "Functional Element" column. ---------------------------------------------------
  fchr_plus[["reported_data"]] <-
    fchr_plus[["reported_data"]] %>% dplyr::mutate(
      "Functional Element" = dplyr::case_when(
        .data$`Functional Data Element` == "(1) DIRECT ENGINEERING LABOR HOURS" ~ "Direct Engineering Labor",
        .data$`Functional Data Element` == "(2) DIRECT ENGINEERING LABOR DOLLARS" ~ "Direct Engineering Labor",
        .data$`Functional Data Element` == "(3) ENGINEERING OVERHEAD DOLLARS" ~ "Engineering Overhead",
        .data$`Functional Data Element` == "(4) TOTAL ENGINEERING DOLLARS" ~ "Summary",
        .data$`Functional Data Element` == "(5) DIRECT TOOLING LABOR HOURS" ~ "Direct Tooling Labor",
        .data$`Functional Data Element` == "(6) DIRECT TOOLING LABOR DOLLARS" ~ "Direct Tooling Labor",
        .data$`Functional Data Element` == "(7) DIRECT TOOLING & EQUIPMENT DOLLARS" ~ "Direct Tooling & Equipment",
        .data$`Functional Data Element` == "(8) DIRECT QUALITY CONTROL LABOR HOURS" ~ "Direct Quality Control Labor",
        .data$`Functional Data Element` == "(9) DIRECT QUALITY CONTROL LABOR DOLLARS" ~ "Direct Quality Control Labor",
        .data$`Functional Data Element` == "(10) DIRECT MANUFACTURING LABOR HOURS" ~ "Direct Manufacturing Labor",
        .data$`Functional Data Element` == "(11) DIRECT MANUFACTURING LABOR DOLLARS" ~ "Direct Manufacturing Labor",
        .data$`Functional Data Element` == "(12) MANUFACTURING OPERATIONS OVERHEAD DOLLARS (Including Tooling and Quality Control)" ~ "Manufacturing Operations Overhead",
        .data$`Functional Data Element` == "(13) TOTAL MANUFACTURING OPERATIONS DOLLARS (Sum of rows 6, 7, 9, 11, and 12)" ~ "Summary",
        .data$`Functional Data Element` == "(14) RAW MATERIAL DOLLARS" ~ "Raw Material",
        .data$`Functional Data Element` == "(15) PURCHASED PARTS DOLLARS" ~ "Purchased Parts",
        .data$`Functional Data Element` == "(16) PURCHASED EQUIPMENT DOLLARS" ~ "Purchased Equipment",
        .data$`Functional Data Element` == "(17) MATERIAL HANDLING OVERHEAD DOLLARS" ~ "Material Handling/Overhead",
        .data$`Functional Data Element` == "(18) TOTAL DIRECT-REPORTING SUBCONTRACTOR DOLLARS" ~ "Direct-Reporting Subcontractor",
        .data$`Functional Data Element` == "(19) TOTAL MATERIAL DOLLARS" ~ "Summary",
        .data$`Functional Data Element` == "(20) OTHER COSTS NOT SHOWN ELSEWHERE (Specify in Remarks)" ~ "Other Costs Not Shown Elsewhere",
        .data$`Functional Data Element` == "(21) TOTAL COST (Direct and Overhead)" ~ "Summary",
        TRUE ~ NA_character_
      )
    )
  fchr_plus[["reported_data"]]$`Functional Element` <-
    fchr_plus[["reported_data"]]$`Functional Element` %>%
    forcats::as_factor()

  # Add "Functional Category" column. ---------------------------------------------------
  fchr_plus[["reported_data"]] <-
    fchr_plus[["reported_data"]] %>% dplyr::mutate(
      "Functional Category" = dplyr::case_when(
        .data$`Functional Data Element` == "(1) DIRECT ENGINEERING LABOR HOURS" ~ "Engineering",
        .data$`Functional Data Element` == "(2) DIRECT ENGINEERING LABOR DOLLARS" ~ "Engineering",
        .data$`Functional Data Element` == "(3) ENGINEERING OVERHEAD DOLLARS" ~ "Engineering",
        .data$`Functional Data Element` == "(4) TOTAL ENGINEERING DOLLARS" ~ "Engineering",
        .data$`Functional Data Element` == "(5) DIRECT TOOLING LABOR HOURS" ~ "Tooling",
        .data$`Functional Data Element` == "(6) DIRECT TOOLING LABOR DOLLARS" ~ "Tooling",
        .data$`Functional Data Element` == "(7) DIRECT TOOLING & EQUIPMENT DOLLARS" ~ "Tooling",
        .data$`Functional Data Element` == "(8) DIRECT QUALITY CONTROL LABOR HOURS" ~ "Quality Control",
        .data$`Functional Data Element` == "(9) DIRECT QUALITY CONTROL LABOR DOLLARS" ~ "Quality Control",
        .data$`Functional Data Element` == "(10) DIRECT MANUFACTURING LABOR HOURS" ~ "Manufacturing",
        .data$`Functional Data Element` == "(11) DIRECT MANUFACTURING LABOR DOLLARS" ~ "Manufacturing",
        .data$`Functional Data Element` == "(12) MANUFACTURING OPERATIONS OVERHEAD DOLLARS (Including Tooling and Quality Control)" ~ "Manufacturing Operations",
        .data$`Functional Data Element` == "(13) TOTAL MANUFACTURING OPERATIONS DOLLARS (Sum of rows 6, 7, 9, 11, and 12)" ~ "Manufacturing & Manufacturing Operations",
        .data$`Functional Data Element` == "(14) RAW MATERIAL DOLLARS" ~ "Material",
        .data$`Functional Data Element` == "(15) PURCHASED PARTS DOLLARS" ~ "Material",
        .data$`Functional Data Element` == "(16) PURCHASED EQUIPMENT DOLLARS" ~ "Material",
        .data$`Functional Data Element` == "(17) MATERIAL HANDLING OVERHEAD DOLLARS" ~ "Material",
        .data$`Functional Data Element` == "(18) TOTAL DIRECT-REPORTING SUBCONTRACTOR DOLLARS" ~ "Material",
        .data$`Functional Data Element` == "(19) TOTAL MATERIAL DOLLARS" ~ "Material",
        .data$`Functional Data Element` == "(20) OTHER COSTS NOT SHOWN ELSEWHERE (Specify in Remarks)" ~ "Other",
        .data$`Functional Data Element` == "(21) TOTAL COST (Direct and Overhead)" ~ "Summary",
        TRUE ~ NA_character_
      )
    )
  fchr_plus[["reported_data"]]$`Functional Category` <-
    fchr_plus[["reported_data"]]$`Functional Category` %>%
    forcats::as_factor()

  # Add "Short Name" column. --------------------------------------------------
  # This section is badly in need of refactoring with the add of a function.
  fchr_plus[["reported_data"]] <-
    fchr_plus[["reported_data"]] %>% dplyr::mutate(
      "Short Name" = dplyr::case_when(
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Summary" &
          .data$`Functional Element`       == "Summary"
        ~ "NR $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Summary" &
          .data$`Functional Element`       == "Summary"
        ~ "NR $ TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Engineering" &
          .data$`Functional Element`       == "Direct Engineering Labor"
        ~ "NR Direct Eng $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Engineering" &
          .data$`Functional Element`       == "Direct Engineering Labor"
        ~ "NR Direct Eng $ TD",
        .data$`Unit of Measure`            == "Hours" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Engineering" &
          .data$`Functional Element`       == "Direct Engineering Labor"
        ~ "NR Eng Hrs AC",
        .data$`Unit of Measure`            == "Hours" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Engineering" &
          .data$`Functional Element`       == "Direct Engineering Labor"
        ~ "NR Eng Hrs TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Manufacturing" &
          .data$`Functional Element`       == "Direct Manufacturing Labor"
        ~ "NR MFG Direct $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Manufacturing" &
          .data$`Functional Element`       == "Direct Manufacturing Labor"
        ~ "NR MFG Direct $ TD",
        .data$`Unit of Measure`            == "Hours" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Manufacturing" &
          .data$`Functional Element`       == "Direct Manufacturing Labor"
        ~ "NR MFG Hrs AC",
        .data$`Unit of Measure`            == "Hours" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Manufacturing" &
          .data$`Functional Element`       == "Direct Manufacturing Labor"
        ~ "NR MFG Hrs TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Quality Control" &
          .data$`Functional Element`       == "Direct Quality Control Labor"
        ~ "NR QC Direct $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Quality Control" &
          .data$`Functional Element`       == "Direct Quality Control Labor"
        ~ "NR QC Direct $ TD",
        .data$`Unit of Measure`            == "Hours" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Quality Control" &
          .data$`Functional Element`       == "Direct Quality Control Labor"
        ~ "NR QC Hrs AC",
        .data$`Unit of Measure`            == "Hours" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Quality Control" &
          .data$`Functional Element`       == "Direct Quality Control Labor"
        ~ "NR QC Hrs TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Tooling" &
          .data$`Functional Element`       == "Direct Tooling & Equipment"
        ~ "NR Tool/Equip $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Tooling" &
          .data$`Functional Element`       == "Direct Tooling & Equipment"
        ~ "NR Tool/Equip $ TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Tooling" &
          .data$`Functional Element`       == "Direct Tooling Labor"
        ~ "NR Tool Direct $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Tooling" &
          .data$`Functional Element`       == "Direct Tooling Labor"
        ~ "NR Tool Direct $ TD",
        .data$`Unit of Measure`            == "Hours" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Tooling" &
          .data$`Functional Element`       == "Direct Tooling Labor"
        ~ "NR Tool Hrs AC",
        .data$`Unit of Measure`            == "Hours" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Tooling" &
          .data$`Functional Element`       == "Direct Tooling Labor"
        ~ "NR Tool Hrs TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Direct-Reporting Subcontractor"
        ~ "NR Direct Rep Sub $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Direct-Reporting Subcontractor"
        ~ "NR Direct Rep Sub $ TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Engineering" &
          .data$`Functional Element`       == "Engineering Overhead"
        ~ "NR Eng OH $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Engineering" &
          .data$`Functional Element`       == "Engineering Overhead"
        ~ "NR Eng OH $ TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Manufacturing Operations" &
          .data$`Functional Element`       == "Manufacturing Operations Overhead"
        ~ "NR MFG Ops OH $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Manufacturing Operations" &
          .data$`Functional Element`       == "Manufacturing Operations Overhead"
        ~ "NR MFG Ops OH $ TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Material Handling/Overhead"
        ~ "NR Mat OH $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Material Handling/Overhead"
        ~ "NR Mat OH $ TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Other" &
          .data$`Functional Element`       == "Other Costs Not Shown Elsewhere"
        ~ "NR Other $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Other" &
          .data$`Functional Element`       == "Other Costs Not Shown Elsewhere"
        ~ "NR Other $ TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Purchased Equipment"
        ~ "NR Purch Equip $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Purchased Equipment"
        ~ "NR Purch Equip $ TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Purchased Parts"
        ~ "NR Purch Parts $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Purchased Parts"
        ~ "NR Purch Parts $ TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Raw Material"
        ~ "NR Raw Mat $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Raw Material"
        ~ "NR Raw Mat $ TD",
        .data$`Unit of Measure`            == "Quantity" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Summary" &
          .data$`Functional Element`       == "Summary"
        ~ "Units AC",
        .data$`Unit of Measure`            == "Quantity" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Summary" &
          .data$`Functional Element`       == "Summary"
        ~ "Units TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Summary" &
          .data$`Functional Element`       == "Summary"
        ~ "Rec $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Summary" &
          .data$`Functional Element`       == "Summary"
        ~ "Rec $ TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Engineering" &
          .data$`Functional Element`       == "Direct Engineering Labor"
        ~ "Rec Direct Eng $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Engineering" &
          .data$`Functional Element`       == "Direct Engineering Labor"
        ~ "Rec Direct Eng $ TD",
        .data$`Unit of Measure`            == "Hours" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Engineering" &
          .data$`Functional Element`       == "Direct Engineering Labor"
        ~ "Rec Eng Hrs AC",
        .data$`Unit of Measure`            == "Hours" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Engineering" &
          .data$`Functional Element`       == "Direct Engineering Labor"
        ~ "Rec Eng Hrs TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Manufacturing" &
          .data$`Functional Element`       == "Direct Manufacturing Labor"
        ~ "Rec MFG Direct $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Manufacturing" &
          .data$`Functional Element`       == "Direct Manufacturing Labor"
        ~ "Rec MFG Direct $ TD",
        .data$`Unit of Measure`            == "Hours" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Manufacturing" &
          .data$`Functional Element`       == "Direct Manufacturing Labor"
        ~ "Rec MFG Hrs AC",
        .data$`Unit of Measure`            == "Hours" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Manufacturing" &
          .data$`Functional Element`       == "Direct Manufacturing Labor"
        ~ "Rec MFG Hrs TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Quality Control" &
          .data$`Functional Element`       == "Direct Quality Control Labor"
        ~ "Rec QC Direct $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Quality Control" &
          .data$`Functional Element`       == "Direct Quality Control Labor"
        ~ "Rec QC Direct $ TD",
        .data$`Unit of Measure`            == "Hours" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Quality Control" &
          .data$`Functional Element`       == "Direct Quality Control Labor"
        ~ "Rec QC Hrs AC",
        .data$`Unit of Measure`            == "Hours" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Quality Control" &
          .data$`Functional Element`       == "Direct Quality Control Labor"
        ~ "Rec QC Hrs TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Tooling" &
          .data$`Functional Element`       == "Direct Tooling & Equipment"
        ~ "Rec Tool/Equip $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Tooling" &
          .data$`Functional Element`       == "Direct Tooling & Equipment"
        ~ "Rec Tool/Equip $ TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Tooling" &
          .data$`Functional Element`       == "Direct Tooling Labor"
        ~ "Rec Tool Direct $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Tooling" &
          .data$`Functional Element`       == "Direct Tooling Labor"
        ~ "Rec Tool Direct $ TD",
        .data$`Unit of Measure`            == "Hours" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Tooling" &
          .data$`Functional Element`       == "Direct Tooling Labor"
        ~ "Rec Tool Hrs AC",
        .data$`Unit of Measure`            == "Hours" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Tooling" &
          .data$`Functional Element`       == "Direct Tooling Labor"
        ~ "Rec Tool Hrs TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Direct-Reporting Subcontractor"
        ~ "Rec Direct Rep Sub $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To DaSte" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Direct-Reporting Subcontractor"
        ~ "Rec Direct Rep Sub $ TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Engineering" &
          .data$`Functional Element`       == "Engineering Overhead"
        ~ "Rec Eng OH $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Engineering" &
          .data$`Functional Element`       == "Engineering Overhead"
        ~ "Rec Eng OH $ TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Manufacturing Operations" &
          .data$`Functional Element`       == "Manufacturing Operations Overhead"
        ~ "Rec MFG Ops OH $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Manufacturing Operations" &
          .data$`Functional Element`       == "Manufacturing Operations Overhead"
        ~ "Rec MFG Ops OH $ TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Material Handling/Overhead"
        ~ "Rec Mat OH $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Material Handling/Overhead"
        ~ "Rec Mat OH $ TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Other" &
          .data$`Functional Element`       == "Other Costs Not Shown Elsewhere"
        ~ "Rec Other $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Other" &
          .data$`Functional Element`       == "Other Costs Not Shown Elsewhere"
        ~ "Rec Other $ TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Purchased Equipment"
        ~ "Rec Purch Equip $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Purchased Equipment"
        ~ "Rec Purch Equip $ TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Purchased Parts"
        ~ "Rec Purch Parts $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Purchased Parts"
        ~ "Rec Purch Parts $ TD",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Raw Material"
        ~ "Rec Raw Mat $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Recurring" &
          .data$`Functional Category`      == "Material" &
          .data$`Functional Element`       == "Raw Material"
        ~ "Rec Raw Mat $ TD",
        TRUE ~ NA_character_
      )
    )

  # Reorder columns before return. --------------------------------------------
  # Reorder the columns.
  fchr_plus[["reported_data"]] <-
    fchr_plus[["reported_data"]] %>%
    dplyr::select(
      "WBS Element Code",
      "WBS Reporting Element",
      "Functional Category",
      "Functional Element",
      "Functional Data Element Number",
      "Functional Data Element",
      "Recurring / Nonrecurring",
      "To Date / At Completion",
      "Reported Data Field",
      "Unit of Measure",
      "Reported Data Value",
      "Number of Units to Date",
      "Number of Units At Completion",
      "Remarks",
      "Short Name"
    ) %>%
    dplyr::arrange(.data$`WBS Element Code`,
                   .data$`To Date / At Completion`,
                   .data$`Recurring / Nonrecurring`,
                   .data$`Functional Data Element Number`)

  return(fchr_plus)
}
