#' Add Supplemental Columns to a CDSR FCHR (DD Form 1921-1) Tibble
#'
#' Takes a CSDR Functional Cost-Hour Report (FCHR) (DD Form 1921-1) object and
#' adds supplemental columns to the to the \code{`reported_data`} tibble to
#' allow for additional/easier sorting/filtering. The user can specify which
#' column(s) are not added by passing \code{FALSE} to the various \code{add_*}
#' parameters. The user can also gather the FCHR (DD Form 1921-1) object on the
#' fly using  \code{already_gathered = FALSE} parameter if the FCHR object has
#' not already been gathered using the \code{gather_fchr()} function.
#'
#' @title add_supplemental_fchr_columns
#' @author Daniel Germony \email{daniel.r.germony.civ@@mail.mil}
#' @param fchr_object Required. A list object of CSDR FCHR 1921-1 data that has
#'   been imported using the \code{import_cdsr_excel()} function.
#' @param add_wbs_el_lvl Optional. Should a "WBS Element Level" column be added?
#'   Set to \code{TRUE} by default.
#' @param add_func_cat Optional. Should a "Functional Category" be added? Set to
#'   \code{TRUE} by default.
#' @param add_func_el Optional. Should a "Functional Element" column be added?
#'   Set to \code{TRUE} by default.
#' @param add_func_data_el_numb Optional. Should a "Functional Data Element
#'   Number" column be added? Set to \code{TRUE} by default.
#' @param add_rec_nr Optional. Should a "Recurring / Nonrecurring" column be
#'   added? Set to \code{TRUE} by default.
#' @param add_to_ac Optional. Should a "To Date / At Completion" column be
#'   added? Set to \code{TRUE} by default.
#' @param add_short_name Optional. Should a "Short Name" column be added? Set to
#'   \code{TRUE} by default.
#' @param add_prog_name Optional. Should a "Major Program Name" column be added?
#'   Set to \code{TRUE} by default.
#' @param add_prog_phase Optional. Should a "Major Program Phase/Milestone"
#'   column be added? Set to \code{TRUE} by default.
#' @param add_pmp Optional. Should a "Prime Mission Product" column be added?
#'   Set to \code{TRUE} by default.
#' @param add_rep_org_type Optional. Should a "Reporting Organization Type"
#'   column be added? Set to \code{TRUE} by default.
#' @param add_rep_org_address Optional. Should a "Organization Name & Address"
#'   column be added? Set to \code{TRUE} by default.
#' @param add_rep_divison_name Optional. Should a "Approved Plan Number" column
#'   be added? Set to \code{TRUE} by default.
#' @param add_plan_numb Optional. Should a "Approved Plan Number" column be
#'   added? Set to \code{TRUE} by default.
#' @param add_customer Optional. Should a "Customer" column be added? Set to
#'   \code{TRUE} by default.
#' @param add_k_numb Optional. Should a "Contract No" column be added? Set to
#'   \code{TRUE} by default.
#' @param add_last_mod_numb Optional. Should a "Latest Modification" column be
#'   added? Set to \code{TRUE} by default.
#' @param add_solicition_numb Optional. Should a "Solicitation No" column be
#'   added? Set to \code{TRUE} by default.
#' @param add_k_name Optional. Should a "Contract Name" column be added? Set to
#'   \code{TRUE} by default.
#' @param add_order_numb Optional. Should a "Task Order/Deliver Order/Lot
#'   Number" column be added? Set to \code{TRUE} by default.
#' @param add_pop_start_date Optional. Should a "Period of Performance Start
#'   Date" column be added? Set to \code{TRUE} by default.
#' @param add_pop_end_date Optional. Should a "Period of Performance End Date"
#'   column be added? Set to \code{TRUE} by default.
#' @param add_report_cycle Optional. Should a "Report Cycle" column be added?
#'   Set to \code{TRUE} by default.
#' @param add_sub_numb Optional. Should a "Submission Number" column be added?
#'   Set to \code{TRUE} by default.
#' @param add_resub_numb Optional. Should a "Resubmission Number" column be
#'   added? Set to \code{TRUE} by default.
#' @param add_as_of_date Optional. Should a "Report As Of" column be added? Set
#'   to \code{TRUE} by default.
#' @param add_poc_name Optional. Should a "Point of Contact Name" column be
#'   added? Set to \code{TRUE} by default.
#' @param add_poc_department Optional. Should a "Department" column be added?
#'   Set to \code{TRUE} by default.
#' @param add_poc_phone Optional. Should a "Telephone Number" column be added?
#'   Set to \code{TRUE} by default.
#' @param add_poc_email Optional. Should a "Email Address" column be added? Set
#'   to \code{TRUE} by default.
#' @param add_prep_date Optional. Should a "Date Prepared" column be added? Set
#'   to \code{TRUE} by default.
#' @param add_appn Optional. Should a "Appropriation" column be added? Set to
#'   \code{TRUE} by default.
#' @param already_gathered Optional. Indicates if the \code{fchr_object} has
#'   already been gathered using the \code{gather_fchr()} function or not. Use
#'   \code{TRUE} if it has already been gathered (the default) and \code{FALSE}
#'   otherwise.
#' @return Returns a list of two tibbles; the first tibble includes submission
#'   metdata from a CDSR FCHR (DD Form 1921-1) and the second includes the
#'   reported data from the submission in which each row is a Functional Data
#'   Element for each WBS in the submission.
#' @export

add_supplemental_fchr_columns <- function(fchr_object,
                                          add_wbs_el_lvl        = TRUE,
                                          add_func_cat          = TRUE,
                                          add_func_el           = TRUE,
                                          add_func_data_el_numb = TRUE,
                                          add_rec_nr            = TRUE,
                                          add_to_ac             = TRUE,
                                          add_short_name        = TRUE,
                                          add_prog_name         = FALSE,
                                          add_prog_phase        = FALSE,
                                          add_pmp               = FALSE,
                                          add_rep_org_type      = FALSE,
                                          add_rep_org_address   = FALSE,
                                          add_rep_divison_name  = FALSE,
                                          add_plan_numb         = TRUE,
                                          add_customer          = FALSE,
                                          add_k_numb            = FALSE,
                                          add_last_mod_numb     = FALSE,
                                          add_solicition_numb   = FALSE,
                                          add_k_name            = FALSE,
                                          add_order_numb        = FALSE,
                                          add_pop_start_date    = TRUE,
                                          add_pop_end_date      = TRUE,
                                          add_report_cycle      = FALSE,
                                          add_sub_numb          = TRUE,
                                          add_resub_numb        = TRUE,
                                          add_as_of_date        = FALSE,
                                          add_poc_name          = FALSE,
                                          add_poc_department    = FALSE,
                                          add_poc_phone         = FALSE,
                                          add_poc_email         = FALSE,
                                          add_prep_date         = FALSE,
                                          add_appn              = FALSE,
                                          already_gathered      = TRUE) {

  # Custom Functions ----------------------------------------------------------

  # This function is used later to pull metadata from the metadata table and
  # insert/mutate it onto new columns of the main FCHR table.
  .mutate_metadata <- function(fchr_object, field) {

    field <- dplyr::enquo(field)

    fchr_object[[2]] <-
      fchr_object[[2]] %>%
      dplyr::mutate(
        !! field :=
          tidyr::pivot_wider(fchr_object[["metadata"]],
                      names_from  = .data$metadata_field,
                      values_from = .data$repoted_value) %>%
          dplyr::select(!! field) %>%
          as.character()
      )

    return(fchr_object)
  }

  # Gather FCHR object if the users says it has not already been gathered. ----
  if (already_gathered == FALSE) {
    fchr_object <- fchr_object %>% dstCSDR::gather_fchr()
  }

  # Add WBS Element Level column. ---------------------------------------------
  fchr_object[["reported_data"]] <-
    fchr_object[["reported_data"]] %>%
    dplyr::mutate(
      "WBS Element Level" = purrr::map_int(
        fchr_object[["reported_data"]]$`WBS Element Code`,
        dstCSDR::wbs_code_to_lvl))

  # Add Recurring / Nonrecurring" column. -------------------------------------
  fchr_object[["reported_data"]] <-
    fchr_object[["reported_data"]] %>%
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

  fchr_object[["reported_data"]]$`Recurring / Nonrecurring` <-
    fchr_object[["reported_data"]]$`Recurring / Nonrecurring` %>%
    forcats::as_factor()

  # Add "To Date / At Completion" column. --------------------------------------
  fchr_object[["reported_data"]] <-
    fchr_object[["reported_data"]] %>%
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

  fchr_object[["reported_data"]]$`To Date / At Completion` <-
    fchr_object[["reported_data"]]$`To Date / At Completion` %>%
    forcats::as_factor()

  # Add "Functional Data Element Number" column. ------------------------------
  fchr_object[["reported_data"]] <-
    fchr_object[["reported_data"]] %>% dplyr::mutate(
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

  fchr_object[["reported_data"]]$`Functional Data Element Number` <-
    fchr_object[["reported_data"]]$`Functional Data Element Number` %>%
    forcats::as_factor()

  # Add "Functional Element" column. ---------------------------------------------------
  fchr_object[["reported_data"]] <-
    fchr_object[["reported_data"]] %>% dplyr::mutate(
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
  fchr_object[["reported_data"]]$`Functional Element` <-
    fchr_object[["reported_data"]]$`Functional Element` %>%
    forcats::as_factor()

  # Add "Functional Category" column. ---------------------------------------------------
  fchr_object[["reported_data"]] <-
    fchr_object[["reported_data"]] %>% dplyr::mutate(
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
  fchr_object[["reported_data"]]$`Functional Category` <-
    fchr_object[["reported_data"]]$`Functional Category` %>%
    forcats::as_factor()

  # Add "Short Name" column. --------------------------------------------------
  # This section is badly in need of refactoring with the add of a function.
  fchr_object[["reported_data"]] <-
    fchr_object[["reported_data"]] %>% dplyr::mutate(
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
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Engineering" &
          .data$`Functional Element`       == "Direct Manufacturing Labor"
        ~ "NR MFG Direct $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Manufacturing" &
          .data$`Functional Category`      == "Engineering" &
          .data$`Functional Element`       == "Direct Manufacturing Labor"
        ~ "NR MFG Direct $ TD",
        .data$`Unit of Measure`            == "Hours" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Manufacturing" &
          .data$`Functional Category`      == "Engineering" &
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
          .data$`Functional Category`      == "Engineering" &
          .data$`Functional Element`       == "Direct Manufacturing Labor"
        ~ "NR MFG Hrs AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Engineering" &
          .data$`Functional Element`       == "Direct Quality Control Labor"
        ~ "NR QC Direct $ AC",
        .data$`Unit of Measure`            == "TY $K" &
          .data$`To Date / At Completion`  == "To Date" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Quality Control" &
          .data$`Functional Category`      == "Engineering" &
          .data$`Functional Element`       == "Direct Quality Control Labor"
        ~ "NR QC Direct $ TD",
        .data$`Unit of Measure`            == "Hours" &
          .data$`To Date / At Completion`  == "At Completion" &
          .data$`Recurring / Nonrecurring` == "Nonrecurring" &
          .data$`Functional Category`      == "Quality Control" &
          .data$`Functional Category`      == "Engineering" &
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
  fchr_object[["reported_data"]]$`Short Name` <-
    fchr_object[["reported_data"]]$`Short Name` %>%
    forcats::as_factor()

  # Reorder non-metadata columns. ---------------------------------------------
  fchr_object[["reported_data"]] <-
    fchr_object[["reported_data"]] %>%
    dplyr::select(
      "WBS Element Code",
      "WBS Element Level",
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

  # Remove non-metadata columns the user marked as FALSE ----------------------
  # WBS Element Level
  if (add_wbs_el_lvl == FALSE) {
    fchr_object[["reported_data"]] <-
      fchr_object[["reported_data"]] %>%
      dplyr::select(-.data$`WBS Element Level`)
  }

  # Functional Category
  if (add_func_cat == FALSE) {
    fchr_object[["reported_data"]] <-
      fchr_object[["reported_data"]] %>%
      dplyr::select(-.data$`Functional Category`)
  }

  # Functional Element
  if (add_func_el == FALSE) {
    fchr_object[["reported_data"]] <-
      fchr_object[["reported_data"]] %>%
      dplyr::select(-.data$`Functional Element`)
  }

  # Functional Data Element Number
  if (add_func_data_el_numb == FALSE) {
    fchr_object[["reported_data"]] <-
      fchr_object[["reported_data"]] %>%
      dplyr::select(-.data$`Functional Data Element Number`)
  }

  # Recurring / Nonrecurring
  if (add_rec_nr == FALSE) {
    fchr_object[["reported_data"]] <-
      fchr_object[["reported_data"]] %>%
      dplyr::select(-.data$`Recurring / Nonrecurring`)
  }

  # To Date / At Completion
  if (add_to_ac == FALSE) {
    fchr_object[["reported_data"]] <-
      fchr_object[["reported_data"]] %>%
      dplyr::select(-.data$`To Date / At Completion`)
  }

  # Short Name
  if (add_short_name == FALSE) {
    fchr_object[["reported_data"]] <-
      fchr_object[["reported_data"]] %>%
      dplyr::select(-.data$`Short Name`)
  }

  # Add metadata columns ------------------------------------------------------

  if (add_prog_name        == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Major Program Name")}
  if (add_prog_phase       == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Major Program Phase/Milestone")}
  if (add_pmp              == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Prime Mission Product")}
  if (add_rep_org_type     == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Reporting Organization Type")}
  if (add_rep_org_address  == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Organization Name & Address")}
  if (add_rep_divison_name == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Division Name  & Address")}
  if (add_plan_numb        == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Approved Plan Number")}
  if (add_customer         == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Customer")}
  if (add_k_numb           == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Contract No")}
  if (add_last_mod_numb    == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Latest Modification")}
  if (add_solicition_numb  == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Solicitation No")}
  if (add_k_name           == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Contract Name")}
  if (add_order_numb       == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Task Order/Deliver Order/Lot Number")}
  if (add_pop_start_date   == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Period of Performance Start Date")}
  if (add_pop_end_date     == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Period of Performance End Date")}
  if (add_report_cycle     == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Report Cycle")}
  if (add_sub_numb         == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Submission Number")}
  if (add_resub_numb       == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Resubmission Number")}
  if (add_as_of_date       == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Report As Of")}
  if (add_poc_name         == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Point of Contact Name")}
  if (add_poc_department   == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Department")}
  if (add_poc_phone        == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Telephone Number")}
  if (add_poc_email        == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Email Address")}
  if (add_prep_date        == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Date Prepared")}
  if (add_appn             == TRUE) {fchr_object <- .mutate_metadata(fchr_object, "Appropriation")}

  return(fchr_object)
}
