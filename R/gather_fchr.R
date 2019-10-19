#' Gather CDSR FCHR (DD Form 1921-1)
#'
#' Gathers the "Cost and Hours..." columns from a CSDR Functional Cost-Hour
#' Report (FCHR) (DD Form 1921-1) object in a new column called "Reported Data
#' Field". This longer and narrower version of the FCHR is eaiser to
#' pivot/sort/filter.
#'
#' @title gather_fchr
#' @author Daniel Germony \email{daniel.r.germony.civ@@mail.mil}
#' @param fchr_object Required. A list object of CSDR FCHR 1921-1 data that has
#'   been imported using the \code{import_cdsr_excel()} function.
#' @return Returns a CSDR FCHR (DD Form 1921-1) object with all "Cost and
#'   Hours..." columns converted from columns to rows in a new "Reported Data
#'   Field" column.
#' @export
#' @examples
#'
#' \dontrun{
#'
#' # Gather a FCHR object imported from a user desktop.
#' fchr <- import_fchr_excel("C:/Users/john.doe/Desktop/DD Form 1921-1 submission.xls")
#' fchr %>% gather_fchr()
#'
#' # Import and gather the example FCHR file in one step.
#' dstCSDR_files("Example_FCHR_1921_1.xls") %>%
#'   import_fchr_excel() %>%
#'   gather_fchr()
#'
#' }
gather_fchr <- function(fchr_object) {

  fchr_gathered <- fchr_object

  # Gather the 1921-1 data. ---------------------------------------------------
  fchr_gathered[["reported_data"]] <-
    fchr_gathered[["reported_data"]] %>%
    tidyr::gather(
      key = "Reported Data Field",
      value = "Reported Data Value",
      .data$`Costs and Hours Incurred To Date - Nonrecurring`:.data$`Costs and Hours Incurred At Completion - Total`
    )

  fchr_gathered[["reported_data"]]$`Reported Data Field` <-
    fchr_gathered[["reported_data"]]$`Reported Data Field` %>%
    forcats::as_factor()

  # Pivot longer the "Units to..." data. --------------------------------------
  fchr_gathered[["reported_data"]] <-
    fchr_gathered[["reported_data"]] %>%
    tidyr::pivot_longer(
      cols = c("Number of Units to Date", "Number of Units At Completion"),
      names_prefix = "Number of Units ",
      values_to = "Number of Units"
    ) %>%
    dplyr::mutate(
      "double_count" = dplyr::case_when(
        .data$`Reported Data Field` == "Costs and Hours Incurred To Date - Nonrecurring" & .data$name == "At Completion" ~ TRUE,
        .data$`Reported Data Field` == "Costs and Hours Incurred To Date - Recurring" & .data$name == "At Completion" ~ TRUE,
        .data$`Reported Data Field` == "Costs and Hours Incurred To Date - Total" & .data$name == "At Completion" ~ TRUE,
        .data$`Reported Data Field` == "Costs and Hours Incurred At Completion - Nonrecurring" & .data$name == "to Date" ~ TRUE,
        .data$`Reported Data Field` == "Costs and Hours Incurred At Completion - Recurring" & .data$name == "to Date" ~ TRUE,
        .data$`Reported Data Field` == "Costs and Hours Incurred At Completion - Total" & .data$name == "to Date" ~ TRUE,
        TRUE ~ NA
      )
    ) %>%
    dplyr::filter(is.na(.data$double_count)) %>%
    dplyr::select(-.data$double_count, -.data$name)

  # Reorder columns before return. --------------------------------------------
  # Reorder the columns.
  fchr_gathered[["reported_data"]] <-
    fchr_gathered[["reported_data"]] %>%
    dplyr::select(
      "WBS Element Code",
      "WBS Reporting Element",
      "Functional Category",
      "Functional Data Element",
      "Reported Data Field",
      "Unit of Measure",
      "Reported Data Value",
      "Number of Units",
      "Remarks"
    ) %>%
    dplyr::arrange(.data$`WBS Element Code`)

  return(fchr_gathered)

}
