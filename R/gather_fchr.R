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

gather_fchr <- function(fchr_object) {

  fchr_gathered <- fchr_object

  # Gather the 1921-1 data. ---------------------------------------------------
  fchr_gathered[["reported_data"]] <- fchr_gathered[["reported_data"]] %>%
    tidyr::gather(
      key = "Reported Data Field",
      value = "Reported Data Value",
      .data$`Costs and Hours Incurred To Date - Nonrecurring`:.data$`Costs and Hours Incurred At Completion - Total`
    )

  fchr_gathered[["reported_data"]]$`Reported Data Field` <-
    fchr_gathered[["reported_data"]]$`Reported Data Field` %>%
    forcats::as_factor()

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
      "Number of Units to Date",
      "Number of Units At Completion",
      "Remarks"
    ) %>%
    dplyr::arrange(.data$`WBS Element Code`)

  return(fchr_gathered)

}
