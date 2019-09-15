#' Transpose CSDR FCHR (DD Form 1921-1) Data
#'
#' Takes CSDR FCHR (DD Form 1921-1) data and performs a transpose/pivot_wider so
#' each row is a WBS and each column is one of the CSDR FCHR's (DD Form
#' 1921-1's) data cells. The primary use case for this is to lay CSDR FCHR (DD
#' Form 1921-1) data alongside a CSDR CDSR (DD Form 1921) submission.
#'
#' @title widen_fchr
#' @author Daniel Germony \email{daniel.r.germony.civ@@mail.mil}
#' @param fchr_plus_object Required. A list object of CSDR FCHR 1921-1 data that
#'   has been imported using \code{import_cdsr_excel()} and processed thru
#'   \code{add_supplemental_fchr_columns()}.
#' @param ... Optional. Allows for use of \code{filter()} from \code{{dplyr}}
#'   and other piped functions.
#' @return Returns a tibble with each WBS element as a row and CSDR 1921-1 data
#'   as columns.
#'
#' @export

widen_fchr <- function(fchr_plus_object, ...) {

  wider_fchr_plus <- fchr_plus_object

  # Arrange, select, pivot_wider, and add remarks back.
  wider_fchr_plus[["reported_data"]] <-
    wider_fchr_plus[["reported_data"]] %>%
    dplyr::arrange(
      .data$`WBS Element Code`,
      .data$`Reported Data Field`,
      .data$`To Date / At Completion`,
      .data$`Functional Data Element Number`
    ) %>%
     dplyr::select(
       -.data$`Functional Data Element Number`,
       -.data$`Unit of Measure`,
       -.data$`Short Name`,
       -.data$`Reported Data Field`
    ) %>%
    tidyr::pivot_wider(
      names_from = c(
        .data$`Recurring / Nonrecurring`,
        .data$`To Date / At Completion`,
        .data$`Functional Category`,
        .data$`Functional Element`,
        .data$`Functional Data Element`
      ),
      names_sep = " / ",
      values_from = .data$`Reported Data Value`
    )

  return(wider_fchr_plus)

}
