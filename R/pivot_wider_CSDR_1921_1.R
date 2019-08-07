#' Transpose CSDR 1921-1 Data
#'
#' Takes CSDR 1921-1 data and performs a transpose/pivot_wider so each row is
#' a WBS and each column is one of the CSDR 1921-1's data cells. The primary
#' use case for this is to lay CSDR 1921-1 data alongside a CSDR 1921 submission.
#'
#' @title pivot_wider_CSDR_1921_1
#' @author Daniel Germony \email{daniel.r.germony.civ@@mail.mil}
#' @param CSDR_1921_1_plus Required. A tibble of CSDR 1921-1 data that has
#'   been imported using the \code{import_CSDR_1921()} function and run thru the
#'   \code{add_cols_CSDR_1921_1()} function.
#' @param ... Optional. Allows for use of \code{filter()} from \code{{dplyr}} and other piped functions.
#' @return Returns a tibble with each WBS element as a row and CSDR 1921-1
#'   data as columns.
#' @export


pivot_wider_CSDR_1921_1 <- function(CSDR_1921_1_plus, ...) {

  # Import the pipe! ----------------------------------------------------------
  `%>%` <- magrittr::`%>%`

  # Arrange, select, pivot_wider, and add remarks back.
  CSDR_1921_1_wide <-
    CSDR_1921_1_plus %>%
    dplyr::arrange(`18. WBS ELEMENT CODE`,
                   `Recurring / Nonrecurring`,
                   `To Date / At Completion`,
                   `Functional Element`
    ) %>%
    dplyr::select(`18. WBS ELEMENT CODE`,
                  `Recurring / Nonrecurring`,
                  `Functional Element`,
                  `VALUE`
    ) %>%
    tidyr::pivot_wider(
      names_from = c(`Recurring / Nonrecurring`, `Functional Element`),
      names_sep = " / ",
      values_from = `VALUE`
    ) %>%
    dplyr::left_join({
      CSDR_1921_1_plus %>%
        dplyr::select("18. WBS ELEMENT CODE",
                      "22. REMARKS") %>%
        dplyr::distinct()
    }, by = "18. WBS ELEMENT CODE")

  return(CSDR_1921_1_wide)

}
