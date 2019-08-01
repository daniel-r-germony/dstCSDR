#' Import a CSDR 1921 and 1921-1
#'
#' Takes two paths to Excel files (one for a CSDR 1921 and one for a 1921-1)
#' and runs the three primary \code{dstCSDR} package import functions:
#' \enumerate{
#'   \item \code{import_CSDR_1921()}
#'   \item \code{import_CSDR_1921-1()}
#'   \item \code{add_cols_CSDR_1921_1()}
#' }
#'
#' @title import_CSDR
#' @author Daniel Germony \email{daniel.r.germony.civ@@mail.mil}
#' @param path_CSDR_1921 Required. Path to the xls/xlsx file which contains
#'    the CSDR 1921. Note the Excel file must have the CSDR 1921 data on the
#'    first worksheet (remove any coversheets prior to using this script) and
#'    cannot have columns or rows moved/changed from the standard from (i.e.,
#'    the script assumes a cPet produced/compliant file is provided).
#' @param path_CSDR_1921_1 Required. Path to the xls/xlsx file which contains
#'    the CSDR 1921-1 data. Note the Excel file must include only CSDR 1921-1
#'    worksheets, one worksheet per WBS (remove any coversheets prior to using
#'    this script) and cannot have columns or rows moved/changed from the
#'    standard from (i.e., the script assumes a cPet produced/compliant file
#'    is provided).
#' @return Returns a list with three objects, one of the CSDR 1921 tibble, one
#'    with the CSDR 1921-1 tibble, and one with the CSDR 1921-1+ tibble (i.e.,
#'    the CSDR 1921-1 tibble with additional columns.
#' @export

import_CSDR <- function(path_CSDR_1921, path_CSDR_1921_1) {
  # Import CSDR 1921 ------------------------------------------------------------
  CSDR_1921 <- import_CSDR_1921(path_CSDR_1921)

  # Import CSDR 1921-1 ----------------------------------------------------------
  CSDR_1921_1 <- import_CSDR_1921_1(path_CSDR_1921_1)

  # Add columns to CSDR 1921-1 --------------------------------------------------
  CSDR_1921_1_plus <- add_cols_CSDR_1921_1(CSDR_1921_1)

  return(list(CSDR_1921, CSDR_1921_1, CSDR_1921_1_plus))

}
