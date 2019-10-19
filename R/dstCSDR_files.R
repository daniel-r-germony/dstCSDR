#' Get the path to, or a list of, `{dstCSDR}` file(s)
#'
#' `{dstCSDR}` comes with various CSDR related files in the package's `extdata`
#' directory. This function is used to return a list of the files or to specify
#' one for you may want to use.
#'
#' @param path Name of a specific `{dstCSDR}` file or `NULL`.
#' @return If \code{path} is `NULL` (the default), a list of all files will be
#'   returned. If \code{path} is a specific file name (with extension) is
#'   provided, the full file path is returned.
#' @export
#' @examples
#'
#' # Lists all the {dstCSDR} files.
#' dstCSDR_files()
#'
#' # Return a full path to the "Example_CSDR_1921.xls" file.
#' dstCSDR_files("Example_CDSR_1921.xls")
#'
#' \dontrun{
#'
#' # Copy "cost_reporting_params.xlsx" and put it at the `to = "..."` location.
#' dstCSDR_files("cost_reporting_params.xlsx") %>%
#'   file.copy(to = "C:/Users/john.doe/Desktop")
#' }

dstCSDR_files <- function(path = NULL) {

  if (is.null(path)) {
    dir(system.file("extdata", package = "dstCSDR"))

  } else {
    system.file("extdata", path, package = "dstCSDR", mustWork = TRUE)
  }

}
