#' Get the path to, or a list of, dstCSDR example file(s)
#'
#' dstCSDR comes with various CSDR related example files in the package's
#' `extdata` directory. This function is used to return a list of the example
#' files or to specify one for you may want to use as an example.
#'
#' @param path Name of a specific dstCSDR example file or `NULL`.
#' @return If \code{path} is `NULL` (the default), a list of all example files
#'   will be returned. If \code{path} is a specific file name (with extension)
#'   is provided, the full file path is returned.
#' @export
#' @examples
#' example_dstCSDR_files()
#' example_dstCSDR_files("Example_CDSR_1921.xls")

example_dstCSDR_files <- function(path = NULL) {

  if (is.null(path)) {
    dir(system.file("extdata", package = "dstCSDR"))

  } else {
    system.file("extdata", path, package = "dstCSDR", mustWork = TRUE)
  }

}
