#' Create Cost Reporting SOW, CDRLs, Attachments with Params
#'
#' Imports a pre-formatted and filled in Excel file with info on the planned
#' contract and system then generates a scope of work, CDRLs, and associated
#' contract attachments from template documents.
#'
#' @param params_path Required. A full path to a filled-in
#'   "cost_reporting_params.xlsx" file. If a path is not provided to the
#'   function call, a file selection box will be opened.
#' @param output_dir Required. A directory path to where the user wants the
#'   generated files to be placed. If using Windows, a pop-up box will ppear to
#'   specify an output directory. Non-Windows users must pass a directory path
#'   in the function call.
#' @param files_to_render Optional. Specify which files are rendered and how
#'   they are returned. Options:
#'   \describe{
#'     \item{\code{"default"}}{The default value and will render and return the
#'     following:
#'       \enumerate{
#'         \item SOW: All SOW language in one ".docx" file.
#'         \item CDRLs: All cost reporting CDRLs in one ".docx" file.
#'         }}
#'     \item{\code{"cade_ready"}}{Returns the same files as \code{"default"}
#'     but splits the CDRL document into multiple files (each CSDR CDRL gets
#'     its own file and each non-CSDR CDRL gets lumped into one file) for
#'     easier uploading into the CADE's Program Planning Module.
#'     }}
#'
#' @return Returns nothing to R but does puts the following files at the
#'   \code{output_dir}, dependent on what is passed to the
#'   \code{files_to_render} paramater:
#'     \enumerate{
#'       \item A Word document with cost reporting scope of work language,
#'       filled in with the param values from the "cost_reporting_params.xlsx"
#'       file.
#'       \item A Word document or documents with cost reporting CDRLs, filled
#'       in with the param values from the "cost_reporting_params.xlsx" file.
#'       \item A partially complete CSDR Plan attachment, filled in with the
#'       param  values from the "cost_reporting_params.xlsx" file.
#'       \item A partially complete RDT attachment, filled in with the param
#'       values from the "cost_reporting_params.xlsx" file.
#'       \item A partially complete LSPD Report attachment (if required),
#'       filled in with the param values from the "cost_reporting_params.xlsx"
#'       file.
#'       \item A complete AUMC Report attachment (if required), filled in with
#'       the param values from the "cost_reporting_params.xlsx" file.
#'       }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Render the cost reporting documents and select both the
#' # "cost_reporting_params.xlsx" file and output directory using pop-up boxes.
#' render_cr_docs()
#'
#' # Render the cost reporting documents with the default
#' # "cost_reporting_params.xlsx" values and put the output on a fake user's
#' # desktop.
#' render_cr_docs(params_path = here::here("extdata", "cost_reporting_params.xlsx"),
#'                output_dir = "C:/Users/john.doe/Desktop")
#'          }
render_cr_docs <- function(params_path = file.choose(),
                           output_dir = choose.dir(),
                           files_to_render = "default") {

  # Error prevention: Check that `files_to_render` was an allowable value.
  if (!(files_to_render == "default" || files_to_render == "cade_ready")) {
    stop(glue::glue(
        "\n\n The `files_to_render` paramater must be either \"default\" or \"cade_ready\". \n
        The function was passsed the value, \"{files_to_render}\"")
    )
  }

  # Import the params Excel document and convert it to a params list for Rmd.
  params_from_excel <-
    readxl::read_excel(path = params_path,
                       col_names = TRUE) %>%
    dplyr::select(.data$params, .data$`User Input`) %>%
    tidyr::pivot_wider(names_from = .data$params,
                       values_from = .data$`User Input`)

  # Reader the SOW document docx. =============================================
  rmarkdown::render(
    input = system.file("extdata",
                        "cost_reporting_sow_template.Rmd",
                        package="dstCSDR"),
    output_file = paste0(Sys.Date(),
                         "_",
                         params_from_excel$system_name_abbr,
                         "_cost-reporting-sow.docx"),
    output_dir = if (exists("output_dir"))
                   output_dir
                 else
                   here::here(),
    params = params_from_excel
  )

  # Default
  if (files_to_render == "default") {
    rmarkdown::render(
      input = system.file("extdata",
                          "cost_reporting_sow_template.Rmd",
                          package = "dstCSDR"),
      output_file = paste0(Sys.Date(),
                           "_",
                           params_from_excel$system_name_abbr,
                           "_cost-reporting-sow.docx"),
      output_dir = if (exists("output_dir"))
        output_dir
      else
        here::here(),
      params = params_from_excel
    )
  }

}
