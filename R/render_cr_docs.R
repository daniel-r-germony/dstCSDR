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
#'   generated files to be placed. If a path is not passed by Windows users, a
#'   pop-up box will appear. Non-Windows users must pass a directory path.
#'
#' @return Returns nothing to R but does put the following files at the
#'   \code{output_dir}: \enumerate{ \item A Word document with cost reporting
#'   scope of work language, filled in with the param values from the
#'   "cost_reporting_params.xlsx" file. \item A Word document with cost
#'   reporting CDRLs, filled in with the param values from the
#'   "cost_reporting_params.xlsx" file. \item A partially complete CSDR Plan
#'   attachment, filled in with the param values from the
#'   "cost_reporting_params.xlsx" file. \item A partially complete RDT
#'   attachment, filled in with the param values from the
#'   "cost_reporting_params.xlsx" file. \item A partially complete LSPD Report
#'   attachment (if required), filled in with the param values from the
#'   "cost_reporting_params.xlsx" file. \item A complete AUMC Report attachment
#'   (if required), filled in with the param values from the
#'   "cost_reporting_params.xlsx" file. }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' render_cr_docs(
#'   params_path = here::here("inst", "cost_reporting_params.xlsx"),
#'   output_dir = here::here()
#' )}
render_cr_docs <- function(params_path = file.choose(), output_dir = choose.dir()) {

  params_from_excel <-
    readxl::read_excel(path = params_path,
                       col_names = TRUE) %>%
    dplyr::select(params, `User Input`) %>%
    tidyr::pivot_wider(names_from = params, values_from = `User Input`)

  rmarkdown::render(
    input = here::here("inst", "extdata", "cost_reporting_sow_template.Rmd"),
    #output = "word_document",
    output_file = paste0(
      Sys.Date(), "_", params_from_excel$system_name_abbr, "_cost-reporting-sow.docx"),
    output_dir = if (exists("output_dir")) output_dir else here::here(),
    params = params_from_excel
  )
}
