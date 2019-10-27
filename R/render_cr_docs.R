#' Create Cost Reporting SOW, CDRLs, Attachments with Params
#'
#' Imports a pre-formatted and filled in Excel file with info on the planned
#' contract and system then generates a scope of work, CDRLs, and  (comming
#' soon) associated contract attachments from template documents.
#'
#' @param params_path Required. A full path to a filled-in
#'   "cost_reporting_params.xlsx" file. If a path is not provided to the
#'   function call, a file selection box will be opened.
#' @param output_dir Required. A directory path to where the user wants the
#'   generated files to be placed. If using Windows, a pop-up box will ppear to
#'   specify an output directory. Non-Windows users must pass a directory path
#'   in the function call.
#' @param combine_cdrls Optional. Should all CDRLs be written into one .docx or
#'   one .docx per CDRL. Options:
#'     \describe{
#'       \item{\code{"TRUE"}}{The CDRLs will be included in one Word file. The
#'         default.}
#'       \item{\code{"FALSE"}}{Each CDRL is written to its own Word file.}}
#'
#' @return Returns nothing to R but does puts the following files at the
#'   \code{output_dir}:
#'     \enumerate{
#'       \item A Word file (.docx) with cost reporting scope of work language,
#'       filled in with the param values from the "cost_reporting_params.xlsx"
#'       file.
#'       \item A Word file (.docx) or Word files with cost reporting CDRLs,
#'       filled in with the param values from the "cost_reporting_params.xlsx"
#'       file.
#       \item A partially complete CSDR Plan attachment, filled in with the
#       param  values from the "cost_reporting_params.xlsx" file.
#       \item A partially complete RDT attachment, filled in with the param
#       values from the "cost_reporting_params.xlsx" file.
#       \item A partially complete LSPD Report attachment (if required),
#       filled in with the param values from the "cost_reporting_params.xlsx"
#       file.
#       \item A complete AUMC Report attachment (if required), filled in with
#       the param values from the "cost_reporting_params.xlsx" file.
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
                           combine_cdrls = FALSE) {

  # SETUP =====================================================================

  # Error prevention: Check that `combine_cdrls` was an allowable value.
  if (!(combine_cdrls == TRUE || combine_cdrls == FALSE)) {
    stop(glue::glue(
        "\n\n The `combine_cdrls` paramater must be either \"TRUE\" or \"FALSE\". \n
        The function was passsed the value, \"{combine_cdrls}\"")
    )
  }

  # Import the params Excel document and convert it to a params list for Rmd.
  params_from_excel <-
    readxl::read_excel(path = params_path,
                       col_names = TRUE) %>%
    dplyr::select(.data$params, .data$`User Input`) %>%
    tidyr::pivot_wider(names_from = .data$params,
                       values_from = .data$`User Input`)

  # Source the .cdrl_block_info.R script to read some of the basic CDRL info
  # into R (which will be needed in the render step).
  source(here::here("R",".cdrl_block_info.R"),
         local = TRUE) # `local = TRUE` lets this script access the `params_from_excel`

  # This is a function to help render each CDRL into its own Word document.
  .render_cr_cdrl_doc <- function(params_path = params_path,
                                  output_dir = output_dir,
                                  rmd_cdrl,
                                  cdrl_title_abbr){
    rmarkdown::render(
      input = system.file("Rmd",
                          rmd_cdrl,
                          package = "dstCSDR"),
      output_file = paste0(Sys.Date(),
                           "_",
                           stringr::str_replace(
                             params_from_excel$prime_mission_product_abbr,
                             pattern = " ",
                             replacement = "-"),
                           "_",
                           cdrl_title_abbr,
                           "_CDRL",
                           ".docx"),
      output_dir = if (exists("output_dir"))
        output_dir
      else
        here::here(),
      params = params_from_excel
    )
  }

  # EXECUTION =================================================================

  # Render the SOW document docx. =============================================
  rmarkdown::render(
    input = system.file("Rmd",
                        "cost_reporting_sow_template.Rmd",
                        package="dstCSDR"),
    output_file = paste0(Sys.Date(),
                         "_",
                         stringr::str_replace(
                           params_from_excel$prime_mission_product_abbr,
                           pattern = " ",
                           replacement = "-"),
                         "_cost-reporting-sow.docx"),
    output_dir = if (exists("output_dir"))
                   output_dir
                 else
                   here::here(),
    params = params_from_excel
  )

  # Render each CDRL into its own Word document. ==============================
  if (as.logical(params_from_excel$RDT_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "RDT_CDRL.Rmd", "RDT")

  if (as.logical(params_from_excel$FlexFile_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "FF_CDRL.Rmd", "FF")

  if (as.logical(params_from_excel$QtyData_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "Qty_CDRL.Rmd", "Qty")

  if (as.logical(params_from_excel$MR_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "MRR_CDRL.Rmd", "MR")

  if (as.logical(params_from_excel$TDR_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "TDR_CDRL.Rmd", "TDR")

  if (as.logical(params_from_excel$CWBS_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "CWBS_CDRL.Rmd", "CWBS")

  if (as.logical(params_from_excel$CDSR_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "CDSR_CDRL.Rmd", "CSDR")

  if (as.logical(params_from_excel$FCHR_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "FCHR_CDRL.Rmd", "FCHR")

  if (as.logical(params_from_excel$PCR_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "PCR_CDRL.Rmd", "PCR")

  if (as.logical(params_from_excel$CBDR_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "CBDR_CDRL.Rmd", "CBDR")

  if (as.logical(params_from_excel$SFCHR_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "SFCHR_CDRL.Rmd", "SFCHR")

  if (as.logical(params_from_excel$SDR_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "SDR_CDRL.Rmd", "SDR")

  if (as.logical(params_from_excel$SMR_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "SMR_CDRL.Rmd", "SMR")

  if (as.logical(params_from_excel$ERP_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "ERP_CDRL.Rmd", "ERP")

  if (as.logical(params_from_excel$BOM_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "BOM_CDRL.Rmd", "BOM")

  if (as.logical(params_from_excel$LSPD_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "LSPD_CDRL.Rmd", "LSPD")

  if (as.logical(params_from_excel$AUMC_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "AUMC_CDRL.Rmd", "AUMC")

  if (as.logical(params_from_excel$CFSR_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "CFSR_CDRL.Rmd", "CFSR")

  if (as.logical(params_from_excel$PaCR_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "PaCR_CDRL.Rmd", "PaCR")

  if (as.logical(params_from_excel$PAC_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "PAC_CDRL.Rmd", "PAC")

  if (as.logical(params_from_excel$MR_CDRL_lgl) == TRUE)
    .render_cr_cdrl_doc(params_path, output_dir, "PAC_CDRL.Rmd", "PAC")

  # Render Section L language into its own Word document. =====================

  rmarkdown::render(
    input = system.file("Rmd",
                        "Section_L.Rmd",
                        package="dstCSDR"),
    output_file = paste0(Sys.Date(),
                         "_",
                         stringr::str_replace(
                           params_from_excel$prime_mission_product_abbr,
                           pattern = " ",
                           replacement = "-"),
                         "_section-l.docx"),
    output_dir = if (exists("output_dir"))
      output_dir
    else
      here::here(),
    params = params_from_excel
  )

  # Render all CDRLs into one Word document ===================================
  if (combine_cdrls == TRUE) {
  # Add code that stitches each CDRL into one word doc
  # Also delete each of the individual CDRL files
  }

}
