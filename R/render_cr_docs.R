#' Create Cost Reporting SOW, CDRLs, Attachments with Params
#'
#' Imports a pre-formatted and filled in Excel file with info on the planned
#' contract and system then generates a scope of work, CDRLs, and (coming
#' soon) associated contract attachments from template documents.
#'
#' @param params_path Required. A full path to a filled-in
#'   "cost_reporting_params.xlsx" file. If a path is not provided to the
#'   function call, a file selection box will be opened.
#' @param output_dir Required. A directory path to where the user wants the
#'   generated files to be placed. If using Windows, a pop-up box will appear to
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
#'
#' \dontrun{
#' ## Render the cost reporting documents and select both the
#' ## "cost_reporting_params.xlsx" file and output directory using pop-up boxes.
#' .render_cr_docs()
#'
#' ## Render the cost reporting documents with the default
#' ## "cost_reporting_params.xlsx" values and put the output on a fake user's
#' ## desktop.
#' .render_cr_docs(params_path = here::here("extdata", "cost_reporting_params.xlsx"),
#'                output_dir = "C:/Users/john.doe/Desktop")
#'          }
.render_cr_docs <- function(params_path = file.choose(),
                           output_dir = utils::choose.dir(),
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

  params_from_excel <- params_from_excel %>%
    dplyr::mutate(
      # Date Columns
      draft_RFP_release_date   = as.Date(as.numeric(.data$draft_RFP_release_date), origin = "1899-12-30"),
      RFP_release_date         = as.Date(as.numeric(.data$RFP_release_date),       origin = "1899-12-30"),
      contract_award_date      = as.Date(as.numeric(.data$contract_award_date),    origin = "1899-12-30"),
      contract_end_date        = as.Date(as.numeric(.data$contract_end_date),      origin = "1899-12-30"),
      cdrl_prepared_date       = as.Date(as.numeric(.data$cdrl_prepared_date),     origin = "1899-12-30"),
      cdrl_approved_date       = as.Date(as.numeric(.data$cdrl_approved_date),     origin = "1899-12-30"),
      # Phone Numbers
      CSDR_plan_POC_phone      = as.numeric(.data$CSDR_plan_POC_phone),
      pco_phone                = as.numeric(.data$pco_phone),
      #Contract Values
      est_contract_value       = as.numeric(.data$est_contract_value),
      est_contract_swdev_value = as.numeric(.data$est_contract_swdev_value),
      est_contract_swmx_value  = as.numeric(.data$est_contract_swmx_value),
      est_contract_sys_value   = as.numeric(.data$est_contract_sys_value),
      # Logical
      RDT_CDRL_lgl             = as.logical(.data$RDT_CDRL_lgl),
      FlexFile_CDRL_lgl        = as.logical(.data$FlexFile_CDRL_lgl),
      QtyData_CDRL_lgl         = as.logical(.data$QtyData_CDRL_lgl),
      MR_CDRL_lgl              = as.logical(.data$MR_CDRL_lgl),
      TDR_CDRL_lgl             = as.logical(.data$TDR_CDRL_lgl),
      CWBS_CDRL_lgl            = as.logical(.data$CWBS_CDRL_lgl),
      CDSR_CDRL_lgl            = as.logical(.data$CDSR_CDRL_lgl),
      FCHR_CDRL_lgl            = as.logical(.data$FCHR_CDRL_lgl),
      PCR_CDRL_lgl             = as.logical(.data$PCR_CDRL_lgl),
      CBDR_CDRL_lgl            = as.logical(.data$CBDR_CDRL_lgl),
      SFCHR_CDRL_lgl           = as.logical(.data$SFCHR_CDRL_lgl),
      SDR_CDRL_lgl             = as.logical(.data$SDR_CDRL_lgl),
      SMR_CDRL_lgl             = as.logical(.data$SMR_CDRL_lgl),
      ERP_CDRL_lgl             = as.logical(.data$ERP_CDRL_lgl),
      BOM_CDRL_lgl             = as.logical(.data$BOM_CDRL_lgl),
      LSPD_CDRL_lgl            = as.logical(.data$LSPD_CDRL_lgl),
      AUMC_CDRL_lgl            = as.logical(.data$AUMC_CDRL_lgl),
      CFSR_CDRL_lgl            = as.logical(.data$CFSR_CDRL_lgl),
      PaCR_CDRL_lgl            = as.logical(.data$PaCR_CDRL_lgl),
      PAC_CDRL_lgl             = as.logical(.data$PAC_CDRL_lgl)
    )

# Write Basic CDRL Block Data to the Environment =============================
#
# This script puts basic CDRL data in one place, making it easier to update
# stuff like CDRL titles and DID reference (rather than having to update each
# CDRL, one at a time). This script is sourced by `.render_cr_docs.R`.
#
# @return Technically, nothing is returned this this is not a function but many
#   CDRL titles, subtitles, DID numbers, and distro statement letters are
#   written to the global environment.

prime_vs_sub <-
  if (params_from_excel$reporting_organization_type == ">>>CSDR_Plan_type<<<") {"Contractor"} else
  if (params_from_excel$reporting_organization_type == "Prime Contractor") {"Contractor"} else {"Subcontractor"}

# Block 2: CDRL Titles ========================================================

CWBS_CDRL_title_abbr         <- "CWBS"
RDT_CDRL_title_abbr          <- "RDT"
FlexFile_CDRL_title_abbr     <- "FF"
QtyData_CDRL_title_abbr      <- "Qty"
CDSR_CDRL_title_abbr         <- "CDSR"
FCHR_CDRL_title_abbr         <- "FCHR"
PCR_CDRL_title_abbr          <- "PCR"
CBDR_CDRL_title_abbr         <- "CBDR"
SFCHR_CDRL_title_abbr        <- "SFCHR"
MR_CDRL_title_abbr           <- "MRR"
TDR_CDRL_title_abbr          <- "TDR"
SDR_CDRL_title_abbr          <- "SDR"
SMR_CDRL_title_abbr          <- "SMR"
ERP_CDRL_title_abbr          <- "ERP"
CFSR_CDRL_title_abbr         <- "CFSR"
PaCR_CDRL_title_abbr         <- "PaCR"
LSPD_CDRL_title_abbr         <- "LSPD"
AUMC_CDRL_title_abbr         <- "AUCM"
BOM_CDRL_title_abbr          <- "BOM"
PAC_CDRL_title_abbr          <- "PAC"

# Block 2: CDRL Titles ========================================================

CWBS_CDRL_title         <- "Contractor Work Breakdown Structure (CWBS)"
RDT_CDRL_title          <- "Resource Distribution Table (RDT)"
FlexFile_CDRL_title     <- "Cost and Hour Report (FlexFile)"
QtyData_CDRL_title      <- "Quantity Data Report"
CDSR_CDRL_title         <- "Cost Data Summary Report (DD Form 1921)"
FCHR_CDRL_title         <- "Functional Cost-Hour Report (DD Form 1921-1)"
PCR_CDRL_title          <- "Progress Curve Report (DD Form 1921-2)"
CBDR_CDRL_title         <- "Contractor Business Data Report (DD Form 1921-3)"
SFCHR_CDRL_title        <- "Sustainment Functional Cost-Hour Report (DD Form 1921-5)"
MR_CDRL_title           <- "Maintenance and Repair Parts Data Report"
TDR_CDRL_title          <- "Technical Data Report"
SDR_CDRL_title          <- "Software Development Report (3026-1)"
SMR_CDRL_title          <- "Software Maintenance Report (3026-2)"
ERP_CDRL_title          <- "Software Development Enterprise Resource Planning (ERP) Report (3026-3)"
CFSR_CDRL_title         <- "Contract Funds Status Report (CFSR)"
PaCR_CDRL_title         <- "Performance & Cost Report"
LSPD_CDRL_title         <- "Lot Size Pricing Data (LSPD)"
AUMC_CDRL_title         <- "Average Unit Manufacturing Cost (AUCM) Report"
BOM_CDRL_title          <- "Bill of Materials (BOM)"
PAC_CDRL_title          <- "CSDR PAC Briefing Material"

# Block 3: CDRL Subtitles =====================================================
CWBS_CDRL_subtitle         <- ""
RDT_CDRL_subtitle          <- ""
FlexFile_CDRL_subtitle     <- "Contractor Cost Data Report (CCDR)"
QtyData_CDRL_subtitle      <- "Contractor Cost Data Report (CCDR)"
CDSR_CDRL_subtitle         <- "Contractor Cost Data Report (CCDR)"
FCHR_CDRL_subtitle         <- "Contractor Cost Data Report (CCDR)"
PCR_CDRL_subtitle          <- "Contractor Cost Data Report (CCDR)"
CBDR_CDRL_subtitle         <- "Contractor Cost Data Report (CCDR)"
SFCHR_CDRL_subtitle        <- "Contractor Cost Data Report (CCDR)"
MR_CDRL_subtitle           <- ""
TDR_CDRL_subtitle          <- ""
SDR_CDRL_subtitle          <- "Software Resources Data Report (SRDR)"
SMR_CDRL_subtitle          <- "Software Resources Data Report (SRDR)"
ERP_CDRL_subtitle          <- "Software Resources Data Report (SRDR)"
CFSR_CDRL_subtitle         <- ""
PaCR_CDRL_subtitle         <- ""
LSPD_CDRL_subtitle         <- ""
AUMC_CDRL_subtitle         <- ""
BOM_CDRL_subtitle          <- ""
PAC_CDRL_subtitle          <- ""

# Block 4: CDRL DID Numbers ===================================================
CWBS_CDRL_did         <- "DI-MGMT-81334D"
RDT_CDRL_did          <- stringr::str_c("See Attachment ", params_from_excel$RDT_attach_numb)
FlexFile_CDRL_did     <- "DI-FNCL-82162"
QtyData_CDRL_did      <- "DI-MGMT-82164"
CDSR_CDRL_did         <- "DI-FNCL-81565C"
FCHR_CDRL_did         <- "DI-FNCL-81566C"
PCR_CDRL_did          <- "DI-FNCL-81567C"
CBDR_CDRL_did         <- "DI-FNCL-81765B"
SFCHR_CDRL_did        <- "DI-FNCL-81992"
MR_CDRL_did           <- "DI-MGMT-82163"
TDR_CDRL_did          <- "DI-MGMT-82165"
SDR_CDRL_did          <- "DI-MGMT-82035A"
SMR_CDRL_did          <- "DI-MGMT-82035A"
ERP_CDRL_did          <- "DI-MGMT-82035A"
CFSR_CDRL_did         <- "DI-MGMT-81468"
PaCR_CDRL_did         <- "???"  # TODO: Need a DID number for the P&RC
LSPD_CDRL_did         <- stringr::str_c("See Attachment ", params_from_excel$LSPD_attach_numb)
AUMC_CDRL_did         <- "DI-MGMT-81334D(T)"
BOM_CDRL_did          <- "DI-MGMT-81994"
PAC_CDRL_did          <- "DI-MGMT-81605"

# Block 9: DIST. STATEMENT REQUIRED ===========================================
CWBS_CDRL_dist          <- "D"
RDT_CDRL_dist           <- "D"
FlexFile_CDRL_dist      <- "D"
QtyData_CDRL_dist       <- "D"
CDSR_CDRL_dist          <- "D"
FCHR_CDRL_dist          <- "D"
PCR_CDRL_dist           <- "D"
CBDR_CDRL_dist          <- "D"
SFCHR_CDRL_dist         <- "D"
MR_CDRL_dist            <- "D"
TDR_CDRL_dist           <- "D"
SDR_CDRL_dist           <- "D"
SMR_CDRL_dist           <- "D"
ERP_CDRL_dist           <- "D"
CFSR_CDRL_dist          <- "D"
PaCR_CDRL_dist          <- "D"
LSPD_CDRL_dist          <- "D"
AUMC_CDRL_dist          <- "D"
BOM_CDRL_dist           <- "D"
PAC_CDRL_dist           <- "D"

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
                        package = "dstCSDR"),
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
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "RDT_CDRL.Rmd", "RDT")

  if (as.logical(params_from_excel$FlexFile_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "FF_CDRL.Rmd", "FF")

  if (as.logical(params_from_excel$QtyData_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "Qty_CDRL.Rmd", "Qty")

  if (as.logical(params_from_excel$MR_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "MRR_CDRL.Rmd", "MR")

  if (as.logical(params_from_excel$TDR_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "TDR_CDRL.Rmd", "TDR")

  if (as.logical(params_from_excel$CWBS_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "CWBS_CDRL.Rmd", "CWBS")

  if (as.logical(params_from_excel$CDSR_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "CDSR_CDRL.Rmd", "CSDR")

  if (as.logical(params_from_excel$FCHR_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "FCHR_CDRL.Rmd", "FCHR")

  if (as.logical(params_from_excel$PCR_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "PCR_CDRL.Rmd", "PCR")

  if (as.logical(params_from_excel$CBDR_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "CBDR_CDRL.Rmd", "CBDR")

  if (as.logical(params_from_excel$SFCHR_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "SFCHR_CDRL.Rmd", "SFCHR")

  if (as.logical(params_from_excel$SDR_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "SDR_CDRL.Rmd", "SDR")

  if (as.logical(params_from_excel$SMR_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "SMR_CDRL.Rmd", "SMR")

  if (as.logical(params_from_excel$ERP_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "ERP_CDRL.Rmd", "ERP")

  if (as.logical(params_from_excel$BOM_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "BOM_CDRL.Rmd", "BOM")

  if (as.logical(params_from_excel$LSPD_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "LSPD_CDRL.Rmd", "LSPD")

  if (as.logical(params_from_excel$AUMC_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "AUMC_CDRL.Rmd", "AUMC")

  if (as.logical(params_from_excel$CFSR_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "CFSR_CDRL.Rmd", "CFSR")

  if (as.logical(params_from_excel$PaCR_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "PaCR_CDRL.Rmd", "PaCR")

  if (as.logical(params_from_excel$PAC_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "PAC_CDRL.Rmd", "PAC")

  if (as.logical(params_from_excel$MR_CDRL_lgl) == TRUE)
    dstCSDR::.render_cr_cdrl_doc(params_path, output_dir, "PAC_CDRL.Rmd", "PAC")

  # Render Section L language into its own Word document. =====================

  rmarkdown::render(
    input = system.file("Rmd",
                        "Section_L.Rmd",
                        package = "dstCSDR"),
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
