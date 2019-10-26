#' Write Basic CDRL Block Data to the Environment
#'
#' This script puts basic CDRL data in one place, making it easier to update
#' stuff like CDRL titles and DID reference (rather than having to update each
#' CDRL, one at a time). This script is sourced by `render_cr_docs.R`.
#'
#' @return Technically, nothing is returned this this is not a function but many
#'   CDRL titles, subtitles, DID numbers, and distro statement letters are
#'   written to the global environment.

prime_vs_sub <-
  if (params$CSDR_Plan_type == ">>>CSDR_Plan_type<<<") {"Contractor"} else
  if (params$CSDR_Plan_type == "Contract") {"Contractor"} else {"Subcontractor"}

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
SDR_CDRL_did          <- "DI–MGMT-82035A"
SMR_CDRL_did          <- "DI–MGMT-82035A"
ERP_CDRL_did          <- "DI–MGMT-82035A"
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
