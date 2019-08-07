## CCDR XML Module

#' Get CCDR Files from a Directory
#'
#' For a given directory (i.e., file path), return a recursive list of all CSDR XML files.
#'
#' @param file_path The root directory to search from.
#'
#' @return A 6 column table (tbl_df) of XML files and their type (e.g., 1921, 1921-1).
#' @export
#'
#' @examples
#' file_path <- system.file("extdata", package = "dstCSDR")
#' get_files_ccdr(file_path)
get_files_ccdr <- function(file_path) {

  ccdr_xml_ext <- tibble::tibble(ext = c("1921.xml", "1921_1.xml", "1921_1P1.xml"),
                                 type = c("1921", "1921-1", "1921-1 (2003)"))

  pattern_ccdr <- paste0("\\.(", paste(ccdr_xml_ext$ext, collapse = "|"), ")$")

  ccdr_files <- tibble::tibble(filepath = list.files(path = file_path, pattern = pattern_ccdr, recursive = TRUE, include.dirs = TRUE))
  ccdr_files <- dplyr::mutate(ccdr_files, dir = stringi::stri_extract_last(str = filepath, regex = ".*/"),
                              file = dplyr::if_else(is.na(dir), filepath, stringr::str_remove(filepath, dir)))
  ccdr_files <- dplyr::select(ccdr_files, -filepath)

  ccdr_files <- tibble::rowid_to_column(ccdr_files, "id")
  ccdr_files <- dplyr::mutate(ccdr_files, id_report = dplyr::group_indices(ccdr_files, dir),
                              ext = "",
                              type = "")

  for (file_index in 1:nrow(ccdr_xml_ext)) {
    match_ext <- stringr::str_detect(ccdr_files$file, paste0("\\.", ccdr_xml_ext$ext[file_index], "$"))
    ccdr_files[match_ext, c("ext", "type")] = ccdr_xml_ext[file_index,]
  }

  dplyr::select(ccdr_files, id, id_report, dplyr::everything())
}

#' Read a DD Form 1921 XML File
#'
#' Parse the data for a given file name with a valid extension (i.e., '1921.xml').
#'
#' @param file_path The file to read.
#'
#' @return A list of three tables (tbl_df): \cr
#'    \describe{
#'       \item{metadata}{The document metadata (e.g., Program Name, Contract Number, Report As Of Date, etc.).}
#'       \item{cost_report}{The WBS level cost report data.}
#'       \item{cost_summary}{The summary level cost report data (e.g., Subtotal, G&A, Fee).}
#'    }
#' @export
#'
#' @examples
#' file_path <- system.file("extdata", package = "dstCSDR")
#' get_files_ccdr(file_path)
#'
#' cost_report_1 <- read_xml_ccdr(paste0(file_path, ccdr_files$file[1]))
read_xml_ccdr <- function(file_path) {

  xml_data <- XML::xmlParse(file_path)
  xml_root <- XML::xmlRoot(xml_data)

  ccdr_version <- XML::xmlGetAttr(xml_root, "csdrDID")

  message(paste("reading csdr version", ccdr_version))

  list(metadata = read_xml_ccdr_header(xml_root),
       cost_report = read_xml_ccdr_body(xml_root),
       cost_summary = read_xml_ccdr_summary(xml_root))

}

# Not exported
read_xml_ccdr_header <- function(xml_root) {

  header_vars <- tibble::tribble(~xpath, ~var_name, ~data_type,
                                 "programName", "program_name", "chr",
                                 "programPhase", "program_phase", "chr",
                                 "primeMissionProduct", "prime_mission_product", "chr",
                                 "contractorType", "contractor_type", "chr",
                                 "contractorNameAddress/contractorName", "contractor_name", "chr",
                                 "contractorNameAddress/address/address1", "address1", "chr",
                                 "contractorNameAddress/address/address2", "address2", "chr",
                                 "contractorNameAddress/address/city", "city", "chr",
                                 "contractorNameAddress/address/state", "state", "chr",
                                 "contractorNameAddress/address/zipcode", "zipcode", "chr",
                                 "contractorNameAddress/address/isInternational", "is_international", "chr",
                                 "approvedPlanNumber", "plan_number", "chr",
                                 "customerName", "customer_name", "chr",
                                 "contractType", "contract_type", "chr",
                                 "contractPrice", "contract_price", "num",
                                 "contractCeiling", "contract_ceiling", "num",
                                 "typeAction/contractNumber", "contract_number", "chr",
                                 "typeAction/latestModification", "latest_mod", "chr",
                                 "typeAction/solicitationNumber", "solicitation_number", "chr",
                                 "typeAction/name", "action_name", "chr"
  )

  get_xpath <- function(xpath) {
    node_eval_str <- paste0("XML::xmlValue(xml_root",
                            paste(paste0("[['",stringr::str_split(xpath, "/", simplify = TRUE), "']]"),
                                  collapse = ""),
                            ")")

    eval(parse(text = node_eval_str))
  }

  header <- tibble::as_tibble(setNames(lapply(header_vars$xpath, get_xpath), header_vars$var_name))

  # Will warn if converting text to numeric (NAs introduced by coercion)
  suppressWarnings(dplyr::mutate_at(header, .vars = header_vars$var_name[header_vars$data_type == "num"], .funs = as.numeric))
}

# Not exported
read_xml_ccdr_body <- function(xml_root) {

  process_wbs_element <- function(r) {
    wbs <- XML::xmlValue(r[["wbsElementCode"]])
    item <- XML::xmlValue(r[["wbsElementName"]])
    a <- XML::xmlToDataFrame(XML::xmlChildren(r[["costsIncurredToDate"]]))
    b <- XML::xmlValue(r[["numberOfUnitsAtCompletion"]])
    c <- XML::xmlToDataFrame(XML::xmlChildren(r[["costsIncurredAtCompletion"]]))

    values <- as.numeric(c(t(a), b, t(c)))
    names(values) <- c(paste0("ToDate_", c("NR", "R", "T")), paste0("AtComp_", c("Units", "NR", "R", "T")))

    dplyr::bind_cols(WBS = wbs, Item = item, tibble::as_tibble(rbind(values)))
  }

  # Read children of wbsElements (all cost rows on the report)
  child_elements <- XML::xmlChildren(xml_root[["wbsElements"]])

  # Convert cost rows into data frame
  dplyr::bind_rows(lapply(child_elements, process_wbs_element))
}

# Not exported
read_xml_ccdr_summary <- function(xml_root) {

  # Read in all summary element costs into data frame
  cost_summary <- XML::xmlToDataFrame(XML::xmlChildren(xml_root[["summaryElements"]]), stringsAsFactors = F)
  cost_summary <- sapply(cost_summary, as.numeric)
  cost_summary[is.na(cost_summary)] <- 0

  dplyr::bind_cols(Item = c("SubTotalCost", "GA", "UB", "MR", "FCCM", "TotalCost", "Fee", "TotalPrice"),
                   tibble::as_tibble(cost_summary))

}
