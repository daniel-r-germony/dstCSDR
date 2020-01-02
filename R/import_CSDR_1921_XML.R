## ===== Functions to import CSDR files from XML =====
## Designed for work with xml2 and dplyr

#' Import CCDR files from XML
#'
#' Import CSDR (DD Form 1921 and 1921-1) files from the XML source.
#'
#' @param the_file The file to read.
#'
#' @name import_xml_data
#'
#' @return A list of three tables (tbl_df): \cr
#'    \describe{
#'       \item{metadata}{The document metadata (e.g., Program Name, Contract Number, Report As Of Date, etc.).}
#'       \item{cost_report}{The WBS level cost report data.}
#'       \item{cost_summary}{The summary level cost report data (e.g., Subtotal, G&A, Fee).}
#'    }
#'
NULL

#' Get CCDR file list from a directory
#'
#' \code{get_files_ccdr} returns a tibble of all valid files within a directory.
#'
#' @export
#'
#' @param file_path The root directory to search from.
#'
#' @return A 6 column table (tbl_df) of XML files and their type (e.g., 1921, 1921-1).
#'
#' @examples
#' \dontrun{\donttest{
#' file_path <- system.file("extdata", package = "dstCSDR")
#' get_files_ccdr(file_path)
#' }}
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

  ccdr_files <- dplyr::mutate(ccdr_files, dir = ifelse(is.na(dir), "", dir))

  dplyr::select(ccdr_files, id, id_report, dplyr::everything())
}

#' Read 2007 CCDR metadata
#'
#' @keywords internal
#'
#' @param xml_list List object from xml2 parsed file.
#'
read_xml_header_2007 <- function(xml_list) {

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
                                 "typeAction/name", "action_name", "chr",
                                 "periodOfPerformance/startDate", "pop_start", "date",
                                 "periodOfPerformance/endDate", "pop_end", "date",
                                 "appropriationTypes/appropriationType", "appropriation_type", "chr",
                                 "reportCycle", "report_cycle", "chr",
                                 "submissionNumber", "submission_number", "num",
                                 "resubmissionNumber", "resubmission_number", "num",
                                 "reportAsOf", "report_as_of", "date",
                                 "pointOfContact/name", "poc_name", "chr",
                                 "pointOfContact/department", "poc_department", "chr",
                                 "pointOfContact/phone", "poc_phone", "chr",
                                 "pointOfContact/email", "poc_email", "chr",
                                 "datePrepared", "date_prepared", "date"
  )

  get_xpath <- function(xpath) {
    node_eval_str <- paste0("unlist(xml_list",
                            paste(paste0("[['",stringr::str_split(xpath, "/", simplify = TRUE), "']]"),
                                  collapse = ""),
                            ")")

    return_str <- eval(parse(text = node_eval_str))
    ifelse(is.null(return_str), NA_character_, return_str)
  }

  header <- tibble::as_tibble(setNames(lapply(header_vars$xpath, get_xpath), header_vars$var_name))

  # Will warn if converting text to numeric (NAs introduced by coercion)
  header <- suppressWarnings(dplyr::mutate_at(header, .vars = header_vars$var_name[header_vars$data_type == "num"], .funs = as.numeric))
  suppressWarnings(dplyr::mutate_at(header, .vars = header_vars$var_name[header_vars$data_type == "date"], .funs = as.Date))
}

## ===== 1921 Functions =====

#' Import DD Form 1921 file from XML
#'
#' \code{import_cdsr_xml} parses CSDR data (DD Form 1921). Requires a file with a valid extension (i.e., '1921.xml').
#'
#' @export
#'
#' @rdname import_xml_data
#'
#' @examples
#' \dontrun{\donttest{
#' file_path <- system.file("extdata", package = "dstCSDR")
#' get_files_ccdr(file_path)
#'
#' cost_report_1 <- import_cdsr_xml(paste(file_path, ccdr_files$file[1], sep = "/"))
#' }}
import_cdsr_xml <- function(the_file) {

  xml_data <- xml2::read_xml(the_file)
  xml_list <- xml2::as_list(xml_data)[["Form1921"]]

  ccdr_version <- xml2::xml_attr(xml_data, "csdrDID")

  message(paste("reading csdr version", ccdr_version))

  list(metadata = read_xml_header_2007(xml_list),
       cost_report = read_csdr_xml_body(xml_list),
       cost_summary = read_csdr_xml_summary(xml_list))

}

#' Read CSDR body data
#'
#' @keywords internal
#'
#' @inheritParams read_xml_header_2007
#'
read_csdr_xml_body <- function(xml_list) {

  process_wbs_element <- function(r) {
    wbs <- unlist(r[["wbsElementCode"]])
    item <- unlist(r[["wbsElementName"]])

    units_td <- as.numeric(unlist(r[["numberOfUnitsToDate"]]))
    units_ac <- as.numeric(unlist(r[["numberOfUnitsAtCompletion"]]))
    units <- c(units_td, units_ac)

    cost_td <- dplyr::bind_cols(lapply(r[["costsIncurredToDate"]], as.numeric))
    cost_ac <- dplyr::bind_cols(lapply(r[["costsIncurredAtCompletion"]], as.numeric))
    cost <- dplyr::bind_rows(cost_td, cost_ac)

    time_period <- c("to date", "at completion")

    tibble::add_column(cost, WBS = wbs, Item = item, Time_Period = time_period, Units = units, .before = 1)
  }

  # Read children of wbsElements (all cost rows on the report)
  child_elements <- xml_list[["wbsElements"]]

  # Convert cost rows into data frame
  dplyr::bind_rows(lapply(child_elements, process_wbs_element))
}

#' Read CSDR summary data
#'
#' @keywords internal
#'
#' @inheritParams read_xml_header_2007
#'
read_csdr_xml_summary <- function(xml_list) {

  # Read in all summary element costs into data frame
  cost_summary <- list_to_df(xml_list[["summaryElements"]])

  dplyr::bind_cols(Item = c("SubTotalCost", "GA", "UB", "MR", "FCCM", "TotalCost", "Fee", "TotalPrice"),
                   tibble::as_tibble(cost_summary))

}

## ===== 1921-1 Functions =====

#' Import DD FOrm 1921-1 file from XML
#'
#' \code{import_fchr_xml} parses FCHR data (DD Form 1921-1). Requires a file with a valid extension (i.e., '1921_1.xml').
#'
#' @export
#'
#' @rdname import_xml_data
#'
#' @examples
#' \dontrun{\donttest{
#' file_path <- system.file("extdata", package = "dstCSDR")
#' get_files_ccdr(file_path)
#'
#' cost_report_2 <- import_fchr_xml(paste(file_path, ccdr_files$file[2], sep = "/"))
#' }}
import_fchr_xml <- function(the_file) {

  xml_data <- xml2::read_xml(the_file)
  xml_list <- xml2::as_list(xml_data)[["Form1921_1"]]

  ccdr_version <- xml2::xml_attr(xml_data, "csdrDID")

  message(paste("reading fchr version", ccdr_version))

  # Same header format as 1921
  list(metadata = read_xml_header_2007(xml_list),
       cost_report = read_fchr_xml_body(xml_list))

}

#' Read FCHR body data
#'
#' @keywords internal
#'
#' @inheritParams read_xml_header_2007
#'
read_fchr_xml_body <- function(xml_list) {

  # Process a WBS element
  process_wbs_element <- function(r) {
    wbs <- unlist(r[["wbsElementCode"]])
    item <- unlist(r[["wbsElementName"]])

    units_td <- as.numeric(unlist(r[["numberOfUnitsToDate"]]))
    units_ac <- as.numeric(unlist(r[["numberOfUnitsAtCompletion"]]))

    # Extract the nested tables (recurring/nonrecurring and to date/at completion) from the report
    extract_nesting <- function(o) {

      # Read in all summary element costs into data frame
      cost <- list_to_df(o)

      time_period <- c("to date", "at completion")
      units = c(units_td, units_ac)

      tibble::add_column(cost, Time_Period = time_period, Units = units, .before = 1)
    }

    costs <- lapply(r[["functionalDataElements"]], extract_nesting)
    tibble::add_column(dplyr::bind_rows(costs), WBS = wbs, Element = item, Item = item, .before = 1)
  }

  # Read children of wbsElements (all cost rows on the report)
  child_elements <- xml_list[["wbsElements"]]

  # Convert cost rows into data frame
  dplyr::bind_rows(lapply(child_elements, process_wbs_element))
}

## ===== Parse Helper Functions =====

#' Read FCHR body data
#'
#' @keywords internal
#'
#' @param list_root List object for the data frame processing.
#'
list_to_df <- function(list_root) {

  list_to_num <- function(r) dplyr::bind_rows(sapply(r, function(x) ifelse(length(x) == 0, 0, as.numeric(x))))

  dplyr::bind_rows(lapply(list_root, list_to_num))
}
