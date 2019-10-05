library(magrittr)
library(here)

# -----------------------------------------------------------------------------

excel_ls <- function(path) {
    excel_ls_table <- path %>%
        fs::dir_ls(regexp = "[.]xls[x]$") %>%
        purrr::map(excel_sheets) %>%
        tibble::enframe() %>%
        tidyr::unnest(cols = c(value)) %>%
        dplyr::mutate(file_name = stringr::str_remove_all(name, paste0(path, "/"))) %>%
        dplyr::mutate(folder_path = stringr::str_remove_all(name, file_name)) %>%
        dplyr::select(path = name,
                      folder_path,
                      file_name,
                      worksheet_title = value)
    
    return(excel_ls_table)
}
# -----------------------------------------------------------------------------

import_excel_worksheets <- function(excel_files_data) {
  for (i in seq_len(nrow(excel_files_data))) {
    if (i != 1) {
      combined_worksheets <- 
        rbind(combined_worksheets,
            (readxl::read_excel(
               path = excel_files_data$path[i],
               sheet = excel_files_data$worksheet_title[i]) %>%
             dplyr::mutate(
                 source_file_name = excel_files_data$file_name[i],
                 source_file_worksheet = excel_files_data$worksheet_title[i])))
        }
    if (i == 1) {
      combined_worksheets <- (
        readxl::read_excel(
          path = excel_files_data$path[i],
          sheet = excel_files_data$worksheet_title[i]) %>%
        dplyr::mutate(
            source_file_name = excel_files_data$file_name[i],
            source_file_worksheet = excel_files_data$worksheet_title[i]))
        }
    }
    return(combined_worksheets)
}
# -----------------------------------------------------------------------------

path <- here("excel")

excel_tables_to_import <- excel_ls(path)

imported_excel_tables <- import_excel_worksheets(excel_tables_to_import)
    
