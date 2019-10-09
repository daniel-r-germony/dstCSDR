render_cr_docs <- function(params_path = file.choose(), output_dir = choose.dir()) {

  # Add a message that non-Windows users must pass an output_dir. Windows users
  # will be provided with a choose.dir popup box.

  params_from_excel <-
    readxl::read_excel(path = params_path,
                       col_names = TRUE) %>%
    dplyr::select(params, `User Input`) %>%
    tidyr::pivot_wider(names_from = params, values_from = `User Input`)

  rmarkdown::render(
    input = here::here("inst", "extdata", "cost_reporting_sow_template.Rmd"),
    #output = "word_document",
    output_file = "cost_reporting_sow.docx",
    output_dir = if (exists("output_dir")) output_dir else here::here(),
    params = params_from_excel
  )
}
