#' WBS Element Code to Level
#'
#' A function to identify the level assoicated with given WBS Element Code.
#'
#' @param wbs_element_code Required.
#' @param ... Optional. Included in order to be %>% frendly.
#'
#' @return Returns an intiger assoicated with WBS Element Code (i.e., 0 for WBS
#'   = "1.0", 1 for "1.1", 3 for "1.2.2.1")
#' @export
#'
#' @examples
wbs_code_to_lvl <- function(wbs_element_code, ...){

  # Error prevention: Ensure the input is a char or factor, otherwise stop.
  if(!is.character(wbs_element_code) &&
              !is.factor(wbs_element_code)) {

    stop(
      'This fuction only works with character or factor objects!\n',
      'You have provided an object of class: ', class(wbs_element_code[1]))
  }

  # Error prevention: Ensure the input looks like a WBS Element Code.
  if(!str_detect(string = wbs_element_code,
                 pattern = wbs_code_regex)) {
    stop(
      'The object provided does not appear to be a WBS Element Code!\n',
      'WBS Element Codes start and end with a diget, include at least one\n',
      'period, and contain no other punctuation or alphabetical characters\n',
      '(their regular expression is "^(\\d)(\\.\\d)+(\\d)*$"). Here are some\n',
      'examples of what WBS Element Codes look like:\n',
      '- 1.0\n',
      '- 1.1.3\n',
      '- 1.2.1.1.5.4.2'
      )
  }

  # Starts with a diget, has at least one grouped period & diget, ends with a
  # diget.
  wbs_code_regex <- "^(\\d)(\\.\\d)+(\\d)*$"


  if (str_detect(string = wbs_element_code,
                 pattern = wbs_code_regex)) {
    str_count(string = wbs_element_code,
              pattern = "\\.")
  }

  #return(wbs_lvl)

}

wbs_codes_col <- cdsr$reported_data %>% select(`WBS Element Code`)

wbs_codes_col[[1]] %>% wbs_code_to_lvl()

pass_single_char_wbs <- "1.1.2"
fail_wbs_not_wbs_char <- "abc"
fail_wbs_df_object <- iris %>% as_tibble()

pass_single_char_wbs %>% wbs_code_to_lvl()
fail_wbs_not_wbs_char %>% wbs_code_to_lvl()
fail_wbs_df_object %>% wbs_code_to_lvl()


