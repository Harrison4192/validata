#' Drop useless columns
#'
#'Removes columns that are all NA or contain only 1 value.
#'
#' @param .data a data frame
#'
#' @return invisibly returns data frame
#' @export
#'
#' @examples iris %>% drop_useless_cols
drop_useless_cols <- function(.data) {
  unique_count <- missing_percent <- variables <- NULL

  .data %>%
    dlookr::diagnose(.) %>%
    dplyr::filter(unique_count == 1 | missing_percent == 100 ) %>%
    dplyr::pull(variables) -> drop_cols

  dc_string <- stringr::str_c(drop_cols, collapse = ", ")

  message(stringr::str_glue("dropping {length(drop_cols)} columns:{dc_string}"))

  .data %>% dplyr::select(-tidyselect::all_of(drop_cols)) -> .data

  invisible(.data)
}
