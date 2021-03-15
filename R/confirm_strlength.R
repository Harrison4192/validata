#' confirm string length
#'
#' returns a count table of string lengths for a character column.
#'
#' @param mdb dataframe
#' @param col unquoted column
#'
#' @return prints a summary and returns a dataframe invisibly
#' @export
#'
#' @examples iris %>% confirm_strlen(Species)
confirm_strlen <- function(mdb, col){
  mdb %>% dplyr::ungroup() -> mdb


  nm <- rlang::as_string(rlang::ensym(col))
  col_nm <- rlang::as_string(stringr::str_glue("{nm}_chr_len"))

  mdb %>%
    dplyr::mutate(!!col_nm := stringr::str_length({{col}})) -> tmp_db

    tmp_db %>%  janitor::tabyl(!!col_nm) %>% janitor::adorn_pct_formatting() -> rt
    print(rt)
    invisible(tmp_db)

}


#' choose string length
#'
#' a helper function for `confirm_strlen` that filters the output dataframe on a specific string length for that column
#'
#' @param mdb dataframe. output from `confirm_strlen`
#' @param len integer vector.
#'
#' @return dataframe with original columns, filtered to the specific string length
#' @export
#'
choose_strlen <- function(mdb, len) {

  mdb %>% names() %>% stringr::str_subset("_chr_len") -> my_col

    mdb %>%
    dplyr::filter(.[[my_col]] %in% len)
  }


