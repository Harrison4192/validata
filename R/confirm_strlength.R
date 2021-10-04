#' confirm string length
#'
#' returns a count table of string lengths for a character column. The helper function `choose_strlen`
#' filters dataframe for rows containing specific string length for the specified column.
#'
#' @param mdb dataframe
#' @param col unquoted column
#'
#' @return prints a summary and returns a dataframe invisibly
#' @export
#'
#' @examples
#'
#' iris %>%
#' tibble::as_tibble() %>%
#' confirm_strlen(Species) -> iris_cs_output
#'
#' iris_cs_output
#'
#' iris_cs_output %>%
#' choose_strlen(6)
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
#'
#' @rdname confirm_strlen
#' @param cs_output dataframe. output from `confirm_strlen`
#' @param len integer vector.
#'
#' @return dataframe with original columns, filtered to the specific string length
#' @export
#'
choose_strlen <- function(cs_output, len) {

  cs_output %>% names() %>% stringr::str_subset("_chr_len") -> my_col

  cs_output %>%
    dplyr::filter(.[[my_col]] %in% len)
  }


