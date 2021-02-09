
#' Names List
#'
#' @param df a df
#' @param len how many elements in combination
#'
#' @return a list of name combinations
#' @keywords internal
#' @export
#'
names_list <- function(df, len){

  df %>%
    names %>%
    gtools::combinations(n = length(.), r = len, v = . ) %>%
    as.data.frame() %>%
    as.list()
}

#' Make Distinct
#'
#' @param df a df
#' @param ... cols
#'
#' @return a list of name lists
#' @keywords internal
#' @export
#'
make_distincts <- function(df, ...){

  df %>%
    dplyr::select(...) -> id_cols

  nms_list <- list()

  for(i in seq_along(id_cols)) {

    nms_list %>% append(list(names_list(id_cols, i))) -> nms_list

  }

  nms_list
}




#' Automatically determine primary key
#'
#' Uses \code{confirm_distinct} in an iterative fashion to determine the primary keys.
#'
#' The goal of this function is to automatically determine which columns uniquely identify the rows of a dataframe.
#' The output is a printed description of the combination of columns that form unique identifiers at each level.
#' At level 1, the function tests if individual columns are primary keys
#' At level 2, the function tests n C 2 combinations of columns to see if they form primary keys.
#' The final level is testing all columns at once.
#'
#' @param df a data frame
#' @param ... columns or a tidyselect specification
#'
#' @return none
#' @export
determine_distinct <- function(df, ...){



    valiData::make_distincts(df, ...) -> dst_list



  new_list <- list()

  for(j in seq_along(dst_list)){

    stringr::str_c("LEVEL ", j) -> col_nm

    dst_list %>%
      purrr::pluck(j) -> the_lev

utils::capture.output(
      the_lev %>%
        purrr::pmap_lgl(., ~valiData::confirm_distinct(df, ...)) -> dst_nms)

    the_lev %>%
      tibble::as_tibble() %>%
      dplyr::filter(dst_nms)  %>%
      tidyr::unite(col = !!col_nm, sep = ", ") %>%
      append(new_list) -> new_list

  }

  new_list %>%
    purrr::map(~if(rlang::is_empty(.)) {. <- 'no primary keys'} else{.}) %>%
    listviewer::jsonedit(.)


}

