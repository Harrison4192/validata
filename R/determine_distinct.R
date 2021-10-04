
#' Names List
#'
#' @param df a df
#' @param len how many elements in combination
#'
#' @return a list of name combinations
#' @keywords internal
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
#' * For completely unique columns, they are recorded in level 1, but then dropped from the data frame to facilitate
#' the determination of multi-column primary keys.
#' * If the dataset contains duplicated rows, they are eliminated before proceeding.
#'
#' @param df a data frame
#' @param ... columns or a tidyselect specification. defaults to everything
#' @param listviewer logical. defaults to TRUE to view output using the listviewer package
#'
#' @return list
#' @export
#'
#' @examples
#'
#' sample_data1 %>%
#' head
#'
#'
#'## on level 1, each column is tested as a unique identifier. the VAL columns have no
#'## duplicates and hence qualify, even though they normally would be considered as IDs
#'## on level 3, combinations of 3 columns are tested. implying that ID_COL 1,2,3 form a unique key
#'## level 2 does not appear, implying that combinations of any 2 ID_COLs do not form a unique key
#'
#' sample_data1 %>%
#' determine_distinct(listviewer = FALSE)
determine_distinct <- function(df, ..., listviewer = TRUE){

  n_dupes(df) -> d_rows

  if(d_rows > 0) {
    print(stringr::str_glue("database has {d_rows} duplicate rows, and will eliminate them"))
    df <- dplyr::distinct(df)}


  get_unique_col_names(df) -> unique_names

  df %>% framecleaner::select_otherwise(..., otherwise = tidyselect::everything(), return_type = "names") %>% setdiff(unique_names) -> db_names

  df %>% dplyr::select(-tidyselect::any_of(unique_names)) -> df

  make_distincts(df, tidyselect::any_of(db_names)) -> dst_list

  distinct_combos <- list()
  new_list <- list()

  for(j in seq_along(dst_list)){

    stringr::str_c("LEVEL ", j) -> col_nm

    dst_list %>%
      purrr::pluck(j) -> the_lev



    filter_list(smaller_list = distinct_combos,
                bigger_list = data.table::transpose(the_lev)) %>%
      data.table::transpose() -> the_lev


utils::capture.output(
      the_lev %>%
        purrr::pmap_lgl(., ~confirm_distinct(df, ...)) -> dst_nms)


    the_lev %>%
      as.data.frame() %>%
      dplyr::filter(dst_nms)  -> d1

d1 %>%
  rows_to_list() -> l1


l1 %>%
  append(distinct_combos) -> distinct_combos


if(nrow(d1) != 0){
      d1 %>%
      tidyr::unite(col = !!col_nm, sep = ", ") %>%
      append(new_list) -> new_list}


  }

  new_list[["LEVEL 1"]] <- as.list(unique_names)
  new_list %>%
    purrr::map(~if(rlang::is_empty(.)) {. <- 'no primary keys'} else{.}) -> output

  if(listviewer){

    output %>%
      listviewer::jsonedit(.)
  } else{

    output
  }



}


pivot_summary <- function(sumr, ...){

  column <- rowname <- NULL

  if (!missing(..1)) {
    sumr %>%
      tidyr::unite(col = "column", ..., remove = T) %>% dplyr::relocate(column) -> sumr1

    sumr1 %>%
      dplyr::select(-1) %>% as.matrix() %>% mode -> output_mode

  }
  else{
    sumr -> sumr1
  }

  sumr1 %>%
    t %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    tibble::as_tibble() %>%
    dplyr::rename(column = rowname) %>%
    dplyr::arrange(column) -> sumr2

  if (!missing(..1)) {
    sumr2 %>%
      janitor::row_to_names(row_number = 1) %>%
      dplyr::mutate(dplyr::across(-1, ~as(., output_mode)))-> sumr3
  }
  else{
    sumr2 -> sumr3
  }

  sumr3

}


get_unique_col_names <- function(df){
  nrow(df) -> rws
  V1 <- column <- NULL

  df %>%
    dplyr::summarize(dplyr::across(.fns = ~dplyr::n_distinct(.) == rws)) %>%
    pivot_summary() %>%
    dplyr::filter(V1) %>%
    dplyr::pull(column)
}

rows_to_list <- function(df){
  df %>% t() %>% as.data.frame() %>% lapply(unlist)}

is_subset_list <- function(chr, chr_list){

  any(purrr::map_lgl(chr_list, ~all(is.element(el = ., set = chr))))
}

filter_list <- function(smaller_list, bigger_list){

  bigger_list %>% purrr::map_lgl(~is_subset_list(chr = ., chr_list = smaller_list)) -> logical_vec
  purrr::discard(bigger_list, logical_vec)

}
