#' confirm overlap - find entries only in df 1
#'
#' returns a tibble whose column are entries unique to the column in the first df
#'
#' @param mdb dataframe output from confirm_overlap
#'
#' @return dataframe
#' @export
co_find_only_in_1 <- function(mdb){

  names(mdb) -> nms

  mdb %>%
    dplyr::filter(.[[nms[3]]] == 0 & .[[nms[2]]] == 1) %>%
    dplyr::select("{nms[2]}" := 1)
}

#' confirm overlap - find entries only in df 2
#'
#' returns a tibble whose column are entries unique to the column in the second df
#'
#' @param mdb dataframe output from confirm_overlap
#'
#' @return dataframe
#' @export
co_find_only_in_2 <- function(mdb){

  names(mdb) -> nms

  mdb %>%
    dplyr::filter(.[[nms[3]]] == 1 & .[[nms[2]]] == 0) %>%
    dplyr::select("{nms[3]}" := 1)
}

#' confirm overlap - find entries in both dfs
#'
#' returns a tibble whose column are entries shared between both columns
#'
#' @param mdb dataframe output from confirm_overlap
#'
#' @return dataframe
#' @export
co_find_in_both <- function(mdb){

  both_flags <- NULL

  mdb %>%
    dplyr::filter(both_flags == 2) %>%
    dplyr::select(1)
}
