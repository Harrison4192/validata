#' confirm overlap - find entries only in df 1
#'
#'
#' @rdname confirm_overlap
#' @param co_output dataframe output from confirm_overlap
#'
#' @export
co_find_only_in_1 <- function(co_output){

  names(co_output) -> nms

  co_output %>%
    dplyr::filter(.[[nms[3]]] == 0 & .[[nms[2]]] == 1) %>%
    dplyr::select("{nms[2]}" := 1)
}

#' confirm overlap - find entries only in df 2
#'
#'
#' @rdname confirm_overlap
#' @param co_output dataframe output from confirm_overlap
#'
#' @export
co_find_only_in_2 <- function(co_output){

  names(co_output) -> nms

  co_output %>%
    dplyr::filter(.[[nms[3]]] == 1 & .[[nms[2]]] == 0) %>%
    dplyr::select("{nms[3]}" := 1)
}

#' confirm overlap - find entries in both dfs
#'
#'
#' @rdname confirm_overlap
#' @param co_output dataframe output from confirm_overlap
#'
#' @export
co_find_in_both <- function(co_output){

  both_flags <- NULL

  co_output %>%
    dplyr::filter(both_flags == 2) %>%
    dplyr::select(1)
}
