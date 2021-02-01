
#' Confirm Overlap
#'
#' A venn style summary of the overlap in unique values of 2 vectors
#'
#' @param vec1 vector 1
#' @param vec2 vector 2
#'
#' @return prints overlap summary and invisibly returns data frame with overlap data
#' @export
#'
#' @examples confirm_overlap(iris$Sepal.Width, iris$Sepal.Length)
confirm_overlap <- function(vec1, vec2){

  x <- flag2 <- flag1 <- both_flags <- NULL


  rlang::enexpr(vec1) %>% deparse %>% stringr::str_replace(stringr::fixed("$"), "_") -> str_col1
  rlang::enexpr(vec2) %>% deparse %>% stringr::str_replace(stringr::fixed("$"), "_") -> str_col2
  stringr::str_glue("only_in_{str_col1}") -> nm_col1
  stringr::str_glue("only_in_{str_col2}") -> nm_col2

  stopifnot(typeof(vec1) == typeof(vec2) )

  list(str_col1, nm_col1)

  db1 <- tibble::tibble(x = vec1)
  db2 <- tibble::tibble(x = vec2)


  db1 %>% dplyr::distinct(x) %>% dplyr::filter(!is.na(x)) %>% dplyr::mutate(flag1 = 1) -> db1
  db2 %>% dplyr::distinct(x) %>% dplyr::filter(!is.na(x)) %>% dplyr::mutate(flag2 = 1) -> db2

  suppressMessages({dplyr::full_join(db1, db2) -> jdb})

  jdb %>%
    tidyr::replace_na(list(
      flag1 = 0,
      flag2 = 0)) %>%
    dplyr::mutate(both_flags = flag1 + flag2) -> jdb

  jdb %>%
    dplyr::summarize(
      !!nm_col1 := sum(flag1 == 1 & flag2 == 0),
      !!nm_col2 := sum(flag1 == 0 & flag2 == 1),
      shared_names = sum(both_flags == 2),
      total_names = jdb %>% nrow) -> jdb_sum
  print(jdb_sum)


  shared_pct <- (jdb_sum$shared_names / jdb_sum$total_names * 100) %>% round

  print(stringr::str_glue("The columns share {shared_pct}% of the total names"))

  jdb %>% invisible

}


#' Confirm Overlap internal
#'
#' A venn style summary of the overlap in unique values of 2 vectors
#'
#' @param vec1 vector 1
#' @param vec2 vector 2
#'
#' @return 1 row tibble
#' @keywords internal
#'
#' @examples confirm_overlap(iris$Sepal.Width, iris$Sepal.Length)
confirm_overlap_internal <- function(vec1, vec2){

  x <- flag2 <- flag1 <- both_flags <- NULL


  rlang::enexpr(vec1) %>% deparse %>% stringr::str_replace(stringr::fixed("$"), "_") -> str_col1
  rlang::enexpr(vec2) %>% deparse %>% stringr::str_replace(stringr::fixed("$"), "_") -> str_col2
  stringr::str_glue("only_in_{str_col1}") -> nm_col1
  stringr::str_glue("only_in_{str_col2}") -> nm_col2

  # stopifnot(typeof(vec1) == typeof(vec2) )

  list(str_col1, nm_col1)

  db1 <- tibble::tibble(x = vec1)
  db2 <- tibble::tibble(x = vec2)


  db1 %>% dplyr::distinct(x) %>% dplyr::filter(!is.na(x)) %>% dplyr::mutate(flag1 = 1) -> db1
  db2 %>% dplyr::distinct(x) %>% dplyr::filter(!is.na(x)) %>% dplyr::mutate(flag2 = 1) -> db2

  suppressMessages({dplyr::full_join(db1, db2) -> jdb})

  jdb %>%
    tidyr::replace_na(list(
      flag1 = 0,
      flag2 = 0)) %>%
    dplyr::mutate(both_flags = flag1 + flag2) -> jdb

  jdb %>%
    dplyr::summarize(
      !!nm_col1 := sum(flag1 == 1 & flag2 == 0),
      !!nm_col2 := sum(flag1 == 0 & flag2 == 1),
      shared_names = sum(both_flags == 2),
      total_names = jdb %>% nrow) -> jdb_sum

  shared_pct <- (jdb_sum$shared_names / jdb_sum$total_names * 100) %>% round

  jdb_sum %>%
    dplyr::mutate(shared_pct_names = stringr::str_c(shared_pct, "%")) -> jdb_sum1

  jdb_sum1


}







