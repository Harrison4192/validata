

#' mode with %
#'
#' returns the mode of a vector with what percent of the data is the mode
#'
#' @param x a vector
#'
#' @return a unit character vector
#' @export
#'
#' @examples
#' c("b", "b", letters) %>% mode_pct()
mode_pct <- function(x) {
  mode_fn(x) -> mode1

  calc_pct(x, mode1)

}

#' statistical mode
#'
#' returns the mode of a vector
#'
#' @param x a vector
#'
#' @return a unit vector
#' @export
#'
#' @examples
#' c("b", "b", letters) %>% mode_fn()
mode_fn <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' top n vals
#'
#' @param x vector
#' @param top_n integer to specify top n modes
#'
#' @return character unit vector
#' @export
#'
#' @examples
#' tibble::tibble(x = 1:10 %>% c(10,10,10,5,5)) -> t1
#' t1 %>% top_n_vals()

top_n_vals <- function(x, top_n = 3){

  tibble::tibble(x = x) -> t1

  t1 %>%
    dplyr::add_count(x) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::distinct(n, .keep_all = T) %>%
    dplyr::slice(1:top_n) %>%
    dplyr::pull(x) -> x1
  purrr::map_chr(x1, .f = ~calc_pct(vec = t1$x, value = .)) %>%
    stringr::str_c(collapse = ", ")
}




calc_pct <- function(vec, value ){
  mean(vec == value, na.rm = T) -> avg1
  (avg1 * 100) %>% round() %>% str_c("%") -> pct

  stringr::str_c(value, " ", "(", pct, ")")
}

calc_pct1 <- function(vec, num, avg1 ){

  mean(cond, na.rm = T) -> avg1
  sum(cond, na.rm = T) -> sum1
  (avg1 * 100) %>% round() %>% str_c("%") -> pct

  stringr::str_c(num, " ", "(", pct, ")")
}



