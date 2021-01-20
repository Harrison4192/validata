#' Automatically determine primary key
#'
#' Uses \code{confirm_distinct} in an iterative fashion to determine the primary key.
#'
#' @param .data a data frame
#'
#' @return none
#' @export
#'
#' @examples iris %>% determine_distinct
determine_distinct <- function(.data){

  unique_count <- unique_rate <- variables <- NULL


  .data %>%
    dlookr::diagnose(.) -> d1

  d1 %>%
    dplyr::arrange(unique_count) %>%
    dplyr::pull(variables) -> var_order

  d1 %>%
    dplyr::filter(unique_rate == 1) -> dx

  if(nrow(dx) > 0){

    dx %>% dplyr::pull(variables) %>% stringr::str_flatten( collapse = ", ") -> unqnms
    warning(stringr::str_glue("The following columns are unique row identifiers: {unqnms} \n
                     you may want to remove these and try again"), call. = F )
  }

  .data %>% dplyr::relocate(tidyselect::all_of(var_order)) -> .data

  .data %>% names %>% rlang::syms(.) -> cols1

  length(cols1) -> ln

  nulls <- NULL


  extra_cols <- list()

  for(i in 1:ln) {
    cols2 <- cols1

    cols2[c(nulls, i)] <-  NULL



    print(stringr::str_glue("removing {rlang::as_name(cols1[[i]])}"))


    dataValidation::confirm_distinct(.data, !!!cols2) -> diff


    if(diff == 0) {

      append(extra_cols, cols1[[i]]) -> extra_cols
      nulls <- nulls %>% c(i)

    }


  }


  extra_cols <- extra_cols %>% purrr::map(~rlang::as_string(.)) %>% unlist
  extra_cols2 <- extra_cols  %>% stringr::str_c(collapse = ", ")
  cols1 <- cols1 %>% purrr::map(~rlang::as_string(.)) %>% unlist
  needed_cols <- setdiff(cols1, extra_cols) %>% stringr::str_c(collapse = ", ")

  print(stringr::str_glue("\n \n  "))

  print(stringr::str_glue("Necessary column keys are {needed_cols}"))

  print(stringr::str_glue("\n \n  "))

  print(stringr::str_glue("not keys: {extra_cols2}"))


}
