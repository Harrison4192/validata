#' Confirm structural mapping between 2 columns
#'
#' The mapping between elements of 2 columns can have 4 different relationships: one - one, one - many, many - one, many - many.
#' This function returns a view of the mappings by row, and prints a summary to the console.
#'
#' @param .data a data frame
#' @param col1 column 1
#' @param col2 column 2
#' @param view View results?
#'
#' @return A view of mappings. Also returns the view as a data frame invisibly.
#' @export
#'
#' @examples iris %>% confirm_mapping(Species, Sepal.Width, view = FALSE)
confirm_mapping <- function(.data, col1, col2, view = T){

  dupe_count <- one_to_many <- many_to_one <- NULL
  .data <- dplyr::ungroup(.data)

  .data %>%
    dplyr::distinct({{ col1 }}, {{ col2 }}) %>%
    janitor::get_dupes({{col1}}) %>%
    dplyr::mutate(one_to_many = 1) %>%
    dplyr::arrange({{col1}}, {{col2}}) -> .data1

  .data %>%
    dplyr::distinct({{ col1 }}, {{ col2 }}) %>%
    janitor::get_dupes({{col2}}) %>%
    dplyr::mutate(many_to_one = 1) %>%
    dplyr::arrange({{col2}}, {{col1}})-> .data2

  .data1 %>% nrow %>% `>`(0)-> one2m

  .data2 %>% nrow %>% `>`(0) ->m2one

  mapping_bools <- c(! (one2m | m2one), one2m & !m2one, m2one & !one2m, one2m & m2one)

  switch(which(mapping_bools),
         "1 - 1 mapping",
         "1 - many mapping",
         "many - 1 mapping",
         "many - many mapping") -> mapping_desc

  print(stringr::str_glue("{mapping_desc} between {rlang::as_name(rlang::ensym(col1))} and {rlang::as_name(rlang::ensym(col2))}"))

  .data3 <-
    dplyr::bind_rows(.data1, .data2) %>%
    dplyr::select({{col1}}, {{col2}}, dupe_count, tidyselect::everything()) %>%
    dplyr::mutate(dplyr::across(c(one_to_many, many_to_one), as.logical))



  if(nrow(.data3) > 0 & view == T) {utils::View(.data3)}

  invisible(.data3)
}
