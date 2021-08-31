#' n_dupes
#'
#' @param x a df
#'
#' @return an integer; number of dupe rows
#' @keywords internal
#'
n_dupes <- function(x){(x %>% nrow) - (dplyr::distinct(x) %>% nrow) -> dupes ;dupes}



#' Confirm Distinct
#'
#' Confirm whether the rows of a data frame can be uniquely identified by the keys in the selected columns.
#' Also reports whether the dataframe has duplicates. If so, it is best to remove duplicates and re-run the function.
#'
#' @param .data A dataframe
#' @param ... (ID) columns
#'
#' @return a Logical value invisibly with description printed to console
#' @export
#'
#' @examples iris %>% confirm_distinct(Species, Sepal.Width)
confirm_distinct <- function(.data, ...) {

  .data %>%
    dplyr::ungroup() %>%
    select_otherwise(..., return_type = "df") -> .data1

  .data1 %>% names() %>% rlang::syms(.) -> cols

  n_dupes(.data) -> d_rows

  if(d_rows > 0) {
    print(stringr::str_glue("database has {d_rows} duplicate rows"))
    .data <- dplyr::distinct(.data)}

  .data1 %>% dplyr::distinct(.) -> new_df


  nrow(new_df) -> new_rows

  names(new_df) %>% stringr::str_c( collapse = ", ") -> col_names

  diff <- nrow(.data) - new_rows

  if(diff == 0){

    print(stringr::str_glue("database is distinct at {col_names}"))
    invisible(TRUE)
  }
  else {

    print(stringr::str_glue("database has {diff} duplicates at {col_names}"))
    invisible(FALSE)


  }

}
