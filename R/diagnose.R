

#' diagnose
#'
#' this function is lifted from the excellent `dlookr` package. I have reproduced it here
#' because I consider it an essential validation function, but the `dlookr` package itself has
#' some dependencies which can be problematic for some machines to download.
#'
#' @param df dataframe
#' @param ... tidyselect
#'
#' @return dataframe summary
#' @export
#'
#' @examples iris %>% diagnose()
diagnose <- function(df, ...) {

  vars <- tidyselect::vars_select(names(df), !!!rlang::quos(...))

  if (length(vars) == 0) vars <- names(df)

  variable_type <- sapply(df, class)

  missing_count <- sapply(vars,
                          function(x) sum(!stats::complete.cases(df[, x])))
  unique_count <- sapply(vars,
                         function(x) dplyr::n_distinct(df[, x]))
  data_count <- nrow(df)

  tibble::tibble(variables = vars, types = variable_type,
         missing_count = missing_count,
         missing_percent = missing_count / data_count * 100,
         unique_count = unique_count,
         unique_rate = unique_count / data_count)
}




