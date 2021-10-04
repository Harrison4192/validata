

#' diagnose
#'
#' this function is inspired by the excellent `dlookr` package. It takes a dataframe and returns
#' a summary of unique and missing values of the columns.
#'
#' @param df dataframe
#' @param ... tidyselect
#' @importFrom framecleaner select_otherwise
#'
#' @return dataframe summary
#' @export
#'
#' @examples iris %>% diagnose()
diagnose <- function(df, ...) {

  df <- select_otherwise(df, ..., otherwise = tidyselect::everything(), return_type = "df")

  vars <- names(df)

  variable_type <- purrr::map_chr(df, ~class(.)[1])

  missing_count <- purrr::map_int(df, count_missing)

  unique_count <- purrr::map_int(df, dplyr::n_distinct)

  data_count <- nrow(df)

  if(data_count == 0){

    return(print("data frame is empty") )
  }

  tibble::tibble(variables = vars, types = variable_type,
         missing_count = missing_count,
         missing_percent = missing_count / data_count * 100,
         unique_count = unique_count,
         unique_rate = unique_count / data_count)
}


count_missing <- function(x){

  sum(is.na(x))
}

#' diagnose_missing
#'
#' faster than diagnose if emphasis is on diagnosing missing values. Also, only shows the columns with
#' any missing values.
#'
#' @param df dataframe
#' @param ... optional tidyselect
#'
#' @return tibble summary
#' @export
#'
#' @examples
#'
#' iris %>%
#' framecleaner::make_na(Species, vec = "setosa") %>%
#' diagnose_missing()
diagnose_missing <- function(df, ...){

  missings <- NULL

  nrow(df) -> total_rows

  df <- select_otherwise(df,
                     ...,
                     otherwise = tidyselect::everything(),
                     return_type = "df")

  missing_count <-  purrr::map_df(df, count_missing)

  missing_count %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    tibble::as_tibble() %>%
    rlang::set_names(c("column", "missings")) %>%
    dplyr::arrange(dplyr::desc(missings )) %>%
    dplyr::filter(missings > 0) %>%
    dplyr::mutate(missing_ratio = missings / total_rows) -> missing_count

  missing_count %>%
    unlist() %>%
    rlang::is_empty() -> misscond

if(misscond){
  print("no missings")} else{
    missing_count
  }
}

#' view_missing
#'
#' View rows of the dataframe where columns in the tidyselect specification contain missings
#' by default, detects missings in any column. The result is by default displayed in the viewer pane.
#' Can be returned as a tibble optionally.
#'
#' @param df dataframe
#' @param ... tidyselect
#' @param view logical. if false, returns tibble
#'
#' @return tibble
#' @export
#'
#' @examples
#'
#' iris %>%
#' framecleaner::make_na(Species, vec = "setosa") %>%
#' view_missing(view = FALSE)
view_missing <- function(df, ..., view = TRUE){

  df %>% select_otherwise(..., otherwise = tidyselect::everything()) -> col_indx

  df %>%
    dplyr::filter(dplyr::if_any(tidyselect::any_of(col_indx), .fns = is.na)) -> missings

  if(view){
    utils::View(missings)
  } else{
    missings
  }
}



#' diagnose category
#'
#' counts the distinct entries of categorical variables. The `max_distinct` argument limits the scope to
#' categorical variables with a maximum number of unique entries, to prevent overflow.
#'
#' @param .data dataframe
#' @param ... tidyselect
#' @param max_distinct integer
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
#' iris %>%
#' diagnose_category()
diagnose_category <- function(.data, ..., max_distinct = 5){
  n <-  NULL

  nrow(.data) -> total_rows

  .data %>%
    purrr::map_int(dplyr::n_distinct) %>%
    subset(. < max_distinct) %>%
    names() -> nms

  .data %>%
    select_otherwise(..., otherwise = where(is.character) | where(is.factor), return_type = "names") -> nms1

  intersect(nms, nms1) -> nms2

  purrr::map(nms2,
             function(x) {.data %>%
                 dplyr::count(!!rlang::sym(x)) %>%
                 dplyr::mutate(column = names(.)[1], .before = 1) %>%
                 dplyr::rename(level = 2) %>%
                 dplyr::arrange(dplyr::desc(n))}) %>%
    rlist::list.rbind() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(ratio = n / total_rows)
}

#' data_mode
#'
#' @param x vector
#' @param prop show frequency as ratio? default T
#'
#' @return named double of length 1
#' @keywords internal
#'
data_mode <- function(x, prop = TRUE){

  x %>%
    table() -> xt

  xt[which.max(xt)] -> xt_mode

  if(prop){

    xt_mode / length(x) -> xt_mode

  }

  xt_mode

}

#' diagnose_numeric
#'
#' Inputs a dataframe and returns various summary statistics of the numeric columns. For example `zeros` returns the number
#' of 0 values in that column. `minus` counts negative values and `infs` counts Inf values. Other rarer metrics
#' are also returned that may be helpful for quick diagnosis or understanding of numeric data. `mode` returns the most common
#' value in the column (chooses at random in case of tie) , and `mode_ratio` returns its frequency as a ratio of the total rows
#'
#' @param .data dataframe
#' @param ... tidyselect
#'
#' @return dataframe
#' @export
#'
#'
#' @examples
#'
#' library(framecleaner)
#'
#' iris %>%
#' diagnose_numeric
diagnose_numeric <- function(.data, ...){

  .data %>%
    select_otherwise(..., where(is.numeric), return_type = "df") -> df

fns <-   list(zeros = ~sum(. == 0, na.rm = T),
       minus = ~sum(. < 0, na.rm = T),
       infs = ~sum(is.infinite(.), na.rm = T),
       min = ~min(., na.rm = T),
       mean = ~mean(., na.rm = T),
       max = ~max(., na.rm = T),
       `|x|<1 (ratio)` = ~mean( -1 < . & . < 1, na.rm =T)  ,
       integer_ratio = ~mean(as.integer(.) == ., na.rm =T),
       mode = ~as.double(names(data_mode(.))),
       mode_ratio = data_mode)


col_list <- list()

   for(fun in seq_along(fns)){
     purrr::map_dbl(df, fns[[fun]]) %>%
       tibble::enframe(name = NULL, value  = names(fns[fun])) -> alist

    rlist::list.append(col_list, alist) -> col_list

   }

message(stringr::str_c(nrow(.data), " rows"))


tibble::tibble(variables = names(df)) %>%
  dplyr::bind_cols(
    rlist::list.cbind(col_list)
  ) %>%
  framecleaner::set_int()


}

