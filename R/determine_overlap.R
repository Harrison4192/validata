#' Determine Overlap
#'
#' Uses \code{confirm_overlap} in a pairise fashion to see venn style comparison of unique values between
#' the columns chosen by a tidyselect specification.
#'
#' @param db a data frame
#' @param ... tidyselect specification. Default being everything.
#'
#' @return tibble
#' @export
#'
#' @examples
#'
#' iris %>%
#' determine_overlap()
#'
determine_overlap <- function(db, ...) {
  db %>%
    framecleaner::select_otherwise(..., otherwise = tidyselect::everything(), return_type = "df") -> db1

  names_list(db1, 2) -> db_names_list

  for (i in 1:length(db_names_list$V1)) {

    testit::has_error(confirm_overlap_internal(db[[db_names_list$V1[i]]], db[[db_names_list$V2[i]]]),
                      silent = T) -> fails_type_check

    if(fails_type_check) {next}
    else{

    tibble::tibble(col1 = db_names_list$V1[i], col2 = db_names_list$V2[i]) %>%
      dplyr::bind_cols(confirm_overlap_internal(db[[db_names_list$V1[i]]], db[[db_names_list$V2[i]]]) %>%
                  rlang::set_names(
                    c(
                      "names_only_col_1",
                      "names_only_col_2",
                      "shared_names",
                      "total_names",
                      "pct_shared_names"
                    )
                  )) -> tib_row

    if (!exists("tib1")) {
      tib_row -> tib1
    } else{
      tib1 %>%
        dplyr::bind_rows(tib_row) -> tib1

    }

  }
}

  tib1
}
