
#' Determine pairwise structural mappings
#'
#' @param df a data frame
#' @param ... columns or a tidyselect specification
#' @param listviewer logical. defaults to TRUE to view output using the listviewer package

#'
#' @return description of mappings
#' @export
#'
#' @examples
#'
#' iris %>%
#' determine_mapping(listviewer = FALSE)
determine_mapping <- function(df, ..., listviewer = TRUE){


  df %>%
    framecleaner::select_otherwise(..., otherwise = tidyselect::everything(), return_type = "df") %>%
    dplyr::ungroup(.) -> df1

  if(ncol(df1) < 2){
    rlang::abort("insufficient non-unique columns")
  }

  cnf_output <- list()

  for(i in 2:ncol(df1)){
    for(j in 1:(i-1)){

      nm1 <- rlang::sym(names(df1)[i])
      nm2 <- rlang::sym(names(df1)[j])

      cnf_output <- append(cnf_output, utils::capture.output(suppressMessages(confirm_mapping(df1, !!nm1, !!nm2, view = F))) )
    }

  }

  cnf_output %>% unlist -> cnf_output1

  list_output <- list()

  list_output[["1 - 1 mapping"]] <- stringr::str_subset(cnf_output1, "1 - 1 mapping") %>% stringr::str_remove(., "1 - 1 mapping between ")
  list_output[["1 - many mapping"]] <- stringr::str_subset(cnf_output1, "1 - many mapping") %>% stringr::str_remove(., "1 - many mapping between ")
  list_output[["many - 1 mapping"]] <- stringr::str_subset(cnf_output1, "many - 1 mapping") %>% stringr::str_remove(., "many - 1 mapping between ")
  list_output[["many - many mapping"]] <- stringr::str_subset(cnf_output1, "many - many mapping") %>% stringr::str_remove(., "many - many mapping between ")

if(listviewer){
listviewer::jsonedit(list_output)} else {
  list_output
}

}
