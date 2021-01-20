
#' Determine pairwise structural mappings
#'
#' @param df a data frame
#'
#' @return description of mappings
#' @export
#'
#' @examples iris %>% determine_mapping
determine_mapping <- function(df){

  df %>%
    dplyr::ungroup(.) -> df1

  suppressMessages(dataValidation::drop_useless_cols(df1)) -> df1

  if(ncol(df1) < 2){
    rlang::abort("insufficient non-unique columns")
  }

  cnf_output <- list()

  for(i in 2:ncol(df1)){
    for(j in 1:(i-1)){

      nm1 <- rlang::sym(names(df1)[i])
      nm2 <- rlang::sym(names(df1)[j])

      cnf_output <- append(cnf_output, utils::capture.output(suppressMessages(dataValidation::confirm_mapping(df1, !!nm1, !!nm2, view = F))) )
    }

  }
  cnf_output %>% unlist

}
