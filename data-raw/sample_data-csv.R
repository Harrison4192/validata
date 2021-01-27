## code to prepare `sample_data.csv` dataset goes here

usethis::use_data(sample_data.csv, overwrite = TRUE)

seed(1)

tibble::tibble(ID_COL1 = sample(c(4295, 3491, 2648, 9094, 2413), size = 50, replace = T),
       ID_COL2 = sample(c(1122, 1034, 2390, 3421, 8403), size = 50, replace = T),
       ID_COL3 = sample(c(1322, 1014, 2999, 3544, 9901), size = 50, replace = T)) -> t1

t1 %>%
  tidyr::expand(ID_COL1, ID_COL2, ID_COL3 ) -> t2

t2 %>%
  dplyr::mutate(
    VAL1 = rnorm(nrow(t2)),
    VAL2 = rnorm(nrow(t2)),
    VAL3 = rnorm(nrow(t2))
  ) -> t3

t3 %>%
  readr::write_csv("sample_data.csv")
