library(pacman)
p_load(rstudioapi, devtools, roxygen2, usethis, pkgdown, ymlthis, magrittr, fs, covr)

file_name  <- rstudioapi::getSourceEditorContext()$path %>% fs::path_file()
use_build_ignore(file_name)

file_name %>% unclass

usethis::use_github_action("check-standard")
usethis::use_github_action("test-coverage")
usethis::use_github_action("render-rmarkdown")
usethis::use_github_action("pkgdown")
use_news_md()
devtools::build_readme()
document()
build_site()
check()




