
# load developer libraries ------------------------------------------------
xfun::gsub_dir(dir = "vignettes", pattern = "cardinality", replacement = "mapping")



library(pacman)
p_load(rstudioapi, devtools, roxygen2, usethis, pkgdown,
       ymlthis, magrittr, fs, covr, gitcreds, credentials,
       badger, hexSticker, gh, xfun)

# install.packages("devtools")
devtools::install_github("Harrison4192/presenter")


# add this file to .Rbuildignore ------------------------------------------


file_name  <- rstudioapi::getSourceEditorContext()$path %>% fs::path_file()
use_build_ignore(file_name)



# begin pkgdown -----------------------------------------------------------

usethis::use_pkgdown()

# create yaml -------------------------------------------------------------

ymlthis::pkgdown_template() %>%
  ymlthis::use_pkgdown_yml()

# website design ----------------------------------------------------------

# bslib::bootswatch_themes()

# usethis: add packages ---------------------------------------------------

usethis::use_pipe()

usethis::use_package("utils")
usethis::use_package("dplyr")
usethis::use_package("stringr")
usethis::use_package("tidyselect")
usethis::use_package("purrr")
usethis::use_package("janitor")
usethis::use_package("tibble")
usethis::use_package("frameCleaneR")
usethis::use_package("BBmisc")

usethis::use_package("badger", type = "Suggests")
usethis::use_package("testit", type = "Suggests")
usethis::use_package("lemon", type = "Suggests")

usethis::use_r("possibly_id_col")
usethis::use_r("select_otherwise")


usethis::use_r("mode")
usethis::use_vignette("validata")
# edit R profile ----------------------------------------------------------


edit_r_profile()

remotes::install_github()
# add rmd sections with usethis -------------------------------------------

use_readme_rmd()
use_news_md()
usethis::use_mit_license()



# add badges to readme ----------------------------------------------------

use_lifecycle_badge("experimental")
use_cran_badge()
use_github_actions_badge()
# `r badger::badge_cran_download("dataCleaner", "grand-total", "blue")`
# `r badger::badge_code_size("Harrison4192/dataCleaner")`
# `r badger::badge_last_commit("Harrison4192/dataCleaner")`

# set github token --------------------------------------------------------

gh_token_help()
git_credential_update()
create_github_token()
gitcreds_set()
gitcreds_get()
set_github_pat()
credentials::git_credential_forget()
gh::gh_whoami()
gh_token()
git_sitrep()
use_github_release()
credentials::credential_helper_get()
git_credential_ask()
# git config --global credential.helper osxkeychain
# use github actions and links --------------------------------------------



usethis::use_github_action("check-standard")
usethis::use_github_action("test-coverage")
usethis::use_github_action("render-rmarkdown")
usethis::use_github_action("pkgdown")
usethis::use_github_actions()
usethis::use_github_links()
usethis::use_github_pages()

# build and check ---------------------------------------------------------

devtools::document()
hdevtools::build_readme()
devtools::build_site()
devtools::check()
 preview_site()
load_all()
devtools::build_vignettes()

usethis::use_release_issue(version = 0.10)

devtools::spell_check()
devtools::release(check = T)

usethis::use_cran_comments(open = rlang::is_interactive())
devtools::check_win_devel()
devtools::check_rhub()

