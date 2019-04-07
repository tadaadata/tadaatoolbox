#! /usr/bin/env Rscript
# Run this script before git pushes, so everything is neat and tidy

desc::desc_set(Date = as.character(lubridate::today()))
desc::desc_set_version(version = "0.16.1.9000")
usethis::use_tidy_style(strict = TRUE)
usethis::use_tidy_description()
devtools::document(roclets = c('rd', 'collate', 'namespace'))
devtools::build_vignettes(clean = TRUE)
devtools::build()
devtools::check(cran = TRUE)
rmarkdown::render("README.Rmd"); file.remove("README.html")
