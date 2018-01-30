#! /usr/bin/env Rscript

devtools::build()
rmarkdown::render("README.Rmd")
file.remove("README.html")
pkgdown::build_site()
