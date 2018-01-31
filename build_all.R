#! /usr/bin/env Rscript
# Run this script before git pushes, so everything is neat and tidy

devtools::build()
rmarkdown::render("README.Rmd")
file.remove("README.html")
file.remove("tests/testthat/Rplots.pdf")
pkgdown::build_site()
