
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tadaatoolbox

[![Build
Status](https://travis-ci.org/tadaadata/tadaatoolbox.svg)](https://travis-ci.org/tadaadata/tadaatoolbox)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version-ago/tadaatoolbox)](https://cran.r-project.org/package=tadaatoolbox)
[![CRAN\_Downloads\_Badge](https://cranlogs.r-pkg.org/badges/tadaatoolbox)](https://cran.r-project.org/package=tadaatoolbox)
[![Rdoc](http://www.rdocumentation.org/badges/version/tadaatoolbox)](http://www.rdocumentation.org/packages/tadaatoolbox)
[![Coverage
status](https://codecov.io/gh/tadaadata/tadaatoolbox/branch/master/graph/badge.svg)](https://codecov.io/github/tadaadata/tadaatoolbox?branch=master)
[![DOI](https://zenodo.org/badge/48081989.svg)](https://zenodo.org/badge/latestdoi/48081989)

This is **tadaatoolbox** version `0.16.1`
([semver](http://semver.org/)(ish)).<br /> It contains helpers for data
analysis and presentation focused on undergrad psychology, the target
audience being students at University of Bremen.

Please consider this as a teaching project and be careful if you intend
to use it for production use.<br /> To see what’s new, see
[NEWS.md](https://github.com/tadaadata/tadaatoolbox/blob/master/NEWS.md).  
Also, since this project is still in the `0.x.y` version, you should
expect the API to change at any time. Once we reach `1.0.0`, API changes
will be rolled out more slowly and include depracation warnings. In the
meantime, the package is available on CRAN primarily for convenience,
because its target audience is likely not familiar with GitHub or
`devtools`.

# Installation

Install the current development version from GitHub (recommended):

    if (!("remotes" %in% installed.packages())){
      install.packages("remotes")
    }
    
    remotes::install_github("tadaadata/tadaatoolbox")

Or install the most recent stable version from CRAN:

    install.packages("tadaatoolbox")

# Contribution

Please do\! I have no idea where I’m going with this. Feel free to [open
issues](https://github.com/tadaadata/tadaatoolbox/issues).

## Contributers

| Contributer    | Role                |
| :------------- | :------------------ |
| Lukas Burk     | Author & Maintainer |
| Tobias Anton   | Author              |
| Daniel Lüdecke | Contributor         |
| Gesa Graf      | Contributor         |

# Functionality

See the included vignette for a demonstration:
`browseVignettes("tadaatoolbox")` or view [the `pkgdown`ed version of
this repo](http://tadaatoolbox.tadaa-data.de/) for an overview and
function index.  
Here’s a few highlights:

## Tidy (as in pretty) test output

See all of them in action [in this
vignette](http://tadaatoolbox.tadaa-data.de/articles/test_output.html)

  - `tadaa_aov`: For 3 flavours of `aov` with included partial
    \(\eta^2\), Cohen’s f and power.
  - `tadaa_kruskal`: A wrapper for `kruskal.test` tidied and optionally
    `pixiedust`ed.
  - `tadaa_t.test`: For `t.test` with automatic homogenity of variance
    detection, effect size and power.
  - `tadaa_wilcoxon`: A wrapper for `wilcox.test` tidied and optionally
    `pixiedust`ed.
  - `tadaa_levene`: Wrapper for `car::leveneTest` tidied and optionally
    `pixiedust`ed.
  - `tadaa_normtest` lets you do tests for normality (4 methods) over
    multiple variables.
  - `tadaa_pairwise_t`, `tadaa_pairwise_gh` and `tadaa_pairwise_tukey`
    for various pairwise procedures.

## Helpers for plots

  - `tadaa_int`: Simple interaction plot template.
  - `tadaa_balance`: Check equality of group sizes.
  - `tadaa_mean_ci`: Plots means with 95% confidence intervals as
    errorbars.
  - `tadaa_plot_tukey`: For pretty `TukeyHSD` visualization.

# Dependencies

I rely on these awesome package for all the things this package does, so
you might want to consider checking them out.

  - **broom**
  - **car**
  - **DescTools**
  - **ggplot2**
  - **magrittr**
  - **methods**
  - **nortest**
  - **pixiedust**
  - **pwr**
  - **stats**
  - **viridis**

# Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.
