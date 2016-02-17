
<!-- README.md is generated from README.Rmd. Please edit that file -->
tadaatoolbox
============

[![Build Status](https://travis-ci.org/tadaadata/tadaatoolbox.svg)](https://travis-ci.org/tadaadata/tadaatoolbox) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tadaatoolbox)](http://cran.r-project.org/package=tadaatoolbox) [![CRAN\_Downloads\_Badge](http://cranlogs.r-pkg.org/badges/grand-total/tadaatoolbox)](http://cran.r-project.org/package=tadaatoolbox) [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

This is **tadaatoolbox** version `0.9.0` ([semver](http://semver.org/)).<br /> It contains helpers for data analysis and presentation focused on undergrad psychology, the target audience being students at University of Bremen.

Please consider this as a teaching project and be careful if you intend to use it for production use.<br /> To see what's new, see [NEWS.md](https://github.com/tadaadata/tadaatoolbox/blob/master/NEWS.md).

Installation
============

Install the current development version from GitHub (recommended):

    if (!("devtools" %in% installed.packages())){
      install.packages("devtools")
    }

    devtools::install_github("tadaadata/tadaatoolbox")

Or install the most recent stable version from CRAN:

    install.packages("tadaatoolbox")

Contribution
============

Please do! I have no idea where I'm going with this. Feel free to [open issues](https://github.com/tadaadata/tadaatoolbox/issues).

Contributers
------------

| Contributer  | Role      |
|:-------------|:----------|
| Lukas Burk   | aut & cre |
| Tobias Anton | aut       |

Functionality
=============

See the included vignette for a demonstration: `browseVignettes("tadaatoolbox")`

Statistical functions
---------------------

-   `modus`: A simple function to extract the mode of a frequency table
-   `nom_chisqu`: Simple wrapper for `chisq.test` that produces a single value.
-   `nom_phi`: Simple wrapper for `vcd::assocstats` to extract phi.
-   `nom_v`: Simple wrapper for `vcd::assocstats` to extract Cramer's V.
-   `nom_c`: Simple wrapper for `vcd::assocstats` to extract the contingency coefficient c.
-   `nom_lambda`: Simple wrapper for `ryouready::nom.lambda` to extract appropriate lambda.
-   `ord_gamma`: Simple wrapper for `ryouready::ord.gamma`.
-   `ord_somers_d`: Simple wrapper for `ryouready::ord.somers.d`.

### Summaries

-   `tadaa_nom`: All the nominal stats in one table.
-   `tadaa_ord`: All the ordinal stats in one table (**NYI**).

Misc. helpers
-------------

-   `generate_recodes`: To produce recode assignments for `car::recode` for evenly sequenced clusters.
-   `interval_labels`: To produce labels for clusters created by `cut`.

Tidy test output
----------------

-   `tadaa_aov`: For `aov` with included partial eta^2.
-   `tadaa_t.test`: For `t.test` with automatic homogenity of variance detection, effect size and power.

Helpers for plots
-----------------

### CI based on t-distribution

-   `mean_ci_t`: Returns a `data.frame` with `y` (`mean`), `ymin` and `ymax` for the CI bounds.
    -   `confint_t`: For the underlying function to get the CI width. Returns a single value.

### Interaction plots

-   `tadaa_int`: Simple interaction plot template.
