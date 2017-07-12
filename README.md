
<!-- README.md is generated from README.Rmd. Please edit that file -->
tadaatoolbox
============

[![Build Status](https://travis-ci.org/tadaadata/tadaatoolbox.svg)](https://travis-ci.org/tadaadata/tadaatoolbox) [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/tadaatoolbox)](https://cran.r-project.org/package=tadaatoolbox) [![CRAN\_Downloads\_Badge](https://cranlogs.r-pkg.org/badges/grand-total/tadaatoolbox)](https://cran.r-project.org/package=tadaatoolbox) [![CRAN\_Downloads\_Badge](https://cranlogs.r-pkg.org/badges/tadaatoolbox)](https://cran.r-project.org/package=tadaatoolbox)

This is **tadaatoolbox** version `0.13.0` ([semver](http://semver.org/)(ish)).<br /> It contains helpers for data analysis and presentation focused on undergrad psychology, the target audience being students at University of Bremen.

Please consider this as a teaching project and be careful if you intend to use it for production use.<br /> To see what's new, see [NEWS.md](https://github.com/tadaadata/tadaatoolbox/blob/master/NEWS.md).
Also, since this project is still in the `0.x.y` version, you should expect the API to change at any time. Once we reach `1.0.0`, API changes will be rolled out more slowly and include depracation warnings. In the meantime, the package is available on CRAN primarily for convenience, because its target audience is likely not familiar with GitHub or `devtools`.

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

| Contributer    | Role             |
|:---------------|:-----------------|
| Lukas Burk     | Author & Creator |
| Tobias Anton   | Author           |
| Daniel Lüdecke | Contributor      |

Functionality
=============

See the included vignette for a demonstration: `browseVignettes("tadaatoolbox")` or view [the `pkgdown`ed version of this repo](http://tadaatoolbox.tadaa-data.de/).

Tidy test output
----------------

-   `tadaa_aov`: For `aov` with included partial *η*<sup>2</sup> and Cohen's f.
-   `tadaa_kruskal`: A wrapper for `kruskal.test` tidied and optionally `pixiedust`ed.
-   `tadaa_t.test`: For `t.test` with automatic homogenity of variance detection, effect size and power.
-   `tadaa_wilcoxon`: A wrapper for `wilcox.test` tidied and optionally `pixiedust`ed.
-   `tadaa_levene`: Wrapper for `car::leveneTest` tidied and optionally `pixiedust`ed.
-   `tadaa_normtest` lets you do tests for normality (4 methods) over multiple variables.
-   `tadaa_pairwise_t`, `tadaa_pairwise_gh` and `tadaa_pairwise_tukey` for various pairwise procedures.

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
-   `tadaa_ord`: All the ordinal stats in one table.

Misc. helpers
-------------

-   `generate_recodes`: To produce recode assignments for `car::recode` for evenly sequenced clusters.
-   `interval_labels`: To produce labels for clusters created by `cut`.
-   `delete_na`: Customizable way to drop `NA` observations from a dataset.
-   `pval_string`: Shamelessly adapted from `pixiedust::pvalString`, this will format a p-value as a character string in common `p < 0.001` notation and so on. The difference from the `pixiedust` version is that this function will also print `p < 0.05`.

Helpers for plots
-----------------

### CI calculators

-   `mean_ci_t`: Returns a `data.frame` with `y` (`mean`), `ymin` and `ymax` for the CI bounds.
    -   `confint_t`: For the underlying function to get the CI width. Returns a single value.
    -   `confint_norm`: Similar, but baes on normal distribution. Returns a single value.
-   `mean_ci_sem`: Standard error and CI, you guessed it, in one table.

### Plotting templates

-   `tadaa_int`: Simple interaction plot template.
-   `tadaa_balance`: Check equality of group sizes.
-   `tadaa_mean_ci`: Plots means with 95% confidence intervals as errorbars.
-   `tadaa_plot_tukey`: For pretty `TukeyHSD` visualization.

Dependencies
============

I rely on these awesome package for all the things this package does, so you might want to consider checking them out.

-   **stats, methods, broom, magrittr, dplyr, pwr, pixiedust, car, ggplot2, lazyeval**
-   **sjlabelled, sjmisc, haven, ryouready, vcd, cowplot, nortest, lsr, viridis**

Code of Conduct
===============

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
