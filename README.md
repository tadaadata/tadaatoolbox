
<!-- README.md is generated from README.Rmd. Please edit that file -->
tadaatoolbox
============

[![Build Status](https://travis-ci.org/tadaadata/tadaatoolbox.svg)](https://travis-ci.org/tadaadata/tadaatoolbox) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tadaatoolbox)](http://cran.r-project.org/package=tadaatoolbox) [![GitHub commits](https://img.shields.io/github/commits-since/tadaadata/tadaatoolbox/0.10.0.svg?maxAge=2592000)]() [![CRAN\_Downloads\_Badge](http://cranlogs.r-pkg.org/badges/grand-total/tadaatoolbox)](http://cran.r-project.org/package=tadaatoolbox) [![CRAN\_Downloads\_Badge](http://cranlogs.r-pkg.org/badges/tadaatoolbox)](http://cran.r-project.org/package=tadaatoolbox) [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Repo Size](https://reposs.herokuapp.com/?path=tadaadata/tadaatoolbox)](https://github.com/rexfinn/reposs)

This is **tadaatoolbox** version `0.11.9000` ([semver](http://semver.org/)).<br /> It contains helpers for data analysis and presentation focused on undergrad psychology, the target audience being students at University of Bremen.

Please consider this as a teaching project and be careful if you intend to use it for production use.<br /> To see what's new, see [NEWS.md](https://github.com/tadaadata/tadaatoolbox/blob/master/NEWS.md).
Also, since this project is still in the `0.x.y` version, you should expect the API to change at any time. Once we reach `1.0.0`, API changes will be rolled out more slowly and include depracation warnings. In the meantime, the package is available on CRAN primarily for convenience, because its target audience is likely not familiar with GitHub or `devtools`.

As a sideproject, I attempted to visualize the package depencencies [in this document](http://htmlpreview.github.io/?https://github.com/tadaadata/tadaatoolbox/blob/master/dev/dependencies/dependencies.html). It's not optimal yet, but at least it's a glimpse of the first two layers of dependencies.

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

See the included vignette for a demonstration: `browseVignettes("tadaatoolbox")` or view the current version in this repository [on GitHub pages](http://tadaadata.github.io/tadaatoolbox/).

Tidy test output
----------------

-   `tadaa_aov`: For `aov` with included partial *η*<sup>2</sup> and Cohen's f.
-   `tadaa_kruskal`: A wrapper for `kruskal.test` tidied and optionally `pixiedust`ed.
-   `tadaa_t.test`: For `t.test` with automatic homogenity of variance detection, effect size and power.
-   `tadaa_wilcoxon`: A wrapper for `wilcox.test` tidied and optionally `pixiedust`ed.
-   `tadaa_normtest` lets you do tests for normality (4 methods) over multiple variables.

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
-   `tadaa_likertize`: Reduce a range of values to `n` classes (methodologically wonky).
-   `delet_na`: Customizable way to drop `NA` observations from a dataset.
-   `labels_to_factor`: If you mix and match `sjPlot`, `haven` and `ggplot2`, you might need to translate `labels` to `factors`, which is precisely what this functions does. Drop in `data.frame` with `label`, receive `data.frame` with `factors`.
-   `drop_labels`: If you subset a `labelled` dataset, you might end up with labels that have no values with them. This function will drop the now unused `labels`.
-   `pval_string`: Shamalessly adapted from `pixiedust::pvalString`, this will format a p-value as a character string in common `p < 0.001` notation and so on. The difference from the `pixiedust` version is that this function will also print `p < 0.05`.

Helpers for plots
-----------------

### CI based on t-distribution

-   `mean_ci_t`: Returns a `data.frame` with `y` (`mean`), `ymin` and `ymax` for the CI bounds.
    -   `confint_t`: For the underlying function to get the CI width. Returns a single value.
    -   `confint_norm`: Similar, but baes on normal distribution. Returns a single value.
-   `mean_ci_sem`: Standard error and CI, you guessed it, in one table.

### Interaction plots

-   `tadaa_int`: Simple interaction plot template.

### Heatmap

-   `tadaa_heatmap`: Simple heatmap template.

Dependencies
============

I rely on these awesome package for all the things this package does, so you might want to consider checking them out.

-   **stats**
-   **broom**
-   **dplyr**
-   **pwr**
-   **pixiedust**
-   **car**
-   **ggplot2**
-   **lazyeval**
-   **sjmisc**
-   **haven**
-   **ryouready**
-   **vcd**
-   **cowplot**
-   **nortest**

Code of Conduct
===============

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
