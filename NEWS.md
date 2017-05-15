# tadaatoolbox 0.11.0 (CRAN)

## New Features

- `tadaa_aov` now knows about types, uses type 1 by default and can do types 2 and 3.
    - Method for effect size calculation now uses `lsr::etaSquared`, which also takes a `type` argument.
- Add `tadaa_mean_ci`: Plots means with 95% confidence intervals as errorbars (thanks Christoph for the suggestion).
- Add `tadaa_one_sample`: For one-sample t-tests and finally an easy z-test.
    - Add `confint_norm`: Helper to get CIs, similar to `confint_t`
- Add `tadaa_wilcoxon`: For when `tadaa_t.test` isn't non-parametric enough. Same usage.
    - Additionally displays medians of each group.
- Add `tadaa_kruskal`: For when `tadaa_aov` isn't non-parametric enough, too.
- Move `tadaa_sem` ➡ `mean_ci_sem` because it's more `confint` than `tadaa`.
- Add `show_n` option to `tadaa_int`: Optionally display N in subtitle.

## Tweaks, Patches & Bug Fixes

- Fix documentation inconsistencies.
- Turns out `pval_string(0.05)` returned `< 0.05`. Well. That was embarrassing.
- Minor tweaks to `theme_readthedown` regarding text placement.

## Data

- Remove superfluous variables from `ngo`: `index`, `zeng`, `zdeutsch`, `zmathe`.

# tadaatoolbox 0.10.0

## Minor changes

* New function: `tadaa_normtest` lets you do tests for normality (4 methods) over multiple variables.
* New function: `tadaa_heatmap` generates a heatmap. Mhhh, heatmaps.
* New function: `tadaa_sem` shows the standard error of the mean and it's confidence interval
* New function: `pval_string` as a modification of `pixiedust::pvalString` that includes `p < .05`.
* Added a ggplot2 theme for the [rmdformats::readthedown](https://github.com/juba/rmdformats/) Rmd template.
* `tadaa_aov`, `tadaa_t.test` and the new `tadaa_normtest` now return a `data.frame` by default, allowing further shenanigans

## Patch changes

* New options in `tadaa_int`: 
    - Set `grid = TRUE` for the two interaction plots to be printen in a grid via
[cowplot::plot_grid](https://CRAN.R-project.org/package=cowplot).  
    - Choose the plot labels via the `labels` argument.
* `tadaa_int` plot output now also is a little tidier and optimized for smaller widths.
* `tadaa_aov` now also shows [Cohen's f](https://en.wikipedia.org/wiki/Effect_size#Cohen.27s_.C6.922) for easier power calculations based on f (`pwr`, G*power)
* Add option `reduce` to `modus`, so multiple results will be concatenated to a character by default.
* Add additional option `as_character` to `modus` because guessing about return value classes is no joke.
* Fix issues with `generate_recodes` and `interval_labels` (#1).

# tadaatoolbox 0.9.0

## Minor changes

* Add `tadaa_ord` as ordinal equivalent of `tadaa_nom`.
* Make table output of both of the former functions smaller in width by abbreviating column names
* Add `brewer_palette` option to `tadaa_int`
* Dependencies declared in `DESCRIPTION` are still experimental because of uncertainty regarding failing travis builds. I don't know what's going on there.

## Patch changes

* Add `family` tag to `tadaa_` functions so their documentation is linked
* Fix return values of `tadaa_aov`, `_t.test` and `_nom` which did not work as I expected

# tadaatoolbox 0.8.1

* Fix typo in `DESCRIPTION`, misspelling `pixiedust`. Sorry!
* Bump dependencies
    - `pixiedust` depends on R (>= 3.2.1), so we might as well depend on that version, too
    - Specifiy minimal versions for `pixiedust`, `sjmisc`, and `broom`, just to be safe
* Add vignette
    - Overhauling `README` to be less redundant compared to the usage vignette

# tadaatoolbox 0.8.0

* Added a `NEWS.md` file to track changes to the package.
* Working on CRAN compatibility
* Submitted to CRAN
* It's on CRAN, yay!
