# tadaatoolbox 0.17.0

- Ensure compatibility with broom 0.7.0 (#32)
- Remove `tadaa_pairwise_gh`.
- Update infrastructure using `usethis::use_tidy_ci()`, bumps R dependency to `>= 3.2`.
- Add `z.test()` to fill the niche in `stats`. Useful to verify hand-made calculations done in educational contexts, as I assume nobody else will ever need a z-test function.
- Add `tadaa_z.test()` while we're at it.

## Fixing mistakes

- Remove internal Levene-test from `tadaa_t.test()`, defaulting to `var.equal = FALSE` as `stats::t.test()` does. Turns out this was a pretty bad idea due to the multiplicity problem, and the Welch-test is *good enough*.
- Remove the post-hoc / empirical `power` from 
  - `tadaa_t.test()`
  - `tadaa_aov()`
  - `tadaa_one_sample()`
- Remove `tadaa_normtest()`. It never should have been a thing in the first place.

# tadaatoolbox 0.16.1

- Add `inv()` for inverting Likert-scales
- Make `pval_string()` conform to APA guidelines (no more leading zeros)
- More accurate R dependency (`R >= 2.10`) (i.e. _more_ inclusive)
- Add a bunch more tests

# tadaatoolbox 0.16.0

## Fixes & Features

- Add `ord_pairs` to retrieve $N_c$, $N_d$, ties and total number of pairs for contingency tables. Internals for this function are straight up copied from [this gist](https://gist.github.com/marcschwartz/3665743).
- Add `etasq` in case you want to show students what $\eta^2$ is without having to explain ANOVA.
- Fix `ord_somers_d` not returning the correct value for `symmetric = TRUE`
- Removed `drop_labels` because `sjlabelled::zap_labels` is a thing

## Internals

- Eliminate `ryouready` dependency, in favor of the more versatile `DescTools` package:
    - `nom_lambda`: Use `DescTools::Lambda`
    - `ord_gamma`: Use `DescTools::GoodmanKruskalGamma`
    - `ord_somers_d`: Use `DescTools::SomersDelta`
- Eliminate `vcd` dependency, also in favor of `DescTools`.
- (The `DescTools` functions are very similar to what the wrappers do, so the wrappers might be removed in the future.)
- Eliminate `lazyeval` dependency in `tadaa_int` by being better at `ggplot2`.
- Eliminate `dplyr` dependency by being better at R.
- Eliminate `haven` dependency by not re-exporting `as_factor` anymore ¯\\\_(ツ)_/¯
- Eliminate `sjmisc` depencency because why did we depend on that again?
- Eliminate `sjlabelled` dependency (only used for re-exports).
- Eliminate `lsr` dependency in favor of, you guessed it, `DescTools` for eta in `tadaa_aov`.

# tadaatoolbox 0.15.0

- Fix error in SEM calculation in `tadaa_one_sample` for t-tests
- Make sure `tadaa_`-test functions use `tadaatoolbox::pval_string` instead of the `pixiedust` version
- Remove recoded `leist` var from `ngo`, as it should be computed from `leistung` by students.
- Move `cowplot` from `Imports` to `Suggests` because we only need it in one function, *sometimes*.
- More compact table output in `tadaa_nom` and `tadaa_ord`.

## Removals

- `[tadaa_]likertize` is removed. Use `sjmisc::split_var`.
- `labels_to_factor` is removed because various `as_factor`s exist.

# tadaatoolbox 0.14.0

- Silence warnings in functions using `chisq.test`
- Add `ord_tau` to calculate _all_ the Taus
    - Append them on `tadaa_ord`

# tadaatoolbox 0.13.0

## New Features

- Add `tadaa_chisq` for a $\chi^2$-Test with OR and effect size.

## Under the Hood

- Switched from `sjmisc` to the new `sjlabelled` (Thanks, @strengejacke!)
- Also re-export `magrittr::%$%` because it's really handy *sometimes*.
- Fix #30: Undocumented arguments in `tadaa_one_sample`

# tadaatoolbox 0.12.0

## New Features

- Add `tadaa_pairwise_t` as an extension of `stats::pairwise.t.test` that works with two grouping factors and thereby can test interactions.
    - Also knows the *Sidak* method for p-adjustment, both regular and step-down procedures.
    - See [this](https://stats.stackexchange.com/questions/20825/sidak-or-bonferroni) and [that](https://rdrr.io/rforge/mutoss/man/SidakSD.html)
- Add `tadaa_pairwise_gh` for the Games Howell post-hoc procedure as an alternative to `TukeyHSD`.
    - Adapted from [this gist by GitHub user aschleg](https://gist.github.com/aschleg/ea7942efc6108aedfa9ec98aeb6c2096)
    - [Further reading](https://rpubs.com/aaronsc32/games-howell-test)
- Add `tadaa_pairwise_tukey` while we're at it. Just a thin wrapper for `stats::TukeyHSD` but with tidied output and usage consistent with the previous `tadaa_pairwise_*` functions.
- Add `tadaa_plot_tukey` to plot Tukey HSD results as error bars because boy do I like error bars.
- Add `tadaa_balance` as a replacement for `tadaa_heatmap` to check equality of group sizes.
- Re-exports:
    - `%>%` from `magrittr` as all the cool kids to these days.
    - `%<>%` from `magrittr` because I happen to really like it.
    - `[sg]et_label[s]`and `word_wrap` from `sjmisc`, as they're handy.
    - `as_factor` from `haven` as a replacement for the deprecated `labels_to_factor`.

## Tweaks, Patches & Bug Fixes

- `theme_tadaa`:
    - Is now an alias for `theme_readthedown`, will probably become the new canonical version.
    - Now finally adds vertical space to the `x` axis title via proper margining.
- `tadaa_aov`:
    - Default `type` is now `3`, for generally safer results and consistency with SPSS.
    - Added `check_contrasts` option for `type = 3` sums of squares, which ensures each non-ordered factor has `contr.sum` contrasts
    - Now auto-factorizes independent variables by default, fixes #24.
    - Now imports `methods`, which should fix an issue during `knitr` or `rmarkdown` processing where the function `is` couldn't be found. If not, manually `library(methods)` as a workaround.
    - Fix wrong `sprinkle` labelling causing `eta.sq` to be formatted like a p-value.
    - Added `show_power` argument to calculate power via `pwr::pwr.f2.test`.
        - Requires more testing against software like G\*power to ensure accuracy.
- `tadaa_t.test`:
    - Internal Levene test now uses `center = "median"` for more robust results, as it should.
        - Now also uses $\alpha = 0.05$ instead of $\alpha = 0.1$.
        - Use new argument `var.equal` to override internal Levene test.
    - Power should now be properly reported for `alternative = "less"` or `greater`.
    - Added `conf.level` argument used for CI and power calculations
    - `effect_size_t`:
        - Now doesn't return the absolute effect size by default.
        - Added `paired` argument so effects for paired tests are now a thing.
- `tadaa_wilcoxon`:
    - Also fix `direction` argument not being honored.
- `tadaa_int`: 
    - Gains `print` (logical) argument to suppress printing if so desired. The output will still be returned invisibly.
- `tadaa_one_sample`: Should make sense now.
- Remove `na.rm` argument from `tadaa_t.test` and `tadaa_wilcoxon` because it's problematic, and in case of `paired = TRUE` it would have produced flat out wrong results.
- Documentation improvements
- Improved `print = markdown` output of `tadaa_aov`, `tadaa_t.test`, `tadaa_wilcoxon`, `tadaa_one_sample`, `tadaa_kruskal`. Unfortunately `print = "console"` now has headers with unparsed $\LaTeX$-expressions, but who uses that anyway.

## Deprecations

- `labels_to_factor`: Was a wrapper around `haven::as_factor` and is obsolete by now, as `as_factor` can do the same thing this function was built for.
- `tadaa_likertize` is renamed to `likertize`, deprecated since `sjmisc::split_var` is probably better anyway.

# tadaatoolbox 0.11.0

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
    - Set `grid = TRUE` for the two interaction plots to be printed in a grid via
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
