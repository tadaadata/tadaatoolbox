# tadaatoolbox 0.9.0.9000 (devel/GitHub)

# tadaatoolbox 0.9.0 (on CRAN)

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
