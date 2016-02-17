## Test environments
* local OS X install, R 3.2.3
* ubuntu 12.04 (on travis-ci), R 3.2.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

### Notes

- *found 1 marked UTF-8 string*
    - The included data contains a `factor` with labels, one of which contains an Umlaut (`Männlich`), which is entered as an escaped unicode sequence (`\u00e4`)

### Fixes from previous submission:

- Problem: The dependency for R was lower than the R dependency of imported package `pixiedust`, causing an `ERROR` on r-oldrel-windows-ix86+x86_64. 
- Fix: The R dependency for the package has been raised to the level oc pixiedust (>= 3.2.1).

## Reverse dependencies

There are no reverse dependencies.

