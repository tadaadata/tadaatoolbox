#' Delete cases with set amount of missing values
#'
#' @param df A `data.frame`,
#' @param n Number of `NAs` allowed, defaults to `ncol(df) - 1`.
#'
#' @return A filtered version of the input `data.frame`.
#' @export
#' @note Adapted from http://stackoverflow.com/a/30461945/409362.
#' @examples
#' set.seed(1445)
#' dat <- data.frame(
#'   x = sample(c(1:15, NA, NA), 15),
#'   y = sample(c(1:15, NA, NA), 15),
#'   z = sample(c(1:15, NA, NA), 15)
#' )
#' dat
#' # No NsA per row allowed
#' delete_na(dat, 0)
#' # One NA per row allowed
#' delete_na(dat, 1)
delete_na <- function(df, n = ncol(df) - 1) {
  log <- apply(df, 2, is.na)
  logindex <- apply(log, 1, function(x) sum(x) <= n)

  return(df[logindex, ])
}

#' Easy p-value formatting
#'
#' @param pv A p-value in numeric form.
#' @return A formatted `character` representation of the input value.
#' @export
#' @note Simplified version of [`pixiedust::pvalString`] which considers `< 0.05`.
#' @examples
#' pv <- c(.9, .2, .049, .009, .000003)
#' names(pv) <- pval_string(pv)
#' pv
pval_string <- function(pv) {
  if (any(pv < 0, na.rm = TRUE) | any(pv > 1, na.rm = TRUE)) {
    notProb <- which(pv < 0 | pv > 1)
    stop(paste0(
      "Element(s) ", paste(notProb, collapse = ", "),
      " are not valid probabilities"
    ))
  }

  pvals <- sapply(pv, function(p) {
    if (is.na(p)) {
      return(NA)
    }
    ps <- if (p > 0.99) {
      "> .99"
      # } else if (p > 0.1) {
      #   format(round(p, 2), digits = 2)
    } else if (p >= 0.05) {
      gsub("0\\.", "\\.", format(round(p, 3), digits = 3))
    } else if (p > 0.01 & p < 0.05) {
      "< .05"
    } else if (p > 0.001) {
      "< .01"
    } else {
      "< .001"
    }
    return(ps)
  })

  return(pvals)
}
