#' Delete cases with set amount of missing values
#'
#' @param df A \code{data.frame},
#' @param n Number of \code{NAs} allowed, defaults to \code{ncol(df) - 1}.
#'
#' @return A filtered version of the input \code{data.frame}.
#' @export
#' @note Adapted from \url{http://stackoverflow.com/a/30461945/409362}.
#' @examples
#' \dontrun{
#' df <- data.frame(x = sample(c(1:10, NA), 10),
#'                  y = sample(c(1:10, NA), 10),
#'                  z = sample(c(1:10, NA), 10))
#'
#' df <- delete_na(df, 1)
#'
#' # Or with magrittr syntax sugar
#' df %<>% delete_na
#' }
delete_na <- function(df, n = ncol(df) - 1) {
  log      <- apply(df, 2, is.na)
  logindex <- apply(log, 1, function(x) sum(x) <= n)

  return(df[logindex, ])
}

#' Convert all labels to factor variables
#'
#' @param df A \code{data.frame}
#'
#' @return An identical \code{data.frame} with labelled data converted to factors
#' @export
#' @importFrom sjmisc is_labelled
#' @importFrom haven as_factor
#' @examples
#' \dontrun{
#' data %<>% labels_to_factor
#' }
labels_to_factor <- function(df) {
  for (column in names(df)) {
    if (sjmisc::is_labelled(df[[column]])) {
      df[[column]] <- haven::as_factor(df[[column]])
    }
  }
  return(df)
}

#' Re-label a vector after subsetting
#'
#' @param x A vector with now unused labels
#' @return Identical vector with appropriate labels
#' @export
#' @importFrom sjmisc set_labels
#' @importFrom sjmisc get_labels
#' @examples
#' \dontrun{
#' x <- drop_labels(x)
#' }
drop_labels <- function(x) {
  sjmisc::set_labels(x, labels = sjmisc::get_labels(x)[unique(x)])
}

#' Easy p-value formatting
#'
#' @param pv A p-value in numeric form.
#' @return A formatted \code{character} representation of the input value.
#' @export
#' @note Simplified version of \link[pixiedust]{pvalString} which considers \code{< 0.05}.
#' @examples
#' pv <- c(.9, .2, .049, .009, .000003)
#' pval_string(pv)
pval_string <- function(pv) {

  if (any(pv < 0, na.rm = TRUE) | any(pv > 1, na.rm = TRUE)) {
    notProb <- which(pv < 0 | pv > 1)
    stop(paste0("Element(s) ", paste(notProb, collapse = ", "),
                " are not valid probabilities"))
  }

  pvals <- sapply(pv, function(p){
    if (is.na(p)) {return(NA)}
    ps <- if (p > 0.99) {
      "> 0.99"
    } else if (p > 0.1) {
      format(round(p, 2), digits = 2)
    } else if (p >= 0.05) {
      format(round(p, 3), digits = 3)
    } else if (p > 0.01 & p < 0.05) {
      "< 0.05"
    } else if (p > 0.001) {
      "< 0.01"
    } else {
      "< 0.001"
    }
    return(ps)
  })

  return(pvals)
}
