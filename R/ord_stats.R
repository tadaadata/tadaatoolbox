#' Gamma
#'
#' Simple wrapper for \link[ryouready]{ord.gamma}.
#'
#' @param x A \code{table} or dependent numeric variable.
#' @param y Empty or independent grouping variable
#'
#' @return \code{numeric} of length 1.
#' @export
#'
#' @examples
#' df <- data.frame(rating = round(runif(50, 1, 5)),
#'                  group  = sample(c("A", "B", "C"), 50, TRUE))
#' tbl <- table(df)
#' ord_gamma(tbl)
ord_gamma <- function(x, y = NULL){
  if (!is.table(x)) {
    x <- table(x, y)
  }
  gamma <- as.numeric(ryouready::ord.gamma(x))
  return(gamma)
}

#' Somers' D
#'
#' Very simple wrapper for \link[ryouready]{ord.somers.d}.
#' @param x Dependent variable. Alternatively a \code{table}.
#' @param y Independent variable
#' @param symmetric If \code{TRUE}, symmetric D is returned. Default is \code{FALSE}.
#' @param reverse If \code{TRUE}, row and column variable are switched.
#' @return \code{numeric} value
#' @export
#' @importFrom ryouready ord.somers.d
#' @examples
#' ord_somers_d(ngo$abschalt, ngo$geschl)
ord_somers_d <- function(x, y = NULL, symmetric = FALSE, reverse = FALSE){
  if (!is.table(x)) {
    x <- table(x, y)
  }
  if (symmetric) {
    ryouready::ord.somers.d(x)$sd.symmetric
  } else if (!reverse) {
    ryouready::ord.somers.d(x)$sd.rc
  } else {
    ryouready::ord.somers.d(x)$sd.cr
  }
}
