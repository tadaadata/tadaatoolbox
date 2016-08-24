#' Confidence Intervals
#'
#' @param x A Numeric vector.
#' @param alpha Alpha, default is \code{0.05}.
#' @param na.rm If \code{TRUE} (default), missing values are dropped.
#' @return \code{numeric} of length one (size of CI in one direction).
#' @export
#' @import stats
#' @examples
#' df <- data.frame(x = runif(100), y = sample(c("A", "B"), 100, TRUE))
#' confint_t(df$x)
confint_t <- function(x, alpha = 0.05, na.rm = TRUE){
  if (!is.numeric(x)) {
    stop("Data is not a numeric vector")
  }

  alpha    <- alpha/2
  sd       <- sd(x, na.rm = na.rm)
  w_krit   <- qt(1 - (alpha), df = (length(x) - 1))
  width    <- w_krit * sd/sqrt(length(x))

  return(width)
}

#' Get mean and CI for a numeric vector
#'
#' Suitable for use within ggplot's \link[ggplot2]{stat_summary}.
#'
#' @param x A Numeric vector.
#' @param alpha Alpha, default is \code{0.05}.
#' @param na.rm If \code{TRUE} (default), missing values are dropped.
#' @return A \code{data.frame} with y (mean), ymin and ymax values.
#' @export
#' @import stats
#' @examples
#' df <- data.frame(x = runif(100), y = sample(c("A", "B"), 100, TRUE))
#' mean_ci_t(df$x)
mean_ci_t <- function(x, alpha = 0.05, na.rm = TRUE){
  if (!is.numeric(x)) {
    stop("Data is not a numeric vector")
  }

  m     <- mean(x, na.rm = na.rm)
  upper <- m + confint_t(x = x, alpha = alpha, na.rm = na.rm)
  lower <- m - confint_t(x = x, alpha = alpha, na.rm = na.rm)
  ret   <- data.frame(y = m, ymin = lower, ymax = upper)

  return(ret)
}

#' Confidence Intervals
#'
#' @param x A Numeric vector.
#' @param alpha Alpha, default is \code{0.05}.
#' @param na.rm If \code{TRUE} (default), missing values are dropped.
#' @return \code{numeric} of length one (size of CI in one direction)
#' @export
#' @import stats
#' @examples
#' df <- data.frame(x = runif(100), y = sample(c("A", "B"), 100, TRUE))
#' confint_norm(df$x)
confint_norm <- function(x, alpha = 0.05, na.rm = TRUE){
  if (!is.numeric(x)) {
    stop("Data is not a numeric vector")
  }

  alpha    <- alpha/2
  sd       <- sd(x, na.rm = na.rm)
  sem      <- sd/sqrt(length(x))
  w_krit   <- qnorm(1 - (alpha), mean = mean(x), sd = sem)
  width    <- w_krit * sem

  return(width)
}
