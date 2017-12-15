#' Confidence Intervals
#'
#' @rdname confint
#' @param x A Numeric vector.
#' @param alpha Alpha, default is `0.05`.
#' @param na.rm If `TRUE` (default), missing values are dropped.
#' @return `numeric` of length one (size of CI in one direction).
#' @export
#' @import stats
#' @examples
#' set.seed(42)
#' df <- data.frame(x = runif(100), y = sample(c("A", "B"), 100, TRUE))
#' confint_t(df$x)
confint_t <- function(x, alpha = 0.05, na.rm = TRUE){
  if (!is.numeric(x)) {
    stop("Data is not a numeric vector")
  }

  alpha    <- alpha/2
  sd       <- sd(x, na.rm = na.rm)
  w_krit   <- qt(1 - (alpha), df = (length(na.omit(x)) - 1))
  width    <- w_krit * sd/sqrt(length(na.omit(x)))

  return(width)
}

#' Get mean and CI for a numeric vector
#'
#' Suitable for use within ggplot's [ggplot2::stat_summary].
#'
#' @param x A Numeric vector.
#' @param alpha Alpha, default is `0.05`.
#' @param na.rm If `TRUE` (default), missing values are dropped.
#' @return A `data.frame` with y (mean), ymin and ymax values.
#' @export
#' @import stats
#' @examples
#' set.seed(42)
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

#' Standard Error of the Mean with CI
#'
#' @param x a numeric vector or R object which is coercible to one
#' @param conf.level the confidence level (alpha) of the Interval
#' @return a `data.frame` with the mean, SEM and its Confidence Interval
#' @export
#' @examples
#' set.seed(42)
#' iq <- rnorm(100, 100, 15)
#'
#' mean_ci_sem(iq)
mean_ci_sem <- function(x, conf.level = .95) {
  if (!is.numeric(x)) {
    stop("Data is not a numeric vector")
  }
  # SEM if sigma is known // todo: add option or correct for length(x) and NAs
  sem  <- sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
  # clvl <- qnorm(1 - conf.level / 2)
  clvl <- qnorm(conf.level + ((1 - conf.level) / 2))

  CI_upper <- mean(x, na.rm = TRUE) + (sem * clvl)
  CI_lower <- mean(x, na.rm = TRUE) - (sem * clvl)

  bunch_of_stats <- data.frame(y    = mean(x, na.rm = TRUE),
                               SEM  = sem,
                               ymin = CI_lower,
                               ymax = CI_upper)

  return(bunch_of_stats)
}


#' @rdname confint
#' @export
#' @import stats
#' @examples
#' set.seed(42)
#' df <- data.frame(x = runif(100), y = sample(c("A", "B"), 100, TRUE))
#' confint_norm(df$x)
confint_norm <- function(x, alpha = 0.05, na.rm = TRUE){
  if (!is.numeric(x)) {
    stop("Data is not a numeric vector")
  }

  alpha    <- alpha/2
  sd       <- sd(x, na.rm = na.rm)
  sem      <- sd/sqrt(length(na.omit(x)))
  w_krit   <- qnorm(1 - (alpha), mean = mean(x, na.rm = na.rm), sd = sem)
  width    <- w_krit * sem

  return(width)
}
