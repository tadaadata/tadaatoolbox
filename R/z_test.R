#' One- and Two-Sample z-Test
#'
#' Since the "standard" z-test is not available in R as in most real-world scenarios you're
#' only ever going to use a t-test, this function fills that gap for teaching purposes.
#' The function is basically a carbon-copy of [stats::t.test], but with user-supplied
#' variances for x and y and p-value and related calculations use a standard normal distribution.
#' @param x A (non-empty) numeric vector of data values
#' @param y An optional (non-empty) numeric vector of data values. If omitted, a one-sample
#' test is conducted.
#' @param sigma_x,sigma_y The assumed known variance of `x` and `y`. Must be numeric.
#' @param alternative A character string specifying the alternative hypothesis, must be
#' one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param mu A number indicating the true value of the mean (or difference in means if
#' you are performing a two sample test).
#' @param paired A logical indicating whether you want a paired t-test.
#' @param conf.level Confidence level of the interval.
#'
#' @return An object of class `htest`, see [stats::t.test]
#' @export
#' @source [stats::t.test]
#' @examples
#' x <- rnorm(10, 5, 1)
#' y <- 1:10 + rnorm(10, 3, 1.5)
#'
#' # Two sample
#' z.test(x, y, sigma_x = 1, sigma_y = 1.5)
#'
#' # One sample
#' z.test(x, sigma_x = 1, mu = 5)
z.test <- function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
                   mu = 0, sigma_x, sigma_y = NULL, paired = FALSE, conf.level = 0.95) {
  alternative <- match.arg(alternative)
  if (!missing(mu) && (length(mu) != 1 || is.na(mu))) {
    stop("'mu' must be a single number")
  }
  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) ||
    conf.level < 0 || conf.level > 1)) {
    stop("'conf.level' must be a single number between 0 and 1")
  }
  if (!is.null(y)) {
    dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    if (paired) {
      xok <- yok <- complete.cases(x, y)
    } else {
      yok <- !is.na(y)
      xok <- !is.na(x)
    }
    y <- y[yok]
  }
  else {
    dname <- deparse(substitute(x))
    if (paired) {
      stop("'y' is missing for paired test")
    }
    xok <- !is.na(x)
    yok <- NULL
  }
  x <- x[xok]
  if (paired) {
    x <- x - y
    y <- NULL
  }
  nx <- length(x)
  mx <- mean(x)
  vx <- sigma_x
  if (is.null(y)) {
    if (nx < 2) {
      stop("not enough 'x' observations")
    }
    df <- nx - 1
    stderr <- sqrt(vx / nx)
    if (stderr < 10 * .Machine$double.eps * abs(mx)) {
      stop("data are essentially constant")
    }
    tstat <- (mx - mu) / stderr
    method <- if (paired) {
      "Paired z-test"
    } else {
      "One Sample z-test"
    }
    estimate <- setNames(mx, if (paired) {
      "mean of the differences"
    } else {
      "mean of x"
    })
  }
  else {
    ny <- length(y)
    if (nx < 1) {
      stop("not enough 'x' observations")
    }
    if (ny < 1) {
      stop("not enough 'y' observations")
    }
    if (nx + ny < 3) {
      stop("not enough observations")
    }
    my <- mean(y)
    vy <- sigma_y
    method <- "Two Sample z-test"
    estimate <- c(mx, my)
    names(estimate) <- c("mean of x", "mean of y")

    df <- nx + ny - 2
    v <- 0
    if (nx > 1) {
      v <- v + (nx - 1) * vx
    }
    if (ny > 1) {
      v <- v + (ny - 1) * vy
    }
    v <- v / df
    stderr <- sqrt(v * (1 / nx + 1 / ny))

    if (stderr < 10 * .Machine$double.eps * max(
      abs(mx),
      abs(my)
    )) {
      stop("data are essentially constant")
    }
    tstat <- (mx - my - mu) / stderr
  }
  if (alternative == "less") {
    pval <- pnorm(tstat)
    cint <- c(-Inf, tstat + qnorm(conf.level))
  }
  else if (alternative == "greater") {
    pval <- pnorm(tstat, lower.tail = FALSE)
    cint <- c(tstat - qnorm(conf.level), Inf)
  }
  else {
    pval <- 2 * pnorm(-abs(tstat))
    alpha <- 1 - conf.level
    cint <- qnorm(1 - alpha / 2)
    cint <- tstat + c(-cint, cint)
  }
  cint <- mu + cint * stderr
  names(tstat) <- "z"
  names(df) <- "df"
  names(mu) <- if (paired || !is.null(y)) {
    "difference in means"
  } else {
    "mean"
  }
  attr(cint, "conf.level") <- conf.level
  rval <- list(
    statistic = tstat, parameter = df, p.value = pval,
    conf.int = cint, estimate = estimate, null.value = mu,
    alternative = alternative, method = method, data.name = dname
  )
  class(rval) <- "htest"
  return(rval)
}
