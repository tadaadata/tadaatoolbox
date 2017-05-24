#' Tadaa, one-sample tests!
#'
#' If `sigma` is omitted, the function will basically just perform a one-sample \link[stats]{t.test},
#' but if `sigma` is provided, a z-test is performed. It basically works the same way, except that we
#' pretend we know the population sigma.
#' @param data A \code{data.frame}.
#' @param x A numeric vector.
#' @param mu The true mean (\eqn{\mu}) to test for.
#' @param sigma Population sigma. If supplied, a z-test is performed,
#' else a one-sample \link[stats]{t.test} is performed.
#' @param na.rm Whether to drop \code{NA} values. Default is \code{FALSE}.
#' @inheritParams tadaa_t.test
#' @return A \code{data.frame} by default, otherwise \code{dust} object, depending on \code{print}.
#' @import pixiedust
#' @import stats
#' @family Tadaa-functions
#' @export
#' @examples
#' df <- data.frame(x = rnorm(n = 20, mean = 100, sd = 1))
#'
#' tadaa_one_sample(df, x, mu = 101, sigma = 1)
#'
#' # No data.frame, just a vector
#' tadaa_one_sample(x = rnorm(20), mu = 0)
tadaa_one_sample <- function(data = NULL, x, mu, sigma = NULL, direction = "two.sided",
                             na.rm = FALSE, print = "df") {

  # If x is a numeric vector, just use that
  # Otherwise it's a column of 'data', so we'll need that
  if (!is.null(data)) {
    x_lab <- deparse(substitute(x))
    x     <- data[[x_lab]]
  } else if (!is.numeric(x)) {
    stop("Argument 'x' must be numeric or a bare column name of 'data'")
  }

  mean_x <- mean(x, na.rm = na.rm)

  if (is.null(sigma)) {
    # If sigma is unknown, just do a t-test
    results <- broom::tidy(t.test(x = x, mu = mu, direction = direction))
    # Effect size
    results$d <- (mean_x - mu) / sd(x, na.rm = na.rm)
    # Power
    results$power <- pwr::pwr.t.test(n = length(x), d = results$d, type = "one.sample",
                                     alternative = direction)$power
    # Name statistic
    statistic_label <- "t"
  } else {
    # If sigma is known, do manual z-test stuff
    sem     <- sigma/sqrt(length(x))
    results <- data.frame(estimate  = mean_x,
                          statistic = (mean_x - mu)/sem)

    if (direction == "two.sided") {
      p <- pnorm(mean_x, mean = mu, sd = sem)
      p <- pmin(p, 1 - p) * 2
    } else if (direction == "less") {
      p <- pnorm(mean_x, mean = mu, sd = sem, lower.tail = TRUE)
    } else if (direction == "greater") {
      p <- pnorm(mean_x, mean = mu, sd = sem, lower.tail = FALSE)
    } else {
      stop("'direction' must be one of: 'two.sided', 'less', 'greater'")
    }
    results$p.value     <- p
    results$conf.low    <- mean_x - confint_norm(x)
    results$conf.high   <- mean_x + confint_norm(x)
    results$method      <- "z-Test"
    results$alternative <- direction

    # Effect size
    results$d <- (mean_x - mu) / sigma
    # Power
    results$power <- pwr::pwr.norm.test(d = results$d, n = length(x),
                                        alternative = direction)$power
    # Name statistic
    statistic_label <- "z"
  }

  ### Output ###
  if (print == "df") {
    return(results)
  } else {
    method      <- trimws(as.character(results$method))
    alternative <- switch(direction,
                          "two.sided" = paste0("$\\mu \\neq ", mu, "$"),
                          "greater"   = paste0("$\\mu > ", mu, "$"),
                          "less"      = paste0("$\\mu < ", mu, "$"))

    caption     <-  paste0("**", method, "** with alternative hypothesis: ", alternative)

    results$method      <- NULL
    results$alternative <- NULL

    output <- pixiedust::dust(results)
    output <- pixiedust::sprinkle_table(output, caption = caption)
    output <- pixiedust::sprinkle_colnames(output,
                                           estimate  = paste0("$\\mu$ ", x_lab),
                                           statistic = statistic_label,
                                           p.value   = "p",
                                           conf.low  = "CI (lo)",
                                           conf.high = "CI (hi)",
                                           d         = "Cohen\\'s d",
                                           power     = "Power")
    if ("parameter" %in% names(results)) {
      output <- pixiedust::sprinkle_colnames(output, parameter = "df")
    }
    output <- pixiedust::sprinkle(output, col = "p.value", fn = quote(pval_string(value)))
    output <- pixiedust::sprinkle(output, round = 2)
  }

  if (!(print %in% c("df", "console", "html", "markdown"))) {
    stop("Print method must be 'df', 'console', 'html' or, 'markdown'")
  }

  return(pixiedust::sprinkle_print_method(output, print_method = print))

}
