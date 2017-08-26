#' Tadaa, one-sample tests!
#'
#' If \code{sigma} is omitted, the function will just perform a one-sample \link[stats]{t.test},
#' but if \code{sigma} is provided, a z-test is performed. It basically works the same way,
#' except that we pretend we know the population sigma and use the normal distribution
#' for comparison.
#' @param data A \code{data.frame} (optional).
#' @param x A numeric vector or bare column name of \code{data}.
#' @param mu The true mean (\eqn{\mu}) to test for.
#' @param sigma Population sigma. If supplied, a z-test is performed,
#' otherwise a one-sample \link[stats]{t.test} is performed.
#' @param na.rm Whether to drop \code{NA} values. Default is \code{FALSE}.
#' @param conf.level Confidence level used for power and CI, default is \code{0.95}.
#' @param print Print method, default \code{df}: A regular \code{data.frame}.
#' Otherwise passed to \link[pixiedust]{sprinkle_print_method} for fancyness.
#' @inheritParams tadaa_t.test
#' @return A \code{data.frame} by default, otherwise \code{dust} object, depending on \code{print}.
#' @import pixiedust
#' @import stats
#' @family Tadaa-functions
#' @export
#' @examples
#' set.seed(42)
#' df <- data.frame(x = rnorm(n = 20, mean = 100, sd = 1))
#'
#' tadaa_one_sample(df, x, mu = 101, sigma = 1)
#'
#' # No data.frame, just a vector
#' tadaa_one_sample(x = rnorm(20), mu = 0)
tadaa_one_sample <- function(data = NULL, x, mu, sigma = NULL, direction = "two.sided",
                             na.rm = FALSE, conf.level = 0.95,
                             print = c("df", "console", "html", "markdown")) {

  print <- match.arg(print)

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

    # Add SE because why not
    results$se <- results$estimate/results$statistic

    # Effect size
    results$d <- (mean_x - mu) / sd(x, na.rm = na.rm)
    # Power
    results$power <- pwr::pwr.t.test(n = length(x), d = results$d,
                                     type = "one.sample", alternative = direction,
                                     sig.level = 1 - conf.level)$power
    # Name statistic
    statistic_label <- "t"
  } else {
    # If sigma is known, do manual z-test stuff
    sem     <- sigma/sqrt(length(x))
    results <- data.frame(estimate  = mean_x,
                          statistic = (mean_x - mu)/sem,
                          se        = sem)

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
    results$conf.low    <- mean_x - confint_norm(x, alpha = 1 - conf.level)
    results$conf.high   <- mean_x + confint_norm(x, alpha = 1 - conf.level)
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
                          "two.sided" = paste0("$\\mu_1 \\neq$ ", mu),
                          "greater"   = paste0("$\\mu_1 >$ ", mu),
                          "less"      = paste0("$\\mu_1 <$ ", mu))

    caption     <-  paste0("**", method, "** with alternative hypothesis: ", alternative)

    results$ci  <-  paste0("(", round(results$conf.low, 2),
                           " - ", round(results$conf.high, 2), ")")
    CI_lab      <- paste0("$CI_{", round(100 * conf.level, 2), "\\%}$")

    if ("parameter" %in% names(results)) {
      results <- results[c("estimate", "parameter", "se", "statistic",
                           "ci", "p.value", "d", "power")]
    } else {
      results <- results[c("estimate", "se", "statistic", "ci",
                           "p.value", "d", "power")]
    }

    output <- pixiedust::dust(results, caption = caption)
    output <- pixiedust::sprinkle_table(output, halign = "center", part = "head")
    output <- pixiedust::sprinkle_colnames(output,
                                           estimate  = paste0("$\\mu_1$ ", x_lab),
                                           statistic = statistic_label,
                                           se        = "SE",
                                           p.value   = "p",
                                           ci        = CI_lab,
                                           d         = "Cohen\\'s d",
                                           power     = "Power")
    if ("parameter" %in% names(results)) {
      output <- pixiedust::sprinkle_colnames(output, parameter = "df")
    } else if ("se" %in% names(results)) {
      output <- pixiedust::sprinkle_colnames(output, se        = "SE")
    }
    output <- pixiedust::sprinkle(output, col = "p.value", fn = quote(pval_string(value)))
    output <- pixiedust::sprinkle(output, round = 2)
    output <- pixiedust::sprinkle_print_method(output, print_method = print)

    output
  }
}
