#' Tadaa, t-Test!
#'
#' An extension for \link[stats]{t.test} with added boni and tidy and/or pretty output.
#' Before a t-test is performed, \link[car]{leveneTest} is consulted as to wether
#' heteroskedasticity is present (using the default \code{center = "mean"} method for
#' a more robust test), and sets \code{var.equal} accordingly.
#' Afterwards, the effect size is calculated and \link[pwr]{pwr.t.test} or
#' \link[pwr]{pwr.t2n.test} are used to calculate the test's power accordingly.
#' The result is either returned as a \link[broom]{tidy} \code{data.frame} or prettified using
#' various \link[pixiedust]{sprinkle} shenanigans.
#'
#' @param data A \code{data.frame}.
#' @param response The response variable (dependent).
#' @param group The group variable, usually a \code{factor}.
#' @param direction Test direction, like \code{alternative} in \link{t.test}.
#' @param paired If \code{TRUE}, a paired t.test is performed.
#' @inheritParams tadaa_aov
#' @return A \code{data.frame} by default, otherwise \code{dust} object,
#' depending on \code{print}.
#' @import pixiedust
#' @import stats
#' @importFrom car leveneTest
#' @family Tadaa-functions
#' @note The cutoff for the interal Levene's test to decided whether or not to perform
#' a Welch-corrected t-test is set to `0.1` by default.
#' @export
#' @examples
#' df <- data.frame(x = runif(100), y = sample(c("A", "B"), 100, TRUE))
#' tadaa_t.test(df, x, y)
#'
#' df <- data.frame(x = runif(100), y = c(rep("A", 50), rep("B", 50)))
#' tadaa_t.test(df, x, y, paired = TRUE)
#'
#' tadaa_t.test(ngo, deutsch, geschl, print = "console")
tadaa_t.test <- function(data, response, group, direction = "two.sided",
                         paired = FALSE, print = "df") {

  response <- deparse(substitute(response))
  group    <- deparse(substitute(group))

  # Check the type of the group
  if (is.factor(data[[group]])) {
    groups <- levels(data[[group]])
  } else {
    groups <- unique(data[[group]])
  }

  # Subset groups of response
  x <- data[data[[group]] == groups[1], ][[response]]
  y <- data[data[[group]] == groups[2], ][[response]]

  # Kick out NAs if specified
  # if (na.rm) {
  #   x <- x[!is.na(x)]
  #   y <- y[!is.na(y)]
  # }

  # Get n for each group
  n1   <- length(x)
  n2   <- length(y)

  # levene
  levene    <- broom::tidy(car::leveneTest(data[[response]],
                                           data[[group]],
                                           center = "median"))
  var.equal <- ifelse(levene$p.value[[1]] <= .1, FALSE, TRUE)

  # t.test
  test <- broom::tidy(t.test(x = x, y = y, alternative = direction,
                             paired = paired, var.equal = var.equal))

  # Additions
  test$d       <- effect_size_t(data = data, response = response,
                                group = group, na.rm = TRUE)
  if (paired) {
    test$power <- pwr::pwr.t.test(n = n1, d = test$d,
                                  alternative = direction, type = "paired")$power
  } else {
    test$power <- pwr::pwr.t2n.test(n1 = n1, n2 = n2, d = test$d,
                                    alternative = direction)$power
  }

  if (print == "df") {
    return(test)
  } else {
    method <- trimws(as.character(test$method))
    test$method <- NULL
    output <- pixiedust::dust(test)
    output <- pixiedust::sprinkle_table(output, caption = paste0("**", method, "**"))
    output <- pixiedust::sprinkle_colnames(output,
                                           statistic = "t", p.value = "p", parameter = "df",
                                           conf.low = "CI (lo)", conf.high = "CI (hi)",
                                           alternative = "Alternative",
                                           d = "Cohens $\\Delta$", power = "Power")

    if ("estimate" %in% output$body$col_name) {
      output <- pixiedust::sprinkle_colnames(output, estimate = "Diff")
    }
    if ("estimate1" %in% output$body$col_name) {
      output <- pixiedust::sprinkle_colnames(output,
                                             estimate1 = paste("$\\mu$", groups[[1]]),
                                             estimate2 = paste("$\\mu$", groups[[2]]))
    }

    output <- pixiedust::sprinkle(output, cols = "p.value", fn = quote(pval_string(value)))
    output <- pixiedust::sprinkle(output, round = 3)
  }

  if (!(print %in% c("df", "console", "html", "markdown"))) {
    stop("Print method must be 'df', 'console', 'html' or, 'markdown'")
  }

  return(pixiedust::sprinkle_print_method(output, print_method = print))
}

#' Tadaa, Wilcoxon!
#'
#' @param data A \code{data.frame}.
#' @inheritParams tadaa_t.test
#' @param ... Further arguments passed to \link[stats]{wilcox.test}, e.g. \code{correct = FALSE}.
#' @return A \code{data.frame} by default, otherwise \code{dust} object, depending on \code{print}.
#' @import pixiedust
#' @import stats
#' @family Tadaa-functions
#' @export
#' @examples
#' df <- data.frame(x = runif(100), y = sample(c("A", "B"), 100, TRUE))
#' tadaa_wilcoxon(df, x, y)
#'
#' df <- data.frame(x = runif(100), y = c(rep("A", 50), rep("B", 50)))
#' tadaa_wilcoxon(df, x, y, paired = TRUE)
tadaa_wilcoxon <- function(data, response, group, direction = "two.sided",
                           paired = FALSE, print = "df", ...) {

  response <- deparse(substitute(response))
  group    <- deparse(substitute(group))

  # Check the type of the group
  if (is.factor(data[[group]])) {
    groups <- levels(data[[group]])
  } else {
    groups <- unique(data[[group]])
  }

  # Subset groups of response
  x <- data[data[[group]] == groups[1], ][[response]]
  y <- data[data[[group]] == groups[2], ][[response]]

  # Kick out NAs if specified
  # if (na.rm) {
  #   x <- x[!is.na(x)]
  #   y <- y[!is.na(y)]
  # }

  # wilcox test
  test <- broom::tidy(wilcox.test(x = x, y = y, direction = direction,
                                  paired = paired, ...))

  test$median1 <- median(x, na.rm = TRUE)
  test$median2 <- median(y, na.rm = TRUE)

  test <- test[c("statistic", "median1", "median2", "p.value", "method", "alternative")]

  if (print == "df") {
    return(test)
  } else {
    output <- pixiedust::dust(test)
    output <- pixiedust::sprinkle_colnames(output,
                                           statistic = "W", p.value = "p", method = "Method",
                                           alternative = "Direction")

    if ("estimate" %in% output$body$col_name) {
      output <- pixiedust::sprinkle_colnames(output, estimate = "Difference")
    }
    if ("median1" %in% output$body$col_name) {
      output <- pixiedust::sprinkle_colnames(output, median1 = groups[[1]], median2 = groups[[2]])
    }

    output <- pixiedust::sprinkle(output, cols = "p.value", fn = quote(pval_string(value)))
    output <- pixiedust::sprinkle(output, round = 3)
  }

  if (!(print %in% c("df", "console", "html", "markdown"))) {
    stop("Print method must be 'df', 'console', 'html' or, 'markdown'")
  }

  return(pixiedust::sprinkle_print_method(output, print_method = print))
}
