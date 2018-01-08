#' Tadaa, t-Test!
#'
#' An extension for [stats::t.test] with added boni and tidy and/or pretty output.
#' Before a t-test is performed, [car::leveneTest] is consulted as to wether
#' heteroskedasticity is present (using the default `center = "mean"` method for
#' a more robust test), and sets `var.equal` accordingly.
#' Afterwards, the effect size is calculated and [pwr::pwr.t.test] or
#' [pwr::pwr.t2n.test] are used to calculate the test's power accordingly.
#' The result is either returned as a [broom::tidy] `data.frame` or prettified using
#' various [pixiedust::sprinkle] shenanigans.
#'
#' @param data A `data.frame`.
#' @param response The response variable (dependent).
#' @param group The group variable, usually a `factor`.
#' @param direction Test direction, like `alternative` in [t.test].
#' @param paired If `TRUE`, a paired test is performed, defaults to `FALSE`.
#' @param var.equal If set, passed to [stats::t.test] to decide whether to use a
#' Welch-correction. Default is `NULL` to automatically determine heteroskedasticity.
#' @param conf.level Confidence level used for power and CI, default is `0.95`.
#' @inheritParams tadaa_aov
#' @return A `data.frame` by default, otherwise `dust` object,
#' depending on `print`.
#' @import pixiedust
#' @import stats
#' @importFrom car leveneTest
#' @family Tadaa-functions
#' @note The cutoff for the interal Levene's test to decided whether or not to perform
#' a Welch-corrected t-test is set to `0.05` by default. To override the internal tests and
#' decide whether to use a Welch test, set `var.equal` as you would with [stats::t.test].
#' @export
#' @examples
#' set.seed(42)
#' df <- data.frame(x = runif(100), y = sample(c("A", "B"), 100, TRUE))
#' tadaa_t.test(df, x, y)
#'
#' df <- data.frame(x = runif(100), y = c(rep("A", 50), rep("B", 50)))
#' tadaa_t.test(df, x, y, paired = TRUE)
#'
#' tadaa_t.test(ngo, deutsch, geschl, print = "console")
tadaa_t.test <- function(data, response, group, direction = "two.sided",
                         paired = FALSE, var.equal = NULL,
                         conf.level = 0.95, print = c("df", "console", "html", "markdown")) {
  print <- match.arg(print)

  response <- deparse(substitute(response))
  group <- deparse(substitute(group))

  # Check the type of the group
  if (is.factor(data[[group]])) {
    groups <- levels(data[[group]])
  } else {
    groups <- unique(data[[group]])
  }

  # Subset groups of response
  x <- data[data[[group]] == groups[1], ][[response]]
  y <- data[data[[group]] == groups[2], ][[response]]


  # Get n for each group
  n1 <- length(x)
  n2 <- length(y)

  # levene
  if (is.null(var.equal)) {
    levene <- broom::tidy(car::leveneTest(
      data[[response]],
      data[[group]],
      center = "median"
    ))

    var.equal <- (levene$p.value[[1]] >= .05)
  }

  # t.test
  test <- broom::tidy(t.test(
    x = x, y = y, alternative = direction,
    paired = paired, var.equal = var.equal,
    conf.level = conf.level
  ))

  # Additions
  test$d <- effect_size_t(
    data = data, response = response,
    group = group, paired = paired, na.rm = TRUE
  )
  if (paired) {
    test$power <- pwr::pwr.t.test(
      n = n1, d = test$d, sig.level = 1 - conf.level,
      alternative = direction, type = "paired"
    )$power
  } else {
    test$power <- pwr::pwr.t2n.test(
      n1 = n1, n2 = n2, d = test$d,
      sig.level = 1 - conf.level,
      alternative = direction
    )$power
  }

  # For paired tests, wie still want both means, probably
  if (paired || !(all(c("estimate1", "estimate2") %in% names(test)))) {
    test$estimate1 <- mean(x, na.rm = TRUE)
    test$estimate2 <- mean(y, na.rm = TRUE)
  }

  # For non-welch-tests, we still want the difference I guess
  if (var.equal || !("estimate" %in% names(test))) {
    test$estimate <- test$estimate1 - test$estimate2
  }

  # Add SE because why not
  test$se <- test$estimate / test$statistic

  # Sort estimates (and columns... it's hard)
  est_cols <- c("estimate", "estimate1", "estimate2")
  # test     <- test[c(est_cols, names(test)[!(names(test) %in% est_cols)])]
  test <- test[c(
    est_cols, "statistic", "se", "parameter", "conf.low", "conf.high",
    "p.value", "d", "power", "method", "alternative"
  )]

  if (print == "df") {
    return(test)
  } else {
    method <- trimws(as.character(test$method))
    alternative <- switch(direction,
      "two.sided" = "$\\mu_1 \\neq \\mu_2$",
      "greater" = "$\\mu_1 > \\mu_2$",
      "less" = "$\\mu_1 < \\mu_2$"
    )

    caption <- paste0("**", method, "** with alternative hypothesis: ", alternative)
    test$ci <- paste0(
      "(", round(test$conf.low, 2),
      " - ",
      round(test$conf.high, 2), ")"
    )
    CI_lab <- paste0("$CI_{", round(100 * conf.level, 2), "\\%}$")

    # Sort… again
    test <- test[c(
      est_cols, "statistic", "se", "parameter", "ci",
      "p.value", "d", "power"
    )]

    output <- pixiedust::dust(test, caption = caption)
    output <- pixiedust::sprinkle_table(output, halign = "center", part = "head")
    output <- pixiedust::sprinkle_colnames(
      output,
      estimate = "Diff",
      estimate1 = paste("$\\mu_1$", groups[[1]]),
      estimate2 = paste("$\\mu_2$", groups[[2]]),
      statistic = "t",
      p.value = "p",
      parameter = "df",
      se = "SE",
      ci = CI_lab,
      d = "Cohen\\'s d",
      power = "Power"
    )

    output <- pixiedust::sprinkle(output, cols = "p.value", fn = quote(tadaatoolbox::pval_string(value)))
    output <- pixiedust::sprinkle(output, round = 2)
    output <- pixiedust::sprinkle_print_method(output, print_method = print)

    return(output)
  }
}

#' Tadaa, Wilcoxon!
#'
#' @param data A `data.frame`.
#' @inheritParams tadaa_t.test
#' @param ... Further arguments passed to [stats::wilcox.test], e.g. `correct = FALSE`.
#' @return A `data.frame` by default, otherwise `dust` object, depending on `print`.
#' @import pixiedust
#' @import stats
#' @family Tadaa-functions
#' @export
#' @examples
#' set.seed(42)
#' df <- data.frame(x = runif(100), y = sample(c("A", "B"), 100, TRUE))
#' tadaa_wilcoxon(df, x, y)
#'
#' df <- data.frame(x = runif(100), y = c(rep("A", 50), rep("B", 50)))
#' tadaa_wilcoxon(df, x, y, paired = TRUE)
tadaa_wilcoxon <- function(data, response, group, direction = "two.sided",
                           paired = FALSE, print = c("df", "console", "html", "markdown"), ...) {
  print <- match.arg(print)

  response <- deparse(substitute(response))
  group <- deparse(substitute(group))

  # Check the type of the group
  if (is.factor(data[[group]])) {
    groups <- levels(data[[group]])
  } else {
    groups <- unique(data[[group]])
  }

  # Subset groups of response
  x <- data[data[[group]] == groups[1], ][[response]]
  y <- data[data[[group]] == groups[2], ][[response]]


  # wilcox test
  test <- broom::tidy(wilcox.test(
    x = x, y = y, alternative = direction,
    paired = paired, ...
  ))

  test$median1 <- median(x, na.rm = TRUE)
  test$median2 <- median(y, na.rm = TRUE)
  test$diff <- test$median1 - test$median2

  test <- test[c(
    "diff", "median1", "median2", "statistic",
    "p.value", "method", "alternative"
  )]

  if (print == "df") {
    return(test)
  } else {
    method <- trimws(as.character(test$method))
    alternative <- switch(direction,
      "two.sided" = "$M_1 \\neq M_2$",
      "greater" = "$M_1 > M_2$",
      "less" = "$M_1 < M_2$"
    )

    caption <- paste0("**", method, "** with alternative hypothesis: ", alternative)

    test$method <- NULL
    test$alternative <- NULL

    output <- pixiedust::dust(test, caption = caption)
    output <- pixiedust::sprinkle_table(output, halign = "center", part = "head")
    output <- pixiedust::sprinkle_colnames(
      output,
      diff = "Difference",
      statistic = "W",
      p.value = "p",
      median1 = paste("$M_1$", groups[[1]]),
      median2 = paste("$M_2$", groups[[2]])
    )
    output <- pixiedust::sprinkle(output, cols = "p.value", fn = quote(tadaatoolbox::pval_string(value)))
    output <- pixiedust::sprinkle(output, round = 2)
    output <- pixiedust::sprinkle_print_method(output, print_method = print)

    output
  }
}
