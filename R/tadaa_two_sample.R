#' Tadaa, t-Test!
#'
#' An extension for [stats::t.test] with added boni and tidy and/or pretty output.
#' The result is either returned as a [broom::tidy] `data.frame` or prettified using
#' various [pixiedust::sprinkle] shenanigans.
#'
#' @param data A `data.frame`.
#' @param response The response variable (dependent).
#' @param group The group variable, usually a `factor`.
#' @param direction Test direction, like `alternative` in [t.test].
#' @param paired If `TRUE`, a paired test is performed, defaults to `FALSE`.
#' @param var.equal If set, passed to [stats::t.test] to decide whether to use a
#' Welch-correction. Default is `FALSE` to automatically use a Welch-test, which is
#' in general the safest option.
#' @param conf.level Confidence level used for power and CI, default is `0.95`.
#' @inheritParams tadaa_aov
#' @return A `data.frame` by default, otherwise `dust` object,
#' depending on `print`.
#' @import pixiedust
#' @import stats
#' @importFrom car leveneTest
#' @family Tadaa-functions
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
                         paired = FALSE, var.equal = FALSE,
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
    "p.value", "d", "method", "alternative"
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
      "p.value", "d"
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
      d = "Cohen\\'s d"
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


#' Tadaa, z-test! No seriously.
#'
#' This is a wrapper around [z.test], which in itself is a weird thing to use, but
#' why not.
#'
#' @param data A `data.frame` containing variables.
#' @param x,y A bare name of a numeric variable in `data`.
#' @param sigma_x,sigma_y Numeric. Known variances of `x` and `y`.
#' @inheritParams tadaa_t.test
#'
#' @return A [pixiedust::dust] object or `data.frame`.
#' @export
#'
#' @examples
#' set.seed(192)
#' df <- data.frame(
#'   lefties = rnorm(10, mean = 5, sd = 2),
#'   righties = rnorm(10, mean = 5.5, sd = 2.5)
#' )
#' tadaa_z.test(data = df, x = lefties, y = righties, sigma_x = 2, sigma_y = 2.5, print = "console")
tadaa_z.test <- function(data, x, y, sigma_x, sigma_y, direction = "two.sided",
                         paired = FALSE,
                         conf.level = 0.95, print = c("df", "console", "html", "markdown")) {
  print <- match.arg(print)

  x_name <- deparse(substitute(x))
  x <- data[[x_name]]
  y_name <- deparse(substitute(y))
  y <- data[[y_name]]

  # Get n for each group
  n1 <- length(x)
  n2 <- length(y)

  # z.test
  test <- broom::tidy(z.test(
    x = x, y = y, alternative = direction,
    sigma_x = sigma_x, sigma_y = sigma_y,
    paired = paired,
    conf.level = conf.level
  ))

  # Additions
  if (paired) {
    s <- sd(x - y)
  } else {
    s <- sqrt(sum((n1 - 1) * sigma_x, (n2 - 1) * sigma_y) / ((n1 + n2) - 2))
  }
  m_d <- mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)
  test$d <- m_d / s

  # For paired tests, wie still want both means, probably
  if (paired || !(all(c("estimate1", "estimate2") %in% names(test)))) {
    test$estimate1 <- mean(x, na.rm = TRUE)
    test$estimate2 <- mean(y, na.rm = TRUE)
  }

  # For non-welch-tests, we still want the difference I guess
  if (!("estimate" %in% names(test))) {
    test$estimate <- test$estimate1 - test$estimate2
  }

  # Add SE because why not
  test$se <- test$estimate / test$statistic

  # Sort estimates (and columns... it's hard)
  est_cols <- c("estimate", "estimate1", "estimate2")
  # test     <- test[c(est_cols, names(test)[!(names(test) %in% est_cols)])]
  test <- test[c(
    est_cols, "statistic", "se", "parameter", "conf.low", "conf.high",
    "p.value", "d", "method", "alternative"
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
      "p.value", "d"
    )]

    output <- pixiedust::dust(test, caption = caption)
    output <- pixiedust::sprinkle_table(output, halign = "center", part = "head")
    output <- pixiedust::sprinkle_colnames(
      output,
      estimate = "Diff",
      estimate1 = paste("$\\mu_1$", x_name),
      estimate2 = paste("$\\mu_2$", y_name),
      statistic = "z",
      p.value = "p",
      parameter = "df",
      se = "SE",
      ci = CI_lab,
      d = "Cohen\\'s d"
    )

    output <- pixiedust::sprinkle(output, cols = "p.value", fn = quote(tadaatoolbox::pval_string(value)))
    output <- pixiedust::sprinkle(output, round = 2)
    output <- pixiedust::sprinkle_print_method(output, print_method = print)

    return(output)
  }
}
