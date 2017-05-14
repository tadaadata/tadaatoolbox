#' Tadaa, anova!
#'
#' @param formula Formula for model, passed to \code{aov}.
#' @param data Data for model.
#' @param show_effect_size If \code{TRUE} (default), effect sizes partial eta^2 and Cohen's f are appended as columns.
#' @param type Which type of SS to use. Default is \code{1}, can also be \code{2} oder \code{3}.
#' @param print Print method, per default a regular \code{data.frame}.
#' Otherwise passed to \link[pixiedust]{sprinkle_print_method} for fancyness.
#' @return A \code{data.frame} by default, otherwise \code{dust} object, depending on \code{print}.
#' @export
#' @family Tadaa-functions
#' @import stats
#' @examples
#' tadaa_aov(stunzahl ~ jahrgang, data = ngo)
#' tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo)
tadaa_aov <- function(formula, data = NULL, show_effect_size = TRUE, type = 1, print = "df"){

  base_model <- stats::aov(formula = formula, data = data)
  effects    <- lsr::etaSquared(base_model, type = type)

  if (type %in% c(2, 3)) {
    base_model <- car::Anova(base_model, type = type)
  }

  model <- broom::tidy(base_model)

  if (show_effect_size) {
    effects          <- data.frame(term = rownames(effects), effects, row.names = NULL)
    effects$cohens.f <- sqrt(effects$eta.sq.part / (1 - effects$eta.sq.part))
    model            <- merge(model, effects, by = "term", all = T)
  }

  if (print == "df") {
    return(model)
  } else {
    output <- pixiedust::dust(model)
    output <- pixiedust::sprinkle_colnames(output, statistic = "F")
    output <- pixiedust::sprinkle(output, col = 6, fn = quote(pval_string(value)))
    output <- pixiedust::sprinkle(output, col = 3:4, round = 2)
    output <- pixiedust::sprinkle(output, round = 3)

  }

  if (!(print %in% c("df", "console", "html", "markdown"))) {
    stop("Print method must be 'df', 'console', 'html' or, 'markdown'")
  }

  return(pixiedust::sprinkle_print_method(output, print_method = print))
}


#' Tadaa, t-Test!
#'
#' @param data A \code{data.frame}.
#' @param response The response variable (dependent).
#' @param group The group variable, usually a \code{factor}.
#' @param direction Test direction, like \code{alternative} in \link{t.test}.
#' @param paired If \code{TRUE}, a paired t.test is performed with approproate power calculation.
#' @param na.rm If \code{TRUE} (default), missing values are dropped.
#' @inheritParams tadaa_aov
#' @return A \code{data.frame} by default, otherwise \code{dust} object, depending on \code{print}.
#' @import pixiedust
#' @import stats
#' @importFrom car leveneTest
#' @family Tadaa-functions
#' @export
#' @examples
#' df <- data.frame(x = runif(100), y = sample(c("A", "B"), 100, TRUE))
#' tadaa_t.test(df, x, y)
#'
#' df <- data.frame(x = runif(100), y = c(rep("A", 50), rep("B", 50)))
#' tadaa_t.test(df, x, y, paired = TRUE)
tadaa_t.test <- function(data, response, group, direction = "two.sided",
                         paired = FALSE, na.rm = TRUE, print = "df") {

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
  if (na.rm) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }

  # Get n for each group
  n1   <- length(x)
  n2   <- length(y)

  # levene
  levene    <- broom::tidy(car::leveneTest(data[[response]], data[[group]], center = "mean"))
  var.equal <- ifelse(levene$p.value[[1]] <= .1, FALSE, TRUE)

  # t.test
  test <- broom::tidy(t.test(x = x, y = y, direction = direction,
                             paired = paired, var.equal = var.equal))

  # Additions
  test$d       <- effect_size_t(data = data, response = response, group = group, na.rm = na.rm)
  if (paired) {
    test$power <- pwr::pwr.t.test(n = n1, d = test$d, alternative = direction, type = "paired")$power
  } else {
    test$power <- pwr::pwr.t2n.test(n1 = n1, n2 = n2, d = test$d, alternative = direction)$power
  }

  if (print == "df") {
    return(test)
  } else {
    output <- pixiedust::dust(test)
    output <- pixiedust::sprinkle_colnames(output,
                                           statistic = "t", p.value = "p", parameter = "df",
                                           conf.low = "conf_low", conf.high = "conf_high")

    if ("estimate" %in% output$body$col_name) {
      output <- pixiedust::sprinkle_colnames(output, estimate = "Differenz")
    }
    if ("estimate1" %in% output$body$col_name) {
      output <- pixiedust::sprinkle_colnames(output, estimate1 = groups[[1]], estimate2 = groups[[2]])
    }

    output <- pixiedust::sprinkle(output, cols = "p.value", fn = quote(pval_string(value)))
    output <- pixiedust::sprinkle(output, round = 3)
  }

  if (!(print %in% c("df", "console", "html", "markdown"))) {
    stop("Print method must be 'df', 'console', 'html' or, 'markdown'")
  }

  return(pixiedust::sprinkle_print_method(output, print_method = print))
}


#' Tadaa, test for normality!
#'
#' @param data A \code{data.frame}.
#' @param method The type of test to perform. Either \code{ad} for Anderson Darling,
#' \code{shapiro} for Shapiro-Wilk, \code{pearson} for Pearson's chi-square test or
#' \code{ks} for Kolmogorov-Smirnov (not recommended).
#' @param ... Further arguments passed to test functions where applicable,
#' see \link[nortest]{pearson.test} and \link[stats]{ks.test}.
#' @inheritParams tadaa_aov
#' @return A \code{data.frame} by default, otherwise \code{dust} object, depending on \code{print}.
#' @import pixiedust
#' @import nortest
#' @import stats
#' @importFrom broom tidy
#' @importFrom nortest ad.test
#' @importFrom nortest pearson.test
#' @importFrom dplyr bind_rows
#' @family Tadaa-functions
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' ngo %>%
#' select(englisch, deutsch, mathe) %>%
#' tadaa_normtest(method = "shapiro")
#'
#' ngo %>%
#' select(englisch, deutsch, mathe) %>%
#' tadaa_normtest(method = "pearson", print = "console")
#' }
tadaa_normtest <- function(data, method = "ad", print = "df", ...){

  if (print == "df" & length(method) > 1 & length(method) <= 3) {
    res <- bind_rows(lapply(method, function(x) {
      tadaa_normtest(data, method = x)
    }))
    return(res)
  }

  vars    <- names(data)
  results <- lapply(data, function(x){

    res <- if (method == "ad") {
      res <- ad.test(x)
    } else if (method == "shapiro") {
      res <- shapiro.test(x)
    } else if (method == "pearson") {
      res <- pearson.test(x, ...)
    } else if (method == "ks") {
      res <- ks.test(x, y = pnorm, mean = mean(x), sd = sd(x))
    } else {
      stop("Method must be one of: 'ad', 'shapiro', 'pearson', 'ks'")
    }

    res <- tidy(res)
    return(res)
  })
  results          <- dplyr::bind_rows(results)
  results$variable <- as.character(vars)
  results          <- results[c(ncol(results), 1:(ncol(results) - 1))]

  if (print == "df") {
    return(results)
  } else {
    output <- pixiedust::dust(results)
    output <- pixiedust::sprinkle(output, col = 3, fn = quote(pval_string(value)))
    output <- pixiedust::sprinkle(output, round = 3)

    if (!(print %in% c("df", "console", "html", "markdown"))) {
      stop("Print method must be 'df', 'console', 'html' or, 'markdown'")
    }

    return(pixiedust::sprinkle_print_method(output, print_method = print))
  }
}


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
                             na.rm = TRUE, print = "df") {

  # If x is a numeric vector, just use that
  # Otherwise it's a column of 'data', so we'll need that
  if (!is.null(data)) {
    x <- deparse(substitute(x))
    x <- data[[x]]
  } else if (!is.numeric(x)) {
    stop("Argument 'x' must be numeric or a bare column name of 'data'")
  }

  mean_x <- mean(x, na.rm = na.rm)

  if (is.null(sigma)) {
    # If sigma is unknown, just do a t-test
    results <- broom::tidy(t.test(x = x, mu = mu, direction = direction))
    # Effect size
    results$d <- (mean_x - mu) / sd(x)
    # Power
    results$power <- pwr::pwr.t.test(n = length(x), d = results$d, type = "one.sample",
                                     alternative = direction)$power
  } else {
    # If sigma is known, do manual z-test stuff
    sem     <- sigma/sqrt(length(x))
    results <- data.frame(estimate  = mean_x,
                          statistic = (mean_x - mu)/sem)

    if (direction == "two.sided") {
      p <- 2 * pnorm(mean_x, mean = mu, sd = sem)
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
    results$method      <- "z-test"
    results$alternative <- direction

    # Effect size
    results$d <- (mean_x - mu) / sigma
    # Power
    results$power <- pwr::pwr.norm.test(d = results$d, n = length(x),
                                        alternative = direction)$power
  }

  ### Output ###
  if (print == "df") {
    return(results)
  } else {
    output <- pixiedust::dust(results)
    output <- pixiedust::sprinkle(output, col = 3, fn = quote(pval_string(value)))
    output <- pixiedust::sprinkle(output, round = 3)

    if (!(print %in% c("df", "console", "html", "markdown"))) {
      stop("Print method must be 'df', 'console', 'html' or, 'markdown'")
    }

    return(pixiedust::sprinkle_print_method(output, print_method = print))
  }
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
                           paired = FALSE, na.rm = TRUE, print = "df", ...) {

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
  if (na.rm) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }

  # wilcox test
  test <- broom::tidy(wilcox.test(x = x, y = y, direction = direction,
                             paired = paired, ...))

  test$median1 <- median(x, na.rm = na.rm)
  test$median2 <- median(y, na.rm = na.rm)

  test <- test[c("statistic", "median1", "median2", "p.value", "method", "alternative")]

  if (print == "df") {
    return(test)
  } else {
    output <- pixiedust::dust(test)
    output <- pixiedust::sprinkle_colnames(output,
                                           statistic = "W", p.value = "p", method = "Method",
                                           alternative = "direction")

    if ("estimate" %in% output$body$col_name) {
      output <- pixiedust::sprinkle_colnames(output, estimate = "Differenz")
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


#' Tadaa, Kruskal-Wallis!
#'
#' @param formula Formula for model, passed to \code{kruskal.test}.
#' @param data Data for model.
#' @param print Print method, per default a regular \code{data.frame}.
#' Otherwise passed to \link[pixiedust]{sprinkle_print_method} for fancyness.
#' @return A \code{data.frame} by default, otherwise \code{dust} object, depending on \code{print}.
#' @export
#' @family Tadaa-functions
#' @import stats
#' @examples
#' tadaa_kruskal(stunzahl ~ jahrgang, data = ngo)
tadaa_kruskal <- function(formula, data = NULL, print = "console"){

  model <- broom::tidy(kruskal.test(formula = formula, data = data))

  if (print == "df") {
    return(model)
  } else {
    output <- suppressWarnings(pixiedust::dust(model))
    output <- pixiedust::sprinkle_colnames(output, statistic = "Kruskal-Wallis-Chi\u00B2",
                                           p.value = "p", parameter = "df", method = "Method")
    output <- pixiedust::sprinkle(output, col = "p.value", fn = quote(pval_string(value)))
    output <- pixiedust::sprinkle(output, round = 3)

  }

  if (!(print %in% c("df", "console", "html", "markdown"))) {
    stop("Print method must be 'df', 'console', 'html' or, 'markdown'")
  }

  return(pixiedust::sprinkle_print_method(output, print_method = print))
}
