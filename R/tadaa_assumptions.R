#' Tadaa, test for normality!
#'
#' @param data A `data.frame`.
#' @param method The type of test to perform. Either `ad` for Anderson Darling,
#' `shapiro` for Shapiro-Wilk, `pearson` for Pearson's chi-square test or
#' `ks` for Kolmogorov-Smirnov (not recommended).
#' @param ... Further arguments passed to test functions where applicable,
#' see [nortest::pearson.test] and [stats::ks.test].
#' @inheritParams tadaa_aov
#' @return A `data.frame` by default, otherwise `dust` object, depending on `print`.
#' @import pixiedust
#' @import nortest
#' @import stats
#' @importFrom broom tidy
#' @importFrom nortest ad.test
#' @importFrom nortest pearson.test
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
tadaa_normtest <- function(data, method = "ad",
                           print = c("df", "console", "html", "markdown"), ...) {
  print <- match.arg(print)

  vars <- names(data)
  results <- lapply(data, function(x) {
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

  results <- data.frame(
    variable = as.character(vars),
    statistic = sapply(results, "[[", 1),
    p.value = sapply(results, "[[", 2),
    method = sapply(results, "[[", 3)
  )
  rownames(results) <- NULL

  if (print == "df") {
    return(results)
  } else {
    output <- pixiedust::dust(results)
    output <- pixiedust::sprinkle(output, col = "p.value", fn = quote(pval_string(value)))
    output <- pixiedust::sprinkle(output, round = 2)
    output <- pixiedust::sprinkle_print_method(output, print_method = print)

    output
  }
}

#' Levene's Test for Homoskedasticity
#'
#' A thin wrapper around [car::leveneTest] with some formatting done.
#' @param data Data for the test
#' @param formula Formula specifiyng groups, passed to `leveneTest`.
#' @param center Method to use, either `median` (default for robustness) or `mean`.
#' @inheritParams tadaa_aov
#' @return A `data.frame` by default, otherwise `dust` object, depending on `print`.
#' @note The case of `center = "median"` is technically called *Brown–Forsythe test*,
#' so if that's selected the header for non-`df` returns will reflect that.
#' @export
#' @importFrom stats terms
#' @importFrom car leveneTest
#' @importFrom broom tidy
#' @import pixiedust
#' @family Tadaa-functions

#' @examples
#' tadaa_levene(ngo, deutsch ~ jahrgang, print = "console")
#' tadaa_levene(ngo, deutsch ~ jahrgang * geschl, print = "console")
tadaa_levene <- function(data, formula, center = "median",
                         print = c("df", "console", "html", "markdown")) {
  print <- match.arg(print)

  # Labels for groups
  factors <- attr(terms(formula), "term.labels")
  factors <- factors[attr(terms(formula), "order") == 1]
  groups <- paste0(factors, collapse = ":")

  # Actual test
  test <- broom::tidy(car::leveneTest(formula, data = data, center = center))

  # Label terms
  test$term <- c(groups, "Residuals")

  if (print == "df") {
    return(test)
  } else {
    method <- "**Levene\\'s Test** for Homogeneity of Variance"

    if (center == "median") {
      method <- paste(method, "(**Brown-Forsythe** Adaption)")
    }

    output <- pixiedust::dust(test, caption = method)
    output <- pixiedust::sprinkle_table(output, halign = "center", part = "head")
    output <- pixiedust::sprinkle(output, col = "p.value", fn = quote(pval_string(value)))
    output <- pixiedust::sprinkle_colnames(
      output,
      term = "Term",
      statistic = "F",
      p.value = "p"
    )
    output <- pixiedust::sprinkle(output, round = 2, na_string = "")
    output <- pixiedust::sprinkle_print_method(output, print_method = print)

    output
  }
}
