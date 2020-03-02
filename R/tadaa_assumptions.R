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
