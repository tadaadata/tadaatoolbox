#' Tukey HSD pairwise comparisons
#'
#' This function is merely a thin wrapper around [stats::TukeyHSD] with tidying done
#' by [broom::tidy] and optional formatting via [pixiedust::sprinkle].
#' Its input is not a `aov` model like in the original function, but instead the
#' `aov` model is fit internally based on the arguments given. This is meant to
#' enable a consistent usage between the `tadaa_pairwise`-functions.
#' @inheritParams tadaa_pairwise_t
#' @param ... Further arguments passed to [stats::TukeyHSD]
#'
#' @return A `data.frame` or [pixiedust::dust] object depending on `print`.
#' @export
#' @importFrom stats TukeyHSD formula
#' @importFrom broom tidy
#' @import pixiedust
#' @family Tadaa-functions
#' @seealso [tadaa_pairwise_t()]
#' @examples
#' tadaa_pairwise_tukey(data = ngo, deutsch, jahrgang, geschl)
#' tadaa_pairwise_tukey(data = ngo, deutsch, jahrgang, print = "console")
tadaa_pairwise_tukey <- function(data, response, group1, group2 = NULL, print = "df", ...) {
  response <- deparse(substitute(response))
  group1 <- deparse(substitute(group1))
  group2 <- deparse(substitute(group2))

  if (is.null(group2)) {
    formula <- as.formula(paste0(response, " ~ ", group1))
  } else {
    formula <- as.formula(paste0(response, " ~ ", group1, " * ", group2))
  }

  model <- stats::aov(formula, data = data)
  tukey <- stats::TukeyHSD(model, ...)
  tukey <- broom::tidy(tukey)

  if (utils::hasName(tukey, "comparison")) {
    tukey$contrast <- tukey$comparison
    tukey$comparison <- NULL
  }

  if (print == "df") {
    return(tukey)
  } else {
    tukey$term <- as.character(tukey$term)
    output <- pixiedust::dust(tukey)
    output <- pixiedust::sprinkle(output, cols = "adj.p.value", fn = quote(tadaatoolbox::pval_string(value)))
    output <- pixiedust::sprinkle(output, round = 3)
    output <- pixiedust::sprinkle_colnames(
      output,
      adj.p.value = "p (adj.)",
      estimate = "Diff",
      term = "Term",
      # comparison = "Comparison", # old broom
      contrast = "Comparison", # new broom
      conf.low = "CI_low",
      conf.high = "CI_high"
    )
  }

  if (!(print %in% c("df", "console", "html", "markdown"))) {
    stop("Print method must be 'df', 'console', 'html' or, 'markdown'")
  }

  pixiedust::sprinkle_print_method(output, print_method = print)
}

globalVariables("tukey") # but why though
