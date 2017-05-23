#' Tukey HSD pairwise comparisons
#'
#' This function is merely a thin wrapper around \link[stats]{TukeyHSD} with tidying done
#' by \link[broom]{tidy} and optional formatting via \link[pixiedust]{sprinkle}.
#' Its input is not a \code{aov} model like in the original function, but instead the
#' \code{aov} model is fit internally based on the arguments given. This is meant to
#' enable a consistent usage between the \code{tadaa_pairwise}-functions.
#' @inheritParams tadaa_pairwise_t
#' @param ... Further arguments passed to \link[stats]{TukeyHSD}
#'
#' @return A \code{data.frame} or \link[pixiedust]{dust} object depending on \code{print}.
#' @export
#' @importFrom stats TukeyHSD formula
#' @importFrom broom tidy
#' @import pixiedust
#' @family Tadaa-functions
#' @seealso \code{\link{tadaa_pairwise_t}}, \code{\link{tadaa_pairwise_gh}}
#' @examples
#' tadaa_pairwise_tukey(data = ngo, deutsch, jahrgang, geschl)
#' tadaa_pairwise_tukey(data = ngo, deutsch, jahrgang, print = "console")
tadaa_pairwise_tukey <- function(data, response, group1, group2 = NULL, print = "df", ...) {

  response <- deparse(substitute(response))
  group1   <- deparse(substitute(group1))
  group2   <- deparse(substitute(group2))

  if (group2 == "NULL") {
    formula <- as.formula(paste0(response, " ~ ", group1))
  } else {
    formula <- as.formula(paste0(response, " ~ ", group1, " * ", group2))
  }

  model <- stats::aov(formula, data = data)
  tukey <- stats::TukeyHSD(model, ...)
  tukey <- broom::tidy(tukey)

  if (print == "df") {
    return(tukey)
  } else {
    output <- pixiedust::dust(tukey)
    output <- pixiedust::sprinkle(output, cols = "adj.p.value", fn = quote(pval_string(value)))
    output <- pixiedust::sprinkle(output, round = 3)
    output <- pixiedust::sprinkle_colnames(output,
                                           adj.p.value = "p (adj.)",
                                           estimate    = "Diff",
                                           term        = "Term",
                                           comparison  = "Comparison",
                                           conf.low    = "CI_low",
                                           conf.high   = "CI_high")
  }

  if (!(print %in% c("df", "console", "html", "markdown"))) {
    stop("Print method must be 'df', 'console', 'html' or, 'markdown'")
  }

  pixiedust::sprinkle_print_method(output, print_method = print)

}
