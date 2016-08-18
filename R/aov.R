#' Tadaa, anova!
#'
#' @param formula Formula for model, passed to \code{aov}.
#' @param data Data for model.
#' @param show_effect_size If \code{TRUE} (default), effect sizes partial eta^2 and Cohen's f are appended as columns.
#' @param print Print method, per default a regular \code{data.frame}.
#' Otherwise passed to \link[pixiedust]{sprinkle_print_method} for fancyness.
#' @return A \code{data.frame} by default, otherwise \code{dust} object, depending on \code{print}.
#' @export
#' @family Tadaa-functions
#' @import stats
#' @examples
#' tadaa_aov(stunzahl ~ jahrgang, data = ngo)
#' tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo)
tadaa_aov <- function(formula, data = NULL, show_effect_size = TRUE, print = "df"){

  model <- broom::tidy(aov(formula = formula, data = data))

  if (show_effect_size) {
    resid             <- model$sumsq[nrow(model)]
    model$part.eta.sq <- model$sumsq / (resid + model$sumsq)
    model$part.eta.sq[nrow(model)] <- NA
    model$cohens.f    <- sqrt(model$part.eta.sq / (1 - model$part.eta.sq))
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
