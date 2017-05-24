#' Tadaa, ANOVA!
#'
#' @param formula Formula for model, passed to \code{aov}.
#' @param data Data for model.
#' @param show_effect_size If \code{TRUE} (default), effect sizes partial eta^2 and Cohen's f are appended as columns.
#' @param factorize If \code{TRUE} (default), non-\code{factor} independent variables
#' will automatically converted via \code{as.factor}, so beware of your inputs.
#' @param type Which type of SS to use. Default is \code{1}, can also be \code{2} oder \code{3}.
#' @param print Print method, per default a regular \code{data.frame}.
#' Otherwise passed to \link[pixiedust]{sprinkle_print_method} for fancyness.
#' @return A \code{data.frame} by default, otherwise \code{dust} object, depending on \code{print}.
#' @export
#' @family Tadaa-functions
#' @import stats
#' @importFrom lsr etaSquared
#' @import methods
#' @examples
#' tadaa_aov(stunzahl ~ jahrgang, data = ngo)
#' tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo)
#'
#' # Other types of sums
#' tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo, type = 2, print = "console")
#' tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo, type = 3, print = "console")
tadaa_aov <- function(formula, data = NULL, show_effect_size = TRUE,
                      factorize = TRUE, type = 1, print = "df"){

  # Checks
  if (factorize) {
    terms  <- stats::terms(formula)
    orders <- attr(terms, "order")
    vars   <- attr(terms, "term.labels")[orders == 1]

    if (!all(sapply(data[vars], is.factor))) {
      non_factors <- vars[!sapply(data[vars], is.factor)]
      warning("Some independent variables are not factors, auto-converting...")

      for (var in non_factors) {

        data[[var]] <- as.factor(data[[var]])

        warning("Converting ", var, " to factor, please check your results")
      }
    }
  }

  # Model fitting
  base_model <- stats::aov(formula = formula, data = data)
  effects    <- lsr::etaSquared(base_model, type = type)

  if (type %in% c(2, 3)) {
    base_model <- car::Anova(base_model, type = type)
  }

  model <- broom::tidy(base_model)

  if (type %in% c(2, 3)) {
    model$meansq <- model$sumsq/model$df
    model <- model[c("term", "df", "sumsq", "meansq", "statistic", "p.value")]
  }
  if (type == 3) {
    # remove intercept row
    model <- model[-1, ]
  }

  # Put Residual row at the bottom
  model <- rbind(model[model$term != "Residuals", ],
                 model[model$term == "Residuals", ])

  if (show_effect_size) {
    effects          <- data.frame(term = rownames(effects), effects, row.names = NULL)
    effects$cohens.f <- sqrt(effects$eta.sq.part / (1 - effects$eta.sq.part))

    # Drop eta.sq from output, as partial eta^2 suffices
    effects <- effects[names(effects) != "eta.sq"]

    # Merge with test output
    model <- merge(model, effects, by = "term", all = T)
  }

  if (print == "df") {
    return(model)
  } else {
    sstype <- switch(type, "1" = "I", "2" = "II", "3" = "III")

    output <- pixiedust::dust(model,
                              caption = paste("**ANOVA using Type", sstype, "Sum of Squares**"))
    output <- pixiedust::sprinkle(output, col = "p.value", fn = quote(pval_string(value)))
    output <- pixiedust::sprinkle_colnames(output, term = "Term",
                                           sumsq = "SS",
                                           meansq = "MS",
                                           statistic = "F",
                                           p.value = "p",
                                         # eta.sq = "$\\eta^2$",
                                           eta.sq.part = "$\\eta_\\text{part}^2$",
                                           cohens.f = "Cohen's f")
    output <- pixiedust::sprinkle(output, round = 2, na_string = "")
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
