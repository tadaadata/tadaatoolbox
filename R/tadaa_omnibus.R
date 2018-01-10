#' Tadaa, ANOVA!
#'
#' Performs one-, two-way or factorial ANOVA with adjustable sums of squares method and
#' optionally displays effect sizes ((partial) \eqn{\eta^2}, Cohen's f) and
#' power (calculated via [pwr::pwr.f2.test] to work with unbalanced designs).
#'
#' @details
#' If a specified independent variable is not properly encoded as a `factor`, it is
#' automatically converted if `factorize = TRUE` to ensure valid results.
#'
#' If `type = 3` and `check_contrasts = TRUE`, the `"contrasts"` of
#' each non-ordered factor will be checked and set to `contr.sum` to ensure the function
#' yields usable results. It is highly recommended to only use `check_contrasts = FALSE`
#' for debugging or educational purposes, or of you know what you're doing and using
#' your own contrast matrix.
#' @param formula Formula for model, passed to `aov`.
#' @param data Data for model.
#' @param show_effect_size If `TRUE` (default), effect sizes
#' partial eta^2 and Cohen's f are appended as columns.
#' @param show_power (Experimental) If `TRUE` (default), power is calculated
#' via [pwr::pwr.f2.test] and appended as a column.
#' @param factorize If `TRUE` (default), non-`factor` independent variables
#' will automatically converted via `as.factor`, so beware of your inputs.
#' @param type Which type of SS to use. Default is `3`, can also be `1` or `2`.
#' @param check_contrasts Only applies to `type = 3`. If `TRUE` (default),
#' the `contrasts` of each non-ordered `factor` are set to `"contr.sum"`.
#' @param print Print method, default `df`: A regular `data.frame`.
#' Otherwise passed to [pixiedust::sprinkle_print_method] for fancyness.
#' @return A `data.frame` by default, otherwise `dust` object, depending on `print`.
#' @export
#' @family Tadaa-functions
#' @import methods
#' @import stats
#' @import pixiedust
#' @importFrom DescTools  EtaSq
#' @importFrom pwr pwr.f2.test
#' @importFrom car Anova
#' @importFrom broom tidy
#' @examples
#' tadaa_aov(stunzahl ~ jahrgang, data = ngo)
#' tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo)
#'
#' # Other types of sums and print options
#' \dontrun{
#' tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo, type = 1, print = "console")
#' tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo, type = 3, print = "console")
#' tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo,
#'           type = 3, check_contrasts = FALSE, print = "console")
#' }
tadaa_aov <- function(formula, data = NULL, show_effect_size = TRUE, show_power = TRUE,
                      factorize = TRUE, type = 3, check_contrasts = TRUE,
                      print = c("df", "console", "html", "markdown")) {
  print <- match.arg(print)

  # Checks
  terms <- stats::terms(formula)
  orders <- attr(terms, "order")
  vars <- attr(terms, "term.labels")[orders == 1]

  if (factorize) {
    if (!all(sapply(data[vars], is.factor))) {
      non_factors <- vars[!sapply(data[vars], is.factor)]
      warning("Some independent variables are not factors, auto-converting...")

      for (var in non_factors) {
        data[[var]] <- as.factor(data[[var]])

        warning("Converting ", var, " to factor, please check your results")
      }
    }
  }

  if (check_contrasts & type == 3) {
    non_ordered <- vars[!sapply(data[vars], is.ordered)]

    for (var in non_ordered) {
      contrasts(data[[var]]) <- contr.sum(n = levels(data[[var]]))

      # if (interactive()) {
      #   message("Setting contrasts of ", var, " to 'contr.sum'")
      # }
    }
  }

  # Model fitting
  base_model <- stats::aov(formula = formula, data = data)
  effects    <- DescTools::EtaSq(base_model, type = type)

  if (type %in% c(2, 3)) {
    base_model <- car::Anova(base_model, type = type)
  }

  model <- broom::tidy(base_model)

  if (type %in% c(2, 3)) {
    model$meansq <- model$sumsq / model$df
    model <- model[c("term", "df", "sumsq", "meansq", "statistic", "p.value")]
  }
  if (type == 3) {
    # remove intercept row
    model <- model[model$term != "(Intercept)", ]
  }

  # Put interactions below factors
  model <- rbind(
    model[!grepl(":", model$term), ],
    model[grepl(":", model$term), ]
  )

  # Put Residual row at the bottom
  model <- rbind(
    model[model$term != "Residuals", ],
    model[model$term == "Residuals", ]
  )

  # Append Total row
  totals <- data.frame("term" = "Total", lapply(model[-1], sum), stringsAsFactors = FALSE)
  model <- rbind(model, totals)

  if (show_effect_size) {
    effects <- data.frame(term = rownames(effects), effects, row.names = NULL)
    effects$cohens.f <- sqrt(effects$eta.sq.part / (1 - effects$eta.sq.part))

    # Drop eta.sq from output, as partial eta^2 suffices
    effects <- effects[names(effects) != "eta.sq"]

    # Merge with test output
    model <- merge(model, effects, by = "term", all = T)
  }
  if (show_power) {
    fctr_rows <- !(model$term %in% c("Residuals", "Total"))

    model$power[fctr_rows] <- pwr::pwr.f2.test(
      u = model$df[fctr_rows],
      v = model$df[model$term == "Residuals"],
      f2 = model$cohens.f[fctr_rows] ^ 2
    )$power
  }

  if (print == "df") {
    return(model)
  } else {
    sstype <- switch(type, "1" = "I", "2" = "II", "3" = "III")

    # Guess ANOVA type based on number of factors, assuming only 1 response
    # Length coerced to character for switch()'s default result to work
    n_factors <- as.character(length(all.vars(formula)) - 1)
    ways <- switch(n_factors, "1" = "One-Way", "2" = "Two-Way", "Factorial")

    method <- paste0("**", ways, " ANOVA**: Using Type ", sstype, " Sum of Squares")

    # If model has 3 terms (1 factor + resid + total), display part.eta as eta^2,
    # otherwise display as part.eta. Since part.eta == eta^2 for one-way designs,
    # this should be okay I guess
    eta_label <- ifelse(nrow(model) == 3, "$\\eta^2$", "$\\eta_\\text{part}^2$")

    # Extract Residuals and Totals and put them in the footer explicitly
    footer <- model[model$term %in% c("Residuals", "Total"), ]
    model <- model[!(model$term %in% c("Residuals", "Total")), ]

    output <- pixiedust::dust(model, caption = method)
    output <- pixiedust::redust(output, footer, part = "foot")
    output <- pixiedust::sprinkle(output, rows = 1, border = "top", part = "foot")
    output <- pixiedust::sprinkle_table(output, halign = "center", part = "head")
    output <- pixiedust::sprinkle(output, col = "p.value", fn = quote(tadaatoolbox::pval_string(value)))
    output <- pixiedust::sprinkle_colnames(
      output,
      term = "Term",
      sumsq = "SS",
      meansq = "MS",
      statistic = "F",
      p.value = "p"
    )
    if (show_effect_size) {
      output <- pixiedust::sprinkle_colnames(
        output,
        eta.sq.part = eta_label,
        cohens.f = "Cohen's f"
      )
    }
    if (show_power) {
      output <- pixiedust::sprinkle_colnames(output, power = "Power")
    }
    output <- pixiedust::sprinkle_table(output, round = 2, na_string = "", part = "table")
  }

  pixiedust::sprinkle_print_method(output, print_method = print)
}

#' Tadaa, Kruskal-Wallis!
#'
#' @param formula Formula for model, passed to `kruskal.test`.
#' @param data Data for model.
#' @param print Print method, per default a regular `data.frame`.
#' Otherwise passed to [pixiedust::sprinkle_print_method] for fancyness.
#' @return A `data.frame` by default, otherwise `dust` object, depending on `print`.
#' @export
#' @family Tadaa-functions
#' @import stats
#' @examples
#' tadaa_kruskal(stunzahl ~ jahrgang, data = ngo)
tadaa_kruskal <- function(formula, data = NULL, print = c("df", "console", "html", "markdown")) {
  print <- match.arg(print)

  model <- broom::tidy(kruskal.test(formula = formula, data = data))
  model <- model[c("statistic", "parameter", "p.value")]

  if (print == "df") {
    return(model)
  } else {
    method <- "**Kruskal-Wallis Rank Sum Test**"

    output <- suppressWarnings(pixiedust::dust(model, caption = method))
    output <- pixiedust::sprinkle_table(output, halign = "center", part = "head")
    output <- pixiedust::sprinkle_colnames(
      output, statistic = "$\\chi^2$",
      p.value = "p", parameter = "df"
    )
    output <- pixiedust::sprinkle(output, col = "p.value", fn = quote(tadaatoolbox::pval_string(value)))
    output <- pixiedust::sprinkle(output, round = 2)
  }

  pixiedust::sprinkle_print_method(output, print_method = print)
}
