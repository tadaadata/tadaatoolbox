#' Extended Pairwise t-Tests
#'
#' This is an extension of [stats::pairwise.t.test] that's meant to deal with
#' interactions our of the box, while also performing pairwise tests for the primary terms.
#' The output of the function is modeled after [stats::TukeyHSD], unfortunately
#' without confidence intervals or test statistic though.
#' @param data A `data.frame` containing the variables.
#' @param response The response variable, i.e. the dependent numeric vector.
#' @param group1 The grouping variables, typically a `factor`.
#' @param group2 (Optional) second grouping variable.
#' @param p.adjust The p-adjustment method, see [stats::p.adjust.methods], passed
#' to [stats::pairwise.t.test]. Additionally, `sidak` is supported as a
#' method, which is not the case with [stats::p.adjust], as is `sidakSD` for the
#' Sidak step-down procedure.
#' @param paired Defaults to `FALSE`, also passed to [stats::pairwise.t.test].
#' @param pool.sd Defaults to the inverse of `paired`, passed
#' to [stats::pairwise.t.test].
#' @param alternative Defaults to `two.sided`, also passed to [stats::pairwise.t.test].
#' @param print Print method, defaults to `df` for `data.frame` output, otherwise
#' passed to [pixiedust::sprinkle_print_method].
#' @importFrom broom tidy
#' @import pixiedust
#' @import stats
#' @return A `data.frame` with columns `term`, `comparison` and `adj.p.value`.
#' @export
#' @family Tadaa-functions
#' @seealso [tadaa_pairwise_tukey()]
#' @note The adjustment method is applied within each `term`, meaning that the number of
#' pairwise t-tests counted for the adjustment is only equal to the number of rows
#' per `term` of the output.
#' The additional Sidak adjustment method uses the following method:
#' `p_adj <- 1 - pbinom(q = 0, size = length(p_values), prob = p_values)`
#' And is sometimes preferred over Bonferroni.
#' The Sidak-like (1987) step-down procedure (`sidakSD`) is an improvement
#' over the Holm's (1979) step-down procedure.
#'
#' @references https://stats.stackexchange.com/questions/20825/sidak-or-bonferroni
#' @references https://rdrr.io/rforge/mutoss/man/SidakSD.html
#' @examples
#' tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "none", print = "console")
#' tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "bonf", print = "console")
#' tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl, p.adjust = "sidak", print = "console")
tadaa_pairwise_t <- function(data, response, group1, group2 = NULL,
                             p.adjust = "bonf", paired = FALSE, pool.sd = !paired,
                             alternative = "two.sided", print = "df") {
  response <- deparse(substitute(response))
  group1 <- deparse(substitute(group1))
  group2 <- deparse(substitute(group2))

  # This is a little bit of a workaround because of… variables.
  # Since p.adjust is passed to pairwise.t.test this is necessary since
  # we need that function to not perform any adjustments so we
  # can apply the Sidak methods afterwards
  if (p.adjust == "sidak") {
    p.adjust <- "none"
    use_sidak <- TRUE
  } else {
    use_sidak <- FALSE
  }

  if (p.adjust == "sidakSD") {
    p.adjust <- "none"
    use_sidakSD <- TRUE
  } else {
    use_sidakSD <- FALSE
  }


  tests <- stats::pairwise.t.test(
    x = data[[response]],
    g = data[[group1]],
    p.adjust.method = p.adjust,
    pool.sd = pool.sd,
    paired = paired,
    alternative = alternative
  )
  tests <- broom::tidy(tests)
  tests$term <- group1

  if (use_sidak) {
    tests$p.value <- 1 - pbinom(q = 0, size = nrow(tests), prob = tests$p.value)
  } else if (use_sidakSD) {
    tests$p.value <- sidak_sd(tests$p.value)
  }

  if (group2 != "NULL") {
    data[["interaction"]] <- interaction(data[[group1]], data[[group2]], sep = " & ")

    tests_int <- stats::pairwise.t.test(
      x = data[[response]],
      g = data[["interaction"]],
      p.adjust.method = p.adjust,
      pool.sd = pool.sd,
      paired = paired,
      alternative = alternative
    )

    tests_int <- broom::tidy(tests_int)
    tests_int$term <- paste0(group1, ":", group2)

    if (use_sidak) {
      tests_int$p.value <- 1 - pbinom(
        q = 0, size = nrow(tests_int),
        prob = tests_int$p.value
      )
    } else if (use_sidakSD) {
      tests_int$p.value <- sidak_sd(tests_int$p.value)
    }

    tests_g2 <- stats::pairwise.t.test(
      x = data[[response]],
      g = data[[group2]],
      p.adjust.method = p.adjust,
      pool.sd = pool.sd,
      paired = paired,
      alternative = alternative
    )

    tests_g2 <- broom::tidy(tests_g2)
    tests_g2$term <- group2

    if (use_sidak) {
      tests_g2$p.value <- 1 - pbinom(
        q = 0, size = nrow(tests_g2),
        prob = tests_g2$p.value
      )
    } else if (use_sidakSD) {
      tests_g2$p.value <- sidak_sd(tests_g2$p.value)
    }

    tests <- rbind(tests, tests_g2, tests_int)
  }

  tests$comparison <- paste0(tests$group1, " - ", tests$group2)
  tests$adj.p.value <- tests$p.value
  rownames(tests) <- NULL

  test <- tests[c("term", "comparison", "adj.p.value")]

  if (print == "df") {
    return(test)
  } else {
    output <- pixiedust::dust(test)
    output <- pixiedust::sprinkle(output, cols = "adj.p.value", fn = quote(tadaatoolbox::pval_string(value)))
    output <- pixiedust::sprinkle_colnames(output, adj.p.value = "p (adj.)")
    # output <- pixiedust::sprinkle_table(output, cols = 1, caption = "", part = "head")
  }

  if (!(print %in% c("df", "console", "html", "markdown"))) {
    stop("Print method must be 'df', 'console', 'html' or, 'markdown'")
  }

  return(pixiedust::sprinkle_print_method(output, print_method = print))
}

#' Sidak step-down procedure
#' @references https://github.com/Bioconductor-mirror/multtest/blob/master/R/mt.basic.R#L81-L88
#' @keywords internal
sidak_sd <- function(pvals) {
  m <- length(pvals)
  m.good <- sum(!is.na(pvals))
  index <- order(pvals)
  pvals_s <- pvals[index]
  tmp <- pvals_s
  tmp[1] <- 1 - (1 - pvals_s[1])^m.good

  for (i in 2:m) {
    tmp[i] <- max(tmp[i - 1], 1 - (1 - pvals_s[i])^(m.good - i + 1))
  }

  tmp[order(index)]
}
