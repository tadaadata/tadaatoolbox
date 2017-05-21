#' Extended Pairwise t-Tests
#'
#' This is an extension of \link[stats]{pairwise.t.test} that's meant to deal with
#' interactions our of the box, while also performing pairwise tests for the primary terms.
#' The output of the function is modeled after \link[stats]{TukeyHSD}, unfortunately
#' without confidence intervals or test statistic though.
#' @param data A \code{data.frame} containing the variables.
#' @param response The response variable, i.e. the dependent numeric vector.
#' @param group1 The grouping variables, typically a \code{factor}.
#' @param group2 (Optional) second grouping variable.
#' @param p.adjust The p-adjustment method, see \link[stats]{p.adjust.methods}, passed
#' to \link[stats]{pairwise.t.test}.
#' @param paired Defaults to \code{FALSE}, also passed to \link[stats]{pairwise.t.test}.
#' @param pool.sd Defaults to the inverse of \code{paired}, passed
#' to \link[stats]{pairwise.t.test}.
#' @param alternative Defaults to \code{two.sided}, also passed to \link[stats]{pairwise.t.test}.
#'
#' @importFrom broom tidy
#' @import stats
#' @return A \code{data.frame} with columns \code{term}, \code{comparison} and \code{adj.p.value}.
#' @export
#' @family Tadaa-functions
#' @note The adjustment method is applied within each \code{term}, meaning that the number of
#' pairwise t-tests counted for the adjustment is only equal to the number of rows
#' per \code{term} of the output.
#'
#' @examples
#' tadaa_pairwise_t(ngo, deutsch, jahrgang, geschl)
#'
tadaa_pairwise_t <- function(data, response, group1, group2 = NULL,
                       p.adjust = "bonf", paired = FALSE, pool.sd = !paired,
                       alternative = "two.sided") {

  response <- deparse(substitute(response))
  group1   <- deparse(substitute(group1))
  group2   <- deparse(substitute(group2))


  tests <- stats::pairwise.t.test(x = data[[response]],
                                  g = data[[group1]],
                                  p.adjust.method = p.adjust,
                                  pool.sd = pool.sd,
                                  paired = paired,
                                  alternative = alternative)
  tests      <- broom::tidy(tests)
  tests$term <- group1

  if (group2 != "NULL") {
    data[["interaction"]] <- interaction(data[[group1]], data[[group2]], sep = " & ")

    tests_int <- stats::pairwise.t.test(x = data[[response]],
                                        g = data[["interaction"]],
                                        p.adjust.method = p.adjust,
                                        pool.sd = pool.sd,
                                        paired = paired,
                                        alternative = alternative)

    tests_int      <- broom::tidy(tests_int)
    tests_int$term <- paste0(group1, ":", group2)

    tests_g2 <- stats::pairwise.t.test(x = data[[response]],
                                       g = data[[group2]],
                                       p.adjust.method = p.adjust,
                                       pool.sd = pool.sd,
                                       paired = paired,
                                       alternative = alternative)

    tests_g2      <- broom::tidy(tests_g2)
    tests_g2$term <- group2

    tests <- rbind(tests, tests_g2, tests_int)
  }

  tests$comparison  <- paste0(tests$group1, " - ", tests$group2)
  tests$adj.p.value <- tests$p.value
  rownames(tests)   <- NULL

  tests[c("term", "comparison", "adj.p.value")]
}
