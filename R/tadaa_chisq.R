#' Tadaa, Chi-Square Test!
#'
#' A comfortable wrapper of \\link[stats]{chisq.test} with pretty output and
#' effect sizes depending on the size of the contingency table:
#' Phi coefficient and Odds Ratios in case of a 2x2 table, Cramer's V otherwise.
#' The result is either returned as a \link[broom]{tidy} \code{data.frame} or prettified using
#' various \link[pixiedust]{sprinkle} shenanigans.
#'
#' @param data A \code{data.frame}.
#' @param x A vector of categorial data (\code{factor} or \code{character}).
#' @param y Another vector of categorial data (also \code{factor} or \code{character}).
#' @param correct Apply Yate's continuity correction for 2x2 tables,
#' passed to \link[stats]{chisq.test}. Defaults to \code{TRUE}.
#' @param print Print method, default \code{df}: A regular \code{data.frame}.
#' Otherwise passed to \link[pixiedust]{sprinkle_print_method} for fancyness.
#' @return A \code{data.frame} by default, otherwise \code{dust} object,
#' depending on \code{print}.
#' @import pixiedust
#' @import stats
#' @family Tadaa-functions
#' @export
#' @examples
#' tadaa_chisq(ngo, abschalt, geschl)
#'
#' tadaa_chisq(ngo, abschalt, jahrgang)
tadaa_chisq <- function(data, x, y, correct = TRUE,
                        print = c("df", "console", "html", "markdown")) {

  print <- match.arg(print)

  x <- deparse(substitute(x))
  y <- deparse(substitute(y))

  tbl <- table(data[[x]], data[[y]])

  test <- broom::tidy(
    chisq.test(data[[x]], data[[y]], correct = correct)
  )

  if (length(tbl) == 4) {
    test$OR  <- (tbl[1] / tbl[3]) / (tbl[2] / tbl[4])
    test$phi <- nom_phi(tbl)
  } else {
    test$cramers <- nom_v(tbl)
  }

  if (print == "df") {
    return(test)
  } else {
    caption     <- paste0("**", test$method, "**")
    test$method <- NULL

    output <- pixiedust::dust(test, caption = caption)
    output <- pixiedust::sprinkle_table(output, halign = "center", part = "head")
    output <- pixiedust::sprinkle_colnames(output,
                                           statistic = paste("$\\chi^2$"),
                                           p.value   = "p",
                                           parameter = "df")

    if (is.null(test$cramers)) {
      output <- pixiedust::sprinkle_colnames(output,
                                             OR  = "Odds Ratio",
                                             phi = "$\\phi$")
    } else {
      output <- pixiedust::sprinkle_colnames(output,
                                             cramers = "Cramer\\'s V")
    }

    output <- pixiedust::sprinkle(output, cols = "p.value", fn = quote(pval_string(value)))
    output <- pixiedust::sprinkle(output, round = 2)
    output <- pixiedust::sprinkle_print_method(output, print_method = print)

    return(output)
  }
}
