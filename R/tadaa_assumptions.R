#' Tadaa, test for normality!
#'
#' @param data A \code{data.frame}.
#' @param method The type of test to perform. Either \code{ad} for Anderson Darling,
#' \code{shapiro} for Shapiro-Wilk, \code{pearson} for Pearson's chi-square test or
#' \code{ks} for Kolmogorov-Smirnov (not recommended).
#' @param ... Further arguments passed to test functions where applicable,
#' see \link[nortest]{pearson.test} and \link[stats]{ks.test}.
#' @inheritParams tadaa_aov
#' @return A \code{data.frame} by default, otherwise \code{dust} object, depending on \code{print}.
#' @import pixiedust
#' @import nortest
#' @import stats
#' @importFrom broom tidy
#' @importFrom nortest ad.test
#' @importFrom nortest pearson.test
#' @importFrom dplyr bind_rows
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
tadaa_normtest <- function(data, method = "ad", print = "df", ...){

  if (print == "df" & length(method) > 1 & length(method) <= 3) {
    res <- dplyr::bind_rows(lapply(method, function(x) {
      tadaa_normtest(data, method = x)
    }))
    return(res)
  }

  vars    <- names(data)
  results <- lapply(data, function(x){

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
  results          <- dplyr::bind_rows(results)
  results$variable <- as.character(vars)
  results          <- results[c(ncol(results), 1:(ncol(results) - 1))]

  if (print == "df") {
    return(results)
  } else {
    output <- pixiedust::dust(results)
    output <- pixiedust::sprinkle(output, col = "p.value", fn = quote(pval_string(value)))
    output <- pixiedust::sprinkle(output, round = 2)
  }

  if (!(print %in% c("df", "console", "html", "markdown"))) {
    stop("Print method must be 'df', 'console', 'html' or, 'markdown'")
  }

  return(pixiedust::sprinkle_print_method(output, print_method = print))

}

#' Levene's Test for Homoskedasticity
#'
#' A thin wrapper around \link[car]{leveneTest} with some formatting done.
#' @param data Data for the test
#' @param formula Formula specifiyng groups, passed to \code{leveneTest}.
#' @param center Method to use, either \code{median} (default for robustness) or \code{mean}.
#' @inheritParams tadaa_aov
#' @return A \code{data.frame} by default, otherwise \code{dust} object, depending on \code{print}.
#' @note The case of \code{center = "median"} is technically called \emph{Brown–Forsythe test},
#' so if that's selected the header for non-\code{df} returns will reflect that.
#' @export
#' @importFrom stats terms
#' @importFrom car leveneTest
#' @importFrom broom tidy
#' @import pixiedust
#' @family Tadaa-functions

#' @examples
#' tadaa_levene(ngo, deutsch ~ jahrgang, print = "console")
#' tadaa_levene(ngo, deutsch ~ jahrgang * geschl, print = "console")
tadaa_levene <- function(data, formula, center = "median", print = "df") {

  # Labels for groups
  factors <- attr(terms(formula), "term.labels")
  factors <- factors[attr(terms(formula), "order") == 1]
  groups  <- paste0(factors, collapse = ":")

  # Actual test
  test   <- broom::tidy(car::leveneTest(formula, data = data, center = center))

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
    output <- pixiedust::sprinkle_colnames(output,
                                           term      = "Term",
                                           statistic = "F",
                                           p.value   = "p")
    output <- pixiedust::sprinkle(output, round = 2, na_string = "")
  }

  if (!(print %in% c("df", "console", "html", "markdown"))) {
    stop("Print method must be 'df', 'console', 'html' or, 'markdown'")
  }

  return(pixiedust::sprinkle_print_method(output, print_method = print))
}
