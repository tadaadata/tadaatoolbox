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
    res <- bind_rows(lapply(method, function(x) {
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
    output <- pixiedust::sprinkle(output, col = 3, fn = quote(pval_string(value)))
    output <- pixiedust::sprinkle(output, round = 3)

    if (!(print %in% c("df", "console", "html", "markdown"))) {
      stop("Print method must be 'df', 'console', 'html' or, 'markdown'")
    }

    return(pixiedust::sprinkle_print_method(output, print_method = print))
  }
}
