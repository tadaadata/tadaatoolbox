#' Easily cluster numeric vectors in likert-like classes
#'
#' @param x Vector to be clustered.
#' @param classes Number of classes. Defaults to 3, can also be 5.
#' @param method How should the classes be calculated? Defaults to \code{quantiles}, can also be
#' \code{means} for mean and standard deviation.
#' @return An ordered \code{factor} with \code{classes} levels. And descriptive labels.
#' @importFrom car recode
#' @export
#' @examples
#' tadaa_likertize(x = runif(100, 0, 10), classes = 3, method = "quantiles")
#' tadaa_likertize(x = runif(100, 0, 10), classes = 3, method = "meansd")
tadaa_likertize <- function(x, classes = 3, method = "quantiles"){

  if (classes == 3) {
    if (method == "quantiles") {
      quantiles <- quantile(x, (1:2) / classes)
      recodes <- paste0("lo:", quantiles[[1]], " = 1; ",
                        quantiles[[1]], ":", quantiles[[2]], " = 2; ",
                        quantiles[[2]], ":hi = 3")
    } else if (method == "meansd") {
      recodes <- paste0("lo:", mean(x) - sd(x), " = 1; ",
                        mean(x) -  sd(x), ":", mean(x) + sd(x), " = 2; ",
                        mean(x) + sd(x), ":hi = 3")
    }
      xx <- car::recode(x, recodes = recodes)
      xx <- factor(xx, labels = c("niedrig", "mittel", "hoch"), ordered = TRUE)
      return(xx)
  } else if (classes == 5) {
    stop("not yet implemented")
  }
}