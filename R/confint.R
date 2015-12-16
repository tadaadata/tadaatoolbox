#' Confidence Intervals
#'
#' @param data A \code{data.frame}
#' @param alpha Alpha, default is \code{0.05}
#' @param na.rm If \code{TRUE} (default), missing values are dropped.
#'
#' @return \code{numeric} of length one (size of CI in one direction)
#' @export
#'
#' @examples
#' df <- data.frame(x = runif(100), y = sample(c("A", "B"), 100, TRUE))
#' confint_t(df$x)
confint_t <- function(data, alpha = 0.05, na.rm = TRUE){
  if (!is.numeric(data)) {
    stop("Data is not a numeric vector")
  }

  alpha    <- alpha/2
  sd       <- sd(data, na.rm = na.rm)
  w_krit   <- qt(1 - (alpha), df = (length(data) - 1))
  ret      <- w_krit * sd/sqrt(length(data))

  return(ret)
}
