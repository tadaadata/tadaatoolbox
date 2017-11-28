#' Simple Effect Size Calculation for t-Tests
#'
#' Calculates Cohen's d for two sample comparisons.
#' @param data A `data.frame`.
#' @param response The response variable (dependent).
#' @param group The group variable, usually a `factor`.
#' @param absolute If set to `TRUE`, the absolute effect size is returned.
#' @param paired Whether the effect should be calculated for a paired
#' t-test, default is `FALSE`.
#' @param na.rm If `TRUE` (default), missing values are dropped.
#' @return `numeric` of length 1.
#' @export
#' @details The effect size here is Cohen's d as calculated by
#' \eqn{d = \frac{m_{diff}}{S_p}}, where \eqn{m_{diff} = \bar{x}_1 - \bar{x}_2} and
#' \eqn{S_p =
#'   \sqrt{
#'     \frac{n_1 - 1 \cdot {s_{x_1}}^2 + n_2 - 1 \cdot {s_{x_2}}^2}
#'     {n_1 + n_2 - 2}
#'   }
#'}.
#'
#' For `paired = TRUE`, \eqn{S_p} is substituted by \eqn{S_D = S_{x_1 - x_2}}
#' via `sd(x1 - x2)`.
#' @import stats
#' @examples
#' set.seed(42)
#' df <- data.frame(x = runif(100), group = sample(c("A", "B"), 100, TRUE))
#' effect_size_t(df, "x", "group")
effect_size_t <- function(data, response, group, absolute = FALSE,
                          paired = FALSE, na.rm = TRUE){

  # Handle NSE
  # if (is.name(substitute(response)) & is.name(substitute(group))) {
  #   response <- deparse(substitute(response))
  #   group    <- deparse(substitute(group))
  # }

  # Check the type of the group
  if (is.factor(data[[group]])) {
    groups <- levels(data[[group]])
  } else {
    groups <- unique(data[[group]])
  }

  # Subset groups of response
  x <- data[data[[group]] == groups[1], ][[response]]
  y <- data[data[[group]] == groups[2], ][[response]]

  # Get stats for each group
  n1   <- length(x)
  var1 <- var(x, na.rm = na.rm)

  n2   <- length(y)
  var2 <- var(y, na.rm = na.rm)

  # Calculate pooled variance and difference of means
  if (paired) {
    s    <- sd(x - y)
  } else {
    s    <- sqrt(sum((n1 - 1) * var1, (n2 - 1) * var2)/((n1 + n2) - 2))
  }
  m_d  <- mean(x, na.rm = na.rm) - mean(y, na.rm = na.rm)

  # Get effect size
  d    <- m_d/s

  if (absolute) {
    d <- abs(d)
  }

  return(d)
}
