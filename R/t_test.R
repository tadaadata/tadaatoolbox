#' Tadaa, t-Test!
#'
#' @param data A \code{data.frame}.
#' @param response The response variable (dependent).
#' @param group The group variable, usually a \code{factor}.
#' @param direction Test direction, like \code{alternative} in \link{t.test}.
#' @param na.rm If \code{TRUE} (default), missing values are dropped.
#' @param print Print method, default is \code{console}, can also bei \code{knitr} for
#' \code{pixidust}'s markdown print method.
#'
#' @return A \code{data.frame}, optionally markdown'd
#' @export
#'
#' @examples
#' df <- data.frame(x = runif(100), y = sample(c("A", "B"), 100, TRUE))
#' tadaa_t.test(df, "x", "y")
tadaa_t.test <- function(data, response, group, direction = "two.sided", na.rm = TRUE, print = "console") {
  # Check the type of the group
  if (is.factor(data[[group]])) {
    groups <- levels(data[[group]])
  } else {
    groups <- unique(data[[group]])
  }

  # Subset groups of response
  x <- data[data[[group]] == groups[1], ][[response]]
  y <- data[data[[group]] == groups[2], ][[response]]

  # Kick out NAs if specified
  if (na.rm) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }

  # Get n for each group
  n1   <- length(x)
  n2   <- length(y)

  # t.test
  test <- broom::tidy(t.test(x = x, y = y, direction = direction))
  names(test) <- c("Differenz", groups[1], groups[2], "t", "p", "df", "conf_low", "conf_high")

  # Additions
  test$d     <- effect_size_t(data = data, response = response, group = group)
  test$power <- pwr::pwr.t2n.test(n1 = n1, n2 = n2, d = test$d, alternative = direction)$power

  output <- pixiedust::dust(test)
  output <- pixiedust::sprinkle(output, col = c(1:4, 6:10), round = 3)
  output <- pixiedust::sprinkle(output, col = 5, fn = quote(pixiedust::pvalString(value)))

  # Return
  if (print == "console") {
    return(output)
  } else if (print == "knitr") {
    return(pixiedust::sprinkle_print_method(output, "markdown"))
  }
}
