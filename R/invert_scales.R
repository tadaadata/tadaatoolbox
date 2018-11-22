#' Inverting scales
#'
#' @param x A vector of numeric data.
#' @param min The minimum value of the scale.
#' @param max The maximum value of the scale.
#' @export
#' @examples
#' # Assuming you have a Likert-scale from 1 to 9
#' x <- c(4, 5, 2, 3, 7, 8, 3)
#' inv(x, 1, 9)
inv <- function(x, min, max) {

  range <- max + min

  value <- (x * -1) + range

  return(value)
}
