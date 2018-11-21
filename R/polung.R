#' Polung für Testskalen
#'
#' @param x A vector of numric data.
#' @param min the minimum value of the scale.
#' @param max the maximum value of the scale.



pol <- function(x, min, max) {

  range <- max + min

  value <- (x * -1) + range

  return(value)
}
