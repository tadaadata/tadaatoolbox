#' Modus
#'
#' Calculate the mode of a numeric vector. German name kept to avoid confusion.
#'
#' @param x A `vector` with numeric data.
#' @param reduce Since `mode` can be of length > 1, this option pastes the result into a
#' single character value.
#' @param as_character Always return a character. `TRUE` by default, or [dplyr::summarize].
#' will be very unpleased.
#' @return A `vector` of length 1 of type `numeric` or `character`, depending
#' on input.
#' @export
#' @examples
#' \dontrun{
#' x <- c(1, 2, 6, 2, 1, 5, 7, 8, 4, 3, 2, 2, 2)
#' modus(x)
#'
#' # Or for nominal data
#' x <- structure(c(2L, 1L, 2L, 2L, 2L, 1L), .Label = c("Ja", "Nein"), class = "factor")
#' modus(x)
#'}
modus <- function(x, as_character = TRUE, reduce = TRUE){
  mode <- names(table(x)[table(x) == max(table(x))])

  if (reduce & length(mode) > 1) {
    mode <- as.character(paste(mode, collapse = "; "))
    return(mode)
  }

  # Check if the value can be coerced to numeric or if it's a character
  if (suppressWarnings(is.na(as.numeric(mode)))) {
    return(mode)
  } else if (as_character) {
    return(as.character(mode))
  } else {
    return(as.numeric(mode))
  }
}

#' Convert numeric vector to z-values
#'
#' A trivial scaling function. You might as well use [base::scale], which allows
#' arbitrary centers and scales, but returns a `matrix` by default.
#' @param x A numeric vector.
#'
#' @return A vector of z-values of the same length as `x`.
#' @export
#' @importFrom stats sd
#'
#' @examples
#' x      <- rnorm(500, mean = 10, sd = 5)
#' z_vals <- z(x)
#' round(c(mean = mean(z_vals), sd = sd(z_vals)), 2)
z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
}
