#' Modus
#'
#' Calculate the mode of a numeric vector. German name kept to avoid confusion.
#'
#' @param x A \code{vector} with numeric data.
#' @param reduce Since \code{mode} can be of length > 1, this option pastes the result into a
#' single character value
#' @param na.rm If \code{TRUE}, missing values (\code{NA}) will be ignored.
#'
#' @return A \code{vector} of length 1 of type \code{numeric} or \code{character}, depending
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
modus <- function(x, reduce = TRUE, na.rm = FALSE){
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  mode <- names(table(x)[table(x) == max(table(x))])

  if (reduce & length(mode) > 1) {
    mode <- paste(mode, collapse = "; ")
  }

  # Check if the value can be coerced to numeric or if it's a character
  if (suppressWarnings(is.na(as.numeric(mode)))) {
    return(mode)
  } else {
    return(as.numeric(mode))
  }
}
