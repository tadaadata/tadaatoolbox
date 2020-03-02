#' Convenience functions for interval recodes
#'
#' Get recode assigments for even intervals of discrete numeric values compatible
#' with [car::recode].
#'
#' @param from,to A `numeric value` for the beginning and the end of the interval.
#' @param width The width of the interval, e.g. 5 (default) for intervals 0-5.
#'
#' @return A `character` vector of recode assignments compatible with [car::recode].
#' @export
#' @examples
#' \dontrun{
#' x <- round(runif(100, 0, 100), 0)
#' recodes <- generate_recodes(0, 100, 10)
#'
#' library(car)
#' recode(x, recodes = recodes)
#' }
generate_recodes <- function(from, to, width = 5) {
  paste(sapply(seq(from, to, 2 * width), function(x) {
    first <- paste0(x, ":", (x - 1) + width, "='", x, "-", (x - 1) + width, "'")
    second <- paste0(x + width, ":", x + (2 * width) - 1, "='", x + width, "-", x + (2 * width) - 1, "'")
    paste(first, second, sep = "; ", collapse = "; ")
  }), collapse = "; ")
}

#' Convenience functions for interval recodes
#'
#' Get interval labels for even intervals of discrete numeric values compatible
#' with [base::cut].
#'
#' @param from,to A `numeric value` for the beginning and the end of the interval.
#' @param width The width of the interval, e.g. 5 (default) for intervals 0-5.
#'
#' @return A `character` vector of interval labels compatible with [base::cut].
#' @export
#' @examples
#' \dontrun{
#' x <- round(runif(100, 0, 100), 0)
#' labels <- interval_labels(0, 100, 10)
#'
#' cut(x, breaks = seq(0, 100, 10), labels = labels)
#' }
interval_labels <- function(from, to, width = 5) {
  labs <- lapply(seq(from, to - width, 2 * width), function(x) {
    c(paste0(x, "-", x + width - 1), paste0(x + width, "-", x + (2 * width) - 1))
  })
  return(unlist(labs))
}
