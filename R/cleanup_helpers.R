#' Delete cases with set amount of missing values
#'
#' @param df A \code{data.frame},
#' @param n Number of \code{NAs} allowed, defaults to \code{ncol(df) - 1}.
#'
#' @return A filtered version of the input \code{data.frame}.
#' @export
#' @note Adapted from \url{http://stackoverflow.com/a/30461945/409362}.
#' @examples
#' \dontrun{
#' df <- delete_na(df)
#' }
delete_na <- function(df, n = ncol(df) - 1) {
  log      <- apply(df, 2, is.na)
  logindex <- apply(log, 1, function(x) sum(x) <= n)

  return(df[logindex, ])
}
