#' Interaction plots
#'
#' @param data A \code{data.frame}
#' @param response Response variable
#' @param group1 First grouping variable
#' @param group2 Second grouping variable
#'
#' @return A list with two ggplot2 objects
#' @export
#' @examples
#' tadaa_int(qmtut::ngo, stunzahl, jahrgang, geschl)
tadaa_int <- function(data, response, group1, group2){

  sdots <- lazyeval::interp(~mean(variable, na.rm = T), variable = substitute(response))

  data <- dplyr::group_by_(data, substitute(group1), substitute(group2))
  data <- dplyr::summarize_(data, .dots = list(mw = sdots))
 return(data)

}