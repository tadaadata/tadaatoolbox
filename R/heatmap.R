#' Heatmaps
#'
#' Easily generate heatmaps of a continuous variable grouped by two categorical variables.
#' @param data A \code{data.frame}
#' @param heat The continuous variable displayed by the heat-tiles
#' @param x The variable on the x-axis
#' @param group The grouping variable (y-axis)
#' @return A ggplot2 object
#' @export
#' @family Tadaa-functions
#' @import ggplot2
#' @examples
#' tadaa_heatmap(ngo, stunzahl, leistung, jahrgang)
tadaa_heatmap <- function(data = NULL, heat, x, group) {
  !missing(heat)  || stop("heat not specified")
  !missing(x)     || stop("x not specified")
  !missing(group) || stop("group not specified")

  map <- ggplot(data = data, aes_string(x = substitute(x),
                                        y = substitute(group),
                                        fill = substitute(heat))) +
           geom_tile(color = "white", size = 0.1) +
           labs(title = paste("Heatmap for", substitute(heat),
                              "by", substitute(x), "and", substitute(group))) +
           scale_x_discrete() +
           # color plattes getting added later on:
           # scale_fill_brewer(palette = ...) +
           coord_equal() +
           theme(legend.position = "bottom")

  return(map)
}
