#' Heatmaps
#'
#' Easily generate heatmaps of a continuous variables grouped by two categorical variables.
#' @param data A \code{data.frame}
#' @param heat The continuous variable displayed by the heat-tiles
#' @param x The variable on the x-axis
#' @param group The grouping variable (y-axis)
#' @param grid If \code{TRUE}, the resulting graphs will be arranged in a grid via \link[cowplot]{plot_grid}.
#' @return A ggplot2 object
#' @export
#' @family Tadaa-functions
#' @import ggplot2
#' @examples
#' tadaa_heatmap(ngo, stunzahl, jahrgang)
tadaa_heatmap <- function(data, heat, x, group) {
  map <- ggplot(data = data, aes_string(x = substitute(x), y = substitute(group), fill = substitute(heat))) +
           geom_tile(color = "white", size = 0.1) +
           labs(title = paste("Heatmap for", substitute(heat),
                              "by", substitute(x), "and", substitute(group))) +
           # need to find a way to detect the breaks (e.g. length of x) without breaking the function:
           scale_x_continuous() +
           # color plattes getting added later on:
           # scale_fill_brewer(palette = ...) +
           coord_equal() +
           theme(legend.position = "bottom")

  return(map)
}
