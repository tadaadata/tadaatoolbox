#' Interaction plots
#'
#' Easily generate interaction plots of two nominal grouping
#' variables and a numeric response variable.
#' @param data A \code{data.frame}
#' @param response Response variable
#' @param group1 First grouping variable
#' @param group2 Second grouping variable
#' @param grid If \code{TRUE}, the resulting graphs will be arranged in a grid via \link[cowplot]{plot_grid}.
#' @param brewer_palette The name of the \link[RColorBrewer]{RColorBrewer} palette to use, defaults to \code{Set1}
#' @param labels Labels used for the plots when printed in a grid (\code{grid = TRUE}), defaults to \code{c("A", "B")}
#' @return Invisible: A list with two ggplot2 objects named \code{p1} and \code{p2}.
#' Printed: The one or two ggplot2 objects, depending on \code{grid}.
#' @export
#' @family Tadaa-functions
#' @import ggplot2
#' @importFrom cowplot plot_grid
#' @examples
#' tadaa_int(ngo, stunzahl, jahrgang, geschl)
#'
#' # As grid
#' tadaa_int(ngo, stunzahl, jahrgang, geschl, grid = TRUE)
tadaa_int <- function(data, response, group1, group2, grid = FALSE, brewer_palette = "Set1", labels = c("A", "B")){

  sdots <- lazyeval::interp(~mean(variable, na.rm = T), variable = substitute(response))

  data <- dplyr::group_by_(data, substitute(group1), substitute(group2))
  data <- dplyr::summarize_(data, .dots = list(mw = sdots))


  p1 <- ggplot(data = data, aes_string(x = substitute(group1), y = "mw", colour = substitute(group2))) +
          geom_point(shape = 23) +
          geom_line(aes_string(group = substitute(group2))) +
          scale_colour_brewer(palette = brewer_palette) +
          labs(title = paste0("Interaction of\n", substitute(group1), " & ",
                             substitute(group2)), y = paste0("Mean ", substitute(response))) +
          theme(legend.position = "top")

  p2 <- ggplot(data = data, aes_string(x = substitute(group2), y = "mw", colour = substitute(group1))) +
          geom_point(shape = 23) +
          geom_line(aes_string(group = substitute(group1))) +
          scale_colour_brewer(palette = brewer_palette) +
          labs(title = paste0("Interaction of\n", substitute(group2), " & ",
                             substitute(group1)), y = paste0("Mean ", substitute(response))) +
          theme(legend.position = "top")

  if (!grid){
    print(p1)
    print(p2)
  } else {
    print(cowplot::plot_grid(p1, p2, align = "h", labels = labels))
  }

  invisible(list(p1 = p1, p2 = p2))
}


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
