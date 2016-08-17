#' ggplot2 theme to fit the readthedown Rmd format
#'
#' A ggplot theme to fit \code{\link[rmdformats]{readthedown}} in terms of background color and
#' dark grid lines.
#' @param base_size Base text size, defaults to \code{12}.
#' @param base_family Base text family. Use \code{"Roboto Slab"} to match the \code{readthedown}
#' headers, or \code{"Lato"} for the body style.
#' @param bg Background color, defaults to \code{\link[rmdformats]{readthedown}}'s background,
#' \code{#fcfcfc}
#' @param axis_emph Which axis to emphasize visually (black lines). One of \code{"x", "y", "xy"}.
#' @import ggplot2
#' @return A ggplot2 theme
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- qplot(1:10, 1:10, geom = "point")
#'
#' p + theme_readthedown()
#' p + theme_readthedown(base_family = "Lato")
#' p + theme_readthedown(base_family = "Roboto Slab", axis_emph = "x")
#' }
theme_readthedown <- function(base_size = 12, base_family = "", bg = "#fcfcfc", axis_emph = "xy") {
  template_background <- ggplot2::element_rect(fill = bg, color = bg)

  result <- ggplot2::theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.background  = template_background,
          plot.background   = template_background,
          legend.background = template_background,
          legend.key        = template_background,
          strip.background  = ggplot2::element_blank())

  if (axis_emph == "x") {
    result <- result +
      theme(panel.grid.major.x = element_line(size = .1, color = "black"),
            panel.grid.minor.x = element_line(size = .1, color = "black", linetype = "dotted"))
  } else if (axis_emph == "y") {
    result <- result +
      theme(panel.grid.major.y = element_line(size = .1, color = "black"),
            panel.grid.minor.y = element_line(size = .1, color = "black", linetype = "dotted"))
  } else if (axis_emph == "xy") {
    result <- result +
      theme(panel.grid.major.x = element_line(size = .1, color = "black"),
            panel.grid.minor.x = element_line(size = .1, color = "black", linetype = "dotted"),
            panel.grid.major.y = element_line(size = .1, color = "black"),
            panel.grid.minor.y = element_line(size = .1, color = "black", linetype = "dotted"))
  } else if (axis_emph == "") {
  } else {
    warning(paste("Unknow option to axis_emph", axis_emph, "\nDoing nothing"))
  }
  return(result)
}
