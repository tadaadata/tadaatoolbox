#' Interaction plots
#'
#' Easily generate interaction plots of two nominal grouping
#' variables and a numeric response variable.
#' @param data A `data.frame`.
#' @param response Response variable.
#' @param group1 First grouping variable.
#' @param group2 Second grouping variable.
#' @param grid If `TRUE`, the resulting graphs will be arranged in a grid
#' via [cowplot::plot_grid].
#' @param brewer_palette The name of the `RColorBrewer` palette to use,
#' defaults to `Set1`.
#' @param labels Labels used for the plots when printed in a grid (`grid = TRUE`),
#' defaults to `c("A", "B")`.
#' @param show_n If `TRUE`, displays N in plot subtitle.
#' @param print Default is `TRUE`, set `FALSE` to suppress automatic printing.
#' Useful if you intend to further modify the outpur plots.
#' @return Invisible: A list with two ggplot2 objects named `p1` and `p2`.
#' If `print = TRUE`: Printed: The one or two ggplot2 objects, depending on `grid`.
#' @export
#' @family Tadaa-plot functions
#' @import ggplot2
#' @examples
#' \dontrun{
#' tadaa_int(ngo, stunzahl, jahrgang, geschl)
#'
#' # As grid, if cowplot is installed
#' tadaa_int(ngo, stunzahl, jahrgang, geschl, grid = TRUE)
#' }
tadaa_int <- function(data, response, group1, group2, grid = FALSE,
                      brewer_palette = "Set1", labels = c("A", "B"),
                      show_n = FALSE, print = TRUE) {

  if (show_n) {
    subtitle <- paste0("N = ", nrow(data))
  } else {
    subtitle <- NULL
  }

  sdots <- lazyeval::interp(~mean(variable, na.rm = T), variable = substitute(response))

  data <- dplyr::group_by_(data, substitute(group1), substitute(group2))
  data <- dplyr::summarize_(data, .dots = list(mw = sdots))

  title1 <- ifelse(!grid, paste0("Interaction of ",
                                 substitute(group1), " & ", substitute(group2)),
                   paste0("Interaction of\n",
                          substitute(group1), " & ", substitute(group2)))

  title2 <- ifelse(!grid, paste0("Interaction of ",
                                 substitute(group2), " & ", substitute(group1)),
                   paste0("Interaction of\n",
                          substitute(group2), " & ", substitute(group1)))

  p1 <- ggplot(data = data, aes_string(x = substitute(group1), y = "mw",
                                       colour = substitute(group2))) +
          geom_line(aes_string(group = substitute(group2))) +
          geom_point(shape = 23, fill = "white") +
          scale_colour_brewer(palette = brewer_palette) +
          labs(title = title1, y = paste0("Mean ", substitute(response)),
               subtitle = subtitle) +
          theme(legend.position = "top")

  p2 <- ggplot(data = data, aes_string(x = substitute(group2), y = "mw",
                                       colour = substitute(group1))) +
          geom_line(aes_string(group = substitute(group1))) +
          geom_point(shape = 23, fill = "white") +
          scale_colour_brewer(palette = brewer_palette) +
          labs(title = title2, y = paste0("Mean ", substitute(response)),
               subtitle = subtitle) +
          theme(legend.position = "top")

  if (print) {
    if (!grid) {
      print(p1)
      print(p2)
    } else {
      if (!requireNamespace("cowplot")) {
        stop("Sorry, you need to install cowplot for this to work.")
      }
      print(cowplot::plot_grid(p1, p2, align = "h", labels = labels))
    }
  }

  invisible(list(p1 = p1, p2 = p2))
}


#' Grouping design balance
#'
#' Easily generate heatmaps to show how well balanced groups are designed, e.g. for ANOVA.
#' @param data A `data.frame`
#' @param group1 The grouping variable on the x-axis
#' @param group2 The grouping variable on the y-axis
#' @param palette The [viridis::viridis] color palette to use; `c("A", "B", "C", "D")`,
#' defaults to `"D"`
#' @param annotate Should the n of each group be displayed in each cell of the heatmap?
#' @return A ggplot2 object
#' @export
#' @family Tadaa-plot functions
#' @import ggplot2
#' @import viridis
#' @examples
#' tadaa_balance(ngo, jahrgang, geschl)
tadaa_balance <- function(data, group1, group2, palette = "D", annotate = TRUE) {

  group1 <- deparse(substitute(group1))
  group2 <- deparse(substitute(group2))
  # group3 <- deparse(substitute(group3))

  heat <- table(data[[group1]], data[[group2]])
  heat <- as.data.frame(heat)

  if (annotate) {
    anno <- geom_label(aes_string(label = "Freq"), stat = "identity",
                       fill = "white", alpha = .5, size = 5)
  } else {
    anno <- NULL
  }

  balance <- ggplot(heat, aes_string(x = "Var1", y = "Var2", fill = "Freq")) +
    geom_tile(color = "white", size = 0.75) +
    anno +
    labs(title = paste("Design Balance for", substitute(group1),
                       "and", substitute(group2)),
         x = group1, y = group2) +
    scale_x_discrete() +
    viridis::scale_fill_viridis(option = palette) +
    coord_equal(ratio = .625) +
    theme(legend.position = "none")

  balance
}

#' Means with Errorbars
#'
#' @param data A `data.frame`
#' @param response Response variable, numeric.
#' @param group Grouping variable, ideally a `factor`.
#' @param brewer_palette Optional: The name of the `RColorBrewer`` palette to use,
#' defaults to `Set1`. Use `NULL` for no brewer palette.
#' @return A ggplot2 object.
#' @export
#' @family Tadaa-plot functions
#' @import ggplot2
#' @examples
#' tadaa_mean_ci(ngo, deutsch, jahrgang, brewer_palette = "Set1")
tadaa_mean_ci <- function(data, response, group, brewer_palette = "Set1") {

  x <- deparse(substitute(group))
  y <- deparse(substitute(response))

  p <- ggplot(data = data, aes_string(x = x, y = y, color = x)) +
        stat_summary(fun.data = "mean_ci_t", geom = "errorbar", width = 0.6, size = 1.5) +
        stat_summary(fun.y = "mean", geom = "point", size = 3, color = "black") +
        stat_summary(fun.y = "mean", geom = "point", size = 2, color = "white") +
        guides(color = F)
  if (!is.null(brewer_palette)) {
    p <- p + scale_color_brewer(palette = brewer_palette)
  }
  p <- p + labs(title = paste0(y, " by ", x), y = paste0("Mean of ", y, " with 95% CI"))

  p
}

#' Plot TukeyHSD Results as Errorbars
#'
#' This is a simple plotting template that takes the [broom::tidy]'d output of
#' [stats::TukeyHSD] or alternatively the `print = "df"` output
#' of [tadaa_pairwise_tukey] and plots it nicely with error bars.
#' @param data The [broom::tidy]'d output of [stats::TukeyHSD].
#' @inheritParams tadaa_mean_ci
#' @return A [ggplot2] object.
#' @export
#' @import ggplot2
#' @importFrom dplyr arrange
#' @family Tadaa-plot functions
#' @note The `alpha` of the error bars is set to `0.25` if the comparison
#' is not significant, and `1` otherwise. That's neat.
#' @examples
#' tests <- tadaa_pairwise_tukey(data = ngo, deutsch, jahrgang, geschl, print = "df")
#' tadaa_plot_tukey(tests)
tadaa_plot_tukey <- function(data, brewer_palette = "Set1") {

  # Please R CMD check
  comparison <- NULL; estimate <- NULL; conf.low <- NULL
  conf.high <- NULL; term <- NULL; signif <- NULL

  data$signif <- ifelse(data$conf.high > 0 & data$conf.low < 0, "no", "yes")

  data <- dplyr::arrange(data, term, estimate)
  data$comparison <- factor(data$comparison,
                            levels = rev(as.character(data$comparison)),
                            ordered = TRUE)

  p <- ggplot(data = data,
              aes(x     = comparison,
                  y     = estimate,
                  ymin  = conf.low,
                  ymax  = conf.high,
                  color = term,
                  alpha = signif)) +
    geom_errorbar(width = .75, size = 1.25) +
    geom_point(shape = 23, size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    scale_alpha_manual(values = c("no" = 0.25, "yes" = 1), guide = F) +
    labs(title = "Tukey HSD Results", subtitle = "Mean Difference with 95% CI",
         x = "Compared Groups", y = "Mean Difference", color = "Term (Factor)",
         caption = "Confidence intervals not including x = 0 are considered significant") +
    theme(legend.position = "top")

  if (!is.null(brewer_palette)) {
    p <- p + scale_color_brewer(palette = brewer_palette)
  }

  p
}
