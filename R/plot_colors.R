#' [!] Vizualize colors
#'
#' A convenience function to Vizualize colors by either color name or
#' color code.
#'
#' @param col A list of either color names or color codes.
#' @param text Strings that describe each color.
#' @inheritParams graphics::legend
#'
#' @template plot-base
#' @export
#'
#' @examples
#'
#' plot_colors("#ee0000")
#' plot_colors("#ee0000", "RED color")
#' plot_colors(c("red","red4","orange2","green3","skyblue"))
#'
#' @author Vilmantas Gegzna
#' @family \pkg{spHelper} plots
plot_colors <- function(col, text = as.character(col),
                        Title = "Colors", cex = 1.2, as.legend = FALSE, ...) {
    plot_hyPalette(palette = col,
                   by = text,
                   Title = Title,
                   cex = cex,
                   as.legend = as.legend, ...)
}
