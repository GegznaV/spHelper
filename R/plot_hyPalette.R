
#' [!] Plot the color palette used in a hyperSpec object
#'
#' Plot a color palette in a \code{\link[=hyperSpec-class]{hyperSpec}} object,
#'  which was added with function \code{\link{hyAdd_color}}.
#'
#' @template sp-hy
#' @param by \bold{Either} a name of factor variale in \code{sp} which levels
#'           correspond to colors in \code{palette} \bold{or}, if \code{sp} is
#'           not present, a charter vector of names to be ploted as a text.
#' @param palette A list of color names or color codes.
#' @param Title The title.
#'
#' @param as.legend Logical. If \code{TRUE}, the result is used as legend for
#'         existing R base graphics plot.  If \code{FALSE} (default), a
#'         separate plot is drawn.
#'
#' @param xpd A logical value or \code{NA}. If \code{FALSE}, all plotting is
#'          clipped to the plot region, if \code{TRUE}, all plotting is
#'          clipped to the figure region, and if \code{NA}, all plotting is
#'          clipped to the device region.  See also \code{\link[graphics]{clip}}.
#' @param bty The type of box to be drawn around the legend. The allowed values
#'        are "o" and "n" (the default).
#'
#' @param x,y the x and y co-ordinates to be used to position the legend.
#'        They can be specified by keyword or in any way which is accepted
#'        by \link[grDevices]{xy.coords}: See "Details" in \code{\link[graphics]{legend}}.
#' @param ... Other arguments to be passed to \code{\link[graphics]{legend}}.
#'            (except \code{legend}, \code{title} and \code{fill})
#'
#' @inheritParams graphics::legend
#'
#' @template plot-base
#' @export
#'
#' @examples
#'
#' # Example 1
#' sp <- hyAdd_color(Spectra3,"gr")
#' plot_hyPalette(sp, "gr")
#'
#' #-----------------------------------------------------
#' # Example 2
#' plot_hyPalette(by = "RED color", palette = "#ee0000")
#'
#' #-----------------------------------------------------
#' # Example 3
#' # Use data 'sp' from example 1
#' # The legend does not make sense in this context. It's just an example.
#'
#' plot(flu)
#' plot_hyPalette(sp, "gr", as.legend = TRUE, cex = 1)
#'
#'
#' @author Vilmantas Gegzna
#'
#' @seealso \code{\link[graphics]{legend}}
#' @family \pkg{spHelper} plots
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#'
plot_hyPalette <- function(sp = NULL,
                           by = ".color",
                            palette = hyGet_palette(sp),
                            cex = if (as.legend == FALSE) 1.2 else 1,
                            Title = if (!is.null(sp)) "Group colors" else "Colors",
                            as.legend = FALSE,
                            xpd = NA,
                            x = "topright",
                            y = NULL,
                            bty = "n",
                            ...){
    if (!is.null(sp)) {
        chk.hy(sp)
        TEXT <- levels(sp$..[[by]])

        if (length(TEXT) == 0) TEXT <- palette
    } else {
        TEXT <- if (by[1] != ".color") as.character(by) else " "
    }

    if (length(palette) == 0)  {
        TEXT <- "— No colors are present —"
        palette <- NA
    }

    # If number of colors and strings do NOT match ----------------------------
    nText   <- length(TEXT)
    nColors <- length(palette)
    if ( nText != nColors) {
        warning(sprintf(
            "Number of colors (n=%d) does not match number of text lines (n=%d).",
            nColors, nText))
        if (nText < nColors) {
            TEXT[(nText + 1):nColors] <- " "
        } else {
            palette[(nColors + 1):nText] <- NA
        }
    }

    # Plot --------------------------------------------------------------------
    if(as.legend == FALSE) {plot.new(); x = "center"}
    # title(main )
    legend(x, y, legend = TEXT, fill = palette, cex = cex,
           title = Title, bty = bty,
           xpd = xpd, ...)
}


# TEXT <- factor(levels(Spectra$gr),levels(Spectra$gr))

# ggplot(mapping = aes(x = 1, y = 1:length(TEXT), fill = TEXT)) +
#
#     geom_tile()+
#     geom_text(label = TEXT) +
#     theme_void() +
#     scale_fill_manual(values = UsedColors)

