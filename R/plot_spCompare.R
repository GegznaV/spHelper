
#' [!] Compare 2 spectroscopic signals
#'
#' Compare 2 spectroscopic signals: plot 2 signals and their difference.
#'
#' @param sp1,sp2 hyperSpec objects of the same size
#' @param row An integer, that indicates which row must be plotted.
#' @param show.legend Logical
#' @param colors Vector of 3 colors for original, filtered and noise signals
#'              respectively.
#' @param legend.title The title of the legend.
#' @param legend.text Character vertor of length = 3. The entries for the legend
#'        for for original, filtered and noise signals respectively.
#' @param lwd A line width. More details in \link[graphics]{par}.
#' @param x,y the x and y co-ordinates to be used to position the legend.
#'        They can be specified by keyword or in any way which is accepted
#'        by \link[grDevices]{xy.coords}: See 'Details' in \code{\link[graphics]{legend}}.
#' @param ... Other arguments to be passed to \code{\link[graphics]{legend}}
#'            (except \code{legend}, \code{title}, \code{col} and \code{lty}).
#'
#'
#' @template plot-base
#' @inheritParams graphics::legend
#' @export
#'
#' @examples
#'
#' # Apply running medians filter:
#' sp_filt <- apply(Spectra2, 1, function(x) {runmed(x, 15)})
#'
#' plot_spCompare(Spectra2, sp_filt, row = 2)
#'
#' # Modify the legend:
#' plot_spCompare(Spectra2, sp_filt, row = 2,legend.text  = c("Original","Filtered","Noise"))
#'
#' @family \pkg{spHelper} plots
#' @author Vilmantas Gegzna
plot_spCompare <- function(sp1, sp2, row = 1,
                        colors = c('green4','blue3', 'red'),
                        show.legend  = TRUE,
                        legend.title = "Spectra",
                        legend.text  = c(match.call()$sp1, match.call()$sp2,"Difference"),
                        x = "topright",
                        lwd = 1,
                        ...){
    chk.hy(sp1)
    chk.hy(sp2)
    if (length(row) != 1)         stop("length(row) != 1")
    if (length(colors) != 3)      stop("length(colors) != 3")
    if (length(legend.text) != 3) stop("length(legend.text) != 3")


    obj <- list()
    obj$sp1         <- sp1[row[1],"spc",]
    obj$sp2         <- sp2[row[1],"spc",]
    obj$difference  <- obj$sp1  - obj$sp2


    obj <- hyperSpec::collapse(obj)
    obj$.type   <- factor(c(2,2,1), c(1,2), c(' ', '  '))

    obj$.colors <- colors
    obj$lwd     <- lwd

    obj <- obj[c(3,1,2),,]
    plotspc(obj,
         stacked = obj$.type,
         col     = obj$.colors,
         lines.args = list(lwd = obj$lwd))

    if (show.legend == TRUE) {
        legend(x, y, title = legend.title,
               legend = legend.text,
               col = colors,
               lty = 1,
               lwd = lwd)
    }
}

