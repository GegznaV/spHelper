#' [!] Distribution of spectroscopoc data as percentiles
#'
#' @template sp-hy
#' @param ... further parameters for \code{\link[hyperSpec]{plotspc}}.
#' @param show.legend Logical. If \code{TRUE}, the legend is shown.
#' @param cex.legend Size of the legend. more details in \link[graphics]{par}.
#' @param position.legend Position of the legend. More details in \link[graphics]{legend}
#'
#' @author Vilmantas Gegzna
#' @family \pkg{spHelper} plots
#'
#' @return a plot
#' @export
#' @examples
#'
#' plot_spDistribution(laser)
#' plot_spDistribution(flu, cex.legend = .7)

plot_spDistribution <- function(sp, position.legend = "topright",
                                show.legend = TRUE,
                                cex.legend = 1, ...)  {

    # Stats
    Sp_med    <- apply(sp,2,  median)
    Sp_q05    <- quantile(sp, probs = c(.05,.95))
    Sp_q16    <- quantile(sp, probs = c(.25,.75))
    Sp_MinMax <- apply(sp,2,  range)

    # Plots
    plotspc(Sp_MinMax, add = F, col = "grey", fill = c(1,1), ...)
    plotspc(Sp_med,    add = T, col = "green")
    plotspc(Sp_q16,    add = T, col = "skyblue")
    plotspc(Sp_q05,    add = T, col = "orange")
    plotspc(Sp_MinMax, add = T, col = "red")

    # Legend
    if (show.legend == TRUE){
        legend(position.legend,
               col = c("red", "orange", "skyblue", "green"),
               legend = c("Min & Max",
                          " 5% & 95% percentiles",
                          "25% & 75% percentiles",
                          "Median"),
               lty = 1,
               bty = "n",
               cex = cex.legend
        )
    }
}

