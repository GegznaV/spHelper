
#' @name plot_spDiff
#'
#' @title [+] Plot difference between original and reconstructed spectra
#'
#' @description Plot difference between original (e.g., experimental) and
#' reconstructed spectra. Function \code{\link{reconstructSp}} is used
#' to reconstruct spectra, that are subtracted from original spectra afrterwards.
#'
#' @note
#'
#' Function \code{\link[hyperSpec]{plotspc}} is used to make a plot. R \code{base}
#' plotting system annotations can be used to enhance the plot.
#'
#' @template loadings-hy
#' @template scores
#' @template sp-hy
#' @template Title
#' @param color see \code{\link[graphics]{par} col}. Might be a vector giving
#'        individual colors for the spectra.
#'        \bold{Default} values: vector in \code{sp$.color}, if does not exist,
#'        "tan3" is used as a default color.
#' @param stacked if not \code{NULL}, a "stacked" plot is produced.
#'       \code{stacked} may be  \code{TRUE} to stack single spectra. A numeric
#'        or factor is
#'         be interpreted as giving the grouping, character is interpreted as
#'         the name of the extra data column that holds the groups.
#'         \bold{Default} stacking is by \code{sp$ID}, and \code{NULL} if this
#'         variable is missing.
#' @param ... Other parameters to be passed to function
#'          \code{\link[hyperSpec]{plotspc}}.
#' @inheritParams hyperSpec::plotspc
#'
#' @return A plot of calculated difference between expected (original)
#' and reconstructed spectra. (Drawn with R package \pkg{graphics}).
#'
#' @export
#' @examples
#' plot_spDiff(Loadings2, Scores2[1:10,,], Spectra2[1:10,,], stacked = TRUE)
#'
#' @family \pkg{spHelper} plots
#' @family component analysis / factorisation related functions in \pkg{spHelper}
#' @author Vilmantas Gegzna

plot_spDiff <- function(loadings, scores, sp,
                        Title = 'Difference between oginal and reconstructed spectra',
                        color = if (".color" %in% names(sp$..)) sp$.color else "blue3",
                        stacked = if ("ID" %in% names(sp$..)) sp$ID else NULL,
                        spc.nmax = 2000,
                        ...) {
    spREC  <- reconstructSp(loadings,scores, sp)
    spDiff <- sp - spREC

    plotspc(spDiff,
         spc.nmax = spc.nmax,
         col = color,
         stacked = stacked,
         ...,
         title.args = list(main = Title)

    )

    # # Generate a plot with ggplot2
    # p <- plot_sp(spDiff,Title = Title, Facets = T)
    # invisible(p)
}
