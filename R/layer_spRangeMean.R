
# hyperSpec::subset does not work inside function and it's for cycle

#' [!] Plot mean and range of spectroscopic data intensities
#'
#' Plot range of y axis values (usually intensities) of spectroscopic data
#' between selected percentilles.\cr
#' Function \code{qplot_spRange()} returns whole ggplot and
#' function \code{layer_spRange()} returns a layer which can be added to a ggplot.
#'
#'
#' @template sp-hy
#' @template by
#' @param percent Numeric value between 0 and 100 that indicates
#'        \emph{percentage} of samples nearest to median be plotted as a ribbon.
#' @param probs verctor of size 2 with values between 0 and 1 that indicates
#'        \emph{probabilities} at which percentiles should be plotted.
#'
#' @param add (logical) add plot to an existing ggplot object?
#' @param alpha (numeric from 0 to 1) transperency.
#' @param ... further arguments to \code{\link[ggplot2]{geom_ribon}}.
#'
#' @template ggplot
#' @export
#'
#' @author Vilmantas Gegzna
#' @family \pkg{spHelper} plots
#'
#' @rdname qplot_spRangeMean
#'
#' @examples
#' library(spHelper)
#' library(spPlot)
#'
#' qplot_spRangeMean(Spectra2)
#' qplot_spRangeMean(Spectra2, "gr")
#'
#' # Both lines below gives identical plots with different default titles:
#' qplot_spRangeMean(Spectra2, "gr", percent = 50)
#' qplot_spRangeMean(Spectra2, "gr", probs = c(.25, .75))
#'
#'
#' ggplotly_tidy()

layer_spRangeMean <- function(sp,
                          by = NULL,
                          percent = NULL,
                          probs = NULL,
                          palette = hyGet_palette(sp),
                          ...,
                          lty = 2,
                          lwd_mean = 1,
                          alpha = .2,
                          add = TRUE,
                          name_if_by_is_NULL = "All Data")
{

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    force(probs)
    force(percent)
    force(palette)
    force(by)
    force(add)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ============= Prepare variable `Groups` ================================

    # Get input variables
    Groups <- (getVarValues(VAR = by, DATA = sp) %if_null_or_len0%
        rep(name_if_by_is_NULL, nrow(sp))) %>%
        as.factor

    sp$Groups <- Groups

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ============= Check arguments `percent` and `probs` ====================
    #
    # Firstly, check both  - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if (!is.null(percent) & !is.null(probs))
        stop("Either `percent` or `probs` must be provided, but not both!")

    if (is.null(percent) & is.null(probs)) probs <- c(0,1)

    # Secondly, check `percent`  - - - - - - - - - - - - - - - - - - - - - - - -
    if (!is.null(percent)){
        if (length(percent) != 1)
            stop("Length of `percent` must be 1!")
        if (any(percent > 100))
            stop("Max value of `percent` must not exceed 100!")
        if (any(percent <   0))
            stop("Min value of `percent` must not be smaller than 0!")
        probs <- .5 + percent/100/2 * c(-1,+1)
        Title <- paste0("Mean and range (",
                        round(percent,1),"% arround median) of intensity")

    } else { # Thirdly, check `probs`  - - - - - - - - - - - - - - - - - - - - - -
        if (length(probs) != 2) stop("Length of `probs` must be 2!")
        if (any(probs > 1)) stop("Max value of `probs` must not exceed 1!")
        if (any(probs < 0))
            stop("Min value of `probs` must not be smaller than 0!")
        Title <- paste0("Mean and range of intensity between\n",
                        paste0(round(probs*100,1), "%", collapse = " and "),
                        " percentiles")
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ============= Check argument `palette` =================================

    palette <- check_palette(palette, Groups)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ============= Prepare spectroscopic data ===============================

    if (all(probs == c(0, 1))) {
        var_names <- c("ymin","ymax")
    } else {
        var_names <- paste("percentile", probs*100, sep = "_")
    }

    # Convert to long-format data frame and create columns `ymin` and `ymax`
    spDF <- spStat_ldf(sp,
                       Groups,
                       FUN = quantile,
                       probs = probs[1:2],
                       var_names = var_names)

    spDF$mean <- spStat_ldf(sp,
                       Groups,
                       FUN = mean)$mean

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ============= Plot data ================================================

    p <- list(
        geom_ribbon(
            aes_string(
                x  = ".wavelength",
                ymin  = var_names[1],
                ymax  = var_names[2],
                color = "Groups",
                fill  = "Groups"
            ),
            data = spDF,
            alpha = alpha,
            lty = lty
        ),
        geom_line(
            aes_string(
                x  = ".wavelength",
                y  = "mean",
                color = "Groups"
            ),
            data = spDF,
            lwd = lwd_mean,
            lty = lty
        )
    )


    return(p)
}


