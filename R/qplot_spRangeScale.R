#  ??? DELETE ???
#
#
#
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


qplot_spRangeCenter <- function(sp,
                          by = NULL,
                          percent = NULL,
                          probs = NULL,
                          palette = hyGet_palette(sp),
                          ...,
                          lwd_mean = 1,
                          alpha = .2,
                          add = FALSE,
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
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Groups_co <- (getVarValues(VAR = by2, DATA = cutoffs) %if_null_or_len0%
                   rep(name_if_by_is_NULL, nrow(sp))) %>%
        as.factor

    cutoffs$Groups <- Groups_co

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ============= Check arguments `percent` and `probs` ====================

    probs <- percent2probs(50)
    Title <- paste0("Mean and range (",
                        round(percent,1),"% arround median) of intensity")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ============= Check argument `palette` =================================



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


    spCO <- spStat_ldf(sp,
                       Groups,
                       FUN = quantile,
                       probs = probs[1:2],
                       var_names = var_names)

    spDF$mean <- spStat_ldf(sp,
                            Groups,
                            FUN = mean)$mean


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ============= Plot data ================================================


    p <- ggplot(
        spDF,
        aes_string(
            x  = ".wavelength",
            y  = "mean",
            ymin  = var_names[1],
            # "ymin",
            ymax  = var_names[2],
            # "ymax",
            color = "Groups",
            fill  = "Groups"
        )
    ) +
        geom_ribbon(alpha = alpha) +
        geom_line(lwd = lwd_mean) +
        labs(x = labels(sp)$.wavelength,
             y = labels(sp)$spc,
             title = Title)

    return(p)
}


