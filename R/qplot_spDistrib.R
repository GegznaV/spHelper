
# hyperSpec::subset does not work inside function and it's for cycle


#' [!] Plot distributions of spectroscopic data
#'
#' @template sp-hy
#' @template by
#' @param percent Numeric value between 0 and 100 that indicates
#'        \emph{percentage} of samples nearest to median be plotted as a ribbon.
#' @param probs verctor of size 2 with values between 0 and 1 that indicates
#'        \emph{probabilities} at which percentiles should be plotted.
#'
#' @template ggplot
#' @export
#'
#' @author Vilmantas Gegzna
#' @family \pkg{spHelper} plots
#'
#'  @examples
#' qplot_spDistrib(Spectra2, "gr")
#'
#' # Both lines below gives identical plots with different default titles:
#' qplot_spDistrib(Spectra2, "gr", percent = 50)
#' qplot_spDistrib(Spectra2, "gr", probs = c(.25, .75))

#'
qplot_spDistrib <- function(sp, by = stop("`by` is missing"),
                            percent = NULL, probs = NULL,
                            palette = hyGet_palette(sp)) {
    force(probs)
    force(percent)
    force(palette)
    force(by)

    # Prepare data

    # Get input variables
    by <- getVarValues(VAR = by, DATA = sp) %>% as.factor()
    group_names <- levels(by)
    # by <- eval_glue("sp$`{by}`"))


    # Firstly check both  - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if (!is.null(percent) & !is.null(probs))
        stop("Either `percent` or `probs` must be provided, but not both!")

    if (is.null(percent) & is.null(probs))
        probs <- c(0, 1)

    # Secondly check `percent`  - - - - - - - - - - - - - - - - - - - - - - - -
    if (!is.null(percent)) {
        if (length(percent) != 1)
            stop("Length of `percent` must be 1!")

        if (any(percent > 100))
            stop("Max value of `percent` must not exceed 100!")

        if (any(percent <   0))
            stop("Min value of `percent` must not be smaller than 0!")

        probs <- .5 + percent/100/2 * c(-1,+1)
        Title <- paste0("Distribution: ",  round(percent,1),"% arround median")
    }
    else {# Thirdly check `probs`  - - - - - - - - - - - - - - - - - - - - - -
        if (length(probs) != 2) stop("Length of `probs` must be 2!")
        if (any(probs > 1)) stop("Max value of `probs` must not exceed 1!")
        if (any(probs < 0)) stop("Min value of `probs` must not be smaller than 0!")
        Title <- paste0("Distribution between\n",
            paste0(round(probs*100,1), "%", collapse = " and ")," percentiles")
    }

    # Check palette - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    nPal <- length(palette)
    nColNeeded <-  nlevels(by)
    if (nPal < nColNeeded) {
        if (nPal > 0){
            warning(sprintf(paste("There are %d colors in provided palette",
                                  "and %d are needed, thus the DEFAULT colors",
                                  "will be used."),nPal,nColNeeded))
        }
        palette <- NULL

    }
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    # Find min and max
    aggr_sp <- aggregate(sp, by, FUN = quantile, probs = probs[1:2])

    # Reshape
    df <- data.frame()
    for (i in 1:length(group_names)) {
        sp_i <- aggr_sp[aggr_sp$.aggregate == group_names[i],]
        df <- rbind(df,
                    data.frame(.wavelength = wl(sp_i),
                               ymin = as.vector(sp_i$spc[1,]),
                               ymax = as.vector(sp_i$spc[2,]),
                               Groups = rep_len(sp_i$.aggregate, nwl(sp_i)))
        )
    }

    # Plot
    p <- ggplot(df, aes(x = .wavelength,
                   ymin = ymin,
                   ymax = ymax)) +
        geom_ribbon(alpha = .2,
                    aes(color = Groups,
                        fill  = Groups)) +

        labs(x = labels(sp)$.wavelength,
             y = labels(sp)$spc,
             title = Title) +

        theme_bw()

    if (!is.null(palette)){
        p <- p +
            scale_color_manual(values = palette) +
            scale_fill_manual( values = palette)
        }

    return(p)
}
