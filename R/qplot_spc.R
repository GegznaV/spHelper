
#' @title [!!!] Plot spectroscopic curves
#'
#' @description Plot spectroscopic curves in different colors. \cr
#'
#'              [ DESCRIPTION MUST BE UPDATED ]
#'
#'
#' @details \code{qplot_kSp} plots spectra on one graph. \cr
#'          \code{qplot_kSpFacets} plots spectra on separate graphs (facets).\cr
#'
#' @note    Before using as an argument \code{sp},
#'          a matrix of components/loadings (\code{sp}) must be treated
#'          with function \code{\link[hyperSpec]{decomposition}} (or equivalent)
#'          which converts it to \code{\link[=hyperSpec-class]{hyperSpec}} object.
#'
#' @template sp-hy
#' @template labels
#' @template subtitle
#'
#' @param names.in DEPRECATED. Use \code{by} instead.
#'
#' @param by A name of variable in \code{sp} that contains variable names.
#'        If indicated variable does not exist, row names are used instead.\cr
#'        Default is \code{names = 'cNames'}
#'
#' @param normalize A flag that indicates whether components should be
#'  normalized before plotting.
#'      Possible selections: \enumerate{
#'      \item {\code{FALSE}, \code{0}}{ - do not normalize;}
#'      \item {\code{TRUE}, \code{+1}}{ - normalize to max value;}
#'      \item {\code{-TRUE}, \code{-1}}{ - normalize to min value [! this choise can give unexpected results];}
#'      \item {\code{"auto"}}{ - one of the choices above is selected by determining if spectra have any
#'           possitive and any negative peaks:\itemize{
#'           \item \code{Below0 <- any(sp$spc < 0);}
#'           \item \code{Above0 <- any(sp$spc > 0);}
#'           \item \code{normalize <- (Above0 - Below0)}
#'           }
#'           }
#'      }
#'
#' @param legendName A name of a legend. Possible entries: \enumerate{
#'          \item {logical \code{FALSE}}{ - a legend without a name;}
#'          \item {logical \code{TRUE}}{ - a label of a variable \code{name} is used as a name of a legend
#'              (\code{legendName <- labels(sp,name}));}
#'          \item {...}{manual input of the name.}
#'          }
#'
#' @param facets A logical flag. If \code{TRUE}, spectra are plotted on separate graphs/facets
#'          (implemented by function \code{\link[ggplot2]{facet_grid}}). If {\code{FALSE}, all spectra
#'           are plotted on one facet.
#'   }
#'
#' @param filled Logical. If \code{TRUE}, colored fill is used. If \code{FALSE}, no fill is used.
#'      (Fill is an area between ordinate axis and the curve.)
#'
#' @param ... further arguments to geom_line.
#'
#' @template ggplot
#'
#' @examples
#' library(spHepler)
#' library(spPlot)
#'
#'
#'   ggplot() +
#'    qplot_spc(sp, by = "gr", add = TRUE, alpha = .2) +
#'    qplot_spStat(sp,"gr", mean, add = TRUE)
#'
#'
#'
#' qplot_kSp(Loadings2)
#'
#' data(flu, package = "hyperSpec")
#'
#' qplot_kSpFacets(flu, Title = "Flu dataset")
#' qplot_kSpFacets(flu, Title = "Flu dataset", normalize = 1)
#' qplot_kSpFacets(flu, Title = "Flu dataset", normalize = FALSE)
#' qplot_kSpFacets(flu, Title = "Flu dataset", normalize = -1)
#'
#' ## Remove fill -----------------------------------------------------------------
#'
#'
#' qplot_kSp(flu, filled = FALSE)
#' qplot_sp(flu)
#'
#' ## Name of a legend ------------------------------------------------------------
#' flu$c2 <- as.factor(flu$c)
#'
#' qplot_sp(flu, Title = "Flu dataset", by = 'c2', legendName = FALSE)
#' qplot_sp(flu, Title = "Flu dataset", by = 'c2', legendName = TRUE)
#' qplot_sp(flu, Title = "Flu dataset", by = 'c2', legendName = "Concentration")
#'
#' ## Example of line color transitions -------------------------------------------
#'   qplot_sp(laser)
#'
#' @export
#' @family \pkg{spHelper} plots
#' @family component analysis / factorisation related functions in \pkg{spHelper}
#' @author Vilmantas Gegzna
qplot_spc <- function(sp,
                     Title = "Components",
                     xLabel = labels(sp, ".wavelength"),
                     yLabel = labels(sp, "spc"),
                     by = 'cNames',
                     palette = hyGet_palette(sp),
                     legendName = FALSE,
                     filled = TRUE,
                     normalize  = FALSE,
                     facets = FALSE,
                     subTitle = NULL,
                     names.in  = NULL,
                     line_size = 1,
                     add = FALSE,
                     ...)
{

    # DEPRECATED: names.in
    if(!is.null(names.in)) by <- names.in
    # -----------------------------------------------------------------------

    force(by)
    force(palette)

    hyperSpec::chk.hy(sp)

    # Get label of `sp[, names]`, before renaming to "kName"
    if (is.logical(legendName)) {
        if (legendName) {
                legendName <- labels(sp, by)
        } else {
                legendName <- NULL
        }

    }

    #  Choose the way of normalization
    if (normalize == "auto") {
            normalize <- any(sp$spc > 0) - any(sp$spc < 0)
    }

    sp <- switch(as.character(as.numeric(normalize)),
                       `0` =  sp,
                     # `+1` normalize to max value
                       `1` =  sweep(sp, 1, max, `/`),
                     # `-1` normalize to min value
                      `-1` =  sweep(sp, 1, min, `/`),
                       stop("Parameter 'normalize' is incorrect. Must be either -1, 0 or 1.")
    )

    #
    l <- sp

    # Select variable with component names
    by <- as.character(by)

    #if variable does not exist
    if (by %!in% colnames(l)) {
        # l[,by] = as.factor(as.numeric(rownames(l)))
        l[,by] = as.factor(rownames(l))
    }

    if (by != 'cNames') {colnames(l)[colnames(l) == by] <- 'cNames'}

    l$.rownames = 1:nrow(l)                 # Create variable with row numbers
    l <- l[,c('spc','cNames','.rownames')]  # Rename variables

    l <- as.long.df(l)

    # Define the limits
    if (facets == TRUE)  {
        nTicksY <- 2
        limMIN <- ifelse(min(l$spc) >= 0, 0, min(l$spc) * 1.1)
        limMAX <- ifelse(max(l$spc) <= 0, 0, max(l$spc) * 1.1)

    } else {
        nTicksY <- 5
        limMIN <- ifelse(min(l$spc) >= 0, 0, min(l$spc) * 1.05)
        limMAX <- ifelse(max(l$spc) <= 0, 0, max(l$spc) * 1.05)
    }

    # Check palette - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    palette <- check_palette(palette, l$cNames)
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if (add == FALSE){
        # Plot
        p <- ggplot(l, aes_sp_(color = "cNames", fill  = "cNames")) +
            geom_hline(yintercept = 0, size = 1, linetype = 1, alpha = .4) +
            geom_line(size = line_size, ...) +
            # theme_bw() +
            scale_x_continuous(expand = c(0,0)) +
            scale_y_continuous(expand = c(0,0),
                               limits = c(limMIN, limMAX),
                               breaks = number_ticks(nTicksY) ) +
            labs(x = xLabel, y = yLabel)


        if (!is.null(Title) | !is.null(subTitle))
            p <- p + ggtitle(subt(Title, subTitle))

        # Add fill
        if (filled == TRUE)
            p <- p + geom_density(stat = "Identity", alpha = .1)

        # Make facets
        if (facets == TRUE)
            p <- p + facet_grid(cNames ~., scales = "free") +
                # Remove stripes:
                theme(strip.text = element_blank(),
                      strip.background = element_blank())

        # Add name of the legend
        p$labels[p$labels == "cNames"] = legendName

        if (!is.null(palette)) p <- p + scale_color_manual(values = palette)

    } else {
        p <- geom_line(data = l,
                  aes_sp_(color = "cNames"),
                  size = line_size,
                  inherit.aes = FALSE,
                  ...)
    }



    return(p)

} #[END]

#' @export
#' @rdname qplot_spc
gg_spc <- function(sp, by = 'cNames', line_size = 1, ...){
    qplot_spc(sp, by = by, line_size = line_size, ..., add = TRUE)
}
