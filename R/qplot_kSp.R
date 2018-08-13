
#' @title [!+] Plot spectroscopic curves and spectral components (a.k.a. loadings)
#'
#' @description Plot spectroscopic curves in different colors.
#'
#'              Functions \code{qplot_kSp}, \code{qplot_kSpFacets} are designed
#'              to plot spectra of spectral components
#'             (a.k.a. loadings), extracted in principal component analysis
#'             (e.g., \code{\link[stats]{princomp}}), variaus matrix factorization
#'              methods (e.g., \code{\link[NMF]{nmf}})) and some other dimension
#'              reduction methods. \cr
#'              The function \code{qplot_sp} is convenient for all spectroscopic
#'              curves as uses no fill.
#'
#'
#' @details \code{qplot_kSp} plots spectra on one graph. \cr
#'          \code{qplot_kSpFacets} plots spectra on separate graphs (facets).\cr
#'
#' @note    Before using as an argument \code{loadings},
#'          a matrix of components/loadings (\code{loadings}) must be treated
#'          with function \code{\link[hyperSpec]{decomposition}} (or equivalent)
#'          which converts it to \code{\link[=hyperSpec-class]{hyperSpec}} object.
#'
#' @template loadings-hy
#' @template labels
#' @template subtitle
#'
#' @param names.in DEPRECATED. Use \code{by} instead.
#'
#' @param by A name of variable in \code{loadings} that contains variable names.
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
#'           \item \code{Below0 <- any(loadings$spc < 0);}
#'           \item \code{Above0 <- any(loadings$spc > 0);}
#'           \item \code{normalize <- (Above0 - Below0)}
#'           }
#'           }
#'      }
#'
#' @param legendName A name of a legend. Possible entries: \enumerate{
#'          \item {logical \code{FALSE}}{ - a legend without a name;}
#'          \item {logical \code{TRUE}}{ - a label of a variable \code{name} is used as a name of a legend
#'              (\code{legendName <- labels(loadings,name}));}
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
#'
#' @template ggplot
#'
#' @examples
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

qplot_kSp <- function(loadings,
                     Title = "Components",
                     xLabel = labels(loadings, ".wavelength"),
                     yLabel = labels(loadings, "spc"),
                     by = 'cNames',
                     palette = hyGet_palette(loadings),
                     legendName = FALSE,
                     filled = TRUE,
                     normalize  = FALSE,
                     facets = FALSE,
                     subTitle = NULL,
                     names.in  = NULL,
                     line_size = 1)
{

    # DEPRECATED: names.in
    if(!is.null(names.in)) by <- names.in
    # -----------------------------------------------------------------------

    force(by)
    force(palette)

    hyperSpec::chk.hy(loadings)

    # Get label of `loadings[, names]`, before renaming to "kName"
    if (is.logical(legendName)) {
        if (legendName) {
                legendName <- labels(loadings, by)
        } else {
                legendName <- NULL
        }

    }

    #  Choose the way of normalization
    if (normalize == "auto") {
            normalize <- any(loadings$spc > 0) - any(loadings$spc < 0)
    }

    loadings <- switch(as.character(as.numeric(normalize)),
                       `0` =  loadings,
                     # `+1` normalize to max value
                       `1` =  sweep(loadings, 1, max, `/`),
                     # `-1` normalize to min value
                      `-1` =  sweep(loadings, 1, min, `/`),
                       stop("Parameter 'normalize' is incorrect. Must be either -1, 0 or 1.")
    )

    #
    l <- loadings

    # Select variable with component names
    by <- as.character(by)

    #if variable does not exist
    if (by %!in% colnames(l)) {
        # l[,by] = as.factor(as.numeric(rownames(l)))
        l[,by] = as.factor(rownames(l))
    }

    if (by != 'cNames') {colnames(l)[colnames(l) == by] <- 'cNames'}

    l$rows = 1:nrow(l)                 # Create variable with row numbers
    l <- l[,c('spc','cNames','rows')]  # Rename variables

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
    nPal <- length(palette)
    nColNeeded <-  l$cNames %>% as.factor  %>% droplevels  %>% nlevels
    if (nPal < nColNeeded) {
        if (nPal > 0){
            warning(sprintf(paste("There are %d colors in provided palette",
                                  "and %d are needed, thus the DEFAULT colors",
                                  "will be used."), nPal, nColNeeded))
        }
        palette <- NULL

    }
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    # Plot
    p <- ggplot(l, aes(x = .wavelength,
                       y = spc,
                        group = rows,
                        color = cNames,
                        fill  = cNames)) +
        geom_hline(yintercept = 0, size = 1,linetype = 1, alpha = .4) +
        geom_line(size = line_size) +
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

    return(p)
}
#  ------------------------------------------------------------------------
#  [Internal function]
#  Function to to calculate position of ticks
number_ticks <- function(n, min.n = 2, ...) {
    function(limits) pretty(limits, n, min.n = min.n,...)
}

