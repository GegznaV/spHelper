
# get_LOF_by_class --------------------------------------------------------

#' Calculate lack_of_fit (LOF) for spectroscopic data by class
#'
#'
#' @param obj - hyperSpec object.
#' @param by - string with name of factor variable in `obj`.
#' @param FUN - function used to calculate expected values.
#'
#' @return Function returns a list of 4 objects:\cr
#'         data - object `obj`;\cr
#'         centers - hyperSpec object in which variable $spc is matrix with
#'                   expected values (means, medians, etc. indicated by `FUN`)
#'                   for every row;\cr
#'         difference - hyperSpec object in which variable $spc is matrix with
#'                      differences between observed and expected values;\cr
#'         LOF - vector of lack-of-fit values for every row.
#'
#' @export
#'
#' @examples
#' library(spHelper)
#' obj0 <- get_LOF_by_class(obj = Spectra2, by = "gr")
#' str(obj0, max.level = 1)
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' #  Unit tests:
#' #
#' #  require(testthat)
#' # test_that("classes and their centers are sorted correctly",{
#' # require(hyperSpec)
#' # set.seed(1)
#' # obj <- hyperSpec::sample(Spectra2, 150)
#' #
#' # LOF_obj <- get_LOF_by_class(obj, "gr")
#' # N_unique <- (!duplicated(
#' #     LOF_obj$centers[,c("gr","spc")]  %>%
#' #    as.wide.df)) %>%  sum
#' # expect_equal(N_unique, nlevels(obj$gr))
#' #  })
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @family LOF_obj
#' @import data.table
#'
get_LOF_by_class <- function(obj, by, FUN = median){
    # Check validity of inputs
    # require(hyperSpec)
    chk.hy(obj)
    if (!is.character(by) | (length(by) != 1))
        stop("`by` must be character vector of length 1")

    # Extract values
    by_val <- getVarValues(by, obj$.)
    LABELS <- labels(obj)

    # Calculate centers
    # require(data.table)
    centers_by_CLASS_for_every_row <-
    (merge( # merge two datatables:
            # (x - vector of classes and y mean of each class) to create
            # matrix of the size as original obj$spc.
            x = data.table(by_val),
            y = (obj[, c(by, "spc")] %>% # subset only necessary variables
                     as.wide.df() %>%   # convert hyperSpec to appropriate DF
                     data.table(check.names = TRUE) # convert to data table
            )[, lapply(.SD, FUN), by], # calculate expected value of each class
            by.x = "by_val",
            by.y = by,
            sort = FALSE           # do not sort to keep the structure of rows;
        )[, by_val := NULL] # keep numeric variables only.
    ) %>% as.matrix()

    # Make a hyperSpec with centers in `$spc`
    center_of_every_CLASS <- decomposition(obj,
                                           centers_by_CLASS_for_every_row,
                                           wavelength = obj@wavelength)
    labels(center_of_every_CLASS) <- LABELS #reset labels

    # Calculate differences and LOF
    spDiff <- obj - center_of_every_CLASS
    LOF <- quality_of_fit(obj = center_of_every_CLASS, fit = obj)$LOF

    # Return results
    output <- list(data = obj,
                   centers = center_of_every_CLASS,
                   differences = spDiff,
                   LOF = LOF)
    class(output) <- c("LOF_obj", class(output))
    return(output)
}


# plot_LOF_sp -----------------------------------------------------------------

#' Plot spectra colored by LOF values
#'
#' @param obj - \code{LOF_obj} object created with function \code{\link{get_LOF_by_class}}.
#' @param LOF - vector of lack-of-fit values for every row;
#' @param LOF_threshold - LOF threshold values. Rows (lines) that have threshold above
#' @param type - type of plot, string wit one of the following: "data", "differences", "centers". May be abbreviated.
#' @param stacked Either "TRUE" or vector with for stacking.
#'                 More details in \code{\link[hyperSpec]{plotspc}}.
#' @param x,y  position of legend. See \code{\link[graphics]{legend}}.
#' @param colors Colors names for LOF below and above threshold default are: \code{c("black", "red")}.
#'
#' @examples
#' library(spHelper)
#' obj <- get_LOF_by_class(Spectra2, "gr")
#'
#' plot_LOF_sp(obj, "data", "gr")
#' plot_LOF_sp(obj, "differences", "gr")
#' plot_LOF_sp(obj, "centers", "gr")
#'
#' @export
#'
#' @family LOF_obj

plot_LOF_sp <- function(obj,type = c("data", "differences", "centers"),
                        stacked = TRUE, LOF_threshold = 10,
                        x = "topleft", y = NULL, colors =  c("black", "red"),
                        LOF = obj$LOF){
    # Selectcolors
    col <- factor(LOF > LOF_threshold,
                  levels = c(FALSE, TRUE),
                  labels = colors
    )  %>%
        as.character

    # Select type of plot
    type <- match.arg(type[1], c("data", "differences", "centers"))
    obj0 <- obj[[type]]

    # plot

    plot_stacked(obj0,
                 stacked = stacked,
                 col = col)
    grid()

    if (!is.null(x)){
        legend(x, y,
               col = colors,
               pch = 20,
               legend = paste("LOF", c("<",">"), LOF_threshold, "%"), #e.g. c("LOF < 10 %", "LOF > 10 %"),
               bty = "n")
    }
}


# plot_LOF_obj ------------------------------------------------------------

#' Plot MDS of LOF_obj data
#'
#'
#' @param obj - either "LOF_obj" object or "hyperspec" object.
#' @param x,y  position of legend. See \code{\link[graphics]{legend}}.
#' @param colors Colors names for LOF below and above threshold default are: \code{c("black", "red")}.
#' @inheritParams plot_LOF_sp
#'
#' @export
#'
#' @examples
#' library(spHelper)
#' obj <- get_LOF_by_class(Spectra2, "class")
#'
#' plot_LOF_MDS(obj, "data",        x = NULL)
#' plot_LOF_MDS(obj, "differences", x = NULL)
#' plot_LOF_MDS(obj, "centers",     x = NULL)
#'
#' @family LOF_obj

plot_LOF_MDS <- function(obj, type = c("data", "differences", "centers"),
                         LOF_threshold = 10,
                         x = "topright", y = NULL,
                         colors =  c("black", "red"), LOF = NULL){

    # Select variables to plot
    if (inherits(obj, "LOF_obj")) {
        if (is.null(LOF)) LOF <- obj$LOF

        # Select type of plot
        type <- match.arg(type[1], c("data", "differences", "centers"))

        TITLE <- switch(type,
               data =        "MDS representation of spectra",
               differences = "MDS representation of difference spectra",
               centers =     "MDS representation of group centers"
               )
        obj <- obj[[type]]

    } else
        TITLE <- "MDS representation of spectra"

    if (inherits(obj, "hyperSpec")) obj <- obj[[]]

    # Select colors
    col <- factor(LOF > LOF_threshold,
                  levels = c(FALSE, TRUE),
                  labels = colors
    )  %>%
        as.character

    # Do MDS
    obj_0 <- obj %>% dist() %>% cmdscale(k = 2)

    # Plot 1
    plot(obj_0,
         type = "n",
         main = TITLE,
         xlab = "MDS coordinate 1",
         ylab = "MDS coordinate 2"
    )

    grid()

    points(obj_0,
           pch = 20,
           col = col
    )

    if (!is.null(x)){
        legend(x, y,
               col = colors,
               pch = 20,
               legend = paste("LOF", c("<",">"), LOF_threshold, "%"),
                     #e.g. c("LOF < 10 %", "LOF > 10 %"),
               bty = "o",
               box.col = "grey70"
        )
    }
}

# plot_LOF_hist -----------------------------------------------------------

#' Plot histogram of LOF values
#'
#' @param LOF - either "LOF_obj" object or vector with LOF values.
#' @param x,y  position of legend. See \code{\link[graphics]{legend}}.
#' @inheritParams graphics::hist
#'
#' @export
#'
#' @family LOF_obj

plot_LOF_hist <- function(LOF, LOF_threshold = 10,  breaks = "Sturges",
                          col = "tomato3", x = "topright", y = NULL) {
    if (inherits(LOF, "LOF_obj")) LOF <- LOF$LOF
    # Plot 2
    hist(LOF,
         col = col,
         breaks = breaks,
         xlab = "Lack-of-fit (LOF), %",
         ylab = "Number of spectra",
         main = "Distribution of LOF values")

    abline(v = LOF_threshold,
           lwd = 2,
           lty = 2)

    legend(x,y,
           legend = paste("LOF =", LOF_threshold),
           lwd = 2,
           lty = 2,
           bty = "o",
           box.col = "grey70")
}

# plot_stacked spectra -------------------------------------------------------

#' Plot stacked spectra
#'
#' Convenience function to plot stacked spectra.
#' More details in \code{\link[hyperSpec]{plotspc}}
#'
#' @param obj - hyperSpec object.
#' @param ... - further arguments to `plotspc()`.
#' @inheritParams hyperSpec::plotspc
#'
#' @export
#'
#' @examples
#' library(spHelper)
#' plot_stacked(Spectra2,"gr")
#'
#' @family LOF_obj


plot_stacked <- function(obj, stacked = TRUE, spc.nmax = nrow(obj), ...){

    chk.hy(obj)
    stacked <- getVarValues(stacked, obj)
    ARGS <-
        ARGS <- modifyList(x = list(object = obj,
                                    stacked = stacked,
                                    spc.nmax = spc.nmax),
                           val = list(...))

    do.call(plotspc, ARGS)

}

# plot_stacked spectra in ggplot2 -------------------------------------------

# This function is much slower than R `base` alternative.

# ggplot(Spectra, aes(fill = ID,color = ID)) +
#     geom_line(alpha=.5, size = .8) +
#     facet_grid(ID~.) +
#     theme(legend.position="none")
#

#
