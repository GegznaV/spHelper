# ***** Add title with SubTitle ***** ------------------------------------------------------

#' @title [+] Add bold title and subtitle to a plot
#'
#' @description Add bold title with second line in smaller font with subtitle.
#'               Functions \code{\link[base]{bquote}} and
#'                \code{\link[grDevices]{atop}} are used to achieve
#'                this effect.
#'
#' @section Warning!:
#'
#' May not work with plotting functions from package \pkg{hyperSpec}.
#'
#' @param Title The first line of title, which will be in bold.
#' @template subtitle
#'
#' @return Formated title.
#' @export
#'
#'
#' @examples
#'
#' subt("Cars")
#' ## bold("Cars")
#'
#' subt("Cars","Distance vs. speed")
#' ## atop(bold("Cars"), atop(italic("Distance vs. speed")))
#'
#' # ----------------------------------------------------------------
#'
#' plot(cars[,1:2], main = "Cars")
#' plot(cars[,1:2], main = subt("Cars")) # the same as in previous line
#' plot(cars[,1:2], main = subt("Cars","Distance vs. speed"))
#' plot(cars[,1:2], main = subt(subTitle = "Distance vs. speed"))
#'
#' # ----------------------------------------------------------------
#'
#' library(ggplot2)
#'
#' g <- qplot(mpg, wt, data=mtcars)
#' g + ggtitle("Cars") # non-bold title
#' g + ggtitle(subt("Cars")) # bold title
#' g + ggtitle(subt("Cars","Distance vs. speed"))
#' g + ggtitle(subt(subTitle = "Distance vs. speed"))
#'
#'
#' # ----------------------------------------------------------------
#'
#' library(lattice)
#'
#' xyplot(eruptions~waiting, data = faithful)
#'
#' xyplot(eruptions~waiting, data = faithful,
#'  main = "Old Faithful Geyser Data")
#'
#' xyplot(eruptions~waiting, data = faithful,
#'  main = subt("Old Faithful Geyser Data"))
#'
#' xyplot(eruptions~waiting, data = faithful,
#'  main = subt("Old Faithful Geyser", "Data"))
#'
#' xyplot(eruptions~waiting, data = faithful,
#'  main = subt(subTitle = "Old Faithful Geyser Data"))
#'
#' @family \pkg{spHelper} utilities
#' @author Vilmantas Gegzna

subt <- function(Title = NULL, subTitle = NULL)  {
    # library(grDevices)
    # Format Title
    if (is.null(subTitle)) {#If subtitle is not provided, use only main title
        Title <- bquote(bold(.(Title)))
    } else {# otherwise add the subtitle
        Title <- bquote(atop(bold(.(Title)),
						atop(italic(.(subTitle)))))}

    return(Title)
}
