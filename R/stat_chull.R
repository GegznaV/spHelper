# ***** ggplot LAYER ***** ------------------------------------------------

#' [+] A convex hull layer for \code{ggplot2}
#'
#' Create a layer of convex hull for \pkg{ggplot2} plots.\cr
#' @source Source: vignette \href{https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html}{Extending ggplot2}
#' @author Hadley Wickham
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}
#' or \code{\link[ggplot2]{aes_}}. If specified and \code{inherit.aes = TRUE} (the
#' default), it is combined with the default mapping at the top level of the
#'plot. You must supply \code{mapping} if there is no plot mapping.
#' @param data A layer specific dataset - only needed if you want to override
#'        the plot defaults.
#' @param geom The geometric object to use display the data.
#' @param position The position adjustment to use for overlapping points on
#'        this layer.
#' @param na.rm If \code{FALSE} (the default), removes missing values with a warning.
#'        If \code{TRUE} silently removes missing values.
#' @param show.legend logical. Should this layer be included in the legends?
#'       \code{NA}, the default, includes if any aesthetics are mapped.
#'       \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'          rather than combining with them. This is most useful for helper functions
#'          that define both data and aesthetics and shouldn't inherit behaviour from
#'          the default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... Other arguments passed on to \code{\link{layer}}. These are
#'        often aesthetics, used to set an aesthetic to a fixed value, like
#'        \code{color = "red"} or \code{size = 3}.
#'
#' @return Layer of convex hull for \pkg{ggplot2} plots.
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'              geom_point() +
#'              stat_chull(fill = NA, colour = "black")
#'
#'
#' ggplot(mpg, aes(displ, hwy, colour = drv)) +
#'              geom_point() +
#'              stat_chull(fill = NA)
#'
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'              stat_chull(geom = "point", size = 4, colour = "red") +
#'              geom_point()
#'
#' @family \pkg{spHelper} plots
#'
stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...)
{
    StatChull <- ggplot2::ggproto("StatChull", Stat,
                                  compute_group = function(data, scales) {
                                      data[chull(data$x, data$y), , drop = FALSE]
                                  },

                                  required_aes = c("x", "y")
                                )
    layer(
        stat = StatChull, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
        )
}
