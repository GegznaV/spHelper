#' [!] Remove facet stripes from `ggplot2` plot
#'
#' A convenience function to remove facet stripes from `ggplot2` plot.
#' @details
#'
#'  \code{rm_stripes} is a wrapper of function \code{\link[ggplot2]{theme}}
#'  with one of the 3 options (depending on value of input variable "axis")
#'  \code{strip.text.x}, \code{strip.text.y}, or
#'  \code{strip.text} set to \code{element_blank()}.
#'
#' @param axis String indicating axis from which the stripes should be removed:
#' either \code{"x"}, \code{"y"}, \code{"xy"}. Default is \code{"xy"}.
#'
#' @template ggplot-updated
#' @export
#'
#' @examples
#'
#' p <- ggplot(Spectra2, wl.range = c(min ~ 420, 500~600)) +
#'      geom_line(aes(color = gr)) +
#'      facet_wl("class")
#'
#' p
#' p + rm_stripes("x")
#' p + rm_stripes()
#'
#' @author Vilmantas Gegzna
#' @seealso Function \code{\link[ggplot2]{theme}} from package \pkg{ggplot2}.
#' @family \pkg{spHelper} plots

rm_stripes <- function(axis = "xy"){
    switch(axis,
           "x" =    theme(strip.text.x = element_blank()),
           "y" =    theme(strip.text.y = element_blank()),
           # Default value
           theme(strip.text   = element_blank())
    )
}
