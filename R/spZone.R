#' [!] Annotate x axis range in ggplot2 graphs
#'
#' A wrapper function to annotate x axis range for ggplot2 graphs
#'
#' @param xmin Minimum value of x axis.
#' @param xmax Maximum value of x axis.
#' @param x formula indicating min and max values (\code{xmin ~ xmax}).
#' @param fill Fill color.
#' @param alpha Fill transperency.
#' @param ... further arguments to \code{\link[ggplot2]{annotate}}.
#'
#' @export
#'
#' @examples
#'
#' qplot_sp(flu) + spZone(425~450)
#' qplot_sp(flu) + spZone(460,490, fill = "green")
#'
spZone <- function(xmin, xmax, fill="red", alpha=0.2,...) {
    UseMethod("spZone")
}

#' @rdname spZone
#' @export
spZone.default <- function(xmin, xmax, fill="red", alpha=0.2,...) {
    annotate("rect",
             xmin=xmin, xmax=xmax,
             ymin=-Inf, ymax=Inf,
             fill=fill, alpha=alpha,...)
}

#' @rdname spZone
#' @export
spZone.formula <- function(x, fill="red", alpha=0.2,...) {
    X <- as.character(x)[-1] %>% as.numeric()

    if (length(X) != 2) stop("Formula must have shape `xmin ~ xmax`.")
    spZone(X[1],X[2],fill = fill, alpha = alpha, ...)
}
