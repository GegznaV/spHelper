#' [!]Additive planar transform
#'
#'
#' Compute the additive planar transform of a (dataset of) compositions.
#' Default method is function \code{\link[compositions]{apt}} from package \pkg{compositions}.
#'
#' @param x A numeric data frame or matrix.
#' @param ... further parameters to \code{\link[compositions]{apt}}.
#'
#' @return Returns the transformed data matrix with the same dimension as
#'         \code{x}.
#' @export
#' @seealso \code{\link[compositions]{apt}}
#' @family row-wise transformations
#'
#' @examples
#' apt(Spectra2)
#'
#' plot(Spectra2)
#' plot(apt(Spectra2))
apt <- function(x, ...) {UseMethod("apt")}

#' @rdname apt
#' @export
apt.default <- function(x,...) {compositions::apt(x,...)}

#' @rdname apt
#' @export
apt.hyperSpec <- function(x, ...)
{
    .local <- function (x, ...)
    {
        m <- compositions::apt(x@data$spc, ...)

        decomposition(x, m,
                      # the last wavelength is removed
                      wavelength = x@wavelength[-nwl(x)],
                      # retain labels
                      label.wavelength = labels(x)$.wavelength,
                      label.spc = labels(x)$spc
        )
    }
    .local(x, ...)
}
