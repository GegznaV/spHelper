#' [!] Isometric planar transformation
#'
#' Compute the isometric planar transform of a (dataset of) composition(s).
#'  Default method is function
#' \code{\link[compositions]{ipt}} from package \pkg{compositions}.
#'
#' @param x A composition, not necessarily closed. (A numeric data frame or matrix.)
#' @param ... further parameters to \code{\link[compositions]{ipt}}.
#'
#' @return Returns the transformed data matrix with one dimension less than
#'      \code{x}. Thus for \code{hyperSpec} object the last value of
#'      \code{x$.wavelength} is removed.
#' @export
#' @seealso \code{\link[compositions]{ipt}}, \code{\link[compositions]{ilrBase}}
#' @family row-wise transformations
#'
#' @examples
#' ipt(Spectra2)
#'
#' plot(Spectra2)
#' plot(ipt(Spectra2))
#'
ipt <- function(x, V = ilrBase(x), ...) {
    UseMethod("ipt")
}


#' @rdname ipt
#' @export
ipt.default <- function(x, V = ilrBase(x), ...) {
    compositions::ipt(x = x, V = V, ...)
}

#' @rdname ipt
#' @export
ipt.hyperSpec <- function (x, V = ilrBase(x), ...)
{
    .local <- function (x, ...)
    {
        m <- compositions::ipt(x = x@data$spc, V = V, ...)

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
