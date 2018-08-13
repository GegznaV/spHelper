#' [!] Isometric logratio transformation
#'
#' Compute the isometric log ratio transform of a (dataset of) composition(s).
#'  Default method is function
#' \code{\link[compositions]{ilr}} from package \pkg{compositions}.
#'
#' @param x A composition, not necessarily closed. (A numeric data frame or matrix.)
#' @param ... further parameters to \code{\link[compositions]{ilr}}.
#'
#' @return Returns the transformed data matrix with one dimension less than
#'      \code{x}. Thus for \code{hyperSpec} object the last value of
#'      \code{x$.wavelength} is removed.
#' @export
#' @seealso \code{\link[compositions]{ilr}}, \code{\link[compositions]{ilrBase}}
#' @family row-wise transformations
#'
#' @examples
#' ilr(Spectra)
#'
#' plot(Spectra)
#' plot(ilr(Spectra))
#'
ilr <- function(x, V = ilrBase(x), ...) {
    UseMethod("ilr")
}


#' @rdname ilr
#' @method ilr default
#' @export
ilr.default <- function(x, V = ilrBase(x), ...) {
    compositions::ilr(x = x, V = V, ...)
    }

#' @rdname ilr
#' @method ilr hyperSpec
#' @export
ilr.hyperSpec <- function (x, V = ilrBase(x), ...)
{
    .local <- function (x, ...)
    {
        m <- compositions::ilr(x = x@data$spc, V = V, ...)

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

