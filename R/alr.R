#' [!] Additive logratio transformation
#'
#' Compute the additive log ratio transform of a (dataset of) composition(s).
#'  Default method is function
#' \code{\link[compositions]{alr}} from package \pkg{compositions}.
#'
#' @param x A composition, not necessarily closed. (A numeric data frame or matrix.)
#' @param ivar The number of column in \code{x} to be used as denominator variable.
#'        By default the last column is taken.
#' @param ... further parameters to \code{\link[compositions]{alr}}.
#'
#' @return Returns the transformed data matrix with one dimension less than
#'      \code{x}. Thus for \code{hyperSpec} object the last value of
#'      x$.wavelength is removed.
#' @export
#' @seealso \code{\link[compositions]{alr}}
#' @family row-wise transformations
#'
#' @examples
#' data(Spectra2)
#' Spectra <- Spectra2[,,300~600]
#' alr(Spectra)
#'
#' ind <- wl2i(Spectra, 550) # number of column at 550 nm.
#' alr(Spectra, ind)
#'
#' plot(Spectra)
#' plot(alr(Spectra))
#' plot(alr(Spectra, ind))
#'
alr <- function(x, ...) {UseMethod("alr")}


#' @rdname alr
#' @method alr default
#' @export
alr.default <- function(x, ivar = ncol(x),...) {compositions::alr(x, ivar,...)}

#' @rdname alr
#' @method alr hyperSpec
#' @export
alr.hyperSpec <-     function (x, ivar = ncol(x$spc), ...)
{
    .local <- function (x, ...)
    {
        m <- compositions::alr(x@data$spc, ivar = ivar,...)
        decomposition(x, m,
                      # the last wavelength is removed
                      wavelength = x@wavelength[-ivar],
                      # retain labels
                      label.wavelength = labels(x)$.wavelength,
                      label.spc = labels(x)$spc
        )
    }
    .local(x, ...)
}
