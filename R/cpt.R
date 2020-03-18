#' [!] Centered planar transform
#'
#'
#' A data transformation according to the centered planar
#' transformation is done. Default method is function
#' \code{\link[compositions]{cpt}} from package \pkg{compositions}.
#'
#' @param x A numeric data frame or matrix.
#' @param ... further parameters to \code{\link[compositions]{cpt}}.
#'
#' @return Returns the transformed data matrix with the same dimension as
#'         \code{x}.
#' @export
#' @seealso \code{\link[compositions]{cpt}}
#' @family row-wise transformations
#'
#' @examples
#' cpt(Spectra2)
#'
#' plot(Spectra2)
#' plot(cpt(Spectra2))
cpt <- function(x, ...) {UseMethod("cpt")}

#' @rdname cpt
#' @export
cpt.default <- function(x,...) {compositions::cpt(x,...)}

#' @rdname cpt
#' @export
cpt.hyperSpec <- function(x, ...)
{
    .local <- function(x, ...)
    {
        validObject(x)
        x[[]] <- compositions::cpt(x[[]],...)
        x
    }
    .local(x, ...)
}
