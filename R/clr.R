#' [!] Centered logratio transformation
#'
#'
#' A data transformation according to the centered logratio
#' transformation is done. default method is function
#' \code{\link[compositions]{clr}} from package \pkg{compositions}.
#'
#' @param x A numeric data frame or matrix.
#' @param ... further parameters to \code{\link[compositions]{clr}}.
#'
#' @return Returns the transformed data matrix with the same dimension as
#'         \code{x}.
#' @export
#' @seealso \code{\link[compositions]{clr}}
#' @family row-wise transformations
#'
#' @examples
#' clr(Spectra2)
#'
#' plot(Spectra2)
#' plot(clr(Spectra2))
clr <- function(x, ...) {UseMethod("clr")}

#' @rdname clr
#' @export
clr.default <- function(x,...) {compositions::clr(x,...)}

#' @rdname clr
#' @export
clr.hyperSpec <- function(x, ...)
{
    .local <- function(x, ...)
    {
        validObject(x)
        x[[]] <- compositions::clr(x[[]])
        x
    }
    .local(x, ...)
}
