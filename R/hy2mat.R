# Convert hyperSpec object to matrix --------------------------------------
#
#' Extract matrix from either \code{hyperSpec} object or a matrix
#'
#' hy2mat
#'
#' @template sp
#' @return Either a matrix or an error, if it's impossible to extract a matrix.
#' @examples
#'
#' data(flu, package = "hyperSpec")
#'
#' flu
#' a <- hy2mat(flu)
#' b <- hy2mat(flu$spc)
#'
#' identical(a,b)
#' ## [1] TRUE
#'
#'
#' hy2mat(matrix(NA,5,10))
#'
#' @export
#'
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna
#'
hy2mat <- function(sp)
{    switch(class(sp),
            "hyperSpec" = sp$spc,
            "matrix"    = sp,
            stop('The class of the input must be either "hyperSpec" or "matrix"')
            )
}
