#' [!] Gap-Segment Derivative
#'
#' Gap-Segment derivatives of a data \code{matrix}, \code{data.frame},
#' \code{vector} or \code{hyperSpec} object.
#'
#' @inheritParams prospectr::gapDer
#' @return A \code{matrix}, \code{vector} or \code{hyperSpec} object with
#'         the filtered signal(s).
#' @export
#'
#' @note
#' Find examples in \code{\link[prospectr]{gapDer}}.
#'
#' @details
#'
#' Method \code{gapDer.default} is the function \code{\link[prospectr]{gapDer}}
#' from package \pkg{prospectr}. \cr
#'
#' Method \code{gapDer.default} is the function \code{\link[prospectr]{gapDer}}
#' from package \pkg{prospectr}. \cr
#'
#' The sampling interval specified with the \code{delta.wav} argument is used
#' for scaling and get numerically correct derivatives. \cr
#'
#' The convolution function is written in C++/Rcpp for faster computations.
#'
#' @author Antoine Stevens (function \code{prospectr::gapDer})\cr
#' Vilmantas Gegzna (method \code{gapDer.hyperSpec})
#'
#' @references
#' Hopkins (2002). NIR News 14(5), 10.
#'
#' @seealso \code{\link[prospectr]{gapDer}}
#' @family methods for \code{\link[=hyperSpec-class]{hyperSpec}} in \pkg{spHelper}
#' @family \pkg{prospectr} function for \pkg{hyperSpec}
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}

gapDer <- function(X, m = 1, w = 1, s = 1, delta.wav){
    UseMethod("gapDer")
}

#' @rdname gapDer
#' @export
gapDer.default <- function(X, m = 1, w = 1, s = 1, ...){
    prospectr::gapDer(X, m, w, s, ...)
}

#' @rdname gapDer
#' @export
gapDer.hyperSpec <- function(X, m = 1, w = 1, s = 1, ...){
    chk.hy(X)
    Sp <- X; rm(X)
    X  <- Sp[[]]
    colnames(X) <- wl(Sp)
    X  <- prospectr::gapDer(X, m, w, s, ...)
    sp <- new("hyperSpec", spc = X, data = Sp$.., labels = labels(Sp))
    labels(sp) <- labels(Sp)
    return(sp)
}

