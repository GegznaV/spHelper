#' [!] Signal binning
#'
#' Compute average values of a signal in pre-determined bins (col-wise subsets).
#'  The bin size can be determined either directly or by specifying the number
#'  of bins. Sometimes called boxcar transformation in signal processing
#' @inheritParams prospectr::binning
#' @return A \code{matrix}, \code{vector} or \code{hyperSpec} object with
#'         the filtered signal(s).
#' @export
#'
#' @note
#' Find examples in \code{\link[prospectr]{binning}}.
#'
#' @details
#'
#' Method \code{binning.default} is the function \code{\link[prospectr]{binning}}
#' from package \pkg{prospectr}. \cr
#'
#' Method \code{binning.default} is the function \code{\link[prospectr]{binning}}
#' from package \pkg{prospectr}. \cr
#'
#'
#' @author Antoine Stevens & Leonardo Ramirez-Lopez (function \code{prospectr::binning})\cr
#' Vilmantas Gegzna (method \code{binning.hyperSpec})
#'
#' @seealso \code{\link[prospectr]{binning}}
#'
#' @family methods for \code{\link[=hyperSpec-class]{hyperSpec}} in \pkg{spHelper}
#' @family \pkg{prospectr} function for \pkg{hyperSpec}
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}


binning <- function(X, bins, bin.size){
    UseMethod("binning")
}

#' @rdname binning
#' @export

binning.default <- function(X,...){
    prospectr::binning(X,...)
}

#' @rdname binning
#' @export
binning.hyperSpec <- function(X, ...){
    chk.hy(X)
    Sp <- X; rm(X)
    X  <- Sp[[]]
    colnames(X) <- wl(Sp)
    X  <- prospectr::binning(X, ...)
    sp <- new("hyperSpec", spc = X, data = Sp$.., labels = labels(Sp))
    return(sp)
}

