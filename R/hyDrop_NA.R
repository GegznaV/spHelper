# Remove empty columns ----------------------------------------------------

#' [+] Remove variables of \code{hyperSpec} object that contain only \code{NA} values
#'
#' Remove variables (columns) of \code{\link[=hyperSpec-class]{hyperSpec}}
#' object that contain only \code{NA} values.
#'
#' @template sp-hy
#'
#' @return Object without columns with all \code{NA} values.
#' @export
#'
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna
#'
hyDrop_NA <- function(sp){
    NAcols          <- colSums(is.na(sp$..)) == nrow(sp)
    dropTheseNames  <- names(NAcols)[NAcols]
    sp <- sp[,!(colnames(sp) %in% dropTheseNames)]
    return(sp)
}
