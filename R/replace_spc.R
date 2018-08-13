#' [!!!] Replace \code{object$spc} and \code{colnames(object$spc)}
#'
#'
#' @inheritParams hyperSpec::decomposition
#' @param ... further arguments to \link[hyperSpec]{decomposition}
#'
#' @return A \code{hyperSpec} object, updated according to x
#' @export
#'
#' @examples
#'
#' ratios <- colRatios(Scores2[[]])
#' obj1 <- replace_spc(Scores2, ratios)
#'
#' colnames(obj1$spc)  # preserved
#'
#' obj2 <- decomposition(Scores2, ratios)
#' colnames(obj2$spc)  # NOT preserved
#'
#'
#' @seealso \link[hyperSpec]{decomposition}
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna
#'

replace_spc <- function(object, x, ...){
    col_names_x <- colnames(x)

    obj <- decomposition(object, x, ...)

    colnames(obj$spc) <- col_names_x
    return(obj)
}
