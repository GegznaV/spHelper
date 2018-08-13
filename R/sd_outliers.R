#' [!] Outlier detection based on standard deviations (sd)
#'
#'  Detect elements in each \emph{column} that are outside mean +/- n standard
#'  deviations and rows that contain at least one such element.
#'
#' @param x Matrix-like data or \code{hyperSpec} object.
#' @param n A number of standard deviations.
#'
#' @return A list with these elements \itemize{
#' \item{\code{matrix} - a logical matrix, with \code{TRUE} for elements which
#'       are outside column mean +/- n standard deiations;}
#'  \item{\code{rows} - a logical vector that indicate rows that have at least
#'      one such element.}
#'  }
#'
#' @export
#' @seealso \link{mean_Nsd}
#'
#' @family \pkg{spHelper} utilities
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @family outlier detection functions in \pkg{spHelper}
#' @author Vilmantas Gegzna
#'
sd_outliers <- function(x, n = 2) {
    zScores <- scale(x);
    logicalMatrix <- (zScores < -n) | (zScores > n)
    RowLogical <- apply(logicalMatrix, 1, any)    # iRow <- which(RowLogical)
    return(list(matrix = logicalMatrix, rows = RowLogical))
}
# +/- is code for ±


#  ------------------------------------------------------------------------
#' @rdname sd_outliers
#' @export
outside_mean_pm_Nsd <- function(x, n = 2) {
    zScores <- scale(x);
    logicalMatrix <- (zScores < -n) | (zScores > n)
    RowLogical <- apply(logicalMatrix, 1, any)    # iRow <- which(RowLogical)
    return(list(matrix = logicalMatrix, rows = RowLogical))
}
