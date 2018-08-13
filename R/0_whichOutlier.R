#' [! DEPRECATED] Find indices of rows that contain outlier column-wise outliersscores
#'
#' [! DEPRECATED] Return indices of rows in scores matrix that contain outliers. A row is
#' treated as having an oultier if any
#' \href{http://wiki.eigenvector.com/index.php?title=Advanced_Preprocessing:_Variable_Scaling#Autoscale}{autoscaled}
#' score in that row is not between \code{±zLimit}.
#'
#' @param x Matrix-like data.
#'
#' @param zLimit A threshold for standardized (scaled) values
#' (i.e., \href{https://en.wikipedia.org/wiki/Standard_score}{z-scores})
#'  to be treated as an outlier. If \code{(-zLimit) < scale(scores)} or
#'        \code{scale(scores) > (+zLimit)} it is treated as an outlier.\cr
#'       Default \code{zLimit = 2}.
#'
#' @return Vector of indices that indicate rows containing outliers.
#' @export
#'
#' @examples
#'
#'
#' whichOutlier(Scores2)
#' whichOutlier(Scores2,3)
#'
#' @family component analysis / factorisation related functions in \pkg{spHelper}
#' @author Vilmantas Gegzna
#'
whichOutlier <- function(x, zLimit = 2) {
    .Deprecated("outside_mean_pm_Nsd", package = "spHelper")

    sx <- scale(x);
    logicalMatrix <- (sx < -zLimit) | (sx > zLimit)
    iRow <- which(apply(logicalMatrix, 1, any))
    return(iRow)
}

