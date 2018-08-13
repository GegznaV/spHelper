#' [.] Compute a mode (most frequent value)
#'
#' Function, that defines, what is a Mode (a statistic measure)
#' @source \href{http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode}{stackoverflow.com - Mode}
#' @param x A matrix-like data.
#'
#' @export
#'
Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
