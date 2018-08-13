#' Return transposed matrix of spectra in hyperSpec object
#'
#' Transpose matrix of spectra in hyperSpec object, add column with wavelengths
#' and return as a dataframe
#'
#' @param obj hyperSpec object
#'
#' @return A data frame with wavelengths in the first column and each row of
#' hyperSpec object in a separate column.
#' @export
#'
#' @examples
#'   library(spHelper)
#'
#'   chondro[[]][1,1:6]
#'   hy_spc2df(chondro)[1:6,1:2]

hy_spc2df <- function(obj){
    data <- t(hyperSpec::as.wide.df(obj[,"spc"]))
    rownames(data) <- make.unique(rownames(data))
    data.frame(wl = hyperSpec::wl(obj),data)
}

# =============================================================================
