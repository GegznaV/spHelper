# Reconstruct spectra --------------------------------------------------------
#
#' @name reconstructSp
#' @aliases reconstructSp
#' @aliases getReconstructed
#'
#' @title [+] Reconstruct spectra from loadings and scores (i.e. components and amplitudes)
#'
#' @description Reconstruct spectra from loadings and scores (i.e. from
#' spectroscopic components and their amplitudes).
#'
#' @template loadings
#' @template scores
#' @param sp \code{hyperSpec} object which has the same number or rows as
#'  \code{scores}. If \code{sp} is provided, reconstructed spectra will replace
#' other spectroscopic data in \code{sp}. This object will be returned as a
#' result of this function.
#'
#' @return Reconstruction of the original spectroscopic data matrix, based on
#'         \code{scores} and \code{loadings} as either a matrix or
#'         \code{hyperSpec} object. \cr
#'      \code{reconstructed <- scores \%*\% loadings}
#' @export
#'
#' @seealso \code{\link[ChemometricsWithR]{reconstruct}} in \pkg{ChemometricsWithR}, \cr
#'          \code{\link[ChemometricsWithR]{project}} in \pkg{ChemometricsWithR},\cr
#'          \code{\link[Rssa]{reconstruct}} in \pkg{Rssa}
#'
#' @examples
#' reconstructSp(Loadings2, Scores2)
#' reconstructSp(Loadings2, Scores2, Spectra2)
#'
#' @import hyperSpec
#' @family component analysis / factorisation related functions in \pkg{spHelper}
#' @author Vilmantas Gegzna
#'
reconstructSp  <-  function(loadings, scores, sp = NULL)     {
    reconstructed <- (hy2mat(scores)) %*% (hy2mat(loadings))
    if (class(sp) == "hyperSpec")   {
        sp[[]] <- reconstructed
        return(sp)            # hyperSpec object
    } else  {
        return(reconstructed) # matrix
    }
}

