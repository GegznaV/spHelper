#' [+] Calculate amplitudes of spectroscopic components (a.k.a. scores) and label the resulting object
#'
#' Calculate amplitudes of spectroscopic components (a.k.a. scores) by
#' matrix multiplication (see section "Detais") and properly label created
#' \code{hyperSpec} object.
#'
#' @details
#' Equation of matrix multiplication to calculate scores:
#'
#' \deqn{scores = sp * loadings * inv(loadings' * loadings)}
#'
#' This formula is taken and adapted from [1].
#'
#' @references [1] M. Brydegaard et al. IEEE Photonics J 2011:3(3);406-21.
#'
#' @template sp
#' @template loadings
#' @param xLabel A label for x axis. Default is "Component".\cr
#'        \code{labels(scores,".wavelength") <- xLabel}
#'
#' @param yLabel A label for y axis. Default is "Amplitude".\cr
#'        \code{labels(scores,"spc") <- yLabel}
#'
#' @param  scores A matrix of known/already calculated scores to convert to
#'         \code{hyperSpec} object.\cr
#'         If this argument is provided, \emph{matrix multiplication is not
#'         performed,} but component names are copied from \code{loadings} to
#'         \code{scores}.
#' @param  names.var A name of variable in \code{loadings}, that contains names
#'         of components (loadings). These names will be transfered to
#'         \code{scores}.\cr\bold{NOTES:}\cr
#'         1. if \code{names.var} does not
#'         exist (e.g., misspelled), component names No1, No2, ... will be used. \cr
#'         2. This parameter applicable only if class of \code{sp} is
#'          \code{hyperSpec}.
#'
#' @return Amplitudes of the components (i.e., scores), tha corespond to
#' observations (spectra) in object \code{sp}.
#'
#'
#' @export
#'
#' @import hyperSpec
#'
#' @seealso \code{\link{qplot_scores}}
#' @family component analysis / factorisation related functions in \pkg{spHelper}
#' @author Vilmantas Gegzna
#'
#' @examples
#'
#' sc <- getScores(Spectra2, Loadings2)
#' sc
#' qplot_scores(sc)

getScores <- function(sp, loadings = NULL,
                      xLabel = "Component",
                      yLabel = "Amplitude",
                      names.var = "cNames",
                      scores = NULL)
{
    y2 <- hy2mat(sp)
    if (is.null(loadings) & is.null(scores))
        stop("Either argument 'loadings' or 'scores' must be provided.")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.null(scores)) {
    # Prepare loadings
        loadings2 <- hy2mat(loadings)
        if (dim(y2)[2] == dim(loadings2)[2])   loadings2 <- t(loadings2) #transpose, if needed

    # Apply MATRIX MULTIPLICATION
        scores <- y2 %*% (loadings2 %*% base::solve(crossprod(loadings2)))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create names of components
    if (!is.null(loadings) & (names.var %in% colnames(loadings))) {
        cNames <- gsub("max:( )?","c", loadings$..[["cNames"]])
    } else {
        cNames <- paste0("No", 1:ncol(scores))
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Convert (sorted) amplitudes  to "hyperSpec"" object
    if (class(sp) == "hyperSpec") {

        scores <- decomposition(sp, scores,
                                label.wavelength = xLabel,
                                label.spc        = yLabel)
        colnames(scores$spc) <- cNames
        labels(scores)$spc   <- yLabel
    } else {# if scores is a matrix:
        colnames(scores) <- cNames
    }
    # ======================================================================
    return(scores)
}
