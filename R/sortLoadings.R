# Sort component spectra =======================================================================
#
#' [!+] Process (Sort, flip, name, etc.) spectra of components (a.k.a. loadings)
#'
#'
#' @description Sort rows of \code{hyperSpoec} object by possition of maximum
#'  value in rows (in other words, sort component spectra, a.k.a. loadings, by
#'  possition of top peak) and do additional tasks:
#'  \enumerate{
#'      \item{If \code{sp} is provided, convert resulting matrix to corresponding
#'          \code{\link[=hyperSpec-class]{hyperSpec}} object by using function
#'          \code{\link[hyperSpec]{decomposition}}.}
#'
#'      \item{If \code{PCA.flip = TRUE} and \code{sp} is provided, flip the
#'      loadings of a component in respect with x axis if the mean of the
#'      component's scores is negative:
#'       (\code{sign(mean(Scores_of_component_i)) < 0})
#'          \code{loadings} and \code{sp} are used to calculate the scores.}
#'  }
#'
#' @template loadings
#' @template sp-hy
#'
#' @param PCA.flip Logical. If \code{TRUE}, some components are flipped.
#'  Set to \code{TRUE} if PCA loadings are used. Default \code{PCA.flip = FALSE}.
#'  The flipping follows the rule:
#' \deqn{loading_i * (-score_i) = (-loading_i) * score_i}{loading[i] * (-score[i]) = (-loading[i]) * score[i]}
#' where \eqn{-loading_i}{-loading[i]} represents the i-th flipped loading.
#'
#' @param sort Logical. Indicates if returned componenst must be sorted.
#'       If \code{FALSE}, only additional tasks are performed.
#'       Default is \code{TRUE}.
#' @param wl.units The units of wavelength axis. Will be used as suffix to
#'       values in columns \code{cNames} and \code{PeakAt}, if applicable.
#'
#' @param label.spc The new label for the matrix of loadings spectra. Default is
#'        \code{"Comp. spectra"}.
#'
#' @return Either matrix (if \code{sp} is not provided) or
#' \code{hyperSpec} object with prepreocessed (sorted, flipped, named, etc.)
#' loadings.
#' In case of \code{hyperSpec} object, 3 columns
#' (\code{cNames} and \code{PeakAt}, \code{order.of.rows}) are added/overwritten.
#'
#' @note spectra (object of class \code{\link[=hyperSpec-class]{hyperSpec}})
#'          which will be used to convert sorted loadings into
#'          \code{\link[=hyperSpec-class]{hyperSpec}} object.
#'
#'
#' @export
#' @examples
#' # Load data
#' library(spHelper)
#' data(Loadings2,package = "spHelper")
#'
#' # ======================================================================
#' unsorted_loadings <- Loadings2[c(2,3,5,1,4),,]
#' unsorted_loadings # print unsorted
#'
#' sortLoadings(unsorted_loadings) # print sorted
#'
#' # ======================================================================
#'
#' # Extract loadings as a matrix:
#' unsorted_loadings_matrix <- Loadings2[c(2,3,5,1,4),,][[]]
#'
#' # returns a matrix -----
#' L1 <- sortLoadings(unsorted_loadings_matrix)
#' class(L1)
#' ## [1] "matrix"
#'
#' # returns a hyperSpec object -----
#' L2 <- sortLoadings(unsorted_loadings_matrix, Spectra2)
#' class(L2)
#' ## [1] "hyperSpec"
#' # ======================================================================
#' @seealso \code{\link[hyperSpec]{decomposition}}
#' @family component analysis / factorisation related functions in \pkg{spHelper}
#' @author Vilmantas Gegzna

sortLoadings <- function(loadings, sp = NULL, PCA.flip = FALSE, sort = TRUE,
                         label.spc = "Comp. spectra", wl.units = "nm") {
    if (PCA.flip == TRUE) { # flip
        if (!is.null(sp)) { # is `sp` exists
            scores  <- getScores(hy2mat(sp), loadings)
            # ------------------------------------------------------------------
            # Flip, if average of amplitudes/scores is negative
            # signCoefs    <- sign(rowMeans(scores))
            meanSign     <- function(x){sign(mean(x))}
            signCoefs    <- apply(scores, MARGIN = 2, meanSign)
            loadings     <- sweep(loadings,  MARGIN = 1, signCoefs,`*`)

            # # Normalize
            # maxSpInt     <- apply(loadings, MARGIN = 1, max)
            # PCAvarimax2  <- sweep(loadings, MARGIN = 1, maxSpInt,`/`)

            # ==================================================================
        } else {
            warning("Variable `sp` is missing thus `PCA.flip` is reset to `FALSE`")
        }
    }

	# Sort in accordance with the position of matrix's row maxima
	#  (y_max) on x axis
    index.of.max <- apply(loadings, 1, which.max)

    if (sort == TRUE) {
        OrderOfRows  <- order(index.of.max)

        # Matrix with Sorted components
        loadings <- loadings[OrderOfRows,]

        # Position of maxima
        index.of.max <- index.of.max[OrderOfRows]

    } else {
        OrderOfRows <- 1:nrow(loadings)
    }

    if (inherits(sp, "hyperSpec")) {
        # Convert (sorted) components to "hyperSpec"" object
        loadings <- decomposition(sp, loadings,
                                  scores = FALSE,
                                  label.spc = label.spc,
                                  retain.columns = F)
        # Create names of components
        PeakAt <- wl(loadings)[index.of.max] %>%
                round() %>%
                paste0(., wl.units) %>%
                make.unique(sep = "_")

        NewCols <- c("cNames", "PeakAt",  "order.of.rows")
        ColExists <- NewCols %in% colnames(loadings)

        # WARNS, if columns with the names in `NewCols` already exist
        if (any(ColExists)) {
            warning(sprintf("The columns %s in object `loadings = %s` will be overwritten.",
                    paste0('`$', NewCols[ColExists], '`', collapse = ', and '),
                    as.character(match.call()$loadings)))
        }
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        loadings$cNames        <- paste0("max: ", PeakAt)
        loadings$PeakAt        <- PeakAt
        loadings$order.of.rows <- OrderOfRows
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        labels(loadings,'spc') <- labels(sp,'spc')
    }
    return(loadings)
}
