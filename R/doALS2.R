
#' [!.] Wrapper function for alternating least squares multivariate curve resolution (MCR-ALS)
#'
#' This function is the same as \link[alsace]{doALS} from package \pkg{alsace}
#' (version 1.6.0), just with more possible input parameters, which are passed to
#' \link[ALS]{als} from package \pkg{ALS}.
#'
#' @inheritParams ALS::als
#' @inheritParams alsace::doALS
#' @param ... Other parametars to be passed to function \link[ALS]{als}.
#'
#' @return The same as in \link[alsace]{doALS}.
#' @export
#'
#' @import ALS
#' @import alsace
#' @seealso \link[alsace]{doALS}, \link[ALS]{als}
#' @examples
#'
#' # /NO examples YET/
#'
doALS2 <- function(Xl, PureS, maxiter = 100, normS = 0.5, uniC = FALSE,
    uniS = FALSE, nonnegS = TRUE, nonnegC = TRUE, optS1st = FALSE,
    baseline = FALSE, closureC = list(), ... )
{
    Cini <- lapply(Xl, function(xl) xl[, 1:ncol(PureS)])
    capture.output(result <- als(PsiList  = Xl,
                                 CList    = Cini,
                                 S        = PureS,
                                 maxiter  = maxiter,
                                 normS    = normS,
                                 nonnegS  = nonnegS,
                                 nonnegC  = nonnegC,
                                 optS1st  = optS1st,
                                 uniC     = uniC,
                                 uniS     = uniS,
                                 baseline = baseline,
                                 closureC = list(),
                                 ...))

    colnames(result$S) <- paste("Component", 1:ncol(PureS))
    for (i in 1:length(result$CList))
        colnames(result$CList[[i]]) <- colnames(result$S)

    predicted.values <- lapply(1:length(Xl), function(ii) Xl[[ii]] - result$resid[[ii]])
    predvals2 <- sum(unlist(predicted.values)^2)
    npoints <- prod(c(length(result$CList), nrow(result$CList[[1]]), nrow(result$S)))
    result$summ.stats <- list(lof = 100 * sqrt(result$rss/predvals2),
                              rms = sqrt(result$rss/npoints),
                              r2  = 1 - result$rss/predvals2)
    # class(result) <- "ALS"
    # result
    structure(result, class = "ALS")
}
