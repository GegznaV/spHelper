#' [!] Simulate spectroscopic data
#'
#' Simulation of spectroscopic data: Loadings (spectra of spectroscopic
#' components), Scores (amplitudes of these components) an Spectra (mixture of
#' the spectroscopic components multiplied by the amplitudes with random noise
#' added).
#'
#' [\emph{This function works, but is not well documented yet.}]
#'
#' @param x Values for x axis ("wavelengths").
#' @param nGr Number of groups.
#' @param nInGr Number of samples in a group (vector of values for each group).
#' @param N Total number of samples.
#' @param nDim Number of spectral components (dimensions).
#' @param w.possile A vector of possible values of Gaussian curve parameter "w'
#'        (i.e., width). Values of w will be randomly sampled from this vector.
#' @param plots Logical. If \code{TRUE} - makes plots. Default is \code{FALSE}.
#'
#' @return List of \code{\link[=hyperSpec-class]{hyperSpec}} objects:
#' \describe{
#'      \item{Spectra}{Spectra of each observation, made of \code{(Scores * Loadings) + NOISE}.}
#'      \item{Loadings}{Normalized spectra of \bold{components}.}
#'      \item{Scores}{\bold{Amplitudes} of components for each observation.}
#' }
#'
#' In the list the \code{hyperSpec} objects contain spectroscopic data and
#' additional variables.
#'
#' Additional variables for \code{Spectra} and \code{Scores}:
#'
#' \describe{
#'   \item{class}{A factor variable for classification.}
#'   \item{gr}{A factor variable for other type classification.}
#' }
#'
#' Additional variables for \code{Loadings}:
#' \describe{
#'   \item{PeakAt}{Position of components top peak.}
#'   \item{cNames}{Names of components.}
#'   \item{order.of.rows}{Original order of components before sorting.}
#' }
#'
#' @export
#'
#' @examples
#'
#' simSpectra()
#'
#' simSpectra()$Spectra
#'
#' @family simmulation functions in \pkg{spHelper}
#' @author Vilmantas Gegzna
simSpectra <- function(x = 300:800,
                       nGr = 2,
                       nInGr = c(50,50,50),
                       N = sum(nInGr),
                       nDim  = 4,
                       w.possile = 5:150,
                       plots = FALSE) {
    # x = 300:800
    #
    # nInGr = c(50,50,50)
    # nGr = 2
    # N = sum(nInGr)
    # nDim  = 4
    # w.possile = 5:150

    # generate parameters -----------------------------------------------------

    c <- sort(sample(x, nDim, replace = TRUE))    # c(390, 450, 510, 535)
    w <- sample(w.possile, nDim, replace = TRUE)
    A <- matrix(NA, nrow = N, ncol = nDim)

    # A[,1] = rnorm(N, mean = 100, sd = 10)
    # A[,2] = rnorm(N, mean = 120, sd = 20)
    # A[,3] = c(rnorm(nInGr[1], mean = 100, sd = 10),
    #             rnorm(nInGr[2],mean = 120, sd = 10),
    #             rnorm(nInGr[3],mean = 150, sd = 10)
    #         )

    # Create correlated data --------------------------------------------------
    ACor <- corr_vec2mat(c(.3,-.6,.2)) # c(.3,-.6,.7,.3,.6,.1)
    det(ACor)
    A <- MASS::mvrnorm(N, Sigma = ACor , mu = rep(0,nrow(ACor)), empirical = T)
    A_mu     <- runif(nrow(ACor),50,200)
    A_delta  <- runif(nrow(ACor),5,30)

    A <- sweep(A,2,A_delta,'*')
    A <- sweep(A,2,A_mu,'+')

    Agr <- A[,3]

    A[,3] <- c(rnorm(nInGr[1],mean = 100, sd = 10),
               rnorm(nInGr[2],mean = 120, sd = 10),
               rnorm(nInGr[3],mean = 150, sd = 10)
    )

    A <- cbind(A,rnorm(N, mean = 100, sd = 30))

    # cor(A)
    #  ------------------------------------------------------------------------
    y <- matrix(NA,N,length(x))

    for (i in 1:N) {
        NOISE <- rnorm(length(x),sd = runif(1,0,5))
        y[i,] <- GaussAmp(x, c, w, A[i,]) %>%  apply(2,sum) + NOISE
    }
    #  ------------------------------------------------------------------------
    class = as.factor(c("N","S1","K","l")[kmeans(Agr, nGr + 1)$cluster])
    group = as.factor(LETTERS[kmeans(A[,c(3)], nGr)$cluster])

    Spectra <- new("hyperSpec",
                   wavelength = x,
                   spc = y,
                   data = data.frame(gr = group, class = class),
                   label = list(        spc = "I / a.u.",
                                        .wavelength = expression(list(lambda, nm)))
    )

    #     plot(Spectra,spc.nmax = 200, col = Spectra$class)
    #     plot(Spectra,spc.nmax = 200, stacked = Spectra$gr, col = Spectra$class)

    # hyperspectra objects ----------------------------------------------------

    Loadings <- sortLoadings(GaussAmp(x, c, w),sp = Spectra, sort = T)
    Scores   <- getScores(sp = Spectra,loadings = Loadings, scores = A)

    if (plots) {
        qplot_kAmp(Scores, by = "gr")
        qplot_kAmp(Scores, by = "class")
        qplot_kSp(Loadings)
        qplot(Spectra,spc.nmax = 200, col = Spectra$class)
    }

    result <- list()
    result$Spectra  <- Spectra
    result$Loadings <- Loadings
    result$Scores   <- Scores

    # devtools::use_data(Spectra, Loadings, Scores, overwrite = F)
    # save.image("simSpectrum.RData")
    return(result)
}


