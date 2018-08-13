#' [!] Calculate quality of fit parameters (for spectroscopic data)
#'
#' Calculate LOF (lack-of-fit), GOF (goodness-of-fit) and RMSE (root of mean
#' squared error) for original and reconstructed spectra.
#'
#' @param obj The first set of (\emph{original}) spectra. Either a matrix or a
#'  hyperSpec object.
#' @param fit The second set of usually processed or reconstructed spectra, that
#'        will be compared to the first set. Either a matrix or a hyperSpec object.
#'
#' @return A dataframe with following parameters for each row in \code{obj} and \code{fit}:
#' \itemize{
#' \item{\code{$LOF} - Lack of fit in percent (\%):
#'  \deqn{\frac{\sum{(obj - fit)^2}}{\sum{obj^2}} \cdot 100\%}{sum((obj - fit)^2)/sum((obj)^2) * 100\%}}
#' \item{\code{$GOF} - Goodness of fit in percent (\%): \deqn{100\% - LOF}{100 - LOF}}
#' \item{\code{$RMSE} - Root of mean squared error: \deqn{\sqrt{(obj - fit)^2}}{sqrt(obj - fit)^2}}
#' }
#' @export
#'
#' @examples
#' obj <- Spectra2
#' fit <- reconstructSp(Loadings2, Scores2)
#'
#' evaluation <- quality_of_fit(obj, fit)
#'
#' head(evaluation)
#'
#' par(mfrow = c(3,1))
#'  plot(density(evaluation$RMSE), main = "RMSE", col = 2)
#'  plot(density(evaluation$LOF), main = "Lack-of-fit")
#'  plot(density(evaluation$GOF), main = "Goodness-of-fit")
#' par(mfrow = c(1,1))
#'
#' # Density plot of LOF by group:
#' DATA <- cbind(obj$.., evaluation)
#' qplot(LOF, data = DATA, fill = gr, color = gr,
#'  geom = "density", alpha = I(.2))

quality_of_fit <- function(obj = NULL, fit) {
    obj <- hy2mat(obj)
    fit <- hy2mat(fit)
    # % ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    sqDiff <- (obj - fit)^2
    MSE  <- apply(sqDiff, 1, mean)
    RMSE <- sqrt(MSE)
    # % ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    sp_power = apply(obj^2, 1, mean)
    # % ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    LOF  = sqrt(MSE / sp_power) * 100
    # formulė iš  mcr-als toolbox algoritmo funkcijos pcarep.m:sigma=(sqrt(sst1/sst2))*100;
    GOF  = 100 - LOF
    # % ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    metrics <- data.frame(LOF, GOF, RMSE)
    return(metrics)
}


#' @export
#' @rdname quality_of_fit
spFitEval <- function(obj, fit) {
    .Deprecated("quality_of_fit")
    quality_of_fit(obj, fit)
}

# dplot <- function(obj, ...) {densityplot(obj, ..., panel = function(obj, ...) {
#    panel.densityplot(obj, ..., plot.points = FALSE)
#    panel.rug(obj)})}
#
# dplot(~value | variable, data = reshape2::melt(evaluation))
