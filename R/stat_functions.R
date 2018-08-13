#' Confidence interval for parameter lambda of Poisson distribution
#'
#' @param x data
#' @param conf.level confidence level, number between 0 and 1, usually 0.95.
#'
#' @return List with results... ???
#'
#' @export
#' @family statistical functions
#'
#' @examples
#'
#' library(spHelper)
#'
#' x <- c(3,0,2,1,0,4,3,2,1,2)
#' poisson_lambda_CI(x)
#'

#'
poisson_lambda_CI <- function(x = NULL,
                              lambda = mean(x),
                              n  = length(x),
                              Sn = sum(x),
                              conf.level = 0.95) {


    a <- (1 - conf.level) / 2 #

    l_lower <- qchisq(1-a , df = 2*Sn,     lower.tail = FALSE) / (2*n)
    l_upper <- qchisq(a   , df = 2*Sn + 2, lower.tail = FALSE) / (2*n)

    output <- list(
        model = "CI for parameter lambda of Poisson distribution",
        parameter = "lambda",
        values = data.frame(CI_lower = l_lower, lambda = lambda, CI_upper = l_upper),
        data = x
    )

    class(output) <- c("CI_model", "list")
    return(output)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Confidence interval for variance (parameter sigma squared) of Noral distribution
#'
#' CI for case where variance of population is unknown.
#'
#' @param x data
#' @param var variance
#' @param n number of samples
#' @param conf.level confidence level, number between 0 and 1, usually 0.95.
#'
#' @return List with results... ???
#'
#' @export
#' @family statistical functions
#'
#' @examples
#'
#' library(spHelper)
#'
#' x <- c(3,0,2,1,0,4,3,2,1,2)
#' normal_var_CI(x)
#'
#' normal_var_CI(var = 10, n = 20)
#'

normal_var_CI <- function(x = NULL, var = var(x), n  = length(x), conf.level = 0.95) {
    a <- (1 - conf.level) / 2 # alpha value

    # var <- var(x)
    # n  <- length(x)

    k <- var*(n-1)

    val <- var
    l_lower <- k / qchisq(a  , df = n-1, lower.tail = FALSE)
    l_upper <- k / qchisq(1-a, df = n-1, lower.tail = FALSE)

    output <- list(
        model = "CI for parameter sigma squared of Normal distribution",
        parameter = "Variance",
        values = data.frame(CI_lower = l_lower, Variance = val, CI_upper = l_upper),
        data = x
    )

    class(output) <- c("CI_model", "list")
    return(output)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Confidence interval for median
#'
#' CI for case where variance of population is unknown.
#'
#' @param x data
#'
#' @return List with results... ???
#'
#' @export
#' @family statistical functions
#'
#' @examples
#'
#' library(spHelper)
#'
#' x <- c(3,0,2,1,0,4,3,2,1,2)
#' median_CI(x)
#'


median_CI <- function(x = NULL, MEDIAN = median(x), n = length(x), IQR = IQR(x)) {

    se <- 1.58 * IQR/ sqrt(n)

    val <- MEDIAN
    l_lower <- MEDIAN - se
    l_upper <- MEDIAN + se

    output <- list(
        model = "CI for median",
        parameter = "median",
        values = data.frame(CI_lower = l_lower, Median = val, CI_upper = l_upper),
        data = x
    )

    class(output) <- c("CI_model", "list")
    return(output)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Median and quantiles
#'
#' ....Confidence interval for median
#'
#' ....CI for case where variance of population is unknown.
#'
#' @param x data
#'
#' @return List with results... ???
#'
#' @export
#' @family statistical functions
#'
#' @examples
#'
#' library(spHelper)
#'
#' x <- c(3,0,2,1,0,4,3,2,1,2)
#' quartiles(x)
#'


quartiles <- function(x = NULL, MEDIAN = median(x),
                         Q1  = quantile(x,.25),
                         Q3  = quantile(x,.75) ) {
    val <- MEDIAN
    l_lower <- Q1
    l_upper <- Q3

    output <- list(
        model = "Quartiles",
        parameter = "median",
        values = data.frame(Q1 = l_lower, Median = val, Q3 = l_upper),
        data = x
    )

    class(output) <- c("CI_model", "list")
    return(output)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
