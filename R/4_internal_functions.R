
#' Parse a string and extract information to a dataframe.
#'
#' This function similar to \code{\link{regexp2df}}, but accepts both
#' strings and functions as values of argument \code{pattern}.
#'
#' @param x A string to be parsed
#' @param pattern Either a string with regular expression or a function.
#' @param ignore.case  A logical parameter to be passed to \code{\link{regexp2df}}.
#' @param ... further parameters to be passed to \code{\link{regexp2df}}.
#'
#' @return A data frame with parsed information.
#' @export
#'

parse_string <- function(x, pattern, ignore.case = FALSE, ...){

    if (is.function(pattern)) {
        # if `pattern` is a function
        result <- pattern(x)

    } else if (is.character(pattern)) {
        # if `pattern` is a string
        result <- regexp2df(x,
                            pattern = pattern,
                            ignore.case = ignore.case,
                            ...)
    } else stop("`pattern` must be either a function or a string with regular expression.")

    # result is a data frame
    return(result)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#' [!] As long data frame
#'
#' Wrapper for \code{\link[hyperSpec]{as.long.df}}:\cr
#' \code{as.long.df(x,  rownames = TRUE, na.rm = TRUE,...)}
#'
#' @inheritParams hyperSpec::as.long.df
#' @export
ldf <- function(x, ...) hyperSpec::as.long.df(x,  rownames = TRUE, na.rm = TRUE,...)


#' @rdname ldf
#' @export
as.ldf <- function(x,...) hyperSpec::as.long.df(x,  rownames = TRUE, na.rm = TRUE,...) #ldf - long tada frame

#' Rotate x axis tick labels
#'
#' Convenience functions to rotate x axis tick labels in \code{ggplot} plots:\cr
#' \code{x30()} rotates in 30 degrees.\cr
#' \code{x90()} rotates in 90 degrees.\cr
#'
#' @details
#'
#' These functions correct \code{ggplot2} theme elements and may not respond if
#' new theme is applied after e.g. \code{x30()} is apllied. I.e. INCORRECT:\cr
#' \code{qplot(mpg, wt, data = mtcars) + x30() + theme_bw()}.\cr
#' CORRECT:\cr
#' \cr \code{qplot(mpg, wt, data = mtcars) + theme_bw() + x30()}.
#'
#' @examples
#' library(ggplot2)
#' library(spHelper)
#'
#' qplot(mpg, wt, data = mtcars) + x30()
#' qplot(mpg, wt, data = mtcars) + x90()
#'
#' @export
x90 <- function() theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



#' @rdname x90
#' @export
x30 <- function() theme (axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))


# =============================================================================
#' Converts percentage to perobabilities
#'
#'
#'
#' @param percent percentage (number grom 1 to 100)
#'
#' @return Range of probabilities (two numbers) that are nearest to 0.5.
#' @export
#'
#' @examples
#' percent2probs(100)
#' percent2probs(50)
#'
percent2probs <- function(percent) {
    if (!dplyr::between(percent, 1, 100))
        stop("`percent` must be between 1 and 100")
    .5 + percent/100/2 * c(-1,+1)
}
