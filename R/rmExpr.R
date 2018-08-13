#' [.] Convert expressions in \code{ggplot2} object labels to strings
#'
#' Convert expressions in \code{\link[ggplot2]{ggplot}} object labels to strings, to
#' enable them to be plotted with \code{\link[plotly]{ggplotly}}.
#' @param p ggplot2 object
#'
#' @return `ggplot2` object
#' @export
#'
#' @examples
#'
#' \donttest{
#' \dontrun{
#' data(flu, package = "hyperSpec")
#'
#' qplotspc(flu)
#' ggplotly()
#'
#' rmExpr()
#' ggplotly()
#'
#' #--------------------------------
#' p1 <- qplot_sp(Loadings2)
#' p1$labels
#'
#' p2 <- rmExpr(p1)
#' p2$labels
#'
#' ggplotly(p1)
#' ggplotly(p2)
#'
#' #----------------------------------
#' qplotspc(aggregate(chondro, chondro$clusters, mean_pm_sd),
#' mapping = aes(x = .wavelength,
#'               y = spc,
#'               colour = clusters)) +
#'     facet_grid(clusters ~ .) + ggtitle("Spectra of Chondrocytes")
#'
#'  rmExpr()
#'  ggplotly()
#'
#' }}
#'
#' @family \pkg{spHelper} plots
#' @family \pkg{spHelper} utilities
#' @author Vilmantas Gegzna
rmExpr <- function(p = ggplot2::last_plot()) {
    p$labels  <- lapply(p$labels, expr2text)
    return(p)
}

#' [.] Unwrap text from `call` object
#' Assumption: only the second element in a `call` can be text
#' first element is an object of class "name", that can be dropped.
#' This algorithm can loose some text elements.
#'
#' @param x A `call` object
#'
#' @return A string.
#' @export
#' @family \pkg{spHelper} utilities
#' @author Vilmantas Gegzna
uncall <- function(x){

    x <- as.list(x)[-1]
    unwrapCall <- function(y) {
        while (is.call(y)) {y <- as.list(y)[[2]]};
        y
    }
    x <- lapply(x, unwrapCall)
    paste(unlist(x),collapse = ", ")
}


#' [.] Convert `expression` and `call` to text and remove quotes
#'
#' @param x `Call` object or `expression`
#'
#' @export
#' @family \pkg{spHelper} utilities
#' @author Vilmantas Gegzna
#'
expr2text <- function(x) {
    x <- switch(class(x),
                # expression = simsalapar::escapeLatex(as.call(x)),
                expression = {tryCatch(to_math_q(x),
                                       error = function(e) as.character(x))},
                call = uncall(x),
                paste(as.character(x),collapse = " "))

    x <- gsub('(^\")|(\"$)',"", x)
    x <- gsub('\"',", ", x)
    return(x)
}

# =============================================================================

#
# A partial implementation of a R expression -> latex math cnverter
#
# A partial implementation of a R expression -> latex math cnverter
#
# @param x A supported R plotmath expression.
#
# @return A LaTeX expression
# @author Hadley Wickham
#
# @source \href{https://gist.github.com/hadley/5576263}{latx-math.r}
#
# @examples
#
#
# to_math(x_1 + 1^{2 + 4} + 5 + sqrt(y) / 5 %/% 10)
# to_math(paste(x^2, y - 1, z_i))
# to_math(hat(tilde(ring(x))))
#
# to_math_q(expression(x+1))
#
#
# # ERROR (known bug):
#
# \donttest{
# \dontrun{
# to_math_q(expression(x+1))
# to_math(+1)
# to_math(x*+1)
# to_math(x^-a)
#
# }}
# @export
to_math <- function(x) {
    to_math_q(substitute(x))
}

# @rdname to_math
# @export
to_math_q <- function(x) {
    if (is.integer(x) || is.numeric(x)) return(x)
    if (is.name(x)) {
        x2 <- as.character(x)
        x3 <- if (x2 %in% names(symbols)) symbols[[x2]] else x2
        return(x3)
    }

    eval(x, lenv)
}

lenv <- new.env(parent = emptyenv())

dots <- function(...) {
    eval(substitute(alist(...)))
}

# Convert a function into a fexpr: the function recieves only
# unevaluated args
fexpr <- function(f) {
    stopifnot(is.function(f))
    function(...) {
        args <- dots(...)
        do.call(f, args, quote = TRUE)
    }
}

# Helper functions
unary_op <- function(left, right) {
    fexpr(function(e1, e2) {
        paste0(left, to_math_q(e1), right)
    })
}

binary_op <- function(sep) {
    fexpr(function(e1, e2) {
        paste0(to_math_q(e1), " ", sep, " ", to_math_q(e2))
    })
}

# Binary operators
lenv$"+" <- binary_op("+")
lenv$"-" <- binary_op("-")
lenv$"*" <- binary_op("*")
lenv$"/" <- binary_op("/")
lenv$"%+-%" <- binary_op("\\pm")
lenv$"%/%" <- binary_op("\\")
lenv$"%*%" <- binary_op("\\times")
lenv$"%.%" <- binary_op("\\cdot")
lenv$"[" <- binary_op("_")
lenv$"^" <- binary_op("^")

# Grouping
lenv$"{" <- unary_op("{", "}")
lenv$"(" <- unary_op("(", ")")
lenv$paste <- fexpr(function(...) {
    paste0(unlist(lapply(list(...), to_math_q)), collapse = " ")
})

# Other math functions
lenv$sqrt <- unary_op("\\sqrt{", "}")
lenv$log  <- unary_op("\\log{", "}")
lenv$inf  <- unary_op("\\inf{", "}")
lenv$sup  <- unary_op("\\sup{", "}")
lenv$frac <- fexpr(function(a, b) {
    paste0("\\frac{", to_math_q(a), "}{", to_math_q(b), "}")
})

# Labelling
lenv$hat   <- unary_op("\\hat{", "}")
lenv$tilde <- unary_op("\\tilde{", "}")
lenv$dot   <- unary_op("\\dot{", "}")
lenv$ring  <- unary_op("\\ring{", "}")

