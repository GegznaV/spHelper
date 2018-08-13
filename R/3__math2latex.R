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
