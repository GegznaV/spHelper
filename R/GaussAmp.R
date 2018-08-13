# GaussAmp ------------------------------------------------------------------

#' [+] Generate Gaussian curves (GaussAmp).
#'
#' Generate Gaussian curves, according to the equation:\cr
#' \deqn{y = y_0+A\mathrm{e}\frac{-(x-c)^2}{2w^2}}{y = y0 + A*exp(-(((x-c)^2)/(2*w^2)))}
#'
#' @param x Values of an independent variable (a vector).
#' @param c Center positions on x axis of each Gaussian curve (a scalar or a vector).
#'          Default is 0.
#' @param w Width of each curve (a scalar or a vector). Default is 1.
#' @param A Amplitude of each curve (a scalar or a vector).
#'          Default is 1.
#' @param y0 Offsets on y axis (a scalar or a vector). Default is 0.
#'
#' @note The number of curves is determined by maximal number of elements in
#'  any of 4 Gausian curve parameters' (c, w, A, y0) vector. Values in other
#'  parameters vectors are recycled as ilustrated in example 2 (see parameter "A").
#'
#' @return y - values of dependent variable of Gaussian curve.
#'
#' @export
#' @family curves in\pkg{spHelper}
#' @author Vilmantas Gegzna
#'
#' @examples
#' # Example 1
#' x <- seq(-9.9, 10, 0.2)
#' y <- GaussAmp(x)
#'
#' plot(x,y, type = "l", col = "green3"); grid()
#'
#' # Example 2
#'
#'
#' # Make 7 lines
#' y <- GaussAmp(x, c = 1:7,A = c(1,2))
#'
#' dim(y)
#' ##[1]   7 100
#'
#' Obj <- new("hyperSpec",spc = y,    wavelength = x,
#'          label = list (spc = "y", .wavelength = "x"))
#'
#' plot(Obj, col = 1:nrow(Obj)); grid()
#'
GaussAmp <- function(x, c = 0, w = 1, A = 1, y0 = 0){
    P <- max(length(c),length(w),length(A),length(y0))

    c  <- rep_len(c,  P)
    w  <- rep_len(w,  P)
    A  <- rep_len(A,  P)
    y0 <- rep_len(y0, P)

    # Prealocate y
    y = matrix(NA ,P,length(x))

    # Generate the curves
    for (i in 1:P) {y[i,] <- y0[i] + A[i] * exp(-(((x - c[i]) ^ 2) /
                                                    (2 * w[i] ^ 2)))}
    # Output
    return(y)
}
