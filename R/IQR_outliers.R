#' Outlier detection with IQR based distances (limits)
#'
#' Outlier detection with interquartile range (IQR) based distances, defined as:
#' \bold{lower} limit \eqn{Q1 - k * IQR} and
#' \bold{upper} limit \eqn{Q3 + k * IQR}. Here Q1 and Q3 is the first and the
#' third quantile respectively.
#' \bold{Inner} lower/upper limit for probable outlier detection has \code{k = k_inn} and
#' \bold{outer} lower/upper limit for (extreme) outlier detection has \code{k = k_out},
#' (\code{k_out > k_inn}).
#'
#' @param x Matrix-like data or \code{hyperSpec} object.
#' @param k_out Number of IQR distances from Q1 and Q3 for \code{extreme}
#' outlier identification. Default value is 3.
#' @param k_inn Number of IQR diustances from Q1 and Q3 for \code{probable}
#' outlier idientification. Default value is 1.5.
#'
#' @return A list with these variables
#' \itemize{
#'    \item{\emph{outer} / \emph{inner} - matrices with values of outer / inner limits respectively}
#'    \item{Logical matrices indicating \code{TRUE} for points that are:
#'          \itemize{
#'              \item{\emph{is_out} - (extreme) outliers (excluding probable outliers);}
#'              \item{\emph{is_pout} - probable outliers (excluding extreme outliers);}
#'              \item{\emph{is_bothOut} - either probable or extreme outliers;}
#'           }
#'      }
#'
#'      \item{Logical vectors indicating spectra / rows with at least one:
#'           \itemize{
#'              \item{\emph{row_out} -  (extreme) outlier point;}
#'              \item{\emph{row_pout} - probable outlier point (excluding extreme outliers);}
#'              \item{\emph{row_bothOut} - either probable or extreme outlier point.}
#'           }
#'      }
#' }
#' If x is a \code{hyperSpec} object, then the listed variables are returned as
#' \code{hyperSpec} objects. \cr
#'
#' \bold{Note} that probable outliers are points between corresponding lower as
#' well as corresponding upper \code{inner} and \code{outer} limits.
#' Extreme outliers are points outside lower and upper \code{outer} limits.
#'
#' @export
#'
#' @seealso Read about boxplots and outliers
#' (\href{http://www.physics.csbsju.edu/stats/box2.html}{Box Plot: Display of Distribution})
#' @examples
#'
#' outl <- IQR_outliers(Spectra2)
#'
#' plot(outl$outer, col = NA)
#' plot(Spectra2, add = TRUE, spc.nmax = nrow(Spectra2))
#' plot(outl$outer, add = TRUE, col = "red",    lines.args = list(lwd = 3))
#' plot(outl$inner,  add = TRUE, col = "orange", lines.args = list(lwd = 3))
#' legend("topright",legend=c("Outer limit", "Inner limit", "Spectra"),
#'  col = c("red", "orange", "black"), lwd = c(3,3,1), lty = 1)
#' title("IQR based distances")
#'
#' @family \pkg{spHelper} utilities
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @family outlier detection functions in \pkg{spHelper}
#' @author Vilmantas Gegzna


IQR_outliers <- function(x, k_out = 3, k_inn = 1.5) {
    chk.hy(x)
    if (k_out < k_inn) stop("incorrect input values. Check if `k_inn` < `k_out")

    IQRsp <- apply(x, 2, IQR)
    Q <- apply(x, 2, quantile,  probs = c(.25,.75))

    InnerLimit <- Q + k_inn * c(-1,1) * IQRsp
    outerLimit <- Q + k_out * c(-1,1) * IQRsp

    FUN_isOutside <- function(x_row, Limit) {
        # x_row <- hy2mat(x_row)
        # Limit <- hy2mat(Limit)
        (x_row < Limit[1,]) | (x_row > Limit[2,])
    }

    # Find outlier point at each wavelength (in each column)
    is_bothOut  <- apply(x,  1, FUN_isOutside, InnerLimit)
    is_out      <- apply(x,  1, FUN_isOutside, outerLimit) # (extreme) outliers
    is_pout     <- decomposition(x, is_bothOut != is_out, wavelength = wl(x)) # probably outlier

    # rows/spectra with outliers and probable outliers
    row_bothOut <- apply(is_bothOut,  1, any)
    row_out     <- apply(is_out,      1, any)
    row_pout    <- apply(is_pout,     1, any)

    return(list(outer = outerLimit,
                inner = InnerLimit,
                # Outlier point indices
                is_out = is_out,
                is_pout = is_pout,
                is_bothOut = is_bothOut,
                # Outlier spectra (row) indices
                row_out = row_out,
                row_pout = row_pout,
                row_bothOut = row_bothOut))

}
