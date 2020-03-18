
#' [!] Find spectra that represent mean +/- n standard deiations
#'
#' Function finds spectra that represent mean +/- n standard deiations
#' and adds column \code{.name}.
#'
#' @template sp-hy
#' @param n Number of standard deviations, i.e. value of z-score.
#' @param plus.minus Logical. If \code{TRUE}, two spectra representing \code{\u00B1z}
#' are calculated. Otherwise only one spectrum representing \code{n} is calculated.
#'
#' @return A \code{hyperSpec} object with spectra at \code{mean(sp) \u00B1 n*sd(sp)}.
#' @export
#'
#' @examples
#'
#' data(Spectra2)
#' Margins  <- mean_Nsd(Spectra2)
#' Margins3 <- mean_Nsd(Spectra2, n = 3)
#'
#' plotspc(Margins)
#'
#'
#' # Plot data and margins =======================================
#'
#' Marg2 <- as.long.df(Margins,  rownames = TRUE, na.rm = FALSE)
#' Marg3 <- as.long.df(Margins3, rownames = TRUE, na.rm = FALSE)
#'
#' Spectra2$.name <- factor("Spectra")
#'
#' qplotspc(Spectra2, spc.nmax = nrow(Spectra2),
#'          mapping = aes(x = .wavelength, y = spc, group = .rownames, color = .name),
#'          alpha = .25) + theme_light() +
#'     geom_line(data = Marg2, aes(color = .name), lwd = 1) +
#'     geom_line(data = Marg3, aes(color = .name), lwd = 1) +
#'     scale_color_manual(" ", values = c("blue","orange2", "red")) +
#'     scale_alpha_manual(" ", values = ".2", guide = FALSE)
#'
#' @seealso \link[hyperSpec]{scale}, \link{outside_mean_pm_Nsd},
#'   \link[hyperSpec]{mean_sd}
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna

mean_Nsd <- function(sp, n = 2, plus.minus = TRUE) {
    MEAN <- hyDrop_NA(apply(sp,2,mean));
    SD   <- hyDrop_NA(apply(sp,2,sd));

    if (plus.minus == TRUE) {
        coef = c(-1, 1)
        pm <- "\u00B1"
    } else {
        coef = 1
        pm <- if (sign(n) < 0) NULL else "+"
    }

    sp_at_nSD <- (n * coef * SD) + MEAN
    sp_at_nSD$.name <- factor(paste("Mean", pm, n, "\u00D7", "st.dev."))

    return(sp_at_nSD)
}


