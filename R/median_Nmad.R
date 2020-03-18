#' [!] Find spectra that represent median +/- n median absolute deviations (MAD)
#'
#' Function finds spectra that represent mean +/- n MADs
#' and adds column \code{.name}.
#'
#' @template sp-hy
#' @param n Number of MADs.
#' @param plus.minus Logical. If \code{TRUE}, two spectra representing \code{\u00B1 n}
#' are calculated. Otherwise only one spectrum representing \code{n} is calculated.
#' @param center_fun Function that calculates center tendency. Default is
#'        \code{center_fun = median}.
#' @param var_fun Function that calculates variability measure. Default is
#'        \code{var_fun = mad}.
#' @param center_name A string with the name for measure of center tendency
#' (will be used to create a row name in \code{.name}).
#' @param var_name A string with the name for measure of variability
#' (will be used to create a row name in \code{.name}).
#'
#' @return A \code{hyperSpec} object with spectra at \code{median(sp) \u00B1 n*MAD(sp)}.
#'
#' @export
#'
#' @examples
#'
#' data(Spectra2)
#' Margins  <- median_Nmad(Spectra2)
#' Margins3 <- median_Nmad(Spectra2, n = 3)
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
#'     scale_color_manual(" ", values = c("darkgreen","blue", "red")) +
#'     scale_alpha_manual(" ", values = ".2", guide = FALSE)
#'
#' @seealso \link[hyperSpec]{scale},  \link{outside_mean_pm_Nsd},
#'   \link[hyperSpec]{mean_sd}
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna
median_Nmad <- function(sp, n = 2, plus.minus = TRUE,
                        var_fun = mad,
                        center_fun = median,
                        center_name = as.character(match.call()$center_fun),
                        var_name = as.character(match.call()$var_fun)) {

    print(match.call()$center_fun)

    if (plus.minus == TRUE) {
        coef = c(-1, 1)
        pm <- "\u00B1"
    } else {
        coef = 1
        pm <- if (sign(n) < 0) NULL else "+"
    }

    # Default names, if default functions are used:
    if (length(center_name)==0) center_name <- "Median"
    if (length(var_name)==0)    var_name <- "MAD"

    # Calculations
    center     <- hyDrop_NA(apply(sp, 2, center_fun));
    variation  <- hyDrop_NA(apply(sp ,2, var_fun));

    sp_at_nSD <- (n * coef * variation) + center
    sp_at_nSD$.name <- factor(paste(center_name, pm, n, "\u00D7", var_name))

    return(sp_at_nSD)
}


