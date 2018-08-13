
#' [!] Add a standardised label for x-axis in \code{hyperSpec} object
#'
#' @template sp-hy
#' @param modeX Mode of x axis. A string, that indicates either "Fluorescence",
#'       "wavelength", "Raman" or "IR".
#' @param label A manually eneterd text or expression of the label.
#'
#' @return Object with added label.
#' @export
#'
#' @examples
#'
#' Sp2 <- hyAdd_Label_wl(Spectra2, "IR")
#'
#' labels(Spectra2, ".wavelength")
#' #> expression(lambda/nm)
#'
#' labels(Sp2, ".wavelength")
#' #> expression(list(Delta * tilde(nu), cm^-1))
#'
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna

hyAdd_Label_wl <- function(sp, modeX = NULL, label = NULL) {
    label <- label_wl(modeX = modeX, label = label)
    labels(sp, ".wavelength") <- label
    return(sp)
}
