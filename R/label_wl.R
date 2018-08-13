
#' @export
#' @rdname hyAdd_Label_wl
#'
# Labels for wavelengths:
label_wl <- function(modeX = NULL, label = NULL) {
    # if "label" is provided it overrides "modeX" and
    # this `if` sentence is skipped:
    if (is.null(label)) {
        modeX0 <- match(tolower(modeX), c(
                                          "wavelengths",   # 1
                                          "raman shifts",  # 2
                                          # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                          "fluorescence",  # 3
                                          "wavelength",    # 4
                                          "raman",         # 5
                                          "ir"))           # 6
        if (is.na(modeX0))
            stop(sprintf("The value of parameter `modeX` is not supported: %s",modeX))

        label <- switch(modeX0,
                        "1" = "Wavelength, nm",                           # Wavelength (2)
                        "2" = "Raman Shift, 1/cm",                        # Raman shift (2)
                        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        "3" = expression(list(lambda, nm)),               # Fluorescence
                        "4" = expression(list(lambda, nm)),               # wavelength
                        "5" = expression(list(Delta * tilde(nu), cm^-1)), # Raman shift
                        "6" = expression(list(tilde(nu), cm^-1))          # IR wavenumber
        )
    }
    return(label)
}




