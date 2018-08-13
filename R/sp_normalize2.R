# Normalize spectra ---------------------------------------------

#' Normalize spectra
#'
#' Convenience function to normalize spectroscopic data in a \code{hyperSpec} object.
#'
#' @param sp \code{hyperSpec} object.
#' @param norm ("max" | "mean" | "area" | "area_abs" | "nim")\cr
#'             a way of normalization.
#' @param in_range,at (\code{numeric} | \code{formula}) \cr
#'                  either a single numeric value or
#'                  a pair of values describing the range (e.g., 300~350)
#'                  of wavelengths to be used for normalization.
#'                  Default is \code{range = min~max}.
#'
#' @return \code{sp} with normalized spectra in (\code{sp@data$spc}).
#' @export
#'
#' @examples
#'
#' library(magrittr)
#'
#' Spectra2 <- hyperSpec::chondro
#' Spectra2 %>% plotspc()
#'
#' Spectra2 %>% hyperSpec::normalize01() %>% plotspc()
#'
#' Spectra2 %>% sp_normalize2(at = 700) %>% plotspc()
#'
#' Spectra2 %>% sp_normalize2("area") %>% plotspc()
#'
#' Spectra2 %>% sp_normalize2("area", in_range = 1400~1700) %>% plotspc()

sp_normalize2 <- function(sp,
                         norm = "max",
                         in_range = min~max,
                         at = NULL) {

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(at)) in_range <- at
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Subset spectral range
    sp0 <- sp[,, in_range]

    # Calculate voctor of normalization coefficients (k)
    k <- switch(as.character(norm),
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                "max"       = apply(sp0, 1, max)$spc,
                "min"       = apply(sp0, 1, min)$spc,
                "mean"      = apply(sp0, 1, mean)$spc,
                "area"      = {
                    AREA_fun <- function(y) pracma::trapz(x = wl(sp0), y = y);
                    apply(sp0, 1, AREA_fun)$spc
                },
                "area_abs"      = {
                    AREA_fun <- function(y) pracma::trapz(x = wl(sp0), y = y);
                    abs(apply(sp0, 1, AREA_fun)$spc)
                },
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                stop(glue::glue("Incorrect choice of `norm`: {norm}"))
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    )

    # Do the normalization
    sp_norm <- sweep(sp, 1, k, "/")

    # Output:
    sp_norm
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
