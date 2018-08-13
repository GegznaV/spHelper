# Normalize spectra ---------------------------------------------

#' Remove offset
#'
#' Convenience function to remove offset from spectroscopic data in a \code{hyperSpec} object.
#'
#' @param sp \code{hyperSpec} object.
#' @param offset (number)\cr
#'               value of desired offset.
#' @param in_range,at (\code{numeric} | \code{formula}) \cr
#'                  either a single numeric value or
#'                  a pair of values describing the range (e.g., 300~350)
#'                  of wavelengths to be used for normalization.
#'                  Default is \code{range = min~max}.
#'
#' @return \code{sp} with y-axis offset removed in (\code{sp@data$spc}).
#' @export
#'
#' @examples
#'
#' library(magrittr)
#'
#' Spectra2 %>% plotspc()
#'
#' Spectra2 %>% sp_remove_offset() %>% plotspc()
#'
#' Spectra2 %>% sp_remove_offset(in_range = 300~400) %>% plotspc()

# Spectra2 %>% hyperSpec::normalize01() %>% plotspc()
sp_remove_offset <- function(sp,
                             in_range = min~max,
                             offset = 0,
                             method = "not_used_yet",
                             at = NULL) {

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(at)) in_range <- at
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Subset spectral range
    sp0 <- sp[,, in_range]

    current_offset <- apply(sp0, 1, min)

    # Output:
    sweep(sp, 1, current_offset, "-") + offset
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
