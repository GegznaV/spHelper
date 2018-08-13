# Normalize spectra ---------------------------------------------

#' Normalize spectra
#'
#' Convenience function to normalize spectroscopic data in a \code{hyperSpec} object.
#'
#' @param sp \code{hyperSpec} object.
#' @param norm ("I" | "I_max" | "I_mean" | "area" | "max" | "mean" )\cr
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
#'
#' Spectra2 %>% plotspc()
#'
#' Spectra2 %>% sp_normalize() %>% plotspc()
#'
#' Spectra2 %>% sp_normalize("I", 720) %>% plotspc()
#'
#' Spectra2 %>% sp_normalize("area") %>% plotspc()
#' Spectra2 %>% sp_normalize("area", 300~400) %>% plotspc()

sp_normalize <- function(sp,
                         norm = "max_I",
                         in_range = min~max,
                         at = NULL) {

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(at)) in_range <- at
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Subset spectral range
    sp0 <- sp[,, in_range]

    # Calculate voctor of normalization coefficients (k)

    k <- switch(as.character(norm),
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # For compatibility with older versions of function
      "mean_I"    = apply(sp0, 1, mean)$spc,
      "max_I"     = apply(sp0, 1, max)$spc,
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      "I_mean"    = ,
      "mean"      = apply(sp0, 1, mean)$spc,
      "I_max"     = ,
      "max"       = apply(sp0, 1, max)$spc,
      "I_min"     = ,
      "min"       = apply(sp0, 1, min)$spc,
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      "I"         = {
          if (length(in_range) != 1)
              stop('When norm = "max_I", the length of `in_range`/`at` must be 1.')
          if (!is.numeric(in_range))
              stop('When norm = "max_I", `in_range`/`at` must be numeric.')
          sp0$spc
      },
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      "area"      = {
          AREA_fun <- function(y) pracma::trapz(x = wl(sp0), y = y);
          apply(sp0, 1, AREA_fun)$spc
      },
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      "area_abs"      = {
          AREA_fun <- function(y) pracma::trapz(x = wl(sp0), y = y);
          abs(apply(sp0, 1, AREA_fun)$spc)
      },
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      stop(glue::glue("Incorrect choice of `norm`: {norm}"))
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    )

    # Do the normalization
    sp_norm <- sweep(sp, 1, k, "/")

    # Output:
    sp_norm
}
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # # Normalization
    # sp_norm <- switch(norm  %>% as.character,
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                   "mean_I"    = sweep(sp, 1, mean,'/'),
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                   "max_I"     = sweep(sp, 1, max,'/'),
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                   "I_at_510"  = {
    #                       wl_i <- wl2i(sp, 510);
    #                       S <- sweep(sp, 1, sp[,,510][[]], '/');
    #                       if (rm.value.at == TRUE){
    #                           S[,,-wl_i, wl.index = TRUE]
    #                       } else {
    #                           S
    #                       }
    #
    #                   },
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                   "I"  = {
    #                       if (length(at) != 1)
    #                           stop("Length of `at` must be 1.")
    #
    #                       if (!is.numeric(at))
    #                           stop("`at` must be numeric.")
    #
    #                       wl_i <- wl2i(sp, at);
    #                       S <- sweep(sp, 1, sp[,,at][[]], '/');
    #                       if (rm.value.at == TRUE){
    #                           S[,,-wl_i, wl.index = TRUE]
    #                       } else {
    #                           S
    #                       }
    #                   },
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                   "area"      = {
    #                       AREA_fun <- function(y)
    #                           pracma::trapz(x = wl(sp), y = y);
    #
    #                       sweep(sp, 1, AREA_fun,'/')
    #                   },
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #                   stop("Incorrect choice of `norm`:" %.+.% norm)
    # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # )




