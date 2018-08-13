# Filter signal -----------------------------------------------------------

# Filter

#' Filter noise form a spectroscopic signal
#'
#' Function first applies median and then Savitzky-Golay smoothing filters for
#' each spectroscopic curve individually.
#'
#' @param sp hyperSpec object
#' @param k Window for a \emph{median} filter. If k = 0, median filter is not applied.
#' @param n filter length (must be odd) for a Savitzky-Golay (SG)smoothing filter.
#' If n = 0, SG filter is not applied.
#' @param p filter order for a Savitzky-Golay smoothing filter
#'
#' @return
#' @export
#'
#' @examples
#' library(spHelper)
#' library(hyperSpec)
#'
#' Original
#' Spectra2[1]  %>% plotspc()
#'
#' # Filtered
#' Spectra2[1]  %>% sp_filter() %>% plotspc()
#'
sp_filter <- function(sp, k = 3, n = 25, p = 9){
    filtMedian_k  <- k
    medianFilt    <- function(x) {runmed(x, filtMedian_k)}

    filtSG_n <- n
    filtSG_p <- p

    # Make wrapper function
    sgFilt <- function(x) signal::sgolayfilt(x = x, n = filtSG_n, p = filtSG_p)

    # Apply median filter, if filtMedian_k > 0
    sp_filtered <- if (filtMedian_k > 0) {
        apply( sp, 1, medianFilt)
    } else  {
        sp
    }

    # Apply SG filter, if filtSG_n > 0
    sp_filtered <- if (filtSG_n > 0) {
        apply(sp_filtered, 1, sgFilt)
    }

    # Return the result
    return(sp_filtered)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
format_ratio <- function(santykis){
    santykis %<>% round(2)
    santykis[santykis == 0]   <- NaN
    santykis[santykis == Inf] <- NaN
    return(santykis)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# filtMedian_k  <- 5
# medianFilt    <- function(x) {runmed(x, filtMedian_k)}
#
# filtSG_n <- 35
# filtSG_p <- 9
# sgFilt <- function(x) sgolayfilt(x = x, n = filtSG_n, p = filtSG_p)
#
# Spectra_Filt_Med    <- apply(Spectra, 1, medianFilt)
# Spectra_Filt_Med_SG <- apply(Spectra_Filt_Med, 1, sgFilt)
