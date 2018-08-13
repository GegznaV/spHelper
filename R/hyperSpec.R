

#' Initialize new hyperSpec object
#'
#'  A wrapper function to initialize a new \code{hyperSpec} object. More
#'  information in \pkg{hyperSpec} \code{\link[hyperSpec]{initialize}}.
#'
#' @inheritParams  hyperSpec::initialize
#'
#' @return A \code{hyperSpec} object.
#' @export
hyperSpec <- function(spc        = NULL,
                      data       = NULL,
                      wavelength = NULL,
                      labels     = NULL){
    new("hyperSpec",
        spc        = spc,
        data       = data,
        wavelength = wavelength,
        labels     = labels)
}


