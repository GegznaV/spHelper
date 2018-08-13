#' [!v0.1] Read spectroscopic CSV (comma separated values) file
#'
#' @details In \code{read.sp.csv} the default decimal symbol is dot (\code{.}).\cr
#'          In \code{read.sp.csv2} - \emph{comma} (\code{,}).
#'
#'
#' @param file The name of the file with specroscopic data.
#' @param ... parameters to be passed to \code{\link[base]{read.csv}}.
#' @inheritParams read.OceanView
#'
#' @return A \code{\link[=hyperSpec-class]{hyperSpec}} object with technical and
#'         spectroscopic information from file \code{file}.
#' @export
#'
#' @examples
#'
#' \donttest{
#' \dontrun{
#'
#' read.sp.csv2("Spectra_ascii.txt")
#'
#' # Read several files to one `hyperspec` object:
#'
#' Files   <- dir()[1:4]                           # 4 files are selected
#' sp_list <- lapply(Files, read.sp.csv)  # Make a list of objects
#' sp      <- collapse(sp_list)                    # Merge several spectra to one object
#'
#' plotmat(sp)
#' }}
#'
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna
#'
#'


read.sp.csv2 <- function(file, parse_filename = NULL,
                         ylab = "Absorbance",
                         xlab = "Wavelength, nm",
                         ...) {

    # Read spectroscopic data -------------------------------------------------

    sp_data <- read.csv2(file, skip = 1)

    # Create a spectra object --------------------------------------------------

    # Construct a data frame for non-spectroscopic information -----------------
    if (is.function(parse_filename)){
        # Parse and include information contained in a filename
        data <- parse_filename(file)
        data$FileName <- file

    } else {
        data <- data.frame(FileName = file)
    }

    #  ------------------------------------------------------------------------

    LABELS <- modifyList(sp_data_labels(),
                         list(spc = ylab,
                              .wavelength = label_wl("Wavelengths", xlab)

                         )
    )


    # Create a spectra object --------------------------------------------------
    sp <- new('hyperSpec',
              spc        = sp_data$A,
              wavelength = sp_data$nm,
              data       = data,
              label      = LABELS)
    sp <- orderwl(sp)
    # ==========================================================================
    return(sp)
    # ==========================================================================
}

#
# #' @rdname read.sp.csv
# #' @export
#

