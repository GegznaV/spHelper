#' [!] Read data from OceanOptics OOI Base32 ASCII file with header lines
#'
#' Read contents of spectroscopic ASCII data file with header lines that contain
#' metadata to \code{\link[=hyperSpec-class]{hyperSpec}} object. THe file is
#' created by software `OOI Base32 2.0.0.5`.
#'
#' @details In \code{read.OOIBase32} the default decimal symbol is dot (\code{.}).\cr
#'          In \code{read.OOIBase32_2} - \emph{comma} (\code{,}).
#'
#' @inheritParams read.OceanView
#'
#' @param file The name of the file which the data are to be read from.
#' @param version_ version of OOI Base32 file. Curently version "2.0.0.5"
#'         is supported.
#'
#' @param xlab Label for x (wavelength) axis. If \code{NULL} (defaut) -
#'             the label is automatically selected as "Wavelength, nm".
#'
#' @return A \code{\link[=hyperSpec-class]{hyperSpec}} object with technical and
#'         spectroscopic information from file \code{file}.
#' @export
#'
#'
#' @examples
#'
#' \donttest{
#' \dontrun{
#'
#' read.OOIBase32("Spectra_ascii.txt")
#'
#' # Read several files to one `hyperspec` object:
#'
#' Files   <- dir()[1:4]                           # 4 files are selected
#' sp_list <- lapply(Files, read.OOIBase32)  # Make a list of objects
#' sp      <- collapse(sp_list)                    # Merge several spectra to one object
#'
#' plotmat(sp)
#' }}
#'
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @family functions to read spectroscopic data
#' @author Vilmantas Gegzna
#'
#'
# file <- "D:/Dokumentai/R/Spektroskopija/Spectra/2016-07-18 (Fl and Raman)/2016-07-20/PAP meginiai/aliejus;silicis;1.487mW;1;NA;_RamanShift_14-50-01-977.txt"
read.OOIBase32 <- function(file, dec = '.', n = 19,
                              last_headerline_text = ">>>>>Begin Spectral Data<<<<<",
                              version_ = "2.0.0.5",
                              parse_filename = NULL,
                              ignore.case = TRUE,
                              xlab = NULL,
                              ylab = "I, a.u.") {


    header <-  read.OOIBase32.header(file,
                                     dec = dec,
                                     n = n,
                                     last_headerline_text = last_headerline_text,
                                     version_ = version_)

     # Read spectroscopic data -------------------------------------------------

    sp_data <- read.table(file,
                       header = F,
                       skip   = header$last_header_line,
                       nrows  = header$data$n_pixels_in_spectrum,
                       dec    = dec)

    # Create a spectra object --------------------------------------------------

    # Construct a data frame for non-spectroscopic information -----------------
    if (!is.null(parse_filename)){
        # Parse and extract information contained in a filename
        info_from_fileName <- parse_string(x = file,
                                           pattern = parse_filename,
                                           ignore.case = ignore.case
        )
        data <- cbind(header$data, info_from_fileName)
    } else {
        data <- header$data
    }

    data$file_format <- "ASCII"
#  ------------------------------------------------------------------------
    data$FileName <- file

    LABELS <- modifyList(sp_data_labels(),
                         list(spc = ylab,
                              .wavelength = label_wl("Wavelengths", xlab)
                         )
    )

    # Create a spectra object --------------------------------------------------
    sp <- new('hyperSpec',
              spc        = sp_data$V2,
              wavelength = sp_data$V1,
              data  = data,
              label = LABELS)

    # ==========================================================================
    return(sp)
    # ==========================================================================
}

#' @rdname read.OOIBase32
#' @export

read.OOIBase32_2 <- function(file, dec = ',', n = 19,
                               last_headerline_text = ">>>>>Begin Spectral Data<<<<<",
                               version_ = "2.0.0.5",
                               parse_filename = NULL,
                             ...)
{
    read.OOIBase32(file = file,
                      dec = dec,
                      n = n,
                      last_headerline_text = last_headerline_text,
                      version_ = version_,
                      parse_filename = parse_filename,
                   ...)
}

#' @rdname read.OOIBase32
#' @export

read.OOIBase32_0 <- function(file, dec = '.', n = 19,
                               last_headerline_text = ">>>>>Begin Spectral Data<<<<<",
                               version_ = "2.0.0.5",
                               parse_filename = parser_TD2015,
                             ...)
{
    read.OOIBase32(file = file,
                      dec = dec,
                      n = n,
                      last_headerline_text = last_headerline_text,
                      version_ = version_,
                      parse_filename = parse_filename,
                   ...)
}
