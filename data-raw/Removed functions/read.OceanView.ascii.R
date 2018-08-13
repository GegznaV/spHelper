
#' [!v0.1] Read OceanView ASCII file with header lines
#'
#' Read contents of spectroscopic ASCII data file with header lines that contain
#' metadata to \code{\link[=hyperSpec-class]{hyperSpec}} object. The file is
#' created by software `OceanView 1.5.2`.
#'
#'
#'
#' @inheritParams read.OceanView.ts
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#' \dontrun{
#'
#' read.OceanView("Spectra_ascii.txt")
#'
#' # Read several files to one `hyperspec` object:
#'
#' Files   <- dir()[1:4]                           # 4 files are selected
#' sp_list <- lapply(Files, read.OceanView)        # Make a list of objects
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

read.OceanView <- function(file, dec = '.', n = 17,
                              last_headerline_text = ">>>>>Begin Spectral Data<<<<<",
                              version_ = "1.5.2",
                              parse_filename = NULL,
                              xlab = NULL,
                              ylab = "I, a.u.",
                              ignore.case = FALSE) {


    header <-  read.OceanView.header(file,
                                     dec = dec,
                                     n = n,
                                     last_headerline_text = last_headerline_text,
                                     version_ = version_)

    # Read spectroscopic data -------------------------------------------------

    sp_data <- read.table(file,
                       header = F,
                       dec = dec,
                       skip = header$last_header_line,
                       nrows = header$n_pixels_in_spectrum
                       )

    # Create a spectra object --------------------------------------------------

    # Construct a data frame for non-spectroscopic information -----------------
    if (is.null(parse_filename)){
        data <- header
    } else {
        # Parse and include information contained in a filename
        data <- cbind(header,
                      parse_string(x = file,
                                   pattern = parse_filename,
                                   ignore.case = ignore.case)
                      )
    }

    sp_y <- sp_data$V2
    wl_x <- sp_data$V1
#  ----------------------------------------------------------------------------
    data$FileName <- file

    # Select correct label for  wavelength ------------------------------------
    # Here may be a bug if `wl_mode == NULL`
    wl_mode  <- as.character(header$XAxis_mode[1])

    # Create a spectra object --------------------------------------------------
    sp <- new('hyperSpec',
              spc        = sp_y,
              wavelength = wl_x,
              data  = data,
              label = list(spc = ylab,
                           .wavelength = label_wl(wl_mode, xlab),
                           FileName = "File name",
                           t = "t, s",
                           datetime = "Date and Time"))

    # ==========================================================================
    return(sp)
    # ==========================================================================
}

#' @rdname read.OceanView
#' @export

read.OceanView2 <- function(file, dec = ',', n = 17,
                               last_headerline_text = ">>>>>Begin Spectral Data<<<<<",
                               version_ = "1.5.2",
                               parse_filename = NULL,...)
{
    read.OceanView(file = file,
                      dec = dec,
                      n = n,
                      last_headerline_text = last_headerline_text,
                      version_ = version_,
                      parse_filename = parse_filename,...)
}

#' @rdname read.OceanView
#' @export

read.OceanView.ascii0 <- function(file, dec = '.', n = 17,
                               last_headerline_text = ">>>>>Begin Spectral Data<<<<<",
                               version_ = "1.5.2",
                               parse_filename = parser_1,...)
{
    read.OceanView(file = file,
                      dec = dec,
                      n = n,
                      last_headerline_text = last_headerline_text,
                      version_ = version_,
                      parse_filename = parse_filename,...)
}


