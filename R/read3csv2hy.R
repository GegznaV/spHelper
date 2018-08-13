# Read 3 CSV spectra (from Matlab) to hyperSpec --------------------------------------------
#
#' [+] Read spectroscopic data from 3 CSV files ("data", "wavelengths" and "spectra") to \code{hyperSpec} object
#'
#' Read spectroscopic data  ("data", "wavelengths" and
#'  "spectra") stored in 3 CSV files to hyperSpec object. These fileas are
#'  usually exported from Matlab.
#'
#' @param FileName_base A base of file name (a string). See section "Details".
#'
#' @return Object of class \code{\link[=hyperSpec-class]{hyperSpec}}.
#'
#' @details
#' If base name is "TD_2009", then CSV file names must be:\cr
#' [1] "TD_2009 (wavelengths).csv"\cr
#' [2] "TD_2009 (spectra).csv"\cr
#' [3] "TD_2009 (data).csv"
#'
#' Base name can be a path, e.g. "D:/spectra/TD_2009"
#'
#' @section MATLAB code:
#'
#' The MATLAB code used to generate csv files:
#' \href{https://github.com/GegznaV/spHelper/tree/master/inst/doc/Save_DS_for_R.m}{Save_DS_for_R.m}
#'
#' @export
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna

read3csv2hy <- function(FileName_base){

    requiredFiles <- paste0(FileName_base,
                            c(" (wavelengths)", " (spectra)", " (data)"),
                            '.csv')
    #  ------------------------------------------------------------------------
    fileIsFound <- basename(requiredFiles) %in% dir(dirname(requiredFiles))
    if (!all(fileIsFound))
        stop("Some of requitred files are not found in the working directory")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Read wavelengths        # "(wavelengths).csv"
    wl_x   <- as.numeric(read.csv(requiredFiles[1],header = FALSE))
    # Read spectra            # "(spectra).csv"
    sp_y   <-  as.matrix(read.csv(requiredFiles[2],header = FALSE))
    # Read data               # "(data).csv"
    data   <-            read.csv(requiredFiles[3],sep = "|",
                                  na.strings = c("<netirta>","<undefined>","?")
    )
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if ("ID" %in% data)   data$ID   <- as.factor(data$ID)

    #  Create hyperSpec object ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    sp <- new('hyperSpec', spc = sp_y, wavelength = wl_x, data = data)
    return(sp)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
