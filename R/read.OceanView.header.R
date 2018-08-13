#' [!] Read header lines of OceanView file
#'
#' Read header lines with non-spectroscopic information of OceanView
#' (version 1.5.2) file.
#'
#' @inheritParams read.OceanView
#'
#' @param software_info (string) A combination of \code{software} and its \code{version_}.
#' @param text (strings) Text of header as if it was read with function
#'        \code{\link[base]{readLines}}. If \code{text} is provided, \code{file}
#'        is ignored.
#'
#' @return A dataframe with information, extracted from headerlines.
#' @export
#'
#' @examples
#'
#' # Read from file: -------------------------------------------
#'
#' \donttest{
#' \dontrun{
#'  read.OceanView.header("MySpectra.txt")
#' }}
#'
#' # Read as text: ---------------------------------------------
#' header <- c(
#'      "Data from MySpectra.txt Node",
#'      "",
#'      "Date: Fri Jan 15 16:15:16 GMT 2014",
#'      "User: Scientist_1",
#'      "Spectrometer: USB2E2321",
#'      "Trigger mode: 4",
#'      "Integration Time (sec): 1.000000E1",
#'      "Scans to average: 1",
#'      "Electric dark correction enabled: true",
#'      "Nonlinearity correction enabled: true",
#'      "Boxcar width: 0",
#'      "XAxis mode: Wavelengths",
#'      "Number of Pixels in Spectrum: 2048",
#'      ">>>>>Begin Spectral Data<<<<<"
#' )
#'
#' DF <- read.OceanView.header(text = header)
#'
#' class(DF)
#' print(DF)
#'
#'
#' # Examples of file headerline formats: -----------------------
#'
#' ## An example of header lines in OcenView 1.5.2:
#'
#' #    Data from MySpectra.txt Node
#' #
#' #    Date: Fri Jan 15 16:15:16 GMT 2014
#' #    User: Scientist_1
#' #    Spectrometer: USB2E2321
#' #    Trigger mode: 4
#' #    Integration Time (sec): 1.000000E1
#' #    Scans to average: 1
#' #    Electric dark correction enabled: true
#' #    Nonlinearity correction enabled: true
#' #    Boxcar width: 0
#' #    XAxis mode: Wavelengths
#' #    Number of Pixels in Spectrum: 2048
#' #    >>>>>Begin Spectral Data<<<<<
#'
#' ## An example of header lines in OcenView 1.5.0:
#'
#' #    Data from MySpectra.txt Node
#' #
#' #    Date: Fri Jan 15 16:15:16 GMT 2016
#' #    User: Scientist_1
#' #    Spectrometer: USB2E2321
#' #    Autoset integration time: false
#' #    Trigger mode: 4
#' #    Integration Time (sec): 1.000000E1
#' #    Scans to average: 1
#' #    Electric dark correction enabled: true
#' #    Nonlinearity correction enabled: true
#' #    Boxcar width: 0
#' #    XAxis mode: Wavelengths
#' #    Stop averaging: false
#' #    Number of Pixels in Spectrum: 2048
#' #    >>>>>Begin Spectral Data<<<<<
#'
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @family functions to read spectroscopic data
#' @author Vilmantas Gegzna

read.OceanView.header <- function(file = NULL,
                                  dec = ".",
                                  n = 17,
                                  software = "OceanView",
                                  version_ = "1.5.2",
                                  software_info = paste(software, version_),
                                  last_headerline_text = ">>>>>Begin Spectral Data<<<<<",
                                  text) {

    force(file)
    force(dec)
    force(n)
    force(last_headerline_text)
    force(version_)

    # Read file line by line --------------------------------------------------
    if (missing(text))
        text  <- readLines(file, n = n)

    # Extract information from Header lines of file ---------------------------
    last_headerline <- grep(last_headerline_text, text)

    if (length(last_headerline) > 1)
        stop(paste0("Sting that indicates the last header line \n",
                   "(`last_headerline_text` = '", last_headerline_text,"') \n ",
                   "is ambiguous and not unique as it is found in \n",
                   "several lines."))

    if (length(last_headerline) == 1) {
        header    <- paste(text[1:last_headerline], collapse = " ")
        if (dec != ".")
            header <- gsub(dec, '.', header, fixed = TRUE)
        # Parse header ------------------------------------------------------------


        switch (software_info  %>% as.character,
            "OceanView 1.5.2" = {
                pattern <- paste0(
                    'Data from (?<Data_from>.*) Node.*',
                    ' Date: (?<Date>.*)',
                    ' User: (?<User>.*)',
                    ' Spectrometer: (?<Spectrometer>.*)',
                    # 'Autoset integration time: (?<Autoset_integration_time>.*)',
                    ' Trigger mode: (?<Trigger_mode>.*)',
                    ' Integration Time \\((?<Integration_time_Units>.*)\\): ',
                                                   '(?<Integration_time>.*)',
                    ' Scans to average: (?<Scans_averaged>.*)',
                    ' Electric dark correction enabled: (?<Electric_dark_correction>.*)',
                    ' Nonlinearity correction enabled: (?<Nonlinearity_correction>.*)',
                    ' Boxcar width: (?<Boxcar_width>.*)',
                    ' XAxis mode: (?<XAxis_mode>.*)',
                    # 'Stop averaging: (?<Stop_averaging>.*)',
                    ' Number of Pixels in Spectrum: (?<n_pixels_in_spectrum>.*)',
                    ' >>>>>Begin Spectral Data<<<<<'
                )
                 VarOrder <- c(3:4,2,5,7,6,8:13,1)
                 # as.data.frame(varNAmes)
                 #                           .
                 # 1                      User
                 # 2              Spectrometer
                 # 3                 Data_from
                 # 4                      Date
                 # 5              Trigger_mode
                 # 6          Integration_time
                 # 7    Integration_time_Units
                 # 8            Scans_averaged
                 # 9  Electric_dark_correction
                 # 10  Nonlinearity_correction
                 # 11             Boxcar_width
                 # 12               XAxis_mode
                 # 13     n_pixels_in_spectrum
            },

            "OceanView 1.5.0" = {
                pattern <- paste0(
                    'Data from (?<Data_from>.*) Node.*',
                    ' Date: (?<Beggin_at>.*)',
                    ' User: (?<User>.*)',
                    ' Spectrometer: (?<Spectrometer>.*)',
                    ' Autoset integration time: (?<Autoset_integration_time>.*)',
                    ' Trigger mode: (?<Trigger_mode>.*)',
                    ' Integration Time \\((?<Integration_time_Units>.*)\\): ',
                    ' (?<Integration_time>.*)',
                    ' Scans to average: (?<Scans_averaged>.*)',
                    ' Electric dark correction enabled: (?<Electric_dark_correction>.*)',
                    ' Nonlinearity correction enabled: (?<Nonlinearity_correction>.*)',
                    ' Boxcar width: (?<Boxcar_width>.*)',
                    ' XAxis mode: (?<XAxis_mode>.*)',
                    ' Stop averaging: (?<Stop_averaging>.*)',
                    ' Number of Pixels in Spectrum: (?<n_pixels_in_spectrum>.*)',
                    ' >>>>>Begin Spectral Data<<<<<'
                )
                VarOrder <- c(3, 4, 2, 5, 6, 8, 7, 10, 9, 11:15, 1)

            },

            # Otherwise:
            stop(paste0("Format `software_info` = '", software_info,
                       "' is not supported."))
        )

        # Parse strings and extract data ------------------------------------------
        header_data <- regexp2df(header, pattern, stringsAsFactors = FALSE)

        if (length(header_data) == 0)
            warning(paste0("Pattern to parse header lines of of OceanOptics ",
                          "(version ", version_,") file was used. This ",
                          "pattern might be inappropriate for your data
                          as no information was extracted.")
                    )
        # Sort variables ----------------------------------------------------------
        VarOrder <- c(VarOrder, setdiff(1:ncol(header_data), VarOrder))
        header_data <- header_data[,VarOrder]

        header_data$last_headerline <- last_headerline
        header_data$software_info   <- software_info

        varNames <- header_data %>% names


        # Modify class or variables -----------------------------------------------

        # Use `intersect` in case sume varibles in new versions of OceanView
        # files do not exist.

        NumVar <- intersect(
            x = c("Trigger_mode",
                    "Integration_time",
                    "Scans_averaged",
                    "Boxcar_width",
                    "n_pixels_in_spectrum"
                    ),
            y = varNames)

        LogVar <- intersect(
            x = c("Autoset_integration_time",
                  "Electric_dark_correction",
                  "Nonlinearity_correction",
                  "Stop_averaging"),
            y = varNames)

        header_data[,NumVar] <- mapply(as.numeric, header_data[,NumVar])
        header_data[,LogVar] <- mapply(as.logical, header_data[,LogVar])
    # If no header lines were found
    } else {
        last_headerline = 0
        header_data = data.frame()
        warning("No header lines were read. Value of parameter `n` may be incorrect")
    }


    # Output ------------------------------------------------------------------
    return(header_data)
}
