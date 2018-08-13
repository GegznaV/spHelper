#' [!] Read header lines of OOI Base32 file
#'
#' Read header lines with non-spectroscopic information of OOI Base32
#' (version 2.0.0.5) file.
#'
#' @inheritParams read.OceanView.ts
#' @param file The name of the file which header lines are to be read from.
#' @param dec The character used in the file for decimal points. Default is
#' a point (\code{.}).
#' @param n Number of lines to be read. Default is 17.
#' @param last_headerline_text The text, that indicates the last line of the header
#'        lines and the beggining of spectroscopic information.
#'         Default is \code{">>>>>Begin Spectral Data<<<<<"}.
#' @param text Text of header as if it was read with function
#'        \code{\link[base]{readLines}}. If \code{text} is provided, \code{file}
#'        is ignored.
#'
#' @return A list with 2 enties:
#' \enumerate{
#'      \item{$data}{A dataframe with information, that contains these variable names: \cr
#'
#' 1                        Date \cr
#' 2                        Time \cr
#' 3                 Graph_Title \cr
#' 4                        User \cr
#' 5  Spectrometer_Serial_number \cr
#' 6           Spectrometer_Type \cr
#' 7                    ADC_Type \cr
#' 8        Spectrometer_Channel \cr
#' 9               software_info \cr
#' 10           Integration_time \cr
#' 11     Integration_time_Units \cr
#' 12             Scans_averaged \cr
#' 13               Boxcar_width \cr
#' 14   Electric_dark_correction \cr
#' 15            Time_Normalized \cr
#' 16        Dual_beam_Reference \cr
#' 17          Reference_Channel \cr
#' 18                Temperature \cr
#' 19       n_pixels_in_spectrum}
#'      \item{$last_header_line} {The number of the last header line}
#' }
#' @export
#'
#'
#' @examples
#'
#' \donttest{
#' \dontrun{
#'
#'  read.OOIBase32.header("MySpectra.Scope")
#'  read.OOIBase32.header("MySpectra.Scope")$data
#'
#' }}
#'
#'
#' ## OOIBase32 Version 2.0.0.5 Data File:
#'
#' #
#' #      OOIBase32 Version 2.0.0.5 Data File
#' #      ++++++++++++++++++++++++++++++++++++
#' #      Date: 03-04-2014, 15:00:04
#' #      User: Valued Ocean Optics Customer
#' #      Spectrometer Serial Number:
#' #      Spectrometer Channel: Master
#' #      Integration Time (msec): 5000
#' #      Spectra Averaged: 1
#' #      Boxcar Smoothing: 0
#' #      Correct for Electrical Dark: Enabled
#' #      Time Normalized: Disabled
#' #      Dual-beam Reference: Disabled
#' #      Reference Channel: Master
#' #      Temperature: Not acquired
#' #      Spectrometer Type: S2000
#' #      ADC Type: USB2000
#' #      Number of Pixels in File: 2048
#' #      Graph Title:
#' #      >>>>>Begin Spectral Data<<<<<
#'
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @family functions to read spectroscopic data
#' @author Vilmantas Gegzna
file <- "C:/Users/ViG/Desktop/R kodai TD analizei/Duomenu_pvz/Fluorescencija/2014-03-05/ID_020/5.Scope"
read.OOIBase32.header <- function(file = NULL,
                                  dec = ".",
                                  n = 19,
                                  last_headerline_text = ">>>>>Begin Spectral Data<<<<<",
                                  version_ = "2.0.0.5",
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
    last_header_line <- grep(last_headerline_text, text)
    if (length(last_header_line) > 1)
        stop(paste0("Sting that indicates the last header line \n",
                   "(`last_headerline_text` = '", last_headerline_text,"') \n ",
                   "is ambiguous and not unique as it is found in \n",
                   "several lines."))

    if (length(last_header_line) == 1) {
        header    <- paste(text[1:last_header_line], collapse = " ")
        if (dec != ".")
            header <- gsub(dec, '.', header, fixed = TRUE)
        # Parse header ------------------------------------------------------------


        switch (version_  %>% as.character,
            "2.0.0.5" = {
                pattern <- paste0(
                   '(?<software_info>.*) Data File \\+* ',
                        'Date: (?<Date>.*), (?<Time>.*) ',
                        'User: (?<User>.*) ',
                        'Spectrometer Serial Number: (?<Spectrometer_Serial_number>.*) ',
                        'Spectrometer Channel: (?<Spectrometer_Channel>.*) ',
                        'Integration Time \\((?<Integration_time_Units>.*)\\):',
                        ' (?<Integration_time>.*) ',
                        'Spectra Averaged: (?<Scans_averaged>.*) ',
                        'Boxcar Smoothing: (?<Boxcar_width>.*) ',
                        'Correct for Electrical Dark: (?<Electric_dark_correction>.*) ',
                        'Time Normalized: (?<Time_Normalized>.*) ',
                        'Dual-beam Reference: (?<Dual_beam_Reference>.*) ',
                        'Reference Channel: (?<Reference_Channel>.*)',
                        'Temperature: (?<Temperature>.*) ',
                        'Spectrometer Type: (?<Spectrometer_Type>.*) ',
                        'ADC Type: (?<ADC_Type>.*) ',
                        'Number of Pixels in File: (?<n_pixels_in_spectrum>.*) ',
                        'Graph Title: (?<Graph_Title>.*)',
                        '>>>>>Begin Spectral Data<<<<<'
                )
                VarOrder <- c(2, 3, 19, 4, 5, 16, 17, 6, 1, 8, 7, 9:15, 18)
                # as.data.frame(varNames)
                 #                           .
                 #                       varNames
                 # 1                        Date
                 # 2                        Time
                 # 3                 Graph_Title
                 # 4                        User
                 # 5  Spectrometer_Serial_number
                 # 6           Spectrometer_Type
                 # 7                    ADC_Type
                 # 8        Spectrometer_Channel
                 # 9               software_info
                 # 10           Integration_time
                 # 11     Integration_time_Units
                 # 12             Scans_averaged
                 # 13               Boxcar_width
                 # 14   Electric_dark_correction
                 # 15            Time_Normalized
                 # 16        Dual_beam_Reference
                 # 17          Reference_Channel
                 # 18                Temperature
                 # 19       n_pixels_in_spectrum
            },

            # Otherwise:
            stop(paste("`version_` = ", version_, " is not supported."))
        )

        # Parse strings and extract data ------------------------------------------
        header_data <- regexp2df(header, pattern, stringsAsFactors = FALSE)

        if (length(header_data) == 0)
            warning(paste0("Pattern to parse header lines of of OceanOptics ",
                          "(version ", version_,") file was used. This ",
                          "pattern might be inappropriate as no information ",
                          "was extracted.")
                    )
        # Sort variables ----------------------------------------------------------
        VarOrder <- c(VarOrder, setdiff(1:ncol(header_data), VarOrder))
        header_data <- header_data[,VarOrder]

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
                  # "Electric_dark_correction",
                  "Nonlinearity_correction",
                  "Stop_averaging"),
            y = varNames)

        header_data[,NumVar] <- mapply(as.numeric, header_data[,NumVar])
        header_data[,LogVar] <- mapply(as.logical, header_data[,LogVar])

        # `msec` convert to `sec` -------------------------------------------------

        switch(header_data$Integration_time_Units,
               "msec" = {
                   header_data$Integration_time_Units <- "sec"
                   header_data$Integration_time       <-  header_data$Integration_time / 1000
               },
               "sec" = {NULL},
               warning("Integration time units is neither 'msec', nor 'sec', thus labels of inegration time migth be misleading.")
        )


    # If no header lines were found
    } else {
        last_header_line = 0
        header_data = data.frame()
        warning("No header lines were read. Value of parameter `n` may be incorrect")
    }
    # Output ------------------------------------------------------------------
    return(list(data             = header_data,
                last_header_line = last_header_line)
           )
}


