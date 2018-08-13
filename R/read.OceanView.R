#' [!v0.3] Read spectroscopic OceanView file
#'
#' Read contents of \bold{one} spectroscopic \bold{file} produced by OcenView software
#' (Ocen Optics, Inc.) to \code{\link[=hyperSpec-class]{hyperSpec}} object.
#'  Supported file formats areeither \emph{TimeSeries} data file with header or
#'  \emph{ASCII} data file with header.
#'  Suported versions of file are `OceanView 1.5.2` and `OceanView 1.5.0?`
#'
#' @details In \code{read.OceanView} the default decimal symbol is \emph{period}
#'          (\code{.}). In \code{read.OceanView2} - \emph{comma} (\code{,}).
#'
#'
#' @param file (string) A name of OcenView file .
#' @param dec ("." | ",") The character used in the file for decimal points , e.g.
#'            period (\code{"."}) or comma (\code{","}).
#' @param n (integer) Number of header lines to be scanned . Default is 17.
#'          These lines include the header line indicated in
#'          \code{last_headerline_text}.
#'
#' @param last_headerline_text (string) A string, that indicates the last header line.
#'         Default is \code{">>>>>Begin Spectral Data<<<<<"}.
#'
#' @param software ("OceanView" | "OOIBase32") Name of software, that created the file.
#'
#' @param version_ ("1.5.2" | "1.5.0" | string) A version of OceanView file.
#'            Curently supported  versions are "1.5.2" (or newer) and "1.5.0".
#'
#' @param parse_filename (function | string | NULL) Either a function that
#'                       parses string of filename and
#'                       extracts relevant information,\cr or a regular
#'                       expression to be used to parse file name and extract
#'                       relevant information,\cr or \code{NULL}.
#'
#' @param xlab (string | NULL) Label for x (wavelength) axis.
#'              If \code{NULL} (defaut) -
#'             the label is selected automatically.
#'
#' @param ylab (string) Label for y (intensity) axis. Defaut is \code{"I, a.u."}.
#'
#' @param file_format (\code{"auto"}|\code{"ascii"}|\code{"ts"}|\code{"timeseries"})
#'             A string to determine format of spectroscopic OceanView file:\cr
#'             \code{"ascii"} for ASCII file with header;\cr
#'             \code{"ts"} or \code{"timeseries"} for timeseries file with
#'             header;\cr
#'             \code{"auto"} (default) - automatically determines the format of
#'             file, but may result in a \bold{slower} performance than options above.
#'
#' @param ignore.case A logical that indicates if regular extression in
#'          \code{parse_filename} is case sensitive.
#'          Applies ONLY if \code{parse_filename} is a string with regular
#'          expression.
#'
#' @return A \code{\link[=hyperSpec-class]{hyperSpec}} object with technical and
#'         spectroscopic information from file \code{file}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' \dontrun{
#'
#' library(spHelper)
#'
#' read.OceanView("Spectra.txt")
#' read.OceanView("Spectra.txt", dec = ",")
#' read.OceanView2("Spectra.txt")
#'
#'
#' # Read several files to one `hyperspec` object:
#'
#' Files   <- dir()[1:4]                     # 4 files are sellected
#' sp_list <- lapply(Files, read.OceanView)  # Makes a list of objects
#' sp      <- collapse(sp_list)              # Makes one object
#'
#' plotmat(sp)
#'
#' # -------------------------------------------------------------------
#'
#' files <- dir("data-raw/demo spectra", full.names = TRUE)
#'
#' # ts example:
#'
#' file.ts <- files[3]
#' read.OceanView(file.ts) %T>% plot(col=2:3) %>%  summary_hyData
#'
#' # ascii examples:
#'
#' file.ascii.Raman <- files[1]
#' read.OceanView(file.ascii.Raman) %T>% plot(col=2) %>%  summary_hyData
#'
#' file.ascii.Fluorescence <- files[2]
#' read.OceanView(file.ascii.Fluorescence) %T>% plot(col=4) %>%  summary_hyData
#' read.OceanView(file.ascii.Fluorescence)$..  %>% head
#'
#' # .Scope example:
#' file.Scope <- files[4]
#' read.OOIBase32(file.Scope) %T>% plot(col=5) %>%  summary_hyData
#' read.OOIBase32(file.Scope)$..  %>% head
#' # -------------------------------------------------------------------
#' }}
#' @family functions to read spectroscopic data
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna
#'

read.OceanView <- function(file,
                              dec = '.',
                              parse_filename = NULL,
                              ignore.case = FALSE,
                              xlab = NULL,
                              ylab = "I, a.u.",
                              last_headerline_text = ">>>>>Begin Spectral Data<<<<<",
                              software = "OceanView",
                              version_ = "1.5.2",
                              n = 17,
                              file_format = c("auto","ascii","ts","timeseries")[1]

                           ) {

    # Read header lines --------------------------------------------------------
    header <- read.OceanView.header(file,
                                    dec = dec,
                                    n = n,
                                    software = software,
                                    version_ = version_,
                                    last_headerline_text = last_headerline_text
    )

    # Determine the format of an OcenView file --------------------------------
    switch(tolower(file_format),
           "ts"         = {FILE_FORMAT <- "TimeSeries"},
           "timeseries" = {FILE_FORMAT <- "TimeSeries"},
           "ascii"      = {FILE_FORMAT <- "ASCII"},
           "auto"       = {
               # Recognize format automatically
               wl_x <- read.table(file,
                                  dec = dec,
                                  skip = header$last_headerline,
                                  nrows = 1) %>%
                   as.numeric()
               N <- length(wl_x)

               if (N == 2) {
                   FILE_FORMAT <- "ASCII" # Ascii format (has 2 data columns)
               } else if (N == header$n_pixels_in_spectrum) {
                   FILE_FORMAT <- "TimeSeries"    # Time series format
               } else {
                   stop("The format of OceanView file was recognized" %.+.%
                        "neither as 'ascii' with header lines nor as" %.+.%
                        "'time series' with header lines. Operation"  %.+.%
                        "cannot be continued.")
               }
           },

           stop("Incorrect `file_format` of spectroscopic data is indicated: " %++% file_format)
    )

    # Read an appropriate format of OceanView file

    switch(FILE_FORMAT,
   # <<< Time series format >>> ===============================================
           "TimeSeries" = {
               # Read data of x (wavelength) axis  ----------------------------
               if (!"wl_x"  %in% ls()) {
                # (last_haderline + 1) contains values of x axis
                   wl_x <- read.table(file,
                                      dec = dec,
                                      skip = header$last_headerline,
                                      nrows = 1) %>%
                       as.numeric()
               }

               # The remaining lines contain spectra and time of registration:
               # Read spectra  ------------------------------------------------
               time_and_sp <- read.table(file,
                                  header = FALSE,
                                  dec    = dec,
                                  skip   = header$last_headerline + 1,
                                  stringsAsFactors = FALSE)

               n_wl     <- header$n_pixels_in_spectrum
               sp_last  <- ncol(time_and_sp)
               sp_first <- sp_last - n_wl + 1

               sp_y <- as.matrix(time_and_sp[,sp_first:sp_last])

               #  Extract Time and Date ---------------------------------------
               # Time zone ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               # Format of string
               # "Tue Jul 26 12:53:58 EEST 2016" is
               # "%a %b %e %H:%M:%S %Z %Y"
               #
               # Unfortunately, time zone "%Z" can not be captured by R. Thus
               # regular extression is used:
               tz <- gsub("(?:.* \\d{1,2}:\\d{1,2}:\\d{1,2}) (.*) (?:\\d{,4})",
                          "\\1", header$Date)
               # <<<<<<- might be a bug with time zones as
               # R does not accept EEST:
               tz <- gsub("EEST","EET", tz)

               # Time  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               # time      <- chron::times(time_and_sp[,1],format = 'h:m:s')
               timestamp <- time_and_sp[, sp_first - 1]
               datetime  <- as.POSIXct(as.numeric(timestamp)/1000,
                                       origin = '1970-01-01',
                                       tz = tz
               )
               t <- (timestamp - timestamp[1]) / 1000 # time, seconds

               # Construct a data frame for non-spectroscopic information -----
               data <- data.frame(file_format = FILE_FORMAT,
                                  FileName = file,
                                  datetime = datetime,
                                  t = t)
               data <- merge(header, data)

               rm(timestamp, datetime, t)
           }, #END "ts"

   # <<< ASCII format >>> ========================================================
           "ASCII" = {
               # Read spectroscopic data --------------------------------------

               sp_data <- read.table(file,
                                     header = FALSE,
                                     dec    = dec,
                                     skip   = header$last_headerline,
                                     nrows  = header$n_pixels_in_spectrum
               )


              # Necessary information -----------------------------------------
               sp_y <- sp_data$V2
               wl_x <- sp_data$V1
               data <- header

               data$file_format <- FILE_FORMAT
               data$FileName    <- file

           } #END "ascii"

    ) #END switch(FILE_FORMAT)

# Further operations common for all file formats ==============================

    # Construct a data frame of non-spectroscopic information -----------------
    if (!is.null(parse_filename)) {
        # Parse and extract information contained in a filename
        info_from_fileName <- parse_string(x = file,
                                           pattern = parse_filename,
                                           ignore.case = ignore.case
        )
        data <- cbind(data,
                      as.data.frame(
                          # Repeat the same information for each row to enable
                          # a correct `cbind`
                          lapply(info_from_fileName, rep, each = nrow(data))
                          )
        )
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Get x axis / wavelength mode
    wl_mode <- as.character(header$XAxis_mode[1])


    LABELS <- modifyList(sp_data_labels(),
                         list(spc = ylab,
                              .wavelength = label_wl(wl_mode, xlab)

                         )
    )

    # Create a hyperSpec object -----------------------------------------------
    sp <- new('hyperSpec',
              spc        = sp_y,
              wavelength = wl_x,
              data       = data,
              label      = LABELS)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return(sp)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# =============================================================================
#' @rdname read.OceanView
#' @export
read.OceanView2 <- function(file, dec = ',', ..., file_format = "auto") {
    read.OceanView(file = file, dec = dec, ...,  file_format = file_format)
}

# =============================================================================
#' @rdname read.OceanView
#' @export
read.OceanView0 <- function(file, dec = '.', ..., file_format = "auto",
                            parse_filename = parser_1) {
    read.OceanView(file = file, dec = dec, ...,  file_format = file_format,
                   parse_filename = parse_filename)
}


# =============================================================================
#' @rdname read.OceanView
#' @export
read.OceanView.ascii <- function(file, dec = '.', ..., file_format = "ascii") {
    read.OceanView(file = file, dec = dec, ...,  file_format = file_format)
}

# =============================================================================
#' @rdname read.OceanView
#' @export
read.OceanView.ascii2 <- function(file, dec = ',', ..., file_format = "ascii") {
    read.OceanView(file = file, dec = dec, ...,  file_format = file_format)
}

# =============================================================================
#' @rdname read.OceanView
#' @export
read.OceanView.ascii0 <- function(file, dec = '.', ..., file_format = "ascii",
                               parse_filename = parser_1){
    read.OceanView.ts(file = file,dec = dec, ..., file_format = file_format,
                      parse_filename = parse_filename)
}

# =============================================================================
#' @rdname read.OceanView
#' @export
read.OceanView.ts <- function(file, dec = '.', ..., file_format = "ts") {
    read.OceanView(file = file, dec = dec, ...,  file_format = file_format)
}

# =============================================================================
#' @rdname read.OceanView
#' @export
read.OceanView.ts2 <- function(file, dec = ',', ..., file_format = "ts") {
    read.OceanView(file = file, dec = dec, ...,  file_format = file_format)
}

# =============================================================================
#' @rdname read.OceanView
#' @export
read.OceanView.ts0 <- function(file, dec = '.', ..., file_format = "ts",
                               parse_filename = parser_1){
    read.OceanView.ts(file = file,dec = dec, ..., file_format = file_format,
                      parse_filename = parse_filename)
}
# =============================================================================

