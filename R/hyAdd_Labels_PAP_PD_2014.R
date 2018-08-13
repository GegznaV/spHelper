# hyAdd_Labels_PAP_PD_2014 ----------------------------------------------------
#
#' [+] Add labels to "PAP_PD_2014" and transform the dataset
#'
#' Function is designed to label data collected during investigation called
#'  "PAP_PD_2014".
#'
#' Select data columns, that are not removed, and add labels to
#'  \code{\link[=hyperSpec-class]{hyperSpec}} object of "PAP_PD_2014" data.
#'
#'
#' @param sp A \code{\link[=hyperSpec-class]{hyperSpec}} object of PAP_PD_2014
#'  data, created by function \code{\link{read3csv2hy}}.
#' @param language A string, indicating a language of labels. Possible
#' entries are \code{EN} - English and \code{LT} - Lithuanian.
#' Default is \code{EN}.
#'
#' @return A labeled object with reduced number of data columns.
#'
#' @export
#'
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna
#'
hyAdd_Labels_PAP_PD_2014 <- function(sp, language = "EN")  {
    ColsInitial <- colnames(sp) # save initial column names

    data         = sp$..

    # `mutate` does not support `POSIXlt` results, thus do it separately
    data$Time <- strptime(paste(data$Date, data$Time), "%m-%d-%Y %H:%M:%S")
    data$Date <- as.Date(data$Date,format = "%m-%d-%Y")

    # Only necessary columns are selected:
    data <- data %>%
        dplyr::mutate(fileName = file_name_with_path,
					  point       = taskas,
					  spID        = as.factor(gsub('_{1,3}','|', spID)),
					  sp_type     = tyrimas,
                      exp_code    = tyrimo_kodas,
					  excitation  = Zadinimas,
					  # Time        = strptime(paste(Date, Time), "%m-%d-%Y %H:%M:%S"),
					  # Date        = as.Date(Date,format = "%m-%d-%Y"),
					  t.int       = Integration_time/1000,
					  t.int.units = as.factor(paste0("1000*", Integration_time_Units))
        ) %>%

        dplyr::select(ID,
                      ID2,
                      spID,
                      point,
                      exp_code,
					  sp_type,
                      fileName,

                      Date,
                      Time,
					  Electric_dark_correction,
					  t.int,
					  t.int.units,
					  excitation,


                      Dziov,
                      CitoGr,
                      HistGr,
                      HibridGr,
                      Boxcar_width
        )

    Object <- new('hyperSpec', spc = sp$spc, wavelength = wl(sp), data = data)

    # add Labels ------------------------------------------------------------
    Var.Names <- colnames(Object)

    Var.Labels <- switch(language,
           LT =   c("Meginio ID",
                      "Meginio ID2",
                      "Spektro ID",
                      "Tasko numeris meginy",
					  "Eksperimento kodas",
                      "Spektroskopijos tipas",
					  "Bylos pavadinimas",
                      "Data (Spektometre)",
                      "Registravimo pradzios laikas",
					  "Electric dark correction",
                      "Signalo integracijos laikas, s",
					  "Integracijos laiko vienetai",
					  "Zadinanti spinduliuote, nm",
                      "Sausumas",
                      "Ciltologines grupes",
                      "Histologines grupes",
                      "Hibridines grupes",
                      "Boxcar width",
                      "I, sant.vnt."),

           EN =   c( "Specimen ID",
                      "Specimen ID2",
                      "Spectrum ID" ,
                      "Point number in a specimen",
                      "Code of Experiment",
                      "Type of Spectroscopy",
                      "File name",
                      "Date (Spectometer)",
                      "Time of Registration Beginning",
                      "Electric dark correction",
                      "Integration time, s",
                      "Excitation wavelength, nm",
                      "Units of Integration time",
                      "Liquid/Dry",
                      "Cytological groups",
                      "Histological groups",
                      "Hybrid groups",
                      "Boxcar width",
                      "I, units"),

		   stop("The value of `language` is not supported."))

    labels(Object)[Var.Names] <- Var.Labels

    # x axis labels
    Object <- hyAdd_Label_wl(Object, "wavelength")

    # ---------------------------------------------------------------------
	# Reorder levels correctly
    Object$CitoGr <- factor(Object$CitoGr,
	                  levels = c("IPPN", "ASCH", "ASCUS", "LSIL", "HSIL"),
	                  labels = c("IPPN", "ASCH", "ASCUS", "LSIL", "HSIL"))

    Object$HistGr <- factor(Object$HistGr,
                      levels = c("Cervicitas", "CIN1", "CIN2", "CIN3+"),
                      labels = c("Cervicitas", "CIN1", "CIN2", "CIN3/CIS"))

    Object$HibridGr <- factor(Object$HibridGr,
                      levels = c("IPPN", "Cervicitas", "CIN1", "CIN2", "CIN3+"),
                      labels = c("IPPN", "Cervicitas", "CIN1", "CIN2", "CIN3/CIS"))

    # ----------------------------------------------------------------------
    # # Add `.color`: variable with colors
    # Object <- hyAdd_color(Object, "HibridGr")
	# ---------------------------------------------------------------------
    # CHECK if any columns were added or deleted
    ColsFinal   <- colnames(Object)
    message("These columns were:")
    print(list_AddRm(ColsInitial, ColsFinal))
    # ---------------------------------------------------------------------

    return(Object)
}
