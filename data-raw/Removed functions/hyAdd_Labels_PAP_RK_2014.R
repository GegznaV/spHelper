# hyAdd_Labels_... --------------------------------------------------------
#
#' [+] Transform dataset "PAP_RK_2014" and add labels.
#'
#' Function is designed to transform and label data set called "PAP_RK_2014".
#'
#' Select data columns, that are not removed, and add labels to
#'  \code{\link[=hyperSpec-class]{hyperSpec}} object of "PAP_RK_2014" data.
#'
#'
#' @param sp PAP_RK_2014 data, created by function \code{\link{read3csv2hy}} as a
#'           \code{\link[=hyperSpec-class]{hyperSpec}} object of .
#' @param language A string, indicating a language of labels. Possible
#' entries are \code{EN} - English and \code{LT} - Lithuanian.
#' Default is \code{EN}.
#'
#' @return A transformed and labeled \code{hyperSpec} object.
#'
#' @export
#'
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna
#'
hyAdd_Labels_PAP_RK_2014 <- function(sp, language = "EN")  {
    warning("Function is not complete yet.")

    ColsInitial <- colnames(sp) # save initial column names

    data <- sp$..

    data$Time <- strptime(paste(data$Date, data$Time), "%m-%d-%Y %H:%M:%S")
    data$Date <- as.Date(data$Date,format = "%m-%d-%Y")

    # # Only necessary columns are selected:
    # Dates       <- as.character(data$Date)
    # no_timezone <- paste(substr(Dates, 1, 19), substr(Dates, nchar(Dates) - 3, nchar(Dates)))
    # data$Date   <- strptime(no_timezone,format = "%a %b %d %H:%M:%S %Y", tz = 'Europe/Vilnius')

    data$birth_date <- as.Date(data$Gimimo_data, format = "%Y.%m.%d")
    data$age <- 2014 - lubridate::year(data$birth_date)

    data <- data %>%
        dplyr::mutate(ID          = as.factor(ID),
                      spID        = as.factor(gsub('_{1,3}','|', spID)),
                      point       = taskas,
                      exp_code    = tyrimo_kodas,
                      sp_type     = tyrimas,
                      fileName    = file_name_with_path,

                      excitation  = as.factor(Zadinimas),
                      ZPV_ar      = ZPV_kiti_onkogeniniai,
                      CitoGr      = CitGr_2015,
                      HistGr      = HistGr_2015,
                      t.int       = Integration_time/1000,
                      t.int.units = as.factor(paste0("1000*", Integration_time_Units))
        ) %>%

        dplyr::select(ID,
                      # ID2,
                      spID,
                      point,
                      exp_code,
                      sp_type,
                      fileName,

                      Date,
                      Time,
                      # Electric_dark_correction,
                      # Boxcar_width,
                      t.int,
                      t.int.units,
                      excitation,

                      Dziov,
                      birth_date,
                      age,

                      CitoGr,
                      HistGr,
                      ZPV_16,
                      ZPV_18,
                      ZPV_ar,
                      p16_Ki67
        )

    Object <- new('hyperSpec', spc = sp$spc, wavelength = wl(sp), data = data)


#  ------------------------------------------------------------------------

	# Reorder levels correctly
    Object$CitoGr <- factor(Object$CitoGr,
    	               levels = c("IPPN", "LSIL", "HSIL"),
    	               labels = c("IPPN", "LSIL", "HSIL"))

    # Object$HistGr <- factor(Object$HistGr,
    #                    levels = c("Cervicitas", "CIN1", "CIN2", "CIN3+"),
    #                    labels = c("Cervicitas", "CIN1", "CIN2", "CIN3/CIS"))
suppressWarnings({
    Object$HistGr <- factor(Object$HistGr,
                            levels = c("Norma", "Cervicitas", "CIN 1", "CIN 2", "CIN 3",     "CIN 3/CIS", "Karcinoma", "Karcinoma G3"),
                            labels = c("Normal","Cervicitis", "CIN1",  "CIN2",  "CIN3/CIS",   "CIN3/CIS",    "Cancer", "Cancer"   ))
    Object$HistGr <- droplevels(Object$HistGr)
})
    Object$ZPV_16   <- factor(Object$ZPV_16,   levels = c("-", "+"), labels = c("HPV 16(-)", "HPV 16(+)" ))
    Object$ZPV_18   <- factor(Object$ZPV_18,   levels = c("-", "+"), labels = c("HPV 18(-)", "HPV 18(+)" ))
    Object$ZPV_ar   <- factor(Object$ZPV_ar,   levels = c("-", "+"), labels = c("HPV high risk(-)", "HPV high risk(+)" ))

    Object$p16_Ki67 <- factor(Object$p16_Ki67, levels = c("-", "+"), labels = c("p16/Ki67(-)", "p16/Ki67(+)" ))

    Object$Dziov <- factor(Object$Dziov, levels = c("slap", "saus"), labels = c("Wet", "Dry" ))

    levels(Object$sp_type)[levels(Object$sp_type)=="Fl"] <- "Fluorescence"

# add Labels ------------------------------------------------------------
    Var.Names <- colnames(Object)

    Var.Labels <- switch(language,
                         #            LT =   c(
                         #                "Zadinimo spinduliuote, nm",
                         #                "Zadinimo spinduliuote, nm",
                         #                "Meginio ID",
                         #                "Meginio ID (2014)",
                         #                "Meginio ID (2015)",
                         #                "Spektro ID",
                         #                "Tasko numeris meginy",
                         #                "Laikas, s",
                         #                "Bylos aplankas",
                         #                "Bylos pavadinimas",
                         #                "Spektru registravimo data",
                         #                "Tyrejas",
                         #                "Integracijos laikas, s",
                         #                "Integracijos laikas (perskaiciuotas), s",
                         #                "Electric_dark_correction",
                         #                "Nonlinearity_correction",
                         #                "Meginiu partija",
                         #                "Gimimo data",
                         #                "Hibridine dignoze 2014",
                         #                "Citologine diagnoze 2014",
                         #                "Citologine diagnoze 2015",
                         #                "Histologine diagnoze 2014",
                         #                "Histologine diagnoze 2015",
                         #                "ZPV 16",
                         #                "ZPV 18",
                         #                "ZPV kiti onkogeniniai",
                         #                "p16/Ki67",
                         #                "Informacijos dimensija",
                         #                "InfoDim_2",
                         #                "Spalva",
                         #                "Meginio kiekis",
                         #                "Vienalytis",
                         #                "Nuosedu tipas",
                         #                "pastabos 2014",
                         #                "eil_nr_2015",
                         #                "I, sant.vnt."),
                         #
                         EN =   c( "Specimen ID",
                                   # "Specimen ID2",
                                   "Spectrum ID" ,
                                   "Point number in a specimen",
                                   "Code of Experiment",
                                   "Type of Spectroscopy",
                                   "File name",
                                   "Date (Spectometer)",
                                   "Time When Registration Began",
                                   # "Boxcar width",
                                   # "Electric dark correction",

                                   "Integration time, s",
                                   "Units of Integration time",
                                   "Excitation wavelength, nm",
                                   "Consistency of material",
                                   "Date of birth",
                                   "Age",

                                   "Cytological groups",
                                   "Histological groups",
                                   # "Hybrid groups",
                                   "HPV 16 test",
                                   "HPV 18 test",
                                   "High risk HPV test",
                                   "p16/Ki67 expression",

                                   "I, units"),

                         stop("The value of `language` is not supported."))

    labels(Object)[Var.Names] <- Var.Labels
    # ------------------------------------------------------------------------
    labels(Object, "spc") <- "I, units"

    # x axis labels
    # Object <- hyAdd_Label_wl(Object, "wavelength")
    Object <- hyAdd_Label_wl(Object, label =  "Wavelength, nm")

    # ---------------------------------------------------------------------
    # ----------------------------------------------------------------------
    # Add `.color`: variable with colors
    # Object <- hyAdd_color(Object, "HistGr")
	# ---------------------------------------------------------------------
    # CHECK if any columns were added or deleted
    ColsFinal   <- colnames(Object)
    message("These columns were:")
    print(list_AddRm(ColsInitial, ColsFinal))
    # ---------------------------------------------------------------------

    return(Object)
}
