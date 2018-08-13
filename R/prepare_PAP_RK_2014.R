# prepare_... --------------------------------------------------------
#
#' [+] Transform dataset "PAP_RK_2014" and add labels
#'
#' Function is designed to transform data set (hyperSpec object) called
#' "PAP_RK_2014" and labels to it.
#'
#' Select data columns, that are not removed, and add labels to
#'  \code{\link[=hyperSpec-class]{hyperSpec}} object of "PAP_RK_2014" data.
#'
#'
#' @param sp PAP_RK_2014 data, created by function \code{\link{read3csv2hy}} as a
#'           \code{\link[=hyperSpec-class]{hyperSpec}} object.
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
#' @examples
#'
#' # prepare_PAP_RK_2014(sp)
#'

prepare_PAP_RK_2014__MATLAB_failui <- function(sp, language = "EN")  {

    ColsInitial <- colnames(sp) # save initial column names

    data <- sp$..

    # ==========================================================================
    # Transform, create variables and select variables

    # ` dplyr::mutate` does not support `POSIXlt` results
    data$Time       <- strptime(paste(data$Date, data$Time),
                                "%m-%d-%Y %H:%M:%S")  %>% as.POSIXct()
    data$Date       <- as.Date(data$Date, format = "%m-%d-%Y")
    data$birth_date <- as.Date(data$Gimimo_data, format = "%Y.%m.%d")
    data$age        <- 2014 - lubridate::year(data$birth_date)
    data$age_gr     <- cut(data$age,
                           breaks = c(1,35,200),
                           labels = c("Under 35 y.o.","Above 35 y.o."))

    #  ------------------------------------------------------------------------

    # Transform with dplyr ----------------------------------------------------
    data <- data %>%
        dplyr::mutate(ID            = as.factor(ID),
                      ID_nr         = as.factor(sprintf("No.%02g", as.numeric(ID))),
                      spID          = as.factor(gsub('_{1,3}','|', spID)),
                      point         = taskas,
                      Investigation = tyrimo_kodas,
                      Spectroscopy  = plyr::revalue(tyrimas, c("Fl" = "Fluorescence")),
                      fileName      = file_name_with_path,

                      Batch    = factor(Meg_partija,
                                        levels = c("2014","2014 ir 2015", "2015"),
                                        labels = c("2014 only","2014 & 2015", "2015 only")),



                      Excitation    = as.factor(Zadinimas),
                      t.int         = Integration_time/1000,
                      t.int.units   = as.factor(paste0("1000*", Integration_time_Units)),

                      Consistency   = factor(Dziov, levels = c("slap", "saus"), labels = c("Wet", "Dry")),
                      Material = factor(MegDalis, levels = c("N", "S"), labels = c("Sediment", "Supernatant")),

                      Color = factor(Spalva,
                                     ordered = TRUE,
                                     levels = c("1 balta",
                                                "2 balsva",
                                                "3 gelsva",
                                                "4 geltona",
                                                "7 ruda"),
                                     labels = c("Whitish",
                                                "Whitish to Yellowish",
                                                "Yellowish to yellow",
                                                "Yellow",
                                                "Dark red to brownish")),

                      Amount =  factor(Meginio_kiekis,
                                       ordered = TRUE,
                                       levels = c("0 nėra", "1 labai mažai","2 pakankamai","3 daug"),
                                       labels = c("None","Extremely small","Enough","Large")),

                      Sediment_size = factor(Nuosedu_tipas,
                                             ordered = TRUE,
                                             levels = c("smulkios","labiau smulkios", "labiau stambios", "stambios"),
                                             labels = c("small", "more small than large", "more large than small", "large")),

                      # Medical and patient related
                      HPV_16   = factor(ZPV_16,ordered = TRUE,  levels = c("-", "+"), labels = c("HPV 16(-)", "HPV 16(+)" )),
                      HPV_18   = factor(ZPV_18,ordered = TRUE,  levels = c("-", "+"), labels = c("HPV 18(-)", "HPV 18(+)" )),
                      HPV_hr   = factor(ZPV_kiti_onkogeniniai,
                                        ordered = TRUE,
                                        levels = c("-", "+"), labels = c("HPV high risk(-)", "HPV high risk(+)" )),
                      p16_Ki67 = factor(p16_Ki67,ordered = TRUE,levels = c("-", "+"), labels = c("p16/Ki67(-)", "p16/Ki67(+)" )),

                      CitoGr   = factor(CitGr_2015,
                                        ordered = TRUE,
                                        levels = c("IPPN", "LSIL", "HSIL"),
                                        labels = c("IPPN", "LSIL", "HSIL")),

                      HistGr   = factor(HistGr_2015,
                                        ordered = TRUE,
                                        levels = c("Norma", "Cervicitas", "CIN 1", "CIN 2", "CIN 3", "CIN 3/CIS", "Karcinoma", "Karcinoma G3"),
                                        labels = c("Normal","Cervicitis", "CIN1",  "CIN2",  "CIN3",  "CIN3/CIS",   "Cancer", "Cancer(G3)")
                      )
        ) %>%

        dplyr::select(ID,
                      ID_nr,
                      # ID2,
                      ID_2014,
                      ID_2015,
                      spID,
                      point,
                      Investigation,
                      Batch, # meginiu partija
                      Spectroscopy,
                      fileName,

                      Date,
                      Time,
                      # Electric_dark_correction,
                      # Boxcar_width,
                      t.int,
                      t.int.units,
                      Excitation,

                      birth_date,
                      age,
                      age_gr,

                      CitGr_2014,
                      CitoGr,
                      HistGr_2014,
                      HistGr,
                      HPV_16,
                      HPV_18,
                      HPV_hr,
                      p16_Ki67,

                      Consistency,
                      Material,
                      Color,
                      Amount,
                      Sediment_size
        )
    # ==========================================================================
    # Create HyperSpec objectobjects()
    Object <- new('hyperSpec', spc = sp$spc, wavelength = wl(sp), data = data)

    # ==========================================================================
    # add Labels ------------------------------------------------------------

    Var.Labels <- switch(language,
                         EN = list(
                             # Identifications
                             ID             =   "ID of Specimen",
                             ID_nr          =   "Medical sample",
                             ID2            =   "ID2 of Specimen",
                             spID           =   "ID of Spectrum" ,
                             point          =   "ID of Point in Specimen",
                             Investigation  =   "Code of Experiment",
                             fileName       =   "File name",
                             Batch     =   "Batch of Samples",

                             # Spectroscopy related parameters
                             Spectroscopy     =   "Type of Spectroscopy",
                             excitation  =   "Excitation wavelength, nm",

                             # Spectrometer related parameters
                             Date        =   "Date", # (from Spectometer)
                             Time        =   "Start of Registration",
                             Boxcar_width=   "Boxcar width",
                             Electric_dark_correction   =    "Electric dark correction",
                             t.int       =   "Integration time, s",
                             t.int.units =   "Units of Integration time",

                             # Properties of specimens
                             Consistency =   "Consistency of material",
                             Material    =   "Material",
                             Color       =   "Approximate color",
                             Amount      =   "Amount of sample before investigation",
                             Sediment_size =   "Size of sediment",

                             # Meddical and patient related information
                             birth_date  =   "Date of birth",
                             age         =   "Age on 1 January, 2014",
                             age_gr      =   "Age groups",
                             CitoGr_2014 =   "Cytological groups (2014)",
                             CitoGr      =   "Cytological groups",
                             HistGr_2014 =   "Histological groups (2014)",
                             HistGr      =   "Histological groups",
                             HybridGr    =   "Hybrid groups",
                             HPV_16      =   "HPV 16 test",
                             HPV_18      =   "HPV 18 test",
                             HPV_hr      =   "Test of other high risk HPV ",
                             p16_Ki67    =   "p16/Ki67 expression",

                             # Intensity axis
                             spc   =   "I, units",

                             # Wavelength axis
                             .wavelength = "Wavelength, nm"
                         ),

                         # Otherwise
                         stop(paste("Selected `language` is not supported:",language))
    )

    labels(Object) <- Var.Labels
    # ---------------------------------------------------------------------

    # ---------------------------------------------------------------------
    # CHECK if any columns were added or removed
    ColsFinal   <- colnames(Object)
    message("These columns were:")
    print(list_AddRm(ColsInitial, ColsFinal))
    # ---------------------------------------------------------------------

    return(Object)
}








#  ------------------------------------------------------------------------
# Removel lines -----------------------------------------------------------
#  ------------------------------------------------------------------------

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


#  ------------------------------------------------------------------------

# Reorder levels correctly
# Object$CitoGr <- factor(Object$CitoGr,
# 	               levels = c("IPPN", "LSIL", "HSIL"),
# 	               labels = c("IPPN", "LSIL", "HSIL"))

# Object$HistGr <- factor(Object$HistGr,
#                    levels = c("Cervicitas", "CIN1", "CIN2", "CIN3+"),
#                    labels = c("Cervicitas", "CIN1", "CIN2", "CIN3/CIS"))
# suppressWarnings({
#     Object$HistGr <-  factor(Object$HistGr,
#                             levels = c("Norma", "Cervicitas", "CIN 1", "CIN 2", "CIN 3",     "CIN 3/CIS", "Karcinoma", "Karcinoma G3"),
#                             labels = c("Normal","Cervicitis", "CIN1",  "CIN2",  "CIN3/CIS",   "CIN3/CIS",    "Cancer", "Cancer"   )) %>% droplevels
# })
#
# HistGr        = droplevels(plyr::revalue(HistGr_2015,
#                                          replace =  c(
#                             "Norma"        = "Normal",
#                             "Cervicitas"   = "Cervicitis",
#                             "CIN 1"        = "CIN1",
#                             "CIN 2"        = "CIN2",
#                             "CIN 3"        = "CIN3",
#                             "CIN 3/CIS"    = "CIN3/CIS",
#                             "Karcinoma"    = "Cancer",
#                             "Karcinoma G3" = "Cancer(G3)"))),

# levels(Object$Spectroscopy)[levels(Object$Spectroscopy)=="Fl"] <- "Fluorescence"




