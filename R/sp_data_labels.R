# sp_data_labels ===============================================================
#
#' Lables for variables derived from spectroscopic data files
#'
#' @return A named list with labels
#' @export
sp_data_labels <- function(){
    LABELS <- list(

        Data_from                = 'Data from',
        Date                     = 'Date',
        User                     = 'User',
        Spectrometer             = 'Spectrometer',
        Autoset_integration_time = 'Autoset integration time',
        Trigger_mode             = 'Trigger mode',
        Integration_time         = 'Integration time',
        t.int                    = 'Integration time, s',
        t_int                    = 'Integration time, s',
        Integration_time_Units   = 'Units of integration time',
        t.int.units              = 'Units of Integration time',
        t_int_units              = 'Units of Integration time',
        Scans_averaged           = 'Number of scans averaged',
        Electric_dark_correction = 'Electric dark correction',
        Nonlinearity_correction  = 'Nonlinearity correction',
        Boxcar_width             = 'Boxcar width',
        XAxis_mode               = 'X axis mode',
        Stop_averaging           = 'Stop averaging',
        n_pixels_in_spectrum     = 'Number of Pixels in Spectrum',

        t                   = 't, s',
        datetime            = 'Date and Time',

        software_info       = 'Software information',
        file_format         = 'File format',
        file_name           = 'File name',
        FileName            = 'File name',
        last_headerline     = 'Number of the last headerline',

        Time                       = 'Time',
        Spectrometer_Serial_number = 'Serial number of spectrometer',
        Spectrometer_Channel       = 'Spectrometer Channel',
        Electric_dark_correction   = 'Correct for Electrical Dark',
        Time_Normalized            = 'Time Normalized',
        Dual_beam_Reference        = 'Dual-beam Reference',
        Reference_Channel          = 'Reference Channel',
        Temperature                = 'Temperature',
        Spectrometer_Type          = 'Type of Spectrometer',
        ADC_Type                   = 'ADC Type',
        Graph_Title                = 'Title of Spectrum',

        # For medical and other information

        # Identifications
        nr             =   "Absolute index of a spectrum",
        sp_nr          =   "Index of a spectrum",
        ID             =   "ID of Specimen",
        ID_nr          =   "Medical sample",
        ID2            =   "ID2 of Specimen",
        spID           =   "ID of Spectrum" ,
        point          =   "ID of Point in Specimen",
        Investigation  =   "Code of Experiment",
        FileName       =   "File name",
        Batch          =   "Batch of Samples",

        # Spectroscopy related parameters
        Spectroscopy   =   "Type of Spectroscopy",
        excitation     =   "Excitation wavelength, nm",

        # Properties of specimens
        Consistency    =   "Consistency of material",
        Material       =   "Material",
        Color          =   "Approximate color",
        Amount         =   "Amount of sample before investigation",
        Sediment_size  =   "Size of sediment",

        # Meddical and patient related information
        birth_date     =   "Date of birth",
        age_gr         =   "Age groups",
        CitoGr_2014    =   "Cytological groups (2014)",
        CitoGr_2015    =   "Cytological groups (2015)",
        CitoGr         =   "Cytological groups",
        HistGr_2014    =   "Histological groups (2014)",
        HistGr_2015    =   "Histological groups (2015)",
        HistGr         =   "Histological groups",
        HistGr0        =   "Histological groups",
        HistGr1        =   "Histology-based groups",
        HistGr2        =   "Histology-based groups",
        HistGr3        =   "Histology-based groups",
        HistGr4        =   "Histology-based groups",
        HistGr5        =   "Histology-based groups",
        HistGr6        =   "Histology-based groups",
        HistGr7        =   "Histology-based groups",
        HistGr8        =   "Histology-based groups",
        HistGr9        =   "Histology-based groups",
        HPV_16         =   "HPV test",
        HPV_16         =   "HPV 16 test",
        HPV_18         =   "HPV 18 test",
        HPV_hr         =   "HPV test for other high risk types",
        p16_Ki67       =   "p16/Ki67 expression",
        HybridGr       =   "Hybrid groups",
        Hybrid0        =   "Histology and Cytology based groups",
        Hybrid         =   "Histology and Cytology based groups",
        Hybrid_normal     = "Normal vs. others",
        Hybrid_cervicitis = "Cervicitis vs. others",
        Hybrid_cin        = "CIN vs. others",
        Hybrid_cin2_plus  = "CIN2+ vs. others",
        Hybrid_cin3_plus  = "CIN3+ vs. others",
        Hybrid_cin3_cis   = "CIN3/CIS vs. others"
    )
    return(LABELS)
}
