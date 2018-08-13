# hyAdd_Labels_TD2009 --------------------------------------------------------
#
#' [+] Add labels to "TD_2009" dataset
#'
#' Function is designed to label data collected during investigation called "TD_2009".
#' Select data columns, that are not removed, and add labels to
#'  \code{\link[=hyperSpec-class]{hyperSpec}} object of "TD_2009" data.
#'
#' @param sp A \code{\link[=hyperSpec-class]{hyperSpec}} object of TD_2009 data,
#'             created by function \code{\link{read3csv2hy}}.
#' @param language A string, indicating a language of labels. Possible entries are
#' \code{EN} - English and \code{LT} - Lithuanian. Default is \code{EN}.
#'
#' @return A labeled object with reduced number of data columns.
#'
#' @export
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna
#'
hyAdd_Labels_TD2009 <- function(sp,language = "EN")  {
    ColsInitial <- colnames(sp) # save initial column names

    data         = sp$..

    # Only required columns are sellected:
    data <- data %>%
        dplyr::mutate(fileName = file_name_with_path,
                      point      = taskas,
                      sp_type    = tyrimas,
                      exp_code   = tyrimo_kodas,
                      spID       = as.factor(gsub('_{1,3}','|', spID))
        ) %>%
        dplyr::select(ID,
                      spID,
                      point,
                      exp_code,
                      sp_type,
                      fileName,
                      gr,
                      Boos,
                      Safranin,
                      coll_1,
                      coll_2,
                      coll_oth
        )

    Object <- new('hyperSpec', spc = sp$spc,  wavelength = wl(sp), data = data)

    # add Labels ------------------------------------------------------------
    Var.Names <- colnames(Object)

    Var.Labels <- switch(language,
             LT =    c("Meginio ID",
                      "Spektro ID",
                      "Tasko numeris meginy",
                      "Eksperimento kodas",
                      "Spektroskopijos tipas",
                      "Bylos pavadinimas",
                      "Meginio lokalizacija (S, P, D)",
                      "Boos indeksas",
                      "Safranin0 indeksas",
                      "Kolageno 1 kiekis, %",
                      "Kolageno 2 kiekis, % ",
                      "Kitu kolagenu kiekis, %",
                      "I, sant.vnt."    ),

             EN =   c("Specimen ID",
                      "Spectrum ID" ,
                      "Point number in a specimen",
                      "Code of experiment",
                      "Type of spectroscopy",
                      "File name",
                      "Localization of a specimen",
                      "Boos index",
                      "Safranin index",
                      "Collagen 1, %",
                      "Collagen 2, % ",
                      "Other collagens, %",
                      "I, units"),

             stop("The value of `language` is not supported."))


    labels(Object)[Var.Names] <- Var.Labels


    # x axis labels
    Object <- hyAdd_Label_wl(Object, "wavelength")

    # ----------------------------------------------------------------------


    # Define colors ------------------------------------------------------
    # Add `.color`: variable with colors
    Object <- hyAdd_color(Object, "gr", palette = c("#377EB8","#4DAF4A","#984EA3"))

    # ---------------------------------------------------------------------
    # CHECK if any columns were added or deleted
    ColsFinal   <- colnames(Object)
    message("These columns were:")
    print(list_AddRm(ColsInitial, ColsFinal))
    # ---------------------------------------------------------------------

    return(Object)
}
# @examples
# # hyAdd_Labels_TD2009(sp)
#
# ## hyperSpec object
# ## 1020 spectra
# ## 11 data columns
# ## 2048 data points / spectrum
# ## wavelength: paste(lambda, ", ", nm) [numeric] 348.37 348.66 ... 895.92
# ##  data:  (1020 rows x 11 columns)
# ##  1. ID: Spektro ID [factor] D1 D1 ... S9
# ##  2. spID: Meginio ID [factor] D1__1_a1 D1__1_a2 ... S9__4_c2
# ##  3. taskas: Tasko numeris meginy [factor] 1\\a1 1\\a2 ... 4\\c2
# ##  4. fileName: Bylos pavadinimas [factor]
# ##             \\TD_2009\\D\\D1\\1\\a1.txt
# ##             \\TD_2009\\D\\D1\\1\\a2.txt
# ##             ...
# ##             \\TD_2009\\S\\S9\\4\\c2.txt
# ##  5. gr: Grupavimas (S, P, D) [factor] D D ... S
# ##  6. Boos: Boos index [integer] 18 18 ... 21
# ##  7. Safranin: Safranin index [integer] 2 2 ... 2
# ##  8. coll_1: Collagen 1, % [integer] 0 0 ... 10
# ##  9. coll_2: collagen 2, %  [integer] 80 80 ... 40
# ##  10. coll_oth: Other collagens, % [integer] 20 20 ... 50
# ##  11. spc: I, sant.vnt. [matrix2048] 15422 17841 ... 211.7
#
#
# *************************************************************
