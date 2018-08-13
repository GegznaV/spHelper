#' @name DataSet1
#'
#' @title [+] Dataset: a data frame for illustrations
#'
#' @description A dataset containing example data with variables \code{ID}, \code{gr}
#' and \code{.row}.
#'
#' @format A data frame with 40 rows and 3 variables:
#' \describe{
#'   \item{ID}{An identification code.}
#'   \item{gr}{A factor variable with 4 groups.}
#'   \item{.row}{A row number.}
#' }
#' @source Artificially generated in R.
#' @author Vilmantas Gegzna
#' @examples
#'    DataSet1 <- data.frame(ID = gl(n = 20, k = 2),
#'                          gr = gl(n = 4, labels = LETTERS[1:4], k = 10))
#'    DataSet1$.row <- 1:nrow(DataSet1)
#'
#'    # devtools::use_data(DataSet1)
"DataSet1"


# Datasets-Loadings -------------------------------------------------------
#' @name DataSets-sp
#'
#' @title [+] Datasets of simulated spectroscopic data
#' @description Datasets that simulate spectroscopic data (\code{Spectra..}
#' series - a mixture of spectral components)
#' and spectroscopic decomposition data (\code{Loadings..} series - spectra
#' of spectroscopic components and \code{Scores..} series - amplitudes
#' of spectroscopic componens).
#' @format \code{\link[=hyperSpec-class]{hyperSpec}} objects with spectroscopic
#' data and additional variables.
#'
#' Additional variables for \code{Spectra..} and \code{Scores..}:
#'
#' \describe{
#'   \item{class}{A factor variable with 4 classes.}
#'   \item{gr}{A factor variable with 3 classes.}
#' }
#'
#' Additional variables for \code{Loadings..}:
#' \describe{
#'   \item{PeakAt}{Position of components top peak.}
#'   \item{cNames}{Names of components.}
#'   \item{order.of.rows}{Original order of components before sorting.}
#' }
#' @source Artificially generated in R.
#' @author Vilmantas Gegzna

# Datasets Loadings -------------------------------------------------------
#' @rdname Datasets-sp
'Loadings2'
#' @rdname Datasets-sp
'Loadings3'
#' @rdname Datasets-sp
'Loadings4'

# Datasets-Scores ---------------------------------------------------------
#' @rdname Datasets-sp
'Scores2'
#' @rdname Datasets-sp
'Scores3'
#' @rdname Datasets-sp
'Scores4'

# Datasets-Spectra --------------------------------------------------------
#' @rdname Datasets-sp
'Spectra2'
#' @rdname Datasets-sp
'Spectra3'
#' @rdname Datasets-sp
'Spectra4'


# Datasets-Loadings -------------------------------------------------------
#' @name palette_PAP
#'
#' @title [+] Color palette
#' @description Color palette, witch is \code{RColorBrewer::brewer.pal(8, "Set1")},
#' where colors go in a different order.
#'
#' @details
#' \code{palette_PAP <- c("#4DAF4A", "#377EB8", "#F781BF","#FF7F00",  "#E41A1C", "#A65628", "#984EA3",  "#FFFF33")}
"palette_PAP"
