
#' @docType package
#' @name spHelper
#' @title Extension for `hyperSpec` and convenience functions
#'
#'
#'
#' @description
#' [+] - function is well described. \cr
#'
#' [!] - a description is incomplete and needs revision.\cr\cr
#'
#' Functions in spHelper by topic
#'
#' @section Cross-validation:
#'
#' \code{\link{stratifiedFolds}} \cr
#'
#' @section Component analysis / Spectroscopy:
#'
#' \code{\link{sortLoadings}} \cr
#' \code{\link{GaussAmp}} \cr
#' \code{\link{getScores}} \cr
#' \code{\link{infoDim}} \cr
#' \code{\link{qplot_infoDim}} \cr
#' \code{\link{whichOutlier}} \cr
#'
#'
#' @section Spectroscopy / \code{\link[=hyperSpec-class]{hyperSpec}}:
#'
#' \code{\link{hy2mat}} \cr
#' \code{\link{read3csv2hy}} \cr
#'
#' \code{\link{hyAdd_Labels_TD2009}} \cr
#'
#'
#' @section Plotting:
#'
#' \code{\link{qplot_kAmp}} \cr
#' \code{\link{qplot_kSp}} \cr
#' \code{\link{qplot_kSpFacets}} \cr
#' \code{\link{qplot_confusion}}  \cr
#' \code{\link{qplot_spStat}}  \cr
#' \code{\link{plot_spDiff}}    \cr
#'
#'
#'
#' \code{\link{subt}}          \cr
#' \code{\link{qplot_infoDim}} \cr
#' \code{\link{reconstructSp}} \cr
#'
#'
#' @section Regular expressions:
#'
#' \code{\link{regcapturedmatches}} \cr
#' \code{\link{regexp2df}} \cr
#'
#'
#' @section Various:
#'
#'
#' \code{\link{bru}} \cr
#' \code{\link{fCap}} \cr
#' \code{\link{make_firstCapitals}} \cr
#' \code{\link{list_functions}} \cr
#' \code{\link{hyDrop_NA}} \cr
#'
#'
#' @author Vilmantas Gegzna
#'
#' @examples
#'
#' \donttest{
#' \dontrun{
#'
#' # List all functions in package:
#'
#' list_functions()
#'
#' # Plot structure of functions inside the package:
#' require(sna)
#' require(mvbutils)
#'
#' pkgFW <- mvbutils::foodweb(where="package:spHelper", cex=0.7, charlim=60)
#' sna::gplot(pkgFW$funmat, g = 9,
#'            jitter = T,
#'            # mode = "mds",
#'            label.cex = .6,
#'            diag=TRUE,
#'            vertex.cex=1:2,
#'            displaylabels=TRUE,
#'            label.bg="gray90")
#'
#'
#' # Other things to remember
#'
#'
#'  devtools::build_vignettes()
#'
#' plotc(ObjectName[,,500],model = spc~Integration_time)
#' }}
#'
#' @importFrom magrittr '%>%'
#' @importFrom spMisc regexp2df
#' @importFrom checkmate assert_choice assert_numeric assert_vector
#'                       assert_factor assert_flag assert_character
#'                       assert_set_equal assert_string assert_subset
#'                       assert_class
#' @import parallelMap
NULL
#> NULL

# \code{\link{}} \cr
# \code{\link{}} \cr
# \code{\link{}} \cr
# \code{\link{}} \cr
# \code{\link{}} \cr
# \code{\link{}} \cr
# \code{\link{}} \cr
# \code{\link{}} \cr
# @importFrom tidyr '%>%'

eval_glue <- spMisc::eval_glue
