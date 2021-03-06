#' [!] Calculate common center subtracted group centers of spectroscopic data
#'
#' Function calculates common center subtracted group centers of spectroscopic
#' data.
#' In other words, function calculates centers (i.e., means, medians) for each
#' indicated group. Then common center (e.g., a center of all data,
#' a balanced center of all data, a center of certain group or a known spectrum)
#' is subtracted from group centers.
#'
#' @details In context of this function, a \emph{center} is a mean, a median or similar
#' statistic calculated at each wavelangth.
#'
#'
#' @template sp-hy
#' @param by Name of grouping variable.
#' @param FUN Function (default is \code{median}), that calculates center
#' tendencies (CT, e.g., means, medians or similar) for each group. \cr
#' This function calculates CT for each group and CT that is common
#' for whole dataset.
#'
#' @param Center One of the following options to select the common center
#'  tendency (CCT) that will be subtracted from other CTs:\cr
#'      - \code{NULL} (default) or \code{'.All'} to use CT that is common for
#'          whole dataset (e.g. a median of whole dataset);\cr
#'      - numerical vector of length \code{nwl(sp)} to be uses as known/calculated
#'          spectrum;\cr
#'      - string with one of the levels of grouping variable
#'          (available levels: \code{levels(sp$..[,by])}) to use CT of that
#'          level. \cr
#'         \cr
#'      If \code{balanced == TRUE}, values of \code{Center} are ignored and
#'      bananced CT is used.
#'
#' @param balanced Logical. If \code{TRUE} \emph{balanced} center tendency
#'        will be used as a common CT. "Balanced" means that CCT is
#'        a CT (e.g. mean) of group CTs (e.g. "balanced" may be a
#'        mean of group means, a mean of group medians,
#'        a median of group medians, etc.)
#'
#' @param show.all logical. If \code{TRUE}, row ".All" with CT common
#'        for all data will be added. Default is \code{FALSE}.
#'
#' @param show.balanced logical. If \code{TRUE}, row ".All (balanced)" with
#'        balanced central tendency will be added. Default is \code{FALSE}.
#'
#' @return \code{hyperSpec} object with common center tendency subtracted center
#'  tendencies (i.e. spactra) of each group  (e.g. mean-subtracted group means,
#'  median-subtracted medians etc.).
#'
#' @export
#'
#' @author Vilmantas Gėgžna
#' @examples
#' library(spHelper)
#'
#'
#' # === Common center of all spectra as the subtracted center ================
#'
#' CSCs <- center_subtracted_centers(sp = Spectra2, by = "gr")
#'
#' # ggplot2 type plot --------------------------------------------------------
#' qplot_sp(CSCs, names.in = "gr") + ggtitle("CSCs - center subtracted centers")
#'
#' # R base type plot ---------------------------------------------------------
#' names <- CSCs$gr
#' plot(CSCs, col = names)
#' legend("topright", lty = 1, col = names, legend = names, bty = "n")
#' title("CSCs - center subtracted centers")
#'
#'
#' # === Center of a certain group as the subtracted center ===================
#'
#' center_subtracted_centers(Spectra2, "gr", Center = "A")  %>%
#'     qplot_sp(names.in = "gr") +
#'     ggtitle("Subtraced center is center of group 'A'")
#'
#' center_subtracted_centers(Spectra2, "gr", Center = "C")  %>%
#'     qplot_sp(names.in = "gr")+
#'     ggtitle("Subtraced center is center of group 'C'")
#'
#'
#'
#' # === Balanced center as the subtracted center =============================
#'
#' center_subtracted_centers(Spectra2, "gr", balanced = TRUE)  %>%
#'     qplot_sp(names.in = "gr")+
#'     ggtitle(subt("Balanced center subtraced centers (BCSCs)",
#'                  "Balanced = a mean of all group centers\n" %++%
#'                      "Balanced center is mean"))
#'
#'
#' center_subtracted_centers(Spectra2, "gr",
#'                           balanced = TRUE,
#'                           balance.FUN = median)  %>%
#'     qplot_sp(names.in = "gr") +
#'     ggtitle(subt("Balanced center subtraced centers (BCSCs)",
#'                  "Balanced center is median"))
#'
#'
#' # === Scaled data ==========================================================
#'
#'
#' MED <- apply(Spectra2,2,median)
#' MAD <- apply(Spectra2,2,mad)   # median absolute deviation
#' scale(Spectra2,center = MED, scale = MAD)  %>%
#'     center_subtracted_centers(by = "gr")  %>%
#'     qplot_sp(names.in = "gr") +
#'     ggtitle(subt("CSCs of scaled data","Scaling: x = (x-median)/MAD"))
#'
#'
#' scale(Spectra2,center = MED, scale = MAD)  %>%
#'     center_subtracted_centers(by = "gr",
#'                               balanced = TRUE,
#'                               balance.FUN = median)  %>%
#'     qplot_sp(names.in = "gr") +
#'     ggtitle(subt("Balanced median SCs of scaled data",
#'                  "Scaling: x = (x-median)/MAD"))
#'
#' # === Add curves of common & balanced central tendencies =================
#'
#' center_subtracted_centers(Spectra2, "gr",
#'                           show.balanced = TRUE,
#'                           show.all = TRUE)  %>%
#'     qplot_sp(names.in = "gr") +
#'     ggtitle(subt("Curves of common & balanced centers added",
#'                  "Imbalanced center subtracted centers"))
#'
#'
#' center_subtracted_centers(Spectra2, "gr",
#'                           balanced = TRUE,
#'                           show.balanced = TRUE,
#'                           show.all = TRUE)  %>%
#'     qplot_sp(names.in = "gr")+
#'     ggtitle(subt("Curves of common & balanced centers added",
#'                  "Balanced center subtracted centers"))
#'
#'
#' center_subtracted_centers(Spectra2, "gr", Center = "C",
#'                           show.balanced = TRUE,
#'                           show.all = TRUE)  %>%
#'     qplot_sp(names.in = "gr")+
#'     ggtitle(subt("Curves of common & balanced centers added",
#'                  "Group 'C' center subtracted centers"))
#'


center_subtracted_centers <- function(sp,
                                      by = stop("Argument 'by' is missing."),
                                      FUN = median,
                                      Center   = NULL,
                                      balanced = FALSE,
                                      balance.FUN = mean,
                                      show.all = FALSE,
                                      show.balanced = FALSE) {

    if (!is.character(by)) by <- as.character(by)
    if (length(by) > 1)
    {
        warning("length(by) > 1. Only the first value of `by` will be used.")
        by <- by[1]
    }

    # Save original labels
    Labels <- labels(sp)

    # Process data
    DataByGr <- eval_(sprintf("spStat(sp, by = '%s', FUN = FUN)",  by))

    # Parse variable "Center" (1)
    if (!is.null(Center))
    {
        cLen <- length(Center)
        if (cLen %!in% c(1,nwl(DataByGr)))
            stop("Length of variable 'Center' must be either 1 or " %++% nwl(DataByGr))

        # If center is given as a vector
        if (cLen == nwl(DataByGr)) Center <- as.vector(Center)

        # If center is given as a variable name

        if (cLen == 1)
        {   if (!is.character(Center))
                stop("Length of variable 'Center' is 1, but it is not a string.")

            if (Center %in% (eval_glue("DataByGr$`{by}`") %>% levels)) {
                center_by <- Center
            } else {# If Center is given incorrectly
                stop("Variable '" %++% by %++% "' is not found in colnames(sp).")
            }
        }
    } else {
        center_by <- '.All' # default value for center_by
    }

    # Calcultate balanced center
    if (balanced | show.balanced){

        Balanced_center <- eval_(sprintf("subset(DataByGr,  %s != '.All')", by)) %>%
                          apply(., 2, balance.FUN)
        # Annotate balanced center
        level <- as.factor(".All (balanced)")
        Balanced_center$.aggregate <- level
        eval_glue('Balanced_center$`{by}` <- level')
    }

    # Parse variable "Center" (2): calculate center of centers
    if (length(Center) <= 1)
    {
        if (balanced == FALSE) {
            Center  <- eval_(sprintf("subset(DataByGr, %s == '%s')",
                                     by, center_by))
        } else {
            Center <- Balanced_center
        }
    }

    # Subtract centers
    Data <- DataByGr - Center

    # Use original labels
    labels(Data) <- Labels

    # Remove column ".All"
    if (show.all == FALSE){
        Data <- subset(Data, .aggregate != ".All")
    }


    # Remove column ".All"
    if (show.balanced == TRUE){
        Data <- rbind(Data, Balanced_center-Center)
    }

    # # Drop unused levels
    # Data$.aggregate %<>% droplevels()
    # eval_glue("Data$`{by}` %<>% droplevels()")

    # Return result of function
    return(Data)
}


# plot(Center1)
# Data    <- Data - Center
#
# names <- eval_glue("Data$`{by}`)
# plot(Data, col = names)
# legend("topright", lty = 1, col = names, legend = names, bty = "n")
#
# p <- qplot_sp(Data, facets = F,
#               yLabel = "Differences",
#               names.in = by)
# return(p)

