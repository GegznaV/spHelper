#' [+] Calculate summary statistic of spectroscopic data for all spectra and by groups
#'
#' Calculate summary statistic spectra in \code{hyperSpec} object for all
#' spectra and by levels of factor variable \code{by}.
#'
#' @note
#' This function is similar to \code{\link[hyperSpec]{aggregate}},
#' just calculates additional statistic for all spectra. This
#' statistic is labeled by adding level \code{.ALL}) in variable
#' \code{.aggregate} and variable, that has the same name as \code{by}
#' (if \code{Name_of.by} si not modified).
#'
#' @param sp A hyperSpec object.
#' @param by A grouping variable (either variable name of \code{sp},
#'              or vector (factor) of length \code{length(sp)}
#' @param FUN A function to apply.
#' @param Name_of.by (Do not change this!) The name of input \code{by}.
#'@param ...further arguments passed to \code{FUN}.
#' @return A hyperSpec object that contains summary statistic. Additional
#'         column \code{.aggregate} is added. In this column levels of
#'         grouping variable are indicated.
#' @export
#'
#' @seealso \code{\link[hyperSpec]{aggregate}}
#'
#' @examples
#' spStat(Spectra2, gr)
#' spStat(Spectra2, by = gr,          FUN = IQR)
#'
#' ## BUG:
#' # spStat(Spectra2, by = Spectra2$gr, FUN = IQR)
#'
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna
#'
spStat <- function(sp, by, FUN = mean,
                   Name_of.by = as.character(match.call()$by), ...){
	if (missing(by)) stop('Argument `by` is missing with no default.')

    by <- getVarValues(VAR = by, DATA = sp)

    stat_by_gr  <- aggregate(sp, by = by, FUN, ...)
    stat_all    <-     apply(sp, 2,       FUN, ...)

    # Add 2 variables to `stat_all`: 1. `.aggregate`, 2. name of "by":
    stat_all$.aggregate      <- factor('.All') # creates a nev variable.
    stat_all$..[,Name_of.by] <- factor('.All') # changes values of existing variable.

    # eval(parse(text = paste0("stat_all$",Name_of.by, " <- factor('.All')")))

    # Drop NAs at this point in order enable `collapse` and
    # to avoid problems with variables of class `POSIXct`
    stat_by_gr <- hyDrop_NA(stat_by_gr)
    stat_all   <- hyDrop_NA(stat_all)

    # Merge the hyperSpec objects:
    sp2 <- hyperSpec::collapse(stat_by_gr, stat_all)

    return(sp2)
}
