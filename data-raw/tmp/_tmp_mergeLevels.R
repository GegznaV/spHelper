#' (DEPRECATED) Recode a factor by merging levels.
#'
#' Function is modified from package \pkg{rockchalk} (\code{combineLevels}).\cr
#'
#' If the factor is an ordinal factor, then levels may be combined only if they
#' are adjacent. A factor with levels c("Lo","Med","Hi","Extreme") allows us to
#' combine responses "Lo" and "Med", while it will NOT allow us to combine "Lo"
#' with "Hi".
#'
#' A non-ordered factor can be reorganized to combine any values, no
#' matter what positions they occupy in the levels vector.
#'
#' @inheritParams rockchalk::combineLevels
#' @param verbose (logical) print information, if operation is successful.
#'
#' @return A new factor variable, with unused levels removed.
#' @export
#' @author
#' Modified by Vilmantas Gegzna\cr
#' Originally written by Paul E. Johnson <pauljohn@ku.edu>
#'
#' @seealso \code{\link[rockchalk]{combineLevels}}
#' @examples
#' x <- c("M","A","B","C","A","B","A","M")
#' x <- factor(x)
#' levels(x)
#' x2a <- mergeLevels(x, levs = c("M","A"), newLabel = "M_or_A")
#' addmargins(table(x2a, x, exclude=NULL))
#' x2b <- mergeLevels(x, c(1,4), "M_or_A")
#' addmargins(table(x2b, x, exclude=NULL))
#' x3 <- mergeLevels(x, levs = c("M","A","C"), newLabel = "MAC")
#' addmargins(table(x3, x, exclude=NULL))
#' ## Now an ordinal factor
#' z <- c("M","A","B","C","A","B","A","M")
#' z <- ordered(z)
#' levels(z)
#' table(z, exclude=NULL)
#' z2a <-  mergeLevels(z, levs = c(1,2), "Good")
#' addmargins(table(z2a, z, exclude = NULL))
#' z2b <- mergeLevels(z, levs = c("A","B"), "AorB")
#' addmargins(table(z2b, z, exclude = NULL))
#'
mergeLevels <-function (fac, levs, newLabel = "mergedLevels",
                          verbose = FALSE)
{
    adjacent <- function(x) {
        xfull <- seq(min(x), max(x))
        identical(xfull, as.integer(x))
    }

    facl <- levels(fac)
    if (is.character(levs)) {
        if (!identical(sum(levs %in% facl), length(levs))) {
            stop('Requested levels: "', levs,
                 '" are not in the legal list of factor levels: "',
                  facl, '"')
        }
    } else {
        if (sum(!levs %in% 1:length(facl)) > 0)
            stop("Requested levels (`levs`) don't exist in the factor")
    }
    if (is.character(levs)) {
        levsNum <- which(facl %in% levs)
    } else {
        levsNum <- levs
        levs <- facl[levs]
    }
    if (!"ordered" %in% class(fac)) {
        faclnew <- c(facl, "pjTempFacFame")
        facnew <- factor(fac, levels = faclnew)
        facnew[facnew %in% levs] <- "pjTempFacFame"
    }
    else {
        if ("ordered" %in% class(fac)) {
            if (!adjacent(levsNum)) {
                stop("`fac` is ordered. The levels to be combined must be adjacent.")
            }
            faclnew <- c(facl[1:min(levsNum)], "pjTempFacFame",
                         facl[(1 + min(levsNum)):length(facl)])
            facnew <- factor(fac, levels = faclnew)
            facnew[facnew %in% levs] <- "pjTempFacFame"
        }
    }
    facnew <- facnew[, drop = TRUE]
    levels(facnew)[levels(facnew) == "pjTempFacFame"] <- newLabel

    if (verbose == TRUE){
        cat("The original levels", facl,
            "\nhave been replaced by",  levels(facnew), "\n")
    }
    facnew
}
