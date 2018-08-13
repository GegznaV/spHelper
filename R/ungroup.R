#' [!] Mix values of factor variable levels in equal proportions
#'
#' Mix values of factor variable levels in (approximately) equal proportions
#' to disrupt existing structure of levels, i.e. "ungroup".
#'
#' @details
#' Default number of new groups \code{n} is equal to number of factor levels.
#'  \code{n = nlevels(gr)}. If NA's exist, they are treated as one
#'  more additional group thus \code{n = nlevels(gr) + 1}.
#'
#' @param gr A vector indicating original groups.
#' @param n Number of new groups. If \code{NULL}, default number is used
#' which is either \code{nlevels(gr)} or \code{nlevels(gr)+1}, if \code{NA}'s
#' are presented (see "Details" section).
#' @param base Base for names of new groups. Default is \code{"new_group_"}.
#' @param new.names Labels for new groups. If value is not \code{NULL},
#'  \code{n} and \code{base} are ignored.
#'
#'
#' @return A vertor with new groups, which are a mix of proportionally
#'         distributed (ungrouped) original groups.
#' @export
#'
#' @examples
#' gr <- Scores2$gr
#'
#' # -------------------------------------------------------
#' set.seed(1)
#' newGr <- unGroup(gr)
#' table(gr,newGr, useNA = "ifany")
#'
#' # NA's are treated as an additional level ---------------
#' gr[5:10] <- NA
#'
#' set.seed(1)
#' newGr <- unGroup(gr)
#' table(gr,newGr, useNA = "ifany")
#'
#' @author Vilmantas Gegzna
#' @family \pkg{spHelper} utilities
#'
unGroup <- function(gr, n = NULL, base = "new_group_", new.names = NULL){
    gr <- as.factor(gr)

    # If NA's exist, they are treated as a separate group.
    if (any(is.na(gr))) {
        new_level <- "...NA's..."
        levels(gr) <- c(levels(gr), new_level)
        gr[is.na(gr)] <- new_level
    }

    newGr <- rep_len(NA, length(gr))

    GrNames <- levels(gr)
    nGr     <- nlevels(gr)

#default names:
    if (is.null(n)) n <- nGr
    if (is.null(new.names)) new.names <- paste0(base, 1:n)

    for (i in 1:nGr) {
        idxGr_i <- which(gr==GrNames[i])
        newGr[idxGr_i] <- new.names  %>%
                          sample()   %>% # make equal odds for all names to be selected
                          rep_len(length(idxGr_i))   %>%
                          sample() # randomize order of elements
    }
    return(newGr)
}


