#' Balanced accuracy (for binary classification)
#'
#' # This function is based on function `measureBAC` from `mlr` package
#'
#' @param truth a vector with true (reference) values
#' @param response a vector with response (predicted) values
#' @param negative
#' @param positive
#' @export
#' @family measures_
measure_bac_2gr <- function(truth, response, negative, positive) {
     TP <- measure_tp(truth, response, positive)
      P <- sum(truth == positive)
     TN <- measure_tn(truth, response, negative)
      N <- sum(truth == negative)
    # BAC:
    mean(c(TP/P, TN/N))
}
#' Calculate number of true negatives
#' @export
#' @family measures_
measure_tn <- function(truth, response, negative) {
    sum(truth == response & response == negative)
}
#' Calculate number of true positives
#' @export
#' @family measures_
measure_tp <- function(truth, response, positive) {
    sum(truth == response & response == positive)
}

#' Rate of correctly identified values in a certain group
#'
#' @param truth a vector with true (reference) values
#' @param response a vector with response (predicted) values
#' @param level (character) a name of level (group) of interest
#' @export
#' @family measures_
measure_tpr <- function(truth, response, level) {
    sum(truth == response & response == level) /
    sum(truth == level)
}


#' Balanced accuracy (for 2 or more groups)
#'
#' @param truth a vector with true (reference) values
#' @param response a vector with response (predicted) values
#' @export
#' @family measures_
measure_bac <- function(truth, response) {

    truth    <- as.factor(truth)
    response <- as.factor(response)
    levels_t <- levels(truth)
    levels_r <- levels(response)

    if (!dplyr::setequal(levels_t, levels_r)) {
        stop("The factor levels in vectors ",
             "`truth` and `response` must agree.")
    }

    # BAC:
    mean(
        purrr::map_dbl(levels_t, ~measure_tpr(truth, response, .x))
    )


}





