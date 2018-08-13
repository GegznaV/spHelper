#' @name performance_measures
#' @title Performance measures for ROCR::prediction object
#' @description
#'          Function \code{get_performance} calculates the best performance measure when
#'          \code{prediction} object is given. \cr
#'
#' @param pred An object of class \code{prediction} from package \pkg{ROCR}.
#'
#' @param measure (\code{string(1)})\cr A string with the name of
#'                 classification performance measure to use. Currently
#'                 available options:
#' \itemize{
#'       \item \code{"bac"} - for balanced accuracy (mean of sensitivity and specificity);\cr
#'       \item \code{"kappa"} - for Cohens kappa;\cr
#'       \item \code{"wkappa"} - for weighted Cohens kappa;\cr
#'       \item \code{"j"} - for Youden's index;\cr
#'       \item \code{"auc"} - for area under the ROC curve;\cr
#'       \item \code{"acc"} - for accuracy (total proportion of correctly identified cases).
#' }
#' @return Function \code{get_performance} returns a numeric vector with 2 elements:
#' \itemize{
#'     \item the first element is the \emph{highest} value of selected performance measure;
#'     \item the second element is either corresponding \bold{cut-off value}, or \code{NA}
#' if the measure is \code{"auc"}.
#' }
#' @export
#
# last review: 2017-07-30
get_performance <- function(pred, measure) {
    switch(tolower(measure),

           # Acc - accuracy
           acc  = get_max(pred, Acc),

           # AUC Area unther the ROC curve
           auc = c(value = AUC(pred), cutoff = NA),

           # BAC - balanced accuracy: mean of sensitivity and specificity
           bac  = get_max(pred, Bac),

           # Youden's index
           j =  get_max(pred, J),

           # Cohen's kappa
           kappa = get_max(pred, Kappa),

           # Weighted Cohen's kappa
           wkappa = get_max(pred, Wkappa),

           stop(glue::glue("Unsuported performance measure: {measure}"))
    )
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname performance_measures
#' @param FUN Function to apply (one of
#' \code{\link{Acc}},
#' \code{\link{Bac}},
#' \code{\link{J}},
#' \code{\link{Kappa}},
#' \code{\link{Wkappa}},
#'  etc.)
#' @export

get_max <- function(pred, FUN){
    val <- FUN(pred)
    max_i <- which.max(val)
    c(value  = val[max_i],
      cutoff = cutoff(pred)[max_i])
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname performance_measures
#' @export
AUC <- function(pred) {
    pracma::trapz(Se(pred), Sp(pred))
}

#' @rdname performance_measures
#' @export
tp <- function(pred) {
    pred@tp[[1]]
}
#' @rdname performance_measures
#' @export
fp <- function(pred) {
    pred@fp[[1]]
}
#' @rdname performance_measures
#' @export
fn <- function(pred) {
    pred@fn[[1]]
}
#' @rdname performance_measures
#' @export
tn <- function(pred) {
    pred@tn[[1]]
}
#' @rdname performance_measures
#' @export
cutoff <- function(pred) {
    pred@cutoffs[[1]]
}
#' @rdname performance_measures
#' @export
Se <- function(pred) {
    TP <- tp(pred)
    FN <- fn(pred)
    TP / (TP + FN)
}

#' @rdname performance_measures
#' @export
Sp <- function(pred) {
    TN <- tn(pred)
    FP <- fp(pred)
    TN / (TN + FP)
}
#' @rdname performance_measures
#' @export
J <- function(pred) {
    (Se(pred) + Sp(pred)) - 1
}
#' @rdname performance_measures
#' @export
Bac <- function(pred) {
    (Se(pred) + Sp(pred)) / 2
}
#' @rdname performance_measures
#' @export
Acc <- function(pred) {
    TP <- tp(pred)
    FP <- fp(pred)
    FN <- fn(pred)
    TN <- tn(pred)
    (TP + TN) / (TP + FP + TN + FN)
}
#' @rdname performance_measures
#' @export
Kappa <- function(pred) {
    kappa_helper(pred, measure_kappa)
}

#' @rdname performance_measures
#' @export
Wkappa <- function(pred) {
    kappa_helper(pred, measure_wkappa)
}

#' @rdname performance_measures
#' @param FUN_ Function to apply (either \code{\link{measure_kappa}} or
#'                               \code{\link{measure_wkappa}})
#' @export
kappa_helper <- function(pred, FUN_) {
    # Helper for Kappa and Wkappa functions.
    # It creates a square matrix `matrix(c("TP", "FN", "FP", "TN"), 2)`
    # on which approprite kappa value (indicated in the `FUN`)
    # is calculated.
    apply(X =  matrix(c(tp(pred), fn(pred), fp(pred), tn(pred)), ncol = 4),
          MARGIN = 1,
          FUN = function(v) {
              FUN_(conf_mat = matrix(v, nrow = 2))
          }
    )
}

#' @rdname performance_measures
#'
#' @param TP Number of true positives.
#' @param FN Number of false negatives.
#' @param FP Number of false positives.
#' @param TN Number of true negatives.
#'
#' @export
make_conf_matrix <- function(TP, FN, FP, TN) {
    matrix(c(TP, FN, FP, TN), 2)
}
