#' [Internal] measure_kappa_tmp
#'
#' @param pred object of class "prediction" from package ROCR
#'
#' @examples
measure_kappa_tmp <- function(pred) {
    ObjLength <- length(pred@tp[[1]])
    kappa <- matrix(NA, ObjLength)
    for (index in 1:ObjLength) {
        suppressWarnings({
            obj <- make_table(pred, index) %>% psych::cohen.kappa(.)
            kappa[index] <- obj[["kappa"]]
        })

    }
    colnames(kappa) <- "Kappa"
    return(kappa)
}
#' [Internal] Make table (from prediction object)
#'
#' @param pred object of class "\code{prediction}"
#'           from package \pkg{ROCR}
#' @param index index of value in the object
#'
#' @return Clasification table with TP, FP, FN, TN values
make_table <- function(pred, index){
    matrix(c(pred@tp[[1]][index], pred@fp[[1]][index],
             pred@fn[[1]][index], pred@tn[[1]][index]),
           nrow  = 2,
           byrow = TRUE)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
