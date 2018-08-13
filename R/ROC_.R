# ploting/summarizing function `ROCR_`` ---------------------------------------

#' [.]  Do ROC analysis and plot it's results (ROC_)
#'
#' @inheritParams ROCR::prediction
#' @param make_plots Logical. If TRUE (default) Sensitivity-Specificity plot and
#' plot of sensitivity and specificity at each cot-off point are plotted.
#'
#' @return
#' A) plots as listed in description of \code{make_plots}.\cr
#' B) Table of performance measures a optimal cut of point.
#'
#' @export
#'
#' @author Vilmantas Gegzna
#'
#' @seealso This function is based on package \pkg{ROCR}.
#'
#' @examples
#'
#' library(ROCR)
#' library(spHelper)
#'
#' data(ROCR.simple)
#' ROC_(ROCR.simple$predictions, ROCR.simple$labels)
#'   ##  Compared groups                      "0 vs. 1"
#'   ##  Group treated as positive            "1"
#'   ##  AUC                                  "0.83"
#'   ##  Cut-off                              "0.5015"
#'   ##  Sensitivity (True positive rate, Se) "0.85"
#'   ##  Specificity (True negative rate, Sp) "0.85"
#'   ##  Mean of Se and Sp                    "0.85"
#'
#'

# # Make prediction object
# ROC_ <-function(x,labels,label.ordering=NULL, make_plots = TRUE){
#     pred <- prediction0(x,labels,label.ordering)
#     if (make_plots==TRUE) ROC_plots(pred)
#     ROC_table(pred)
# }

# Make prediction object
ROC_ <-function(x, labels, label.ordering = NULL,
                make_plots = TRUE){

    if (missing(x))
        stop("Input variable `x` is missing.")

    if (missing(labels))
        stop("Input variable `labels` is missing.")

    if (!is.factor(labels)){
        warning("Input variable `labels` is converted to factor variable.")
        labels <- as.factor(labels)
    }

    # Preparation: calculations
    Nlev <- nlevels(labels)
    levs <-  levels(labels)
    cmb  <-  combn(Nlev,2) %>% t
    N_cmb <- nrow(cmb)

    if (N_cmb == 2) N_cmb <- 1

    ROC_table_ <- list()
    Compared <- vector("character", N_cmb)

    for (u in 1:N_cmb){
        used_levels   <- levs[c(cmb[u,1],cmb[u,2])]
        x_subset      <- x[labels %in% used_levels]
        labels_subset <- labels[labels %in% used_levels] %>% droplevels
        Compared[u]   <- paste(used_levels, collapse = " vs. ")


        # Do prediction
        pred <- prediction0(x_subset, labels_subset, used_levels)
        if (make_plots==TRUE) {
            cat(paste("\n  ", Compared[u], "\n  "))
            ROC_plots(pred)
            }
        ROC_table_[[u]] <- ROC_table(pred)
    }
    return(ROC_table_)
}



#' @rdname ROC_
#' @inheritParams ROCR::performance
#' @export
ROC_table <- function(prediction.obj){
    pred <- prediction.obj

    # Make necessary prediction objects
    perf       <- performance(pred, measure = "tpr", x.measure = "fpr") # TPR vs FPR, for ROC curve.
    auc.perf   <- performance(pred, measure = "auc") # AUC

    # Function `table_at_opt_cutoff`

    table_at_opt_cutoff <- function(perf, pred, auc.perf){
        mk_table_FUN <- function(x, y, p, lab, auc){
            # Identify labels of  positive and negative groups
            gr_labels <- lab[[1]]  %>% levels

            # Index of optimal cut-off point
            d = sqrt((x - 0)^2 + (y-1)^2)
            ind = which(d == min(d))

            # Make table
            c( `Compared groups`   = paste(gr_labels, collapse = " vs. "),

               `Group treated as positive`  = gr_labels[2],
               `AUC`                                    = sprintf("%.2f",auc[[1]]),
               `Cut-off`                                = sprintf("%.4g",  p[[ind]]),
               `Sensitivity (True positive rate, Se)`   = sprintf("%.2f",  y[[ind]]),
               `Specificity (True negative rate, Sp)`   = sprintf("%.2f",1-x[[ind]]),
               `Mean of Se and Sp`                      = sprintf("%.2f",((y[[ind]]) + (1-x[[ind]]))/2)
            )

        }

        cut.ind <- mapply(mk_table_FUN,
                          perf@x.values, perf@y.values, pred@cutoffs,
                          pred@labels,auc.perf@y.values)
        return(cut.ind)
    }


    tbl <- table_at_opt_cutoff(perf, pred, auc.perf)
    # colnames(tbl) <- rep(" ", ncol(tbl)) # Remove column names

    return(tbl)

    # cat("\n")
    # pander::pander(table_at_opt_cutoff(perf, pred, auc.perf),
    #                caption ="Performance measures at optimal cutoff point.")

}

#' @rdname ROC_
#' @export
ROC_plots <- function(prediction.obj){
    pred <- prediction.obj

    perf_Se_Sp <- performance(pred, "sens", "spec")  # Se vs Sp, for Se-Sp plot, i.e., mirrored ROC curve.
    perf.meanSeSp = performance0(pred,
                                 measure = "meanSeSp",
                                 funnames = "meanSeSp",
                                 # longnames="mean of Sensitivity and Specificity",
                                 longnames="Mean of Sensitiv. and Specific.",
                                 exprs = list(c("cutoffs", "((tp / (tp + fn)) + (tn/(tn+fp)))/2")))

    perf.meanSeSp@alpha.name    <-  perf.meanSeSp@x.name
    perf.meanSeSp@alpha.values  <-  perf.meanSeSp@x.values

    # Plots --------------------------------------------------------------------

    op <- par(mfrow = c(1,2))
    # colorize.palette=colorRampPalette(RColorBrewer::brewer.pal(5,"Set1"))(256)

    ## sensitivity/specificity curve (x-axis: specificity, y-axis: sensitivity)
    plot(perf_Se_Sp, col = "red", colorize = TRUE, colorkey.relwidth=0.5)
    abline(a=1, b= -1,lty = 2, col = "grey")
    title("Sensitivity-Specificity plot")

    # Mean of Se and SP at each cuttoff
    plot(perf.meanSeSp, ylim = c(0.499, 1.005), colorize = TRUE,colorkey.relwidth=0.5)
    abline(h = .5, col = "grey70", lty = 2)
    title("Tradeoff of Se and Sp at each cutoff value")

    par(op)
    # -------------------------------------------------------------------------
}




# Additional lines ===========================================================
    #
    #     perf.kappa = myperformance(pred,
    #                                  measure = "Kappa",
    #                                  funnames = "Kappa",
    #                                  longnames="Cohen's Kappa",
    #                                  exprs = list(c("cutoffs",
    #                                                 "mean(c(tp, fp, fn, tn))")))
    #                                                 # "psych::cohen.kappa(matrix(c(tp, fp, fn, tn),2))$kappa ")))
    #
    #     plot(perf.kappa)
