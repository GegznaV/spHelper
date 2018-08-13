#' [.!] Compare spectra of groups at each wavelength
#'
#' Compare spectra of each pair of indicated groups at each wavelength.
#'
#' @template sp-hy
#' @template by
#' @param measure string with measure of classification performance. Currently
#'  available options:\cr
#' "auc"   - AUC (default) area under the ROC curve;\cr
#' "j"     - Youden's index;\cr
#' "bac"   - Balanced accuracy (mean of Sensitivity (Se) and Specificity (Sp)).\cr
#' \cr
#' "sesp"  - [DEPRECATED!] The same as "bac";\cr
#' "tpr"   - [DEPRECATED!] The same as "bac";\cr
#' "kappa" - [SLOW, DO NOT USE IT] Cohen's kappa.
#'
#' @return Fields of \code{sp_classif_performance} object: \cr
#'
#'  \bold{type} type of data used ("Training data");\cr
#'  \bold{performance} - \code{hyperSepc} object with performance estimates;\cr
#'  \bold{cutoffs} - \code{hyperSepc} object with estimates of critical values
#'                    (cut-off points);\cr
#'  \bold{means} - \code{hyperSepc} object with means of each compared group;\cr
#'  \bold{means.description} - type of those means
#'                             ("10\% trimmed mean (of each group)");\cr
#'  \bold{compared_by_var} - variable name, that was used for grouping; \cr
#'  \bold{measure}- measure of performance. \cr \cr
#'
#' @export
#'
#' @author Vilmantas Gegzna
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#'
#' @examples
#' library(spHelper)
#' library(spPlot)
#' library(ROCR)
#'
#' sp <- sp_filter(Spectra2)
#'
#' # Default measure of performace is AUC:
#' sp_compared <- sp_class_perform(sp, "class")
#'
#' sp_compared <- sp_class_perform(sp, "class", measure = "bac")
#'
#' names(sp_compared)
#'   ##  [1] "type"   "performance"  "cutoffs"  "means"
#'   ##  [5] "means.description" "compared_by_var"   "measure"
#'
#' sp_compared$performance
#'
#'   ##  hyperSpec object
#'   ##     6 spectra
#'   ##     2 data columns
#'   ##     501 data points / spectrum
#'   ##  wavelength: lambda/nm [integer] 300 301 ... 800
#'   ##  data:  (6 rows x 2 columns)
#'   ##     1. spc: Mean of Se and Sp [matrix501] 0.6266667 0.7310526 ... 0.5763674
#'   ##     2. Compared: Compared groups [character] K vs. l K vs. N ... N vs. S1
#'
#' sp_compared$cutoffs
#'
#'   ##  hyperSpec object
#'   ##     6 spectra
#'   ##     2 data columns
#'   ##     501 data points / spectrum
#'   ##  wavelength: lambda/nm [integer] 300 301 ... 800
#'   ##  data:  (6 rows x 2 columns)
#'   ##     1. spc: Cut-offs [matrix501] 162.3499 151.5054 ... 19.60151
#'   ##     2. Compared: Compared groups [character] K vs. l K vs. N ... N vs. S1
#'
#'
#' theme_set(theme_bw())
#' ggplot(sp_compared$performance, aes(color = Compared)) + geom_line()
#'
#' ggplot(sp_compared$cutoffs, aes(color = Compared)) + geom_line()
#'
#' qplot_sp(sp_compared$performance, by = "Compared") + set_ggLims(c(.45,1),"y")
#'
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' Sp2 <- Spectra2[,,500~510]
#' sp_compared_cv <- sp_class_perform_cv(Sp2, "class")
#'
#' names(sp_compared_cv)
#'   ## [1] "data"              "cvo"      "train_performance"
#'   ## [4] "test_performance"  "cutoffs"  "obj"
#'
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sp_class_perform <- function(sp,
                             by = stop("Parameter 'by' is missing."),
                             measure = c("auc", "bac", "j",
                                         # Do not use "sesp","tpr",
                                         # they are for backwards compatibility
                                         "sesp","tpr")
                             ) {

    measure <- measure[1]

    # Select necessary variavles only
    sp <- sp[, c(by, "spc")]

    wl_label <- labels(sp, ".wavelength")

    # Select grouping variable
    gr <- getVarValues(by, sp$..)

    # gr <- sp[[, by, drop = TRUE]]
    sp$gr <- factor(gr, ordered = FALSE)

   # Preparation: calculations
    Nlev  <- nlevels(gr)
    levs  <- levels(gr)
    cmb   <- combn(Nlev,2) %>% t
    N_cmb <- nrow(cmb)

    if (N_cmb == 2) N_cmb <- 1

    # Pre-allocate variables
    all_metrics <- all_cutoffs <- matrix(NA, N_cmb, nwl(sp))
    Compared    <- vector("character", N_cmb)
    Compared_1  <- vector("character", N_cmb)
    Compared_2  <- vector("character", N_cmb)

    # on.exit(print(sprintf("i = %g, u = %g",i,u)))

    for (u in 1:N_cmb) {
        used_levels   <- levs[c(cmb[u,1],cmb[u,2])]
        sp_subset     <- sp[gr %in% used_levels,]
        Compared[u]   <- paste(used_levels, collapse = " vs. ")
        Compared_1[u] <- used_levels[1]
        Compared_2[u] <- used_levels[2]

        groups        <- sp_subset$gr %>% droplevels

        for (i in 1:nwl(sp)) {
            wavelengths <- sp_subset[[,,i, wl.index = TRUE]]

            # Calculate overlap as AUC or other value
            pred    <- prediction0(wavelengths,  groups, used_levels)
            optimal <- calculate_performance(pred, measure)
            all_metrics[u,i] <-       optimal$value
            # [!!! possibly incorrect line, as **mean** is calculated]:
            all_cutoffs[u,i] <-  mean(optimal$cutoff)
        }
        rm(used_levels, sp_subset, groups)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Object for performance measure
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    spc_label = switch(tolower(measure),
                        auc   = "AUC",
                        j     = "Youden's index",

                        sesp  = ,
                        tpr   = ,
                        bac   = "Balanced accuracy",

                        kappa = "Cohen's kappa",

                        "Measure")


    sp_compared <- decomposition(sp, x = all_metrics)
    sp_compared$type       <- spc_label
    sp_compared$Compared   <- Compared
    sp_compared$Compared_1 <- Compared_1
    sp_compared$Compared_2 <- Compared_2

    comp_labels <- list(
        Compared    = "Compared groups",
        Compared_1  = "Compared group 1",
        Compared_2  = "Compared group 2",
        .wavelength = wl_label,
        spc         = spc_label
    )

    labels(sp_compared) <- comp_labels
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Object for cut-off values (if applies)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    sp_cutoffs <- decomposition(sp, x = all_cutoffs)
    spc_label = "Cut-offs"
    sp_cutoffs$type        <- spc_label
    sp_cutoffs$Compared    <- Compared
    sp_cutoffs$Compared_1  <- Compared_1
    sp_cutoffs$Compared_2  <- Compared_2
    labels(sp_cutoffs)     <- comp_labels
    labels(sp_cutoffs)$spc <- spc_label
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Object for means of each group
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    means <-
        hyperSpec::aggregate(sp,
                             by = gr,
                             FUN = mean,
                             trim = 0.1,
                             na.rm = TRUE)[, c(".aggregate", "spc")]
    colnames(means)[colnames(means) == ".aggregate"] <- "group"
    means$type = "Group means"
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    OBJ <- list(
        type = "Training data",
        performance       = sp_compared,
        cutoffs           = sp_cutoffs,
        means             = means,
        means.description = "10% trimmed mean (of each group)",
        compared_by_var   = by,
        measure           = measure
    )

    class(OBJ) <- c("sp_classif_performance")
    return(OBJ)
}

#' Calculate best berformance measure
#'
#' Function that calculates the best performance measure
#'
#' @param pred object of class "prediction" from package ROCR
#' @param measure one of the following performance measures
#'       \code{c("auc","j","bac","kappa")}
#'
#' @return best value of selected performance measure
calculate_performance <- function(pred, measure) {

    # 2016-07-13:
    optimal <- list(measure = measure, value = NA, cutoff = NA)

    switch(tolower(measure),

           # AUC Area unther the ROC curve
           auc = {
               optimal$value  <- performance(pred, measure = "auc")@y.values[[1]]
               optimal$cutoff <- NA
            },

           # Youden's index
           j = {
               PERF <- performance(pred, "sens","spec");
               Youden_ALL <- PERF@x.values[[1]] + PERF@y.values[[1]] - 1
               optimal$value  <- max(Youden_ALL)
               optimal$cutoff <- PERF@alpha.values[[1]][which.max.all(Youden_ALL)]
           },

# 2016-07-13, 2017-07-22
# Mean of TPR and FPR, i.e. mean of Sensitivity and Specificity
           sesp = ,
           tpr  = ,
           bac  = {# BAC - mean of sensitivity and specificity
               PERF <- performance(pred, "sens","spec");
               bac_all <- (PERF@x.values[[1]] + PERF@y.values[[1]])/2

               optimal$value  <- max(bac_all)
               optimal$cutoff <- PERF@alpha.values[[1]][which.max.all(bac_all)]
           },

           # Cohen's kappa - NOT SUPPORTED
           kappa = {
               stop("Unsuported performance measure:" %.+.% measure)
               max(measure_kappa_tmp(pred))
           },

           stop("Unsuported performance measure:" %.+.% measure)
    )
    return(optimal)
}
