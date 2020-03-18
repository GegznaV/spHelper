# =============================================================================
#' @rdname sp_class_perform
#'
#' @return Fields of \code{sp_class_perform_cv} object: \cr
#'
#'  \bold{data} - a \code{hyperSpec} object with data used in calculations; \cr
#'  \bold{cvo} - cross-validation object used for analysis;  \cr
#'  \bold{train_performance} - performance estimates of training datasets for each repetition and fold; \cr
#'  \bold{test_performance} - performance estimates of testing datasets for each repetition and fold; \cr
#'  \bold{cutoffs} - estimates of cut-off values for each repetition and fold;\cr
#'  \bold{obj} - a list of \code{sp_classif_performance} objects for each repetition and fold; \cr
#'
#' @export
#' @param cvo a cross-validation object (cvo), created with function
#'            \code{\link{cvo_create_folds}},\code{\link[caret]{createFolds}}
#'            or similar.
#' @inheritParams cvo_create_folds
#' @examples
#' \dontrun{\donttest{
#' rez <- sp_class_perform_cv(sp = Spectra2, by = "gr")
#'
#' rez
#' }}
#'

sp_class_perform_cv <- function(
    sp,
    by = stop("Parameter 'by' is not specified."),
    measure = c("sesp","j","tpr")[1],
    cvo = cvo_create_folds(sp, by, seeds),
    seeds = NULL,
    # Not implemented yet:
    sp_test  = NULL
){
    # # # For debugging: ****************************************************
    # i = 1
    #
    # # Sugeneruoti mažą hyperSpec ir su juo patikrinti
    # set.seed(5)
    #
    # data(Spectra2)
    # choose_rows <- Spectra2$gr  %in% c("A","B")
    # sp <- Spectra2[choose_rows, , c(300,400,500)]
    # sp <- sample(sp , size =  20)
    #
    # sp$gr %<>% droplevels
    #
    # by  <- "gr"
    # cvo <- cvo_create_folds(sp, by, k = 2, times = 1, seeds = 100)
    # # *********************************************************************

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   # Initialize variables
   rez_test  <- rez_train <- obj_train <- cutoff_train <-
       data_train <- data_test <- list()

   # Number of folds in total
   n_repetitions <-
       if (is.null(sp_test)) {
           cvo_count_folds(cvo)
           # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       } else {# if `sp_test` is provided
           stop("Not implemented yet")
           # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           if (!is.list(sp_test)) sp_test  <- list(sp_test)
           if (!is.list(sp))      sp_train <- list(sp)

           if (length(sp_train) != length(sp_test))
               stop("Number of `hyperSpec` objects in" %.+.%
                    "`sp` and `sp_test` does not match.")
           # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           length(sp_train)
           # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           #
           #
        }

   for (i in 1:n_repetitions) {
       # Return training indices or complement to test indices:
       training_ind <- cvo_get_inds(cvo, fold =  i, type = "train")
       sp_train <- sp[ training_ind, ]
       sp_test  <- sp[-training_ind, ]

       # # # For debugging: **************************************************
       # # If identical results are needed
       # sp_test  <- sp[training_ind, ]
       # # *******************************************************************

       object <- sp_class_perform(sp_train, by = by, measure = measure)

       object$cutoffs$Fold     <- names(cvo)[i]
       object$performance$Fold <- names(cvo)[i]

       rez_test[[i]]   <- predict(object, newdata = sp_test)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       data_train[[i]] <- sp_train
       data_test[[i]]  <- sp_test
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       cutoff_train[[i]] <- object$cutoffs
       rez_train[[i]]    <- object$performance
       obj_train[[i]]    <- object
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   }

   train_performance <- collapse(rez_train)
   test_performance  <- collapse(rez_test)
   cutoff_train      <- collapse(cutoff_train)
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # ============= Return the results =======================================
   result <- list(
       data = sp,
       cvo  = cvo,
       train_performance = train_performance,
       test_performance  = test_performance,
       cutoffs = cutoff_train,
       obj = obj_train

       # data_train = data_train,
       # data_test  = data_test,
   )

    class(result) <- c("sp_class_perform_cv", "list")
    return(result)

} # [END]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ===========================================================================
#' @rdname sp_class_perform
#' @export
print.sp_class_perform_cv <- function(obj){

    bru("-")
    cat("Summary:\n")
    print(summary(obj))

    # cat("\nLength of $obj:" ,length(obj$obj), "\n\n")

    cat("\n*** Summary of $obj[[1]]: ***\n")
    print(obj$obj[[1]])

    cat("\n*** Summary of $cvo: ***\n")

    info_cvo <- cvo_get_info(obj$cvo)
    info_cvo <- data.frame(colnames(info_cvo), t(info_cvo))
    rownames(info_cvo) <- NULL
    names(info_cvo)    <- c("<FIELD>", "<INFORMATION>")
    print(info_cvo)
    bru("-")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ===========================================================================
#' @rdname sp_class_perform
#' @export
print.sp_classif_performance <- function(obj){

    INFORMATION <- sapply(obj,
                       function(x)
                       {   if (is.character(x)) {
                               x
                           } else {
                               class(x) %++% " object"
                           }
                       }
                   )

    FIELD <- names(INFORMATION)
    names(INFORMATION) <- NULL

    rez <- data.frame(   x = FIELD,
                      info = INFORMATION)

    names(rez) <- c("<FIELD>", "<INFORMATION>")
    print(rez)

}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# =============================================================================
#' @rdname sp_class_perform
#' @export

# # pridėt apsauga tam atvejui, kai nėra nei vieno tos grupės atvejo

predict.sp_classif_performance <- function(object, newdata,
                                           what = c("values", "performance"),
                                           ...){
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ============= Check Validity of Inputs =================================
    measure <- tolower(object$measure)

    if (!measure %in% c("sesp","tpr", "j"))
        stop(paste("Measure is not supported for prediction:", object$measure))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ============= Define Functions =========================================

    get_mean <- function(gru){
        object$means$spc[object$means$group == gru, ]
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_cutoff <- function(gru1, gru2){
        object$cutoffs$spc[object$cutoffs$Compared_1 == gru1 &
                           object$cutoffs$Compared_2 == gru2,   ]
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    which_is_higher <- function(gru1, gru2){
        ifelse(get_mean(gru1) > get_mean(gru2), gru1, gru2)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    which_is_lower <- function(gru1, gru2){
        ifelse(get_mean(gru1) > get_mean(gru2), gru2, gru1)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ============= Calculations =============================================

    by <- object$compared_by_var

    gr1 <- object$performance$Compared_1
    gr2 <- object$performance$Compared_2

    new_gr <- newdata[[, by,, drop = TRUE]]
    N_wl   <- nwl(newdata)
    N_combinations <- length(gr1)

    # Preallocate variables
    measure_for_newdata <- matrix(NA, nrow = N_combinations, ncol = N_wl)
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for (i in 1:N_combinations){

        ind <- new_gr %in% c(gr1[i], gr2[i])

        # Subset
        new_gr_i  <-  new_gr[ind] %>% droplevels
        newdata_i <- newdata[ind]

        # Group at each wavelength which is expected to have higher values
        expected_higher <- which_is_higher(gr1[i], gr2[i])

        # Group at each wavelength which is expected to have LOWER values at
        # each wavelength
        expected_lower <- which_is_lower(gr1[i], gr2[i])

        # Predict groups
        N_row <- nrow(newdata_i)
        cutoffs_matrix <- rep_rows(get_cutoff(gr1[i], gr2[i]), N_row)

        higher_than_cutoff <- newdata_i$spc > cutoffs_matrix

        predicted_group <-
            ifelse(higher_than_cutoff  %>% t, # transpose to have a correct
                                              # recycling of values
                   yes = expected_higher,
                   no  = expected_lower
               ) %>% t

       # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        expected_group <- new_gr_i %>% as.matrix %>% rep_cols(N_wl)
       # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        classified_correctly <-
            (predicted_group == expected_group) %>% as.data.frame
       # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        SeSp <- split(classified_correctly, new_gr_i) %>%
                lapply(colMeans) %>%
                do.call("rbind", .)

        measure_for_newdata[i,] <-
            switch(measure,
                   # calculate mean (sensitivity + specificity)
                   "sesp" = colMeans(SeSp),
                   "tpr"  = colMeans(SeSp),
                   "j"    = colSums(SeSp) - 1,
                   stop(paste("Measure is not supported for prediction:",
                              object$measure))
            )


# # # For debugging: ****************************************************
#
# ldf_newdata_i <- ldf(newdata_i)
# ldf_newdata_i$Classified <- factor(
#     x = ldf(
#         hyperSpec(
#             apply(classified_correctly, 1:2, as.numeric),
#             wavelength = wl(newdata_i)
#             )
#         )$spc,
#     levels = c(0,1),
#     labels = c("Error","Correct")
# )
#
# hy_cutoff <- hyperSpec(get_cutoff(gr1[i], gr2[i]),wavelength = wl(newdata))
#
# ggplot(hyperSpec()) +
#     geom_point(data = ldf_newdata_i,
#                aes(color = Classified, shape = gr),
#                alpha = .5) +
#     geom_line(data = ldf(hy_cutoff))
#
# ggplotly_tidy()
#
# # # For debugging: ****************************************************
# # yes = rep_rows(expected_higher,N_row),
# # no  = rep_rows(expected_lower, N_row)
#
# N_row <- nrow(newdata_i)
# cbind(rep_rows(expected_higher,N_row),
#       higher_than_cutoff,
#       predicted_group,
#       expected_group)
# # #  ******************************************************************




    } # [END: for i]

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ============= Return the results =======================================

    result <- decomposition(
                        object = object$performance,
                    wavelength = wl(object$performance),
                             x = measure_for_newdata,
                     label.spc = labels(object$performance)$spc,
              label.wavelength = labels(object$performance)$.wavelength
    )
    return(result)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

} #[END]


# =============================================================================
# DEPRECATED FUNCTIONS ------------------------------------------


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname sp_class_perform
#' @export
#'
sp_classification_performance <- function(
    sp,
    by = stop("Parameter 'by' is not specified."),
    measure = c("sesp","j","tpr")[1],
    cvo = cvo_create_folds(sp, by, seeds),
    seeds = NULL,
    ...)

{
    .Deprecated("sp_class_perform_cv")
    sp_class_perform_cv(sp, by,measure,cvo,seeds,...)
}

#' @rdname sp_class_perform
#' @export
sp_compare_gr_wl <- function(sp,
                             by = stop("Parameter 'by' is missing."),
                             measure = c("auc","j","sesp","tpr"),
                             ...){
    .Deprecated("sp_class_perform")
    sp_class_perform(sp, by, measure)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname sp_class_perform
#' @export
class_perform_sp <- function(sp,
                             by = stop("Parameter 'by' is missing."),
                             measure = c("auc","j","sesp","tpr"),
                             ...){
    .Deprecated("sp_class_perform")
    sp_class_perform(sp, by, measure,...)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
