# @aliases createFoldsBS
# @aliases stratifiedFolds
# @aliases createFolds2

#' @name createFoldsBS
#'
#' @title DEPRECATED!
#'
#' @description
#' DEPRECATED!\cr
#' Create folds with blocking and stratification (BS)
#'   for (repeated) k-fold cross-validation
#' Function randomly divides observations into folds that are used for (repeated)
#' k-fold cross-validation. In these folds observations are:
#' \enumerate{
#'      \item \bold{blocked} by values in variable \code{ID} (i.e. observations
#'        with the same "ID" are treated as one unit (a block) and are always
#'         in the same fold);
#'      \item \bold{stratified} by levels of factor variable \code{gr} (the proportions of
#'      these grouped units of observations per each group (level) are kept aproximately
#'      constant throughout all folds).
#'  }
#'
#' @note If \code{k} is such big, that some folds have no observations of a certain group
#'       (i.e. level in \code{gr}), an error is returned. In that case smaller value of
#'        \code{k} is recommended. \cr \cr
#'         \code{createFolds2}, \code{stratifiedFolds} is a wrapper of
#'         \code{createFoldsBS}.
#'
#'
#' @param data A data frame, that contains variables which names are denoted
#'        by arguments \code{ID} and by \code{gr}.
#'
#' @param stratify_by,gr A vector or a name of factor variable in \code{data}, which levels
#'                 will be used for \emph{stratification}. E.g., vector with
#'                 medical groups.
#'
#' @param block_by,ID A vector or a name of variable in \code{data}, that contains
#'       identification codes/numbers (ID). These codes will be used for blocking.
#'
#' @param k (integer) A number of folds, default \code{k = 5}.
#'
#' @param returnTrain (logical) If \code{TRUE}, returns indices of variables in
#'                  training set. If \code{FALSE}, returns indices of
#'                  variables in test set.
#' @param times (integer) number of repetitions for repeated cross-vatidtion.
#' @param seeds (vector of integers | \code{NULL}) Seeds for random number
#'             generator for each repetition.\cr
#'             If \code{seeds = NULL} random seeds are generated.\cr
#'             If number of repetitions is
#'             greater than number of provided seeds, random seeds are
#'             generated and added to the provided ones. The first seed will
#'             be used to ensure reproducibility of the randomly generated seeds.\cr
#'
#'              (See \code{\link[base]{set.seed}} for more information about
#'              random number generation).
#'
#' @return A list of folds. In each fold there are indices observations.
#'         The structure of outpus is the similar to one created with
#'         \code{\link[caret]{createFolds}()}.
#'
#' @export
#' @seealso \code{\link[caret]{createFolds}}\cr
#' Test if folds are blocked and stratified \code{\link{cvo_test_bs}}
#' @author Vilmantas Gegzna

# @examples
# library(spHelper)
#
# # Load data
#      data("DataSet1")
#
# # Explore data
#      str(DataSet1)
#      table(DataSet1[,c("gr","ID")])
#      summary(DataSet1)
#
# # Explore functions
#      nFolds = 5
#
# # If variables of data frame are provided:
#      Folds1_a <- createFoldsBS(data = DataSet1,
#                               stratify_by = "gr", block_by = "ID",
#                                k = nFolds, returnTrain = FALSE)
#      # str(Folds1_a)
#      cvo_test_bs(Folds1_a, DataSet1, "gr", "ID")
#
# # If "free" variables are provided:
#      Folds1_b <- createFoldsBS(gr = DataSet1$gr, ID = DataSet1$ID,
#                                 k = nFolds, returnTrain = FALSE)
#      # str(Folds1_b)
#      cvo_test_bs(Folds1_b, DataSet1, "gr", "ID")
#
# # Not blocked but stratified
#      Folds1_c <- createFoldsBS(gr = DataSet1$gr, k=nFolds, returnTrain=FALSE)
#      # str(Folds1_c)
#      cvo_test_bs(Folds1_c, DataSet1, "gr", "ID")
#
# # Blocked but not stratified
#      Folds1_d <- createFoldsBS(block_by = DataSet1$ID, k = nFolds, returnTrain = FALSE)
#      # str(Folds1_d)
#      cvo_test_bs(Folds1_d, DataSet1, "gr", "ID")
#
#
# @param times An integer, indicating how many times crossvalidation should be repeated.
# @param seeds \link[base]{set.seed}

createFoldsBS <- function(data = NULL,
                          stratify_by = NULL,
                          block_by = NULL,
                          k = 5,
                          returnTrain = TRUE,
                          times = 1,
                          seeds = NULL,
                          gr = stratify_by,
                          ID = block_by
) {
    .Deprecated("cvo_create_folds")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (k < 2) stop("Number of folds `k` must be at least 2.")
    nFolds <- k
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Choose seeds for random number generation -------------------------------

    # If too few seeds are provided
    len_seeds <- length(seeds)

    if (!is.null(seeds) & (len_seeds < times) & (len_seeds > 1))
        warning("Number of provided `seeds` is not sufficient." %.+.%
                "Random `seeds` will be added.")

    # If just one seed is provided
    if (len_seeds == 1 & (len_seeds < times)) set.seed(seeds)

    # Generate seeds, if needed
    if (is.null(seeds) | (len_seeds < times)) {
        seeds <- c(seeds,
                   sample(-9e6:9e6, times - len_seeds)
        )
    }

    # If too many seeds are provided
    seeds <- rep_len(seeds, times)

    # Force default values, if needed ========================================
    force(data)
    force(stratify_by)
    force(block_by)

    force(gr)
    force(ID)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (inherits(data, "hyperSpec")) data <- data$..

    # Parse input and prepare data ===========================================
    # if `data` is provided:
    ID <- getVarValues(ID, data)
    gr <- getVarValues(gr, data)

    # If either `ID` or `gr` is not provided:
    if (is.null(ID) & length(gr) > 1) {
        ID <- 1:length(gr) # create unique IDs, if not blocked
    }

    if (is.null(gr) & length(ID) > 1) {
        gr <- rep(0, length(ID)) # create one level of `gr`, if not stratified
    }

    if (is.null(gr) & is.null(ID)) {
        N_ <- nrow(data) %if_null_or_len0% length(data)

        ID <- 1:N_       # create unique IDs, if not blocked
        gr <- rep(0, N_) # create one level of `gr`, if not stratified
    }

    sample_size <- length(ID)
    rm(data)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(ID) != length(gr))
        stop("Lengths of vectors `stratify_by` and `block_by` must agree.")
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # DF_all_ID <- data.frame(ID = ID, gr = gr)
    DF_all_ID <- data.frame(gr = gr,
                            ID = ID,
                            stringsAsFactors = FALSE)

    # get unique values only: for blocking

    DF_uni_ID <- unique(DF_all_ID)


    # Calculations  ==========================================================
    DF_uni_ID$Fold <- rep(NA, times = nrow(DF_uni_ID))
    nGr     <- DF_uni_ID$gr %>% as.factor %>% nlevels # NA's are not included

    DFuniID_ByGr <- split(DF_uni_ID, DF_uni_ID$gr)
    n_ByGr       <- sapply(DFuniID_ByGr, nrow)     # unique IDs per class

    # If Number of observatuions in a group is to small
    if (any(n_ByGr < nFolds)) {
        print(sprintf('nFolds = %d', nFolds))
        print(n_ByGr)
        stop("Number of UNIQUE observations in one of the groups" %.+.%
                 "is smaller than number of folds.")
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # For every repetition
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    fold_name_format <- paste0("Fold%0",
                               (log10(nFolds * times) %/% 1) + 1,
                               "g%s")

    for (i in 1:times) {
        set.seed(seeds[i])

        available_folds = (1:nFolds) + nFolds * (i - 1)
        REPETITION <- if (times == 1) "" else paste0("_Rep", i)

        # Assign numbers of fold to each row
        # Split to folds in a stratified way by group 'gr'
        for (gr_i in 1:nGr) {
            GrSize     <-  n_ByGr[gr_i]
            # modulus - how many times observations are divided
            #           proportionally to each fold.
            TimesEach  <-  GrSize %/% nFolds

            # reminder - number of observations, that cannot
            # be divided proportionally.
            nRem   <-  GrSize %%  nFolds

            # Separate permutations ensures more proportional distribution when
            # number of observations is small:

            # Create a list of proportionally distributed per fold
            Proportionals <-  rep(available_folds, times = TimesEach)
            # Permute the list of proportionally distributed
            Proportionals <-  sample(Proportionals, GrSize - nRem)
            # Permute reminders separately
            Reminders     <-  sample(available_folds, nRem)
            # Merge
            BelongsToFoldNr <- c(Proportionals, Reminders)
            DFuniID_ByGr[[gr_i]]$Fold <-
                sprintf(fold_name_format, BelongsToFoldNr, REPETITION)
        }

        # unsplit the dataframe: NA's removed
        # df_with_folds <- unsplit(DFuniID_ByGr, DF_uni_ID$gr[!is.na(DF_uni_ID$gr)])
        df_with_folds <- do.call("rbind", DFuniID_ByGr)


        data_i <- DF_all_ID %>%
            mutate(ORDER = seq_along(ID)) %>%
            merge(df_with_folds, by = "ID", sort = FALSE)  %>%
            arrange(ORDER)

        if (!all(data_i$ID == ID)) {
            warning(paste(
                "Order of indices does not match order of input data.",
                "This might be caused by NA values in the data."
               # , "Either IDs might be incorrectrly sorted inside function 'createFoldsBS'"
                ))
        }

        Ind_all <- 1:nrow(data_i)
        data_i$Test_ind <- Ind_all # Additional column with row numbers.
        # which are treated as indices for test
        # subset.

        DATA <- if (i == 1) data_i else rbind(DATA, data_i)
    }

    Test_ind <- split(DATA$Test_ind,
                      factor(DATA$Fold, levels = sort(unique(DATA$Fold))
                 )
    )

    # Before `return` -------------------------------------------------------
    # Choose which indices (test/train) to remove
    if (returnTrain == TRUE) {
        Train_ind  <- lapply(Test_ind, function(x) {setdiff(Ind_all, x)})
        ind_type   <- "Train"
        return_ind <- Train_ind

    } else {
        ind_type   <- "Test"
        return_ind <- Test_ind
    }

    validation_type <-
        if (times > 1) {
            "Repeated k-fold"
        } else if (times == 1) {
            "k-fold"
        }

    # Add attributtes
    attr(return_ind, "info") <-
        data.frame(
            indices     = ind_type,
            stratified  = nGr > 1,
            blocked     = any(duplicated(ID)),
            cv_type     = validation_type, # type of cross-validation
            k           = k,
            repetitions = times,
            sample_size = sample_size
            # cross_validation_type  = validation_type,

        )

    attr(return_ind, "seeds") <- seeds
    # -----------------------------------------------------------------------
    # Return
    return(return_ind)
}
# [END]
