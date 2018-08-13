# # @rdname roc_performance
# # @export
#
#
#
# roc_add_metrics <- function(roc_el, ...) {
#     roc_m <- roc_calculate_metrics(roc_el)
#     cls <- class(roc_el$roc_elements)
#     roc_el$roc_elements <-
#         class_add(
#             cbind(roc_el$roc_elements, roc_m),
#             c("roc_extended", cls)
#         )
#
#     ind_k <- attr(roc_m, "max_perf_row_index")$kappa
#     ind_b <- attr(roc_m, "max_perf_row_index")$bac
#     ind_j <- attr(roc_m, "max_perf_row_index")$youden
#     roc_el$best_kappa_row  <- roc_el$roc_elements[ind_k, ]
#     roc_el$best_bac_row    <- roc_el$roc_elements[ind_b, ]
#     roc_el$best_youden_row <- roc_el$roc_elements[ind_j, ]
#
#     roc_el
# }
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# roc_replace_with_metrics <- function(roc_el, ...) {
#     roc_m <- roc_calculate_metrics(roc_el)
#     ind_rm <- -c(2:5)
#     roc_el$roc_elements <-
#         class_add(
#             cbind(roc_el$roc_elements, roc_m)[, ind_rm],
#             c("roc_performance", "roc_elements")
#         )
#
#     ind_k <- attr(roc_m, "max_perf_row_index")$kappa
#     ind_b <- attr(roc_m, "max_perf_row_index")$bac
#     ind_j <- attr(roc_m, "max_perf_row_index")$youden
#     roc_el$best_kappa_row  <- roc_el$roc_elements[ind_k, ]
#     roc_el$best_bac_row    <- roc_el$roc_elements[ind_b, ]
#     roc_el$best_youden_row <- roc_el$roc_elements[ind_j, ]
#
#
#     roc_el
# }
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # @rdname roc_performance
# # @param what one of "bac", "kappa", "youden"
# # @export
# roc_compute_best <- function(x, gr, pos_label, optimize_by = "bac", pos_larger = NULL, ...) {
#     roc_el <- roc_analysis(x, gr, pos_label, pos_larger)
#
#     switch(tolower(optimize_by),
#            bac    = roc_replace_with_metrics(roc_el)$best_bac_row,
#            kappa  = roc_replace_with_metrics(roc_el)$best_kappa_row,
#            youden = roc_replace_with_metrics(roc_el)$best_youden_row,
#            stop('Incorrect value of `optimize_by`.\nMust be one of "bac", "kappa", "youden"\n'))
#
# }
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# # @rdname roc_performance
# # @export
# roc_calculate_metrics <- function(roc_el, digits = 3) {
#     TP <- roc_tp(roc_el)
#     FP <- roc_fp(roc_el)
#     FN <- roc_fn(roc_el)
#     TN <- roc_tn(roc_el)
#
#     SE <- calculate_sensitivity(TP, FN)
#     SP <- calculate_specificity(TN, FP)
#
#     # bac = calculate_bac(SE, SP)
#     kappa  = calculate_kappa(TP, FN, FP, TN)
#     youden = calculate_youdens_j(SE, SP)
#
#     mat <- cbind(auc = calculate_auc(SE, SP),
#                  ppv = calculate_ppv(TP, FP),
#                  npv = calculate_npv(TN, FN),
#                  # acc = calculate_acc(TP, FN, FP, TN),
#                  kappa = kappa,
#                  # wkappa = calculate_wkappa(TP, FN, FP, TN),
#                  youden = youden
#     )
#
#     # attr(mat, "rank_bac")   = rank(-bac,   ties.method = "min")
#     # attr(mat, "rank_kappa") = rank(-kappa, ties.method = "min")
#     attr(mat, "max_perf_row_index") = list(kappa = which(kappa == max(kappa)),
#                                            bac = which(bac == max(bac)),
#                                            youden = which(youden == max(youden))
#     )
#
#     round(mat, digits = digits)
# }
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # @rdname roc_performance
# # @export
# roc_get_performance <- function(roc_el, measure) {
#     switch(tolower(measure),
#
#            # Acc - accuracy
#            acc  = calculate_max(roc_el, Acc),
#
#            # AUC Area unther the ROC curve
#            auc = c(value = AUC(roc_el), cutoff = NA),
#
#            # BAC - balanced accuracy: mean of sensitivity and specificity
#            bac  = calculate_max(roc_el, Bac),
#
#            # Youden's index
#            j =  calculate_max(roc_el, J),
#
#            # Cohen's kappa
#            kappa = calculate_max(roc_el, Kappa),
#
#            # Weighted Cohen's kappa
#            wkappa = calculate_max(roc_el, Wkappa),
#
#            stop(glue::glue("Unsuported performance measure: {measure}"))
#     )
# }
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # @rdname roc_performance
# # @param FUN Function to apply (one of
# # \code{\link{Acc}},
# # \code{\link{Bac}},
# # \code{\link{J}},
# # \code{\link{Kappa}},
# # \code{\link{Wkappa}},
# #  etc.)
# # @export
#
# roc_get_max <- function(roc_el, FUN){
#     val <- FUN(roc_el)
#     max_i <- which.max(val)
#     c(value  = val[max_i],
#       cutoff = cutoff(roc_el)[max_i])
# }


# =============================================================================
#
#         # =============================================================================
# Select feature

#
# # Calculate classification performance
#
# roc_el <- roc_analysis(x_subset[, "625.678"],
#                        included_gr,
#                        pos_label = included_levels[2],
#                        optimize_by = optimize_by,
#                        results = "optimal")


# =============================================================================
# Helper functions
# =============================================================================
#' Add an additional S3 class label to an object
#'
#' @param x An object to modify.
#' @param new_class (\code{character})\cr A name of a new class.
#'                   May be a vector of names.
#'
#' @export
#'
#' @examples
#' class_add(list("ok"), "ok_list")
#'
class_add <- function(x, new_class) {
    assert_character(new_class)
    class(x) <- union(c(new_class, class(x)), class(x))
    x
}

# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Add an additional S3 class label to an object
# #
# # @param x An object to modify.
# # @param old_class (\code{character})\cr A name of a class to rmove.
# #                   May be a vector of names.
# #
# # @export
# #
# class_remove <- function(x, old_class) {
#     assert_character(old_class)
#     all_classes <- class(x)
#     assert_subset(old_class, all_classes)
#
#     class(x) <- setdiff(all_classes, old_class)
#     x
# }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head_tail <- function(x, top = 4, bottom = 4, from = 1, to = NULL, digits = 3,
                      hlength = 4, tlength = 4, ellipsis = TRUE)
{
    # [!!!] This function based on psych::headTail, It must be rewritten in the future

    if (is.data.frame(x) | is.matrix(x)) {
        if (is.matrix(x))
            x <- data.frame(unclass(x))
        nvar <- dim(x)[2]
        hlength <- top
        tlength <- bottom
        if (is.null(to)) to <- nvar
        dots <- rep("...", nvar)
        h <- data.frame(head(x[from:to], hlength))
        t <- data.frame(tail(x[from:to], tlength))

        for (i in 1:nvar) {
            if (is.numeric(h[1, i])) {
                h[i] <- signif(h[i], digits)
                t[i] <- signif(t[i], digits)
                # h[i] <- round(h[i], digits)
                # t[i] <- round(t[i], digits)
            } else {
                dots[i] <- NA
            }
        }
        if (ellipsis) {
            head.tail <- rbind(h, ... = dots, t)
        } else {
            head.tail <- rbind(h, t)
        }
    } else {
        h <- head(x, hlength)
        t <- tail(x, tlength)
        if (ellipsis) {
            head.tail <- rbind(h, "...       ...", t)
        } else {
            head.tail <- rbind(h, t)
            head.tail <- as.matrix(head.tail)
        }
    }
    return(head.tail)
}
