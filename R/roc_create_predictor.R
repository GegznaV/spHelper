#' [!!!] roc_create_predictor
#'
#' @param obj object
#' @param ... pass to further methods
#' @export
roc_extract_info <- function(obj, ...) {
    UseMethod("roc_create_predictor")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname roc_extract_info
#' @export
roc_extract_info.manyroc_result <- function(obj, ...) {

    put_smaller_first <- function(pos_is_larger, pos, neg) {
        if (pos_is_larger)
            c(neg, pos)
        else
            c(pos, neg)
    }

    obj2 <- obj %>%
        tidyr::separate(
            compared_groups,
            into = c("neg", "pos"),
            sep = " vs\\. ",
            remove = FALSE
        ) %>%
        dplyr::mutate(pos_is_larger = median_neg < median_pos) %>%
        dplyr::select(feature,
                      compared_groups,
                      median_neg,
                      cutoff,
                      median_pos,
                      pos_is_larger,
                      neg,
                      pos)

    obj2 %$% # list(pos_is_larger, pos, neg)
        purrr::pmap(list(pos_is_larger, pos, neg),
                    put_smaller_first)  %>%
        purrr::reduce(rbind)  %>%
        magrittr::set_colnames(c("below", "above")) %>%
        tibble::as.tibble()  %>%
        dplyr::bind_cols(obj2, .)  %>%
        dplyr::select(feature,
                      compared_groups,
                      neg,
                      pos,
                      median_neg,
                      median_pos,
                      below,
                      cutoff,
                      above
                      ) %>%
        class_add(c("manyroc_info", "roc_df"))
}
