roc_predictor_list <-
    split(roc_predictor, seq_len(nrow(roc_predictor)))

x_train_list <- as.list(as.data.frame(x_train))

mat2list(x_train)


mat2list <- function(x) {
    as.list(as.data.frame(x))
}
gr_train

length(roc_predictor_list)
mat2list(x_train)
split(roc_predictor, roc_predictor$feature)

predictor_obj <- roc_predictor_list[[1]]

split_by_feature <- function(obj) {
    res <- split(obj, obj$feature)
    class_add(res, c("roc_info", "roc_df"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
predictor_obj <- split_by_feature(roc_predictor)[[1]][1,]
x_new  <- x_train_list[[1]]
gr_new <- gr_train
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(tidyr)
library(dplyr)

subplot(
    (roc_predictor %>%
         filter(compared_groups == compared_groups[1]) %>%
         gather(key = "type", value = "intensity", median_neg, cutoff, median_pos) %>%
         ggplot(aes(as.numeric(feature), intensity, color = type)) +
         geom_line() +
         geom_point(shape = 5) +
         scale_color_brewer(palette = 7, type = "qual"))  %>%
        ggplotly(),

    (rez_train[[i]] %>%
         filter(compared_groups == compared_groups[1]) %>%
         gather(key = "type", value = "intensity",  BAC, Kappa,  AUC)  %>%
         ggplot(aes(as.numeric(feature), intensity, color = type)) +
         geom_line() +
         geom_point() + lims(y = c(0, 1)))  %>%
        ggplotly(),

    nrows = 2,
    shareX = TRUE
)

