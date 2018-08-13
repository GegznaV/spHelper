# hyperSpec::subset does not work inside function and it's for cycle

#' [.] plot_sp_summary_gr
#'
#' @param sp
#' @param by
#' @param probs
#'
#' @return ...
#' @export
#'
#' @examples
plot_sp_summary_gr <- function(sp, by = "gr", probs = c(0,1)) {
    force(by)

    # Prepare data

    group_vector <- eval_glue("sp$`{by}`")
    group_names <- levels(as.factor(group_vector))

    # Find min and max
    aggr_sp <- aggregate(sp, group_vector, quantile, probs = probs[1:2])

    # Plot axes
    plotspc(sp, add = F, func = range, col = NA)

    # Plot data by groups
    for (i in 1:length(group_names)) {
        sp_i <- aggr_sp[aggr_sp$.aggregate == group_names[i],]

        plotspc(sp_i,
                # col = palette()[i],
                col = sp_i$.color,
                # fill = c(1,1),
                add = TRUE)
    }
    plot_hyPalette(sp, by, as.legend = T, Title = "")
}
