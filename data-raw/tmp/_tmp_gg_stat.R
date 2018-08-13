#  ------------------------------------------------------------------------


stat_spSummary <- function(mapping = NULL, data = NULL, geom = "line",
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, FUN = mean, ...)
{
    spSummary <- ggplot2::ggproto("spSummary", Stat,
                                  compute_group = function(data, scales) {
                                      data[FUN(data$y), , drop = FALSE]
                                  },

                                  required_aes = c("x", "y")
    )
    layer(
        stat = spSummary, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm,...)
    )
}
