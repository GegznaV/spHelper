#  ------------------------------------------------------------------------
#' @rdname qplot_stat
#' @template same
#' @export
plot_stat <- function(sp, by, FUN = mean, ...) {
    # warning('Use `plot_stat` instead of `qplotStat`')
    qplot_stat(sp, by, FUN = FUN, ...)
}
