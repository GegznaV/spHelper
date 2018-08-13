#' @rdname qplot_infoDim
#' @template same
#' @export

qplot_screeplot <- function(obj, ...) {
    qplot_infoDim(obj, ...)
}
#' @rdname qplot_infoDim
#' @export
qplot_scree <- function(obj, ...) {
    .Deprecated("qplot_screeplot")

    qplot_infoDim(obj, ...)
}
