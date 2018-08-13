#' @rdname qplot_kSp
#' @export
plot_kSp <- function(loadings,
                     Title = "Components",
                     xLabel = labels(loadings, ".wavelength"),
                     yLabel = labels(loadings, "spc"),
                     names.in  = 'kNames',
                     legendName = FALSE,
                     filled = TRUE,
                     normalize  = FALSE,
                     facets = FALSE,
                     subTitle = NULL) {
    qplot_kSp(loadings = loadings,
             Title = Title,
             xLabel = xLabel,
             yLabel = yLabel,
             names.in = names.in,
             legendName = legendName,
             filled = filled,
             normalize = normalize,
             facets = facets,
             subTitle = subTitle)
}
