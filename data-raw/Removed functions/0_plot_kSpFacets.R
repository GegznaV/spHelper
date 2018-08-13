# Komponentų spektrai (atskirai) ---------------------------------
#' @rdname qplot_kSp
#' @export
plot_kSpFacets <- function(...,Title = "Components (stacked)",
                            normalize = "auto", facets = TRUE) {
    p <- qplot_kSp(...,Title = Title, normalize = normalize, facets = facets)
    return(p)
}
