# Komponentų spektrai (atskirai) ---------------------------------
#' @rdname qplot_kSp
#' @template same
#' @export
qplot_kSpFacets <- function(...,Title = "Components (stacked)",
                           normalize = "auto", facets = TRUE) {
    p <- qplot_kSp(...,Title = Title, normalize = normalize, facets = facets)
    return(p)
}


