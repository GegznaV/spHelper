# # Code from ggplot2::facet_grid
#
#
# facets <- var1 ~ var2
#
# is.formula <- function(x)   inherits(x, "formula")
#
#
# if (is.character(facets)) {
#     facets <- stats::as.formula(facets)
# }
# if (is.formula(facets)) {
#     lhs <- function(x) if (length(x) == 2) NULL else {x[-3]}
#
#     rhs <- function(x) if (length(x) == 2) x    else x[-2]
#
#     rows <- plyr::as.quoted(lhs(facets))
#     rows <- rows[!sapply(rows, identical, as.name("."))]
#     cols <- plyr::as.quoted(rhs(facets))
#     cols <- cols[!sapply(cols, identical, as.name("."))]
# }
# if (is.list(facets)) {
#     rows <- as.quoted(facets[[1]])
#     cols <- as.quoted(facets[[2]])
# }
