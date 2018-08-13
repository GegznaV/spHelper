#' @rdname qplot_prediction
#' @param by A grouping variable
#' @export

qplot_proximity  <- function(scores, by,
                             xLabel = paste("Projection", xproj),
                             yLabel = paste("Projection", yproj),
                             Title  = "Proximity of Groups",
                             subTitle = NULL,
                             palette = hyGet_palette(scores),
                             stat = c("chull","ellipse","none"),
                             MDS = c("metric","isoMDS"),
                             plot.scatter = TRUE,
                             alpha = 0.5,
                             k = 2,
                             xproj = 1,
                             yproj = 2) {
    by  <- getVarValues(by, scores)

    # Check palette - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    nPal <- length(palette)
    nColNeeded <-  nlevels(by)
    if (nPal < nColNeeded) {
        if (nPal > 0){
            warning(sprintf(paste("There are %d colors in provided palette",
                                   "and %d are needed, thus the DEFAULT colors",
                                   "will be used."),nPal,nColNeeded))
        }
        palette <- NULL

    }
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    qplot_prediction(scores,
                     Prediction = by,
                     Reference  = by,
                     xLabel = xLabel,
                     yLabel = yLabel,
                     Title  = Title,
                     subTitle = subTitle,
                     palette = palette,
                     stat = stat,
                     MDS = MDS,
                     plot.scatter = plot.scatter,
                     alpha = alpha,
                     k = k,
                     xproj = xproj,
                     yproj = yproj) +
        guides(shape = FALSE,
               color = guide_legend("Groups"))
}
