#' [!!!] Proximity plot created using multi-dimensiolal scaling (MDS)
#'
#' Plot a 2D scatterplot that represents multidimensionally scaled data.
#'
#' @template scores
#' @param Reference  Reference grpups. Either a variable name or a vector.
#' @param Prediction Predicted groups. Either a variable name or a vector.
#' @param stat A statistic to be added, either "chull" - convex hull (default),
#'        "ellipse" or "none"
#' @param type A type of data for scatterplot: either "prediction" (default)
#'        or "reference".
#' @param type.stat A type of data for \code{stat}: either  "prediction"
#'                  (default) or "reference".
#' @param xproj,yproj An integer: a number of MDS projection to plot on x, y axis
#'       (respectively).
#' @param MDS A type of Multi-Dimensional Scaling.
#' @param palette A color palete to be used. Default is `ggplot2` default palette.
#' @param plot.scatter Logical. If \code{true} (default), scatterplot
#'       (`geom_point`)is plotted.
#' @param alpha Transperency of points.
#' @template subtitle
#' @template labels
#' @inheritParams MASS::isoMDS
#'
#' @template ggplot
#' @export
#'
#' @author Vilmantas Gegzna
#' @family \pkg{spHelper} plots
#' @examples
#'
#' # Examples with a `hyperSpec` object:
#'
#' data(Scores2)
#' Scores2$Prediction <- sample(Scores2$gr)
#' Scores2 <- hyAdd_color(sp = Scores2, by = "gr", palette = c("tan3", "green4","skyblue"))
#'
#' qplot_prediction(Scores2,Prediction = "Prediction", Reference = "gr")
#' qplot_prediction(Scores2,Prediction = "Prediction", Reference = "gr", type = "ref")
#'
#' qplot_prediction(Scores2,"Prediction","gr", type.stat = "ref", MDS = "isoMDS")
#'
#'
#' sc <- Scores2[,,c(1,3),wl.index = TRUE]
#'
#'
#' qplot_prediction(sc,"Prediction","gr", type = "reference")
#' qplot_prediction(sc,"Prediction","gr", type = "prediction")
#' qplot_prediction(sc,"Prediction","gr", type = "prediction", type.stat = "ref")
#' qplot_prediction(sc,"Prediction","gr", type = "prediction", type.stat = "ref",stat ="ellipse")
#'
#'
#' sc <- Scores2[,,c(1,2),wl.index = TRUE]
#' sc <- hyAdd_color(sp = sc , by = "class", palette = c("tan3", "green4","skyblue","violet"))
#' sc$ID <- rownames(sc)
#'
#' qplot_proximity(sc, "class")
#' qplot_proximity(sc, "class",  plot.scatter = FALSE) + geom_text(aes(label = sc$ID))
#'

qplot_prediction   <- function(scores,
                       Prediction,
                       Reference,
                       xLabel = paste("Projection", xproj),
                       yLabel = paste("Projection", yproj),
                       Title  = "Proximity of Groups",
                       subTitle = NULL,
                       palette = hyGet_palette(scores),
                       type = c("Prediction", "Reference"),
                       stat = c("chull","ellipse","none"),
                       type.stat = type,
                       MDS = c("metric","isoMDS"),
                       plot.scatter = TRUE,
                       alpha = 0.5,
                       k = 2,
                       xproj = 1,
                       yproj = 2) {
# Get input variables
    try(Prediction <- getVarValues(Prediction, scores), silent = TRUE)
    try(Reference  <- getVarValues(Reference,  scores), silent = TRUE)
    # Check palette - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    nPal <- length(palette)
    nColNeeded <-  max(nlevels(Prediction), nlevels(Reference))
    if (nPal < nColNeeded) {
        if (nPal > 0){
            warning(sprintf(paste("There are %d colors in provided palette",
                                  "and %d are needed, thus the DEFAULT colors",
                                  "will be used."),nPal,nColNeeded))
        }
        palette <- NULL

    }
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    scores <- if (class(scores) == "hyperSpec") hy2mat(scores) else as.matrix(scores)

    type      <- match.arg(fCap(type[1]),      c("Prediction", "Reference"))
    type.stat <- match.arg(fCap(type.stat[1]), c("Prediction", "Reference"))

# Prepare data:
    scores <- scale(scores)
    # PredictorNames <- paste(colnames(scores), collapse = ", ")

    is_misclassified <- factor(Reference !=  Prediction,
                               c("TRUE","FALSE"),c("Misclassified","Correct"))



        # Prediction       <- rep(NA, length(Reference))
        # is_misclassified <- as.factor(rep(FALSE, length(Reference)))
        # type      <- "Groups"
        # type.stat <- "Groups"
        # shGuide   <- FALSE


# Do MDS
    d   <- dist(scores) # Euclidean distances between the rows
    fit <- switch(MDS[1],
                 isoMDS = MASS::isoMDS(d = d, k = k, trace = FALSE),
                 cmdscale(d = d,eig = TRUE, k = k) # k is the number of dim
    )

# Create a data frame
    data <-  data.frame(x = fit$points[,xproj],
                        y = fit$points[,yproj],
               Prediction = Prediction,
                Reference = Reference,
           Classification = is_misclassified
    )

# Plot
    p <- ggplot(data = data, aes_string(x = "x", y = "y", color = type))

    p <- p +
        scale_shape_manual(values = c("Misclassified" = 4, "Correct" = 19)) +
        theme_bw() +
        labs(x = xLabel,
             y = yLabel,
             title = subt(Title, subTitle))

    if (plot.scatter == TRUE) p <- p + geom_point(aes(shape = Classification),
                                                  alpha = alpha)

    # subTitle <- paste("Predictors:", PredictorNames)

    switch(stat[1],
           chull   = {p <- p + stat_chull(  aes_string(color = type.stat),fill = NA)},
           ellipse = {p <- p + stat_ellipse(aes_string(color = type.stat))}
    )

    if (!is.null(palette)) p <- p + scale_color_manual(values = palette)

    return(p)
}
