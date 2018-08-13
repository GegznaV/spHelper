#' ggplot Function for Visualizing Resampling Results of Models With 1 Tuned Parameter
#'
#' Function plots a \pkg{ggplot2} plot to visualize resampling results of \pkg{caret} models with 1 tuned parameter.
#'
#' @inheritParams caret::xyplot.resamples
#' @param LIMITS limits for y axis (numeric vector with 2 elements).
#'
#' @return A \code{ggplot2} object.
#' @export

qplot_resamples <- function (x,
                             data = NULL,
                             metric = x$metric, ...,
                             LIMITS = "auto") {
    if (!is.null(match.call()$data))
        warning("explicit 'data' specification ignored")
    if (x$control$method %in% c("oob", "LOOCV"))
        stop("Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling")

    resamp  <- x$resample
    tNames  <- gsub("^\\.", "", names(x$bestTune))
    mName   <- names(resamp)[names(resamp) %in% metric][1]
    # numVals <- unlist(lapply(resamp, function(u) length(unique(u))))

    # Set limits for y axis
    if (LIMITS == "auto") {
        RA <- range(x$resample[mName])
        lmin <- min(RA, 0)
        lmax <- max(RA, 1)
        LIMITS <- spPlot::ggLims(c(lmin,lmax))
    }

    # Plot
    ggplot(data = resamp, aes_string(tNames, mName)) +
        geom_point(aes(color = "Resamples", shape = "Resamples")) +
        stat_summary(aes(group=mName,color = "Mean"),
                     fun.y=mean, geom="line",
                     lty ="dashed",
                     show.legend = TRUE) +
        stat_summary(aes(group=mName,color = "Mean", shape = "Mean"),
                     fun.y=mean, geom="point",
                     show.legend = TRUE) +
        xlab(trained_RF$modelInfo$parameters$label) +
        LIMITS    +
        scale_colour_discrete(name  =" ",
                              breaks=c("Mean", "Resamples"),
                              labels=c("Mean", "Resamples")) +
        scale_shape_manual(name  =" ",
                           breaks=c("Mean", "Resamples"),
                           labels=c("Mean", "Resamples"),
                           values = c(4,19)) +
        ggtitle("Resampling results across tuning parameters")
}
