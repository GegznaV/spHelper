% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qplot_resamples.R
\name{qplot_resamples}
\alias{qplot_resamples}
\title{ggplot Function for Visualizing Resampling Results of Models With 1 Tuned Parameter}
\usage{
qplot_resamples(x, data = NULL, metric = x$metric, ..., LIMITS = "auto")
}
\arguments{
\item{LIMITS}{limits for y axis (numeric vector with 2 elements).}
}
\value{
A \code{ggplot2} object.
}
\description{
Function plots a \pkg{ggplot2} plot to visualize resampling results of \pkg{caret} models with 1 tuned parameter.
}
