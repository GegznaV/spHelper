% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qplot_spRangeMedian.R
\name{qplot_spRangeMedian}
\alias{qplot_spRangeMedian}
\title{[!] Plot Median and range of spectroscopic data intensities}
\usage{
qplot_spRangeMedian(
  sp,
  by = NULL,
  percent = NULL,
  probs = NULL,
  palette = hyGet_palette(sp),
  ...,
  lwd_median = 1,
  legend_title = by,
  alpha = 0.2,
  add = FALSE,
  name_if_by_is_NULL = "All Data"
)
}
\arguments{
\item{sp}{\code{\link[=hyperSpec-class]{hyperSpec}} object.}

\item{by}{A vector (factor variable) with indicated groups for each case:
either variable name inside the object \code{sp}, or a vector
of length \code{nrow(sp)}.}

\item{percent}{Numeric value between 0 and 100 that indicates
\emph{percentage} of samples nearest to median be plotted as a ribbon.}

\item{probs}{verctor of size 2 with values between 0 and 1 that indicates
\emph{probabilities} at which percentiles should be plotted.}

\item{...}{further arguments to \code{\link[ggplot2]{geom_ribon}}.}

\item{legend_title}{(character|NULL) The title for legend.}

\item{alpha}{(numeric from 0 to 1) transperency.}

\item{add}{(logical) add plot to an existing ggplot object?}
}
\value{
A \code{\link[ggplot2]{ggplot}} object.
       More details in package \pkg{ggplot2}.
}
\description{
Plot range of y axis values (usually intensities) of spectroscopic data
between selected percentilles.\cr
Function \code{qplot_spRange()} returns whole ggplot and
function \code{layer_spRange()} returns a layer which can be added to a ggplot.
}
\examples{
library(spHelper)
library(spPlot)

qplot_spRangeMedian(Spectra2)
qplot_spRangeMedian(Spectra2, "gr")

# Both lines below gives identical plots with different default titles:
qplot_spRangeMedian(Spectra2, "gr", percent = 50)
qplot_spRangeMedian(Spectra2, "gr", probs = c(.25, .75))


ggplotly_tidy()
}
\seealso{
Other \pkg{spHelper} plots: 
\code{\link{check_palette}()},
\code{\link{layer_spRangeMean}()},
\code{\link{plot_colors}()},
\code{\link{plot_hyPalette}()},
\code{\link{plot_spCompare}()},
\code{\link{plot_spDiff}()},
\code{\link{plot_spDistribution}()},
\code{\link{qplot_confusion}()},
\code{\link{qplot_crosstab}()},
\code{\link{qplot_infoDim}()},
\code{\link{qplot_kAmp}()},
\code{\link{qplot_kSp}()},
\code{\link{qplot_prediction}()},
\code{\link{qplot_spRangeCenter}()},
\code{\link{qplot_spStat}()},
\code{\link{qplot_spc}()},
\code{\link{rmExpr}()},
\code{\link{rm_stripes}()},
\code{\link{stat_chull}()}
}
\author{
Vilmantas Gegzna
}
\concept{\pkg{spHelper} plots}
