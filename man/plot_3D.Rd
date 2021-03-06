% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_3D.R
\name{plot_3D}
\alias{plot_3D}
\alias{mds2mat}
\alias{SMACOF2mat}
\alias{tSNE2mat}
\alias{make_3D}
\title{[.] Plot 3D scatterplot}
\usage{
plot_3D(
  data,
  by = NULL,
  text = NULL,
  title = "3D scatterplot",
  add.space = 0.2,
  scene = list(camera = list(eye = list(x = 1.1, y = -1.1, z = 1)))
)

mds2mat(fit)

SMACOF2mat(fit)

tSNE2mat(fit)

make_3D(df, title = NULL, scene = list(), add.space = 0.2)
}
\arguments{
\item{data}{Matrix with 3 columns (size Nx3).}

\item{by}{(Nx1 vector) grouping variable group with names  for each row in \code{data}.}

\item{text}{(Nx1 vector) charter vector with text hor each row in \code{data}.}

\item{title}{The main title of the plot.}

\item{add.space}{(numeric)  fraction of space to be added (value between 0 and 1). Default is 0.2, i.e. 10\%.}

\item{scene}{Description in \url{https://plot.ly/r/reference/#scatter3d-scene}
specification of parameters in \url{https://plot.ly/r/reference/#layout-scene}.}

\item{fit}{An object drim which Nx3 matrix will be extracted. The object is
 created with one of the following functions \cr
\code{\link[base]{cmdscale}}, \code{\link[MASS]{ISOmap}},
            \code{\link[MASS]{sammon}} or similar for \code{mds2mat}();\cr
\code{\link[SMACOF]{mds}} in \pkg{SMACOF} for \code{SMACOF2mat}();\cr
\code{\link[Rtsne]{Rtsne}} in \pkg{Rtsne} for \code{tSNE2mat}().\cr}

\item{df}{A data frame with variables named $x, $y, $z, $x,
$text, $group.}
}
\value{
\code{plotly} object.
}
\description{
Convenience functions to make 3D scatterplot with \pkg{plotly}.
}
\examples{

## NOT YET INCLUDED

}
