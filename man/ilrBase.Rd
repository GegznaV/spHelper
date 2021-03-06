% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ilrBase.R
\name{ilrBase}
\alias{ilrBase}
\alias{ilrBase.default}
\alias{ilrBase.hyperSpec}
\title{[!] `ilrBase` method for `hyperSpec` object}
\usage{
ilrBase(x = NULL, z = NULL, D = NULL, method = "basic")

\method{ilrBase}{default}(x = NULL, z = NULL, D = NULL, method = "basic")

\method{ilrBase}{hyperSpec}(x = NULL, z = NULL, D = NULL, method = "basic")
}
\arguments{
\item{x}{ optional dataset or vector of compositions}

\item{z}{ optional dataset or vector containing ilr or ipt coordinates}

\item{D}{ number of parts of the simplex}

\item{method}{ method to build the basis, one of "basic", "balanced", "optimal"
           "PBhclust", "PBmaxvar" or "PBangprox"}
}
\description{
[!] `ilrBase` method for `hyperSpec` object
}
\note{
there is a bug in \pkg{compositions}, thus parameter
\code{D} might be irresponsive if used through package
\pkg{spHelper}. IIf \code{D} is necessary, consider using
\code{compositions::ilrBase} directly.
}
