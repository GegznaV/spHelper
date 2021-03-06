% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_LOF_by_class.R
\name{get_LOF_by_class}
\alias{get_LOF_by_class}
\title{Calculate lack_of_fit (LOF) for spectroscopic data by class}
\usage{
get_LOF_by_class(obj, by, FUN = median)
}
\arguments{
\item{obj}{- hyperSpec object.}

\item{by}{- string with name of factor variable in `obj`.}

\item{FUN}{- function used to calculate expected values.}
}
\value{
Function returns a list of 4 objects:\cr
        data - object `obj`;\cr
        centers - hyperSpec object in which variable $spc is matrix with
                  expected values (means, medians, etc. indicated by `FUN`)
                  for every row;\cr
        difference - hyperSpec object in which variable $spc is matrix with
                     differences between observed and expected values;\cr
        LOF - vector of lack-of-fit values for every row.
}
\description{
Calculate lack_of_fit (LOF) for spectroscopic data by class
}
\examples{
library(spHelper)
obj0 <- get_LOF_by_class(obj = Spectra2, by = "gr")
str(obj0, max.level = 1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Unit tests:
#
#  require(testthat)
# test_that("classes and their centers are sorted correctly",{
# require(hyperSpec)
# set.seed(1)
# obj <- hyperSpec::sample(Spectra2, 150)
#
# LOF_obj <- get_LOF_by_class(obj, "gr")
# N_unique <- (!duplicated(
#     LOF_obj$centers[,c("gr","spc")]  \%>\%
#    as.wide.df)) \%>\%  sum
# expect_equal(N_unique, nlevels(obj$gr))
#  })
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}
\seealso{
Other LOF_obj: 
\code{\link{plot_LOF_MDS}()},
\code{\link{plot_LOF_hist}()},
\code{\link{plot_LOF_sp}()},
\code{\link{plot_stacked}()}
}
\concept{LOF_obj}
