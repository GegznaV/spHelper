% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_functions.R
\name{poisson_lambda_CI}
\alias{poisson_lambda_CI}
\title{Confidence interval for parameter lambda of Poisson distribution}
\usage{
poisson_lambda_CI(
  x = NULL,
  lambda = mean(x),
  n = length(x),
  Sn = sum(x),
  conf.level = 0.95
)
}
\arguments{
\item{x}{data}

\item{conf.level}{confidence level, number between 0 and 1, usually 0.95.}
}
\value{
List with results... ???
}
\description{
Confidence interval for parameter lambda of Poisson distribution
}
\examples{

library(spHelper)

x <- c(3,0,2,1,0,4,3,2,1,2)
poisson_lambda_CI(x)


}
\seealso{
Other statistical functions: 
\code{\link{median_CI}()},
\code{\link{normal_var_CI}()},
\code{\link{quartiles}()}
}
\concept{statistical functions}
