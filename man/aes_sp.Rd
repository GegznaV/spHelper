% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/4_plotting_helpers.R
\name{aes_sp}
\alias{aes_sp}
\alias{aes_sp_string}
\alias{aes_sp_}
\title{Default aesthetics for plotting a hyperSpec object with ggplot2}
\usage{
aes_sp(...)

aes_sp_string(...)

aes_sp_(...)
}
\arguments{
\item{...}{List of name value pairs giving aesthetics to map to variables.
See \code{\link[ggplot2]{aes}()}.}
}
\description{
Convenience functions that automatically map default
\pkg{ggplot2} aesthetics and enamble easy plotting of
\code{hyperSpec} object with \pkg{ggplot2}. \cr
Default values of aesthetics are \cr \code{x = ".wavelength",
\cr y = "spc", \cr group = ".rownames"}.\cr
These functions can be used with in combination with
\code{\link[hyperSpec]{as.long.df}} and its wrapper \code{\link{ldf}}
that is more convenient for plotting.
}
\details{
Parameters entered to \code{aes_sp}, \code{aes_sp_} or
 \code{aes_sp_string} update and modify the list of mapped aesthetics.
If default aesthetic (x, y, or group) has to be changed or removed,
it shoud be done explicitly, e.g., \code{aes_sp(x = NULL)}.
See examples below.  \cr

 \code{aes_sp} accepts unquoted variable names. It is alternative
               to \code{\link[ggplot2]{aes}()}\cr

 \code{aes_sp_string} requires explicitly quoted variable names
                     with \code{""}. It is alternative to
                      \code{\link[ggplot2]{aes_string}()}\cr

\code{aes_sp_} is alias to \code{aes_sp_string}.
}
\examples{

aes_sp()
   ## * group -> .rownames
   ## * x     -> .wavelength
   ## * y     -> spc

# To add aesthetics:

aes_sp(color = length)
   ## * group  -> .rownames
   ## * x      -> .wavelength
   ## * y      -> spc
   ## * colour -> length


# To remove defaul values of aesthetics, do it expicitly:

aes_sp(x = NULL)
   ## * group -> .rownames
   ## * x     -> .wavelength


# To use `aes_sp` in combination with function `ldf`:

ggplot(ldf(Spectra2[1:10]), aes_sp()) + geom_line()

ggplot() + geom_line(aes_sp(), ldf(Spectra2[1:10]))

}
