% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.OOIBase32.header.R
\docType{data}
\name{file}
\alias{file}
\title{[!] Read header lines of OOI Base32 file}
\format{
An object of class \code{character} of length 1.
}
\usage{
file
}
\arguments{
\item{file}{The name of the file which header lines are to be read from.}

\item{dec}{The character used in the file for decimal points. Default is
a point (\code{.}).}

\item{n}{Number of lines to be read. Default is 17.}

\item{last_headerline_text}{The text, that indicates the last line of the header
lines and the beggining of spectroscopic information.
 Default is \code{">>>>>Begin Spectral Data<<<<<"}.}

\item{text}{Text of header as if it was read with function
\code{\link[base]{readLines}}. If \code{text} is provided, \code{file}
is ignored.}
}
\value{
A list with 2 enties:
\enumerate{
     \item{$data}{A dataframe with information, that contains these variable names: \cr

1                        Date \cr
2                        Time \cr
3                 Graph_Title \cr
4                        User \cr
5  Spectrometer_Serial_number \cr
6           Spectrometer_Type \cr
7                    ADC_Type \cr
8        Spectrometer_Channel \cr
9               software_info \cr
10           Integration_time \cr
11     Integration_time_Units \cr
12             Scans_averaged \cr
13               Boxcar_width \cr
14   Electric_dark_correction \cr
15            Time_Normalized \cr
16        Dual_beam_Reference \cr
17          Reference_Channel \cr
18                Temperature \cr
19       n_pixels_in_spectrum}
     \item{$last_header_line} {The number of the last header line}
}
}
\description{
Read header lines with non-spectroscopic information of OOI Base32
(version 2.0.0.5) file.
}
\examples{

\donttest{
\dontrun{

 read.OOIBase32.header("MySpectra.Scope")
 read.OOIBase32.header("MySpectra.Scope")$data

}}


## OOIBase32 Version 2.0.0.5 Data File:

#
#      OOIBase32 Version 2.0.0.5 Data File
#      ++++++++++++++++++++++++++++++++++++
#      Date: 03-04-2014, 15:00:04
#      User: Valued Ocean Optics Customer
#      Spectrometer Serial Number:
#      Spectrometer Channel: Master
#      Integration Time (msec): 5000
#      Spectra Averaged: 1
#      Boxcar Smoothing: 0
#      Correct for Electrical Dark: Enabled
#      Time Normalized: Disabled
#      Dual-beam Reference: Disabled
#      Reference Channel: Master
#      Temperature: Not acquired
#      Spectrometer Type: S2000
#      ADC Type: USB2000
#      Number of Pixels in File: 2048
#      Graph Title:
#      >>>>>Begin Spectral Data<<<<<

}
\seealso{
Other \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}: 
\code{\link{IQR_outliers}()},
\code{\link{binning}()},
\code{\link{gapDer}()},
\code{\link{hy2mat}()},
\code{\link{hyAdd_Label_wl}()},
\code{\link{hyAdd_Labels_PAP_PD_2014}()},
\code{\link{hyAdd_Labels_TD2009}()},
\code{\link{hyAdd}()},
\code{\link{hyDrop_NA}()},
\code{\link{hyGet_palette}()},
\code{\link{hyRm_palette}()},
\code{\link{mad_outliers}()},
\code{\link{mean_Nsd}()},
\code{\link{median_Nmad}()},
\code{\link{plot_hyPalette}()},
\code{\link{prepare_PAP_RK_2014__MATLAB_failui}()},
\code{\link{read.OOIBase32}()},
\code{\link{read.OceanView.header}()},
\code{\link{read.OceanView}()},
\code{\link{read.sp.csv2}()},
\code{\link{read3csv2hy}()},
\code{\link{replace_spc}()},
\code{\link{sd_outliers}()},
\code{\link{spStat}()},
\code{\link{sp_class_perform}()}

Other functions to read spectroscopic data: 
\code{\link{read.OOIBase32}()},
\code{\link{read.OceanView.header}()},
\code{\link{read.OceanView}()}
}
\author{
Vilmantas Gegzna
}
\concept{\pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}}
\concept{functions to read spectroscopic data}
\keyword{datasets}
