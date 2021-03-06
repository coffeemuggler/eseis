% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aux_eseisobspy.R
\name{aux_eseisobspy}
\alias{aux_eseisobspy}
\title{Convert eseis object to ObsPy stream object}
\usage{
aux_eseisobspy(data)
}
\arguments{
\item{data}{\code{eseis} object, \code{list} element.}
}
\value{
\code{ObsPy} stream object as defined by the architecture of 
package 'reticulate'.
}
\description{
The function converts an eseis object to an ObsPy stream object. The 
functionality is mainly useful when running ObsPy through R using the 
package 'reticulate'. Currently, only single traces (i.e., single eseis 
objects) can be converted. Thus, to convert multiple traces, these need 
to be converted individually and added to the first trace using ObsPy 
functionalities.
}
\examples{

\dontrun{

## load ObsPy library with package 'reticulate'
## (requires ObsPy to be installed on the computer)
obspy <- reticulate::import("obspy")

## load example data set
data(rockfall)

## convert example eseis object to ObsPy stream object
x <- aux_eseisobspy(data = rockfall_eseis)

## filter data set using ObsPy
x_filter <- obspy$traces[[1]]$filter(type = "bandpass", 
                                     freqmin = 0.5, 
                                     freqmax = 1.0)
                                     
## plot filtered trace using ObsPy plotting routine
x$traces[[1]]$plot()

}

}
\author{
Michael Dietze
}
\keyword{eseis}
