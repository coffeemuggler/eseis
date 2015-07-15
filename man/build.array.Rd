\name{build.array}
\alias{build.array}
\title{Function to build an empty data array.}
\description{This function builds an empty structure for storing data from a seismic
array.}
\usage{build.array(n.stations, n.components = 4, summary = TRUE, metadata = TRUE, 
    spectrum = TRUE, event = TRUE)}
\arguments{
  \item{n.stations}{Numeric \code{scalar}, number of stations that form the array.}
  \item{n.components}{Numeric \code{scalar}, number of signal components per station. Can
be one out of 1 (x), 2 (x, y), 3 (x, y, z) and 4 (x, y, z, x-y-z), 
default is 4.}
  \item{summary}{Logical \code{scalar}, option to build a summarising structure that
contains essential common information about the array, i.e. \code{dt}.}
  \item{metadata}{Logical \code{scalar}, option to build structure for station-wise  
metadata, default is \code{TRUE}.}
  \item{spectrum}{Logical \code{scalar}, option to build structure for spectral data, 
default is \code{TRUE}.}
  \item{event}{Logical \code{scalar}, option to build structure for detected events.}
}
\details{The array structure is used for convenient and comprehensive analysis of
an array of seismic stations. Most of the package function support this
array object as input data and perform the desired tasks with the entire 
(or parts of it, if specified) data set. The name array may not be 
confused with the R data structure \code{\link{array}}.}
\value{A list object.}

\author{Michael Dietze}




\examples{
## build a simple array
array <- build.array(n.stations = 1)

## build a complete array
array <- build.array(n.stations = 2, 
                     n.components = 3, 
                     summary = TRUE,
                     metadata = TRUE,
                     spectrum = TRUE,
                     event = TRUE)

## show struture of the array
str(array)
}

\keyword{environmental}
\keyword{seismology}
