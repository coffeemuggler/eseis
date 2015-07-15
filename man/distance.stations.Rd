\name{distance.stations}
\alias{distance.stations}
\title{Function to calculate interstation distances}
\description{This function calculates the distances between seismic stations. It 
supports topography-corrected calculations.}
\usage{distance.stations(stations, DEM)}
\arguments{
  \item{stations}{Numeric \code{matrix} x-, y-, z-coordinates of the seismic stations. If
a DEM is provided station coordinates may be limited to x- and y-values.}
  \item{DEM}{Numeric \code{matrix}, or \code{raster} object, digital elevation model
used for topography correction. if omitted, direct distances between 
stations will be calculated. The coordinate system and projection of the 
station coordinates will be set to those of the DEM. The z-coordinates 
of the stations will be taken from the DEM.}
}

\value{A numeric matrix with station distances}

\author{Michael Dietze}




\examples{
## no examples present!
}

\keyword{environmental}
\keyword{seismology}
