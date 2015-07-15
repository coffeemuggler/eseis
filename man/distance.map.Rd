\name{distance.map}
\alias{distance.map}
\title{Function to calculate a distance map}
\description{This function calculates the distances between grid-cells and seismic
stations.}
\usage{distance.map(stations, DEM, topography.correction = TRUE)}
\arguments{
  \item{stations}{Numeric \code{matrix} x-, y-, z-coordinates of the seismic stations. If
a DEM is provided station coordinates may be limited to x- and y-values.}
  \item{DEM}{Numeric \code{matrix}, or \code{raster} object, digital elevation model
used to calculate the distance of eachpixel to each station. The 
coordinate system and projection of the station coordinates will be set 
to those of the DEM. If station coordinates do not have a z-coordinate, 
this z-coordinate will be taken from the DEM.}
  \item{topography.correction}{Logical \code{scala}, option to calculate distance map in topography
corrected mode.}
}

\value{A list with numeric matrices with pixel distances to stations}

\author{Michael Dietze}




\examples{
## no examples present!
}

\keyword{environmental}
\keyword{seismology}
