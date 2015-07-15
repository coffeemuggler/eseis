\name{tilt.DEM}
\alias{tilt.DEM}
\title{Function to tilt a DEM.}
\description{This function tilts a DEM in x- and y-direction.}
\usage{tilt.DEM(DEM, points, angle.x, angle.y, resolution, projection, 
    shift = TRUE)}
\arguments{
  \item{DEM}{Raster object, the DEM to be processed}
  \item{points}{Numeric \code{matrix}, with x-y-z-coordinates of points (e.g. 
seismic station locations) to be tilted along.}
  \item{angle.x}{Numeric \code{scalar}, tilt angle in x-direction, in degree }
  \item{angle.y}{Numeric \code{scalar}, tilt angle in y-direction, in degree }
  \item{resolution}{Numeric \code{scalar}, resolution of the output DEM, same as input if
omitted}
  \item{projection}{Numeric \code{scalar}, projection of the output DEM, same as input if
omitted}
  \item{shift}{Logical \code{scalar}, option to shift the tilted DEM to new
coordinate origin after tilting and interpolation.}
}

\value{List object with tilted DEM and applied parameters}

\author{Michael Dietze}




\examples{

## load volacano data set and convert to raster
#DEM <- raster::raster((datasets::volcano))

## generate some arbitrary points in space
#points <- cbind(c(0.2, 0.4), c(0.3, 0.5), c(140, 170))

## plot volcano DEM and points
#plot(DEM)
#points(points)

## tilt DEM and points by 0.1 rad
#DEM.tilt.x <- tilt.DEM(DEM = DEM, points = points, angle.x = 0.1)
#plot(DEM.tilt.x$DEM)
#points(DEM.tilt.x$points)

## re-tilt DEM and points back to initial conditions
#DEM.tilt.xy <- tilt.DEM(DEM = DEM, angle.x = 20, angle.y = -0.1)[[1]]
#plot(DEM.tilt.xy)
}

\keyword{environmental}
\keyword{seismology}
