\name{station.setup}
\alias{station.setup}
\title{Function to set a seismic station setup.}
\description{This function sets the setup parameters of seismic stations. A
station is considered here as the combination of sensor and data logger.}
\usage{station.setup(ID, type.sensor, type.logger, coordinates, poles, 
    zeros, k, s, gain, AD)}
\arguments{
  \item{ID}{\code{Character} scalar, ID or name of the station.}
  \item{type.sensor}{\code{Character} scalar, type of the sensor}
  \item{type.logger}{\code{Character} scalar, ID or name of the logger.}
  \item{coordinates}{\code{Numeric} vector, x-, y- and z-coordinate of the station location.}
  \item{poles}{\code{Complex} vector, real and imaginary parts of the pole values (e.g.
\code{c(-0.037-0.037i, -343+0i)}).}
  \item{zeros}{\code{Complex} vector, real and imaginary parts of the zero values.}
  \item{k}{\code{Numeric} scalar, normalisation factor}
  \item{s}{\code{Numeric} scalar, Ground motion sensitivity.}
  \item{gain}{\code{Numeric} scalar, Gain of the logger.}
  \item{AD}{\code{Numeric} scalar, analog-digital conversion factor of the logger.}
}

\value{A list object with the station setup}

\author{Michael Dietze}




\examples{

## Generate setup for a station with a Trillium Compact 120 s seismometer
## and an Omnirecs Data Cube 3 ext logger.
station.01 <- station.setup(ID = "Station 01",
                            type.sensor = "Trillium Compact 120 s",
                            type.logger = "Data Cube 3 ext",
                            coordinates = c(5395031, 5651928, 600),
                            poles = c(-3.691e-02+3.702e-02i,
                                      -3.691e-02-3.702e-02i,
                                      -3.430e+02+0.000e+00i,
                                      -3.700e+02+4.670e+02i,
                                      -3.700e+02-4.670e+02i,
                                      -8.360e+02+1.522e+03i,
                                      -8.360e+02-1.522e+03i,
                                      -4.900e+03+4.700e+03i,
                                      -4.900e+03-4.700e+03i,
                                      -6.900e+03+0.000e+00i,
                                      -1.500e+04+0.000e+00i),
                            zeros = c(0+0i, 
                                      0+0i, 
                                      -392+0i, 
                                      -1960+0i, 
                                      -1490+1740i, 
                                      -1490-1740i),
                            k = 4.34493e+17,
                            s = 754.3,
                            gain = 1,
                            AD = 2.441406e-07)
}

\keyword{environmental}
\keyword{seismology}
