\name{migrate}
\alias{migrate}
\title{Function to migrate signals in space}
\description{The function performs signal migration in space in order to determine 
the location of a seismic signal.}
\usage{migrate(data, d.stations, d.map, v, f, snr, normalisation = FALSE)}
\arguments{
  \item{data}{Numeric \code{matrix}, seismic signals to cross-correlate.}
  \item{d.stations}{Numeric \code{matrix}, inter-station distances.}
  \item{d.map}{\code{List} object, distance maps for each station.}
  \item{v}{Numeric \code{scalar}, mean velocity of seismic waves in the medium,
in m/s.}
  \item{f}{Numeric \code{scalar}, sampling frequency in Hz}
  \item{snr}{Numeric \code{vector}, optional signal-to-noise-ratios for each signal 
trace, used for normalisation.}
  \item{normalisation}{Logical \code{scalar}, option to normalise stations correlations by 
signal-to-noise-ratios. If used, snr data must be provided.}
}

\value{A SpatialGridDataFrame-object with Gaussian probability density 
function values for each grid cell.}

\author{Michael Dietze}




\examples{
## no example yet

}

\keyword{environmental}
\keyword{seismology}
