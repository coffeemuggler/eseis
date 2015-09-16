\name{stalta}
\alias{stalta}
\title{Function to detect events by the STA-LTA-method.}
\description{This function provides methods to detect seismic events in signals. It 
uses the ratio of STA (short-time average) and LTA (long-time average)
or the absolute signal level to trigger events. The result is either
appended to the data set or returned as list object.}
\usage{trigger.stalta(data, stations, component = "z", trigger.ratio, 
    window.size, time.extra, time.min, square.signal = FALSE, 
    dt, append = TRUE)}
\arguments{
  \item{time}{Numeric \code{vector}, optional, time of the data set to be 
  processed.}
  \item{data}{Numeric \code{vector}, data set to be processed.}
  \item{ratio.on}{Numeric \code{scalar} sta-lta-ratio for on-trigger.}
  \item{ratio.off}{Numeric \code{scalar} sta-lta-ratio for off-trigger.}
  \item{t.sta}{Numeric \code{scalar} length of short time average in seconds.}
  \item{t.lta}{Numeric \code{scalar} length of long time average in seconds.}
  \item{dt}{Numeric \code{scalar}, sampling period.}
  \item{extended}{Logical \code{scalar}, option to return extended output (
  sta-levels, lta-levels). Output will be a list in this case. Default 
  is \code{FALSE}.}
  \item{freeze.lta}{Logical \code{scalar}, option to freeze lta value 
  at event onset.}
  \item{envelope = "abs"}{Character \code{scalar}, option to calculate the 
  signal envelope prior to sta and lta evaluation. Options are \code{"abs"} 
  (absolute, fastest), \code{"sqrt"} (root of the square) and \code{"hilbert"} 
  (root of the square of the hilbert transform, way slowest). Default is 
  \code{"abs"}.}
}

\value{A matrix or list object.}

\author{Michael Dietze}




\examples{
## load example data set
data(signal.1, envir = environment())