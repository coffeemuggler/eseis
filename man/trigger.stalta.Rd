\name{trigger.stalta}
\alias{trigger.stalta}
\title{Function to detect events by the STA-LTA-method.}
\description{This function provides methods to detect seismic events in signals. It 
uses the ratio of STA (short-time average) and LTA (long-time average)
or the absolute signal level to trigger events. The result is either
appended to the data set or returned as list object.}
\usage{trigger.stalta(data, stations, component = "z", trigger.ratio, 
    window.size, time.extra, time.min, square.signal = FALSE, 
    dt, append = TRUE)}
\arguments{
  \item{data}{\code{List} object, data set to be processed}
  \item{stations}{Numeric or character \code{vector}, number or name of the stations to 
be processed. If omitted, als stations are processed.}
  \item{component}{Character \code{scalar}, component to be processed. If \code{"sum"} is
specified, the sum of the absolute values of all components are used.}
  \item{trigger.ratio}{Numeric \code{vector} of length two, ratio of STA and LTA. First value
specifies trigger-on value, second value specifies trigger-off value.}
  \item{window.size}{Numeric \code{vector} of length two, window sizes for STA and LTA in
seconds.}
  \item{time.extra}{Numeric \code{vector} of length two, pre- and post-time to be added to
the detected event, in seconds. If omitted, both are set to zero.}
  \item{time.min}{Numeric \code{scalar}, optional minimum length of the triggered event.}
  \item{square.signal}{Logical \code{scalar}, option to use the squared signal for event
triggering, default is \code{FALSE}.}
  \item{dt}{Numeric \code{scalar}, sampling period. Must be provided if deviating
from value in \code{data$summary$dt}.}
  \item{append}{Logical \code{scalar}, option to append result to the input data set. If
set \code{FALSE}, a list object is returned with triggered events for
each station.}
}

\value{A list object.}

\author{Michael Dietze}




\examples{
## load example data set
data(signal.1, envir = environment())

## trigger events
signal.1.events <- trigger.stalta(data = signal.1, 
                                  trigger.ratio = c(4, 2), 
                                  window.size = c(6, 60))
}

\keyword{environmental}
\keyword{seismology}
