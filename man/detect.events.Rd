\name{detect.events}
\alias{detect.events}
\title{Function to detect seismic events.}
\description{This function provides methods to detect seismic events in signals. It 
uses the ratio of STA (short-time average) and LTA (long-time average)
to detect seismic events.}
\usage{detect.events(data, dt, trigger, window.size, time.extra, duration, 
    lta.freeze = FALSE, square.signal = FALSE, extra = FALSE)}
\arguments{
  \item{data}{\code{Numeric} vector or matrix, data set to be processed.}
  \item{dt}{\code{Numeric} scalar, sampling period.}
  \item{trigger}{\code{Numeric} vector of length two, STA/LTA-ratio values for trigger 
on and trigger off.}
  \item{window.size}{\code{Numeric} vector of length two, window sizes for STA and LTA in
seconds.}
  \item{time.extra}{\code{Numeric} vector of length two, pre- and post-time to be added to
the detected event, in seconds. If omitted, both are set to zero.}
  \item{duration}{\code{Numeric} scalar or vector of length two, optional minimum and  
maximum length of the triggered event. If only scalar is provided this 
is taken for minimum duration of an event.}
  \item{lta.freeze}{\code{Logical} scalar, optionally freeze the lta value after 
trigger-on-value is reached.}
  \item{square.signal}{\code{Logical} scalar, option to use the squared signal for event
triggering, default is \code{FALSE}.}
  \item{extra}{\code{Logical} scalar, option to return additional output. If set to
\code{TRUE} the event vectors are written to a list (\code{$events})
and the individual event onsets, offsets and durations as well as 
the STA/LTA ratios are returned as list objects.}
}

\value{Numeric vector or matrix with event states (\code{TRUE}, 
\code{FALSE}). Or list object, containing event onsets,
offsets, duration and the STA/LTA ratio, if extended 
output is used.}

\author{Michael Dietze}




\examples{
## load example data set
data(sac, envir = environment())

## strip time and signal part
t <- strip.time(data = sac, unit = "sec")
s <- strip.signal(data = sac)

## trigger events
events <- detect.events(data = s, 
                        dt = 1 / 200, 
                        trigger = c(4, 2), lta.freeze = TRUE,
                        window.size = c(1, 4))

## plot 30 seconds of the signal and overlay events, very simple
plot(x = t, 
     y = s, 
     type = "l", 
     xlim = c(0, 30), 
     ylim = c(-500, 300), 
     xlab = "t (s)")
lines(x = t, 
      y = events * 100 - 500)  
}

\keyword{environmental}
\keyword{seismology}
