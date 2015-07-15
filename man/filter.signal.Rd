\name{filter.signal}
\alias{filter.signal}
\title{Function to filter seismic signals}
\description{This function filters seismic signals.}
\usage{\method{filter}{signal}(data, f.low, f.high, subtract.mean = TRUE, type, 
    shape = "butter", order = 2, dt, ...)}
\arguments{
  \item{data}{\code{Numeric} matrix or vector, data set(s) to be processed.}
  \item{f.low}{\code{Numeric} scalar, low frequency cut-off, default is zero.}
  \item{f.high}{\code{Numeric} scalar, high frequency cut-off, default is (1/2 dt)^-1.  
The parameter dt is taken from the data set. If a time series is
provided, \code{dt} must be specified separately.}
  \item{subtract.mean}{\code{Logical} scalar, optionally remove the global mean before 
processing, default is \code{TRUE}.}
  \item{type}{\code{Character} scalar, type of filter, one out of \code{"LP"} (low 
pass), \code{"HP"} (high pass), \code{"BP"} (band pass) and \code{"BR"}
(band rejection).}
  \item{shape}{\code{Character} scalar, filter shape, one out of \code{"butter"} (
Butterworth).}
  \item{order}{\code{Numeric} scalar, order of the filter, default is \code{2}.}
  \item{dt}{\code{Numeric} scalar, sampling period.}
  \item{\dots}{Further arguments to pass.}
}
\details{At the moment, only Butterworth filters are supported. If needed, further
filter shapes may be implemented.}
\value{A filtered data set}

\author{Michael Dietze}



\seealso{\code{\link{filter}}, \code{\link{butter}}}
\examples{
## load example data set
data(signal.1, envir = environment())

## extract 3 traces of station 1 to matrix
s.1 <- rbind(signal.1$signal$station1$x,
             signal.1$signal$station1$y,
             signal.1$signal$station1$z)

## filter the data set, i.e. bandpass filter between 1 and 60 Hz
s.1.filtered <- filter.signal(data = s.1,
                              dt = 0.005,
                              f.low = 1, 
                              f.high = 60, 
                              type = "BP")

## plot unfiltered and filtered z-component over each other
time <- signal.1$time$signal
plot(time, s.1[3,],type = "l", col = "grey70")
lines(time, s.1.filtered[3,], col = 2)
## ATTENTION, REPLACE PLOT EXAMPLE BY NEW FUNCTION plot.signal
}
