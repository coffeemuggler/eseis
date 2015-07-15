\name{spectrogram}
\alias{spectrogram}
\title{Function to spectrograms from time series.}
\description{This function creates spectrograms from a seismic trace. It supports the 
standard spectrogram apporach and the Welch method. See details below.}
\usage{spectrogram(time, data, dt, unit, method = "generic", window, 
    overlap, window.sub, overlap.sub, n.fft, plot = FALSE, ...)}
\arguments{
  \item{time}{Numeric \code{vector} with time values.}
  \item{data}{\code{Numeric} vector, seismic trace to be processed.}
  \item{dt}{Numeric \code{scalar}, sampling period.}
  \item{unit}{Character \code{scalar}, unit of the target time series. If omitted, the
output is in seconds.}
  \item{method}{method used for computation of the spetrogram. One out of "generic",
"Welch".}
  \item{window}{spectrogram resolution (i.e. time window length), seconds.}
  \item{overlap}{fraction of overlapping.}
  \item{window.sub}{width of the sub-window [seconds], used for the "Welch-method".}
  \item{overlap.sub}{percentage of overlapping, used for the "Welch-method".}
  \item{n.fft}{number of samples for fft, alternative to window or window.sub,
depending on the chosen method.}
  \item{plot}{\code{logical} scalar, toggle plot output. Only useful for plotting
single time series. Disabled when multidimensional data is provided.}
  \item{\dots}{further parameters passed to the function.}
}
\details{Define/explain the two methods: generic and Welch}
\value{OUTPUT}
\references{ADD cubetools references}
\author{Michael Dietze}




\examples{
## load example data set
data(signal.1, envir = environment())

## extract time and z-component of station 1
t <- signal.1$time$signal
x <- signal.1$signal$station1$z

## time series with generic, data array with Welch
signal.1.Welch <- spectrogram(time = t,
                              data = x,
                              dt = 1/200,
                              unit = "hour",
                              window = 6, 
                              window.sub = 2, 
                              component = "z", 
                              method = "Welch", 
                              plot = TRUE) 
}

\keyword{environmental}
\keyword{seismology}
