\name{plot.spectrogram}
\alias{plot.spectrogram}
\title{Function to plot spectrograms}
\description{This function plots a spectrogram of a signal trace.}
\usage{\method{plot}{spectrogram}(data, legend, dynamic.resolution = 5000, events, 
    col, plot = TRUE, ...)}
\arguments{
  \item{data}{Numeric \code{matrix} with power spectral density estimate. Time and 
frequency vectors must be contained as column and row names, 
respectively.}
  \item{legend}{\code{Character} scalar, optional legend label.}
  \item{dynamic.resolution}{Numeric \code{scalar}, option to decrease graph resolution for faster
plottig, default is 5000. The value specifies the number of subsamples
that are drawn from the data set. If set to \code{0} the entire data set
is plotted.}
  \item{events}{Numeric \code{vector}, optionally indicate events TO DO YETTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT}
  \item{col}{Character \code{vector}, colour palette to use for spectrogram plotting,
e.g. \code{rainbow(n = 10, start = 0.3, end = 0.8)}}
  \item{plot}{\code{Logical} scalar, option to prevent plot output. Only useful in
combination with \code{output = TRUE}.}
  \item{\dots}{Further arguments passed.}
}

\value{A power spectral density estimate image.}

\author{Michael Dietze}




\examples{
## load example data set
data(signal.1, envir = environment())

## extract time and z-component of station 1
t <- signal.1$time$signal
x <- signal.1$signal$station1$z

## time series with generic, data array with Welch
#S <- spectrogram(time = t,
#                 data = x,
#                 dt = 1/200,
#                 unit = "hour",
#                 window = 6, 
#                 window.sub = 2, 
#                 component = "z", 
#                 method = "Welch")

## plot spectrogram straightforward
#plot.spectrogram(S)

## plot spectrogram with logarithmic y-axis
#plot.spectrogram(S, log = "y")

## plot spectrogram with user-defined modifications
#plot.spectrogram(data = S,
#                 main = "PSD station 1 z-component",
#                 xlab = "Time [h]",
#                 ylab = "f [Hz]",
#                 legend = "Energy [db]",
#                 zlim = c(-120, -80),
#                 colour = rainbow(n = 200, start = 0.5, end = 0.7))

## plot spectrograms with even more decreased resolution
#plot.spectrogram(data = S, dynamic.resolution = 100)

}

\keyword{environmental}
\keyword{seismology}
