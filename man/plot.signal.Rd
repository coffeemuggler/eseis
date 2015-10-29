\name{plot.signal}
\alias{plot.signal}
\title{Function to plot seismic traces.}
\description{This function plots seismic traces.}
\usage{\method{plot}{signal}(data, time, unit, t.rel = TRUE, mtext, 
dynamic.resolution = 5000, shared.limits, 
    plot = TRUE, output = FALSE, ...)}
\arguments{
  \item{data}{Numeric \code{vector} or \code{matrix} with respective seismic traces.}
  \item{time}{Numeric \code{vector} with time values.}
  \item{unit}{\code{Character} scalar, time unit. One out of \code{"sec"}, 
\code{"min"}, \code{"hour"}, \code{"day"}, \code{"JD"}. If omitted, an 
automatic unit is used.}
  \item{t.rel}{\code{Logical} scalar, option to enable/disable relative time 
units (i.e. x-axis begins with zero). Default is \code{TRUE}.}
  \item{mtext}{\code{Character} scalar or vector, optional text below the plot title}
  \item{dynamic.resolution}{\code{Numeric} scalar, option to decrease graph resolution for faster
plottig, default is 5000. The value specifies the number of subsamples
that are drawn from the data set. If set to \code{0} the entire data set
is plotted.}
  \item{shared.limits}{\code{Logical} scalar, option to use the same axes limits for all 
plot panels. Only useful when plotting more than one signal trace.
The parameter affects both, x- and y-axes.}
  \item{plot}{\code{Logical} scalar, option to prevent plot output. Only useful in
combination with \code{output = TRUE}.}
  \item{output}{\code{Logical} scalar, option to return the data used for plotting, i.e.
a two-column matrix (t, amplitude).}
  \item{\dots}{Further arguments passed.}
}

\value{A plot of an array of seismic signals through time and, optionally,
a numeric matrix of the data to be plotted.}

\author{Michael Dietze}




\examples{
## load example data
data(signal.1, envir = environment())

## create traces matrix
s.1 <- rbind(signal.1$signal$station1$x,
             signal.1$signal$station1$y,
             signal.1$signal$station1$z)

## straightforward plot of the data set
#plot.signal(data = s.1)

## create time vector
#t <- signal.1$time$signal

## plot of the data set with correct time
#plot.signal(time = t, data = s.1)

## plot only trace 1
#plot.signal(time = t, data = s.1[1,])

## plot in very low resolution (200 data points)
#plot.signal(time = t, data = s.1, dynamic.resolution = 200)

## plot in different colour, line type and headings
#plot.signal(time = t, data = s.1, col = 1:3, lty = 2:4, 
#            xlab = rep("Time [min]", 3), ylim = c(-1500, 1500),
#            main = c("X-component", "Y-component", "Z-component"))

## plot minute 3 to 6
#plot.signal(time = t, data = s.1, xlim = c(11.05, 11.1))

## plot three data sets over each other
#p <- plot.signal(time = t, data = s.1, plot = FALSE, output = TRUE)

#plot(p[1,], p[2,], type = "l", ylim =c(-1000, 1500), 
#     main = "XYZ-components", xlab = "Time [h]", ylab = "amp")
#lines(p[1,], p[3,], col = 3)
#lines(p[1,], p[4,], col = 4)
}

\keyword{environmental}
\keyword{seismology}
