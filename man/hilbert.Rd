\name{hilbert}
\alias{hilbert}
\title{Function to calculate the hilbert transform}
\description{This function calculates the hilbert transform of signals.}
\usage{hilbert(data)}
\arguments{
  \item{data}{Numeric \code{matrix} with seismic traces.}
}

\value{A list object with the merged data set.}
\references{Code is modifed from R-package hht.}
\author{Michael Dietze}




\examples{
## load example data set
data(signal.1, envir = environment())

## extract 3 traces of station 1 to matrix
s.1 <- rbind(signal.1$signal$station1$x,
             signal.1$signal$station1$y,
             signal.1$signal$station1$z)

## calculate hilbert transform of the signals
h.1 <- hilbert(data = s.1[1,])
}

\keyword{environmental}
\keyword{seismology}
