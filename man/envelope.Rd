\name{envelope}
\alias{envelope}
\title{Function to create signal envelopes.}
\description{This function creates the signal envelopes. It returns cosine tapered
sums of the absolute values of the hilbert-transformed signals.}
\usage{envelope(data, power = 1, p = 0.1)}
\arguments{
  \item{data}{Numeric \code{vector} or \code{matrix} with respective seismic traces
to be used for calculating the envelope.}
  \item{sum}{Logical \code{scalar}, option to calculate vector sum of the input 
data. Only useful if \code{data} contains more than one component of the same 
record.} 
  \item{power}{Numeric \code{scalar}, power used prior to signal summation.}
  \item{p}{Numeric \code{scalar}, proportion of the signals to be tapered.}
}

\value{Numeric vector with signal envelope}

\author{Michael Dietze}




\examples{

## no example present yet
}

\keyword{environmental}
\keyword{seismology}
