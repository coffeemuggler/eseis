\name{strip.signal}
\alias{strip.signal}
\title{Function to strip the signal part from sac-files.}
\description{This function converts a sac-object into a vector containing only the
signal part.}
\usage{strip.signal(data)}
\arguments{
  \item{data}{\code{list} object, data set to be converted.}
}

\value{\code{Numeric} vector with signal part of the sac-object.}

\author{Michael Dietze}



\seealso{\code{\link{read.sac}}, \code{\link{strip.time}}}
\examples{
## load example data set
data(sac, envir = environment())

## create data frame object with Julian Day time stamp
signal <- strip.signal(data = sac)

## plot signal
plot.signal(data = signal)

}
