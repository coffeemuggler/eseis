\name{strip.time}
\alias{strip.time}
\title{Function to create time units from sac-files.}
\description{The function strips the time vector from sac objects.}
\usage{strip.time(data, unit = "POSIXct", shift = 0)}
\arguments{
  \item{data}{object, data set to be processed. }
  \item{unit}{\code{Character} scalar, date format of the output, one out of 
\code{"JD"} (Julian Day), \code{"day"}, \code{"hour"}, \code{"min"}, 
\code{"sec"} and \code{"POSIXct"}. Default is \code{"POSIXct"}.}
  \item{shift}{\code{Numeric} scalar, time shift [sec].}
}

\value{A vector of time units.}

\author{Michael Dietze}



\seealso{\code{\link{read.sac}}}
\examples{
## load example data
data(sac, envir = environment())

## create time vector from data array
t <- strip.time(data = sac, unit = "hour")

## check results (in minutes), should be 10 minutes
summary(t * 60)
}

\keyword{environmental}
\keyword{seismology}
