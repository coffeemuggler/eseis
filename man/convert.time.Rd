\name{convert.time}
\alias{convert.time}
\title{Function to convert time units.}
\description{This function converts time units.}
\usage{convert.time(data, unit.in, unit.out)}
\arguments{
  \item{data}{\code{List} object or \code{numeric} vector, data set to be processed.}
  \item{unit.in}{\code{Character} scalar, input date format, one out of 
\code{"JD"} (Julian Day), \code{"day"}, \code{"hour"}, 
\code{"min"}, \code{"sec"}.}
  \item{unit.out}{\code{Character} scalar, output date format, one out of 
\code{"day"} (day), \code{"hour"}, \code{"min"}, \code{"sec"}.}
}
\details{This function primarilty aids plotting purposes. to display signal
traces in appropriate units (default would be seconds) the time units
must be converted.}
\value{\code{Numeric} vector of time units.}

\author{Michael Dietze}



\seealso{\code{\link{read.sac}}, \code{\link{strip.time}},\code{\link{create.ts}}}
\examples{
## load example data
#data(signal.1, envir = environment())

## show time unit of example data set
#signal.1$summary$unit

## convert hours to minutes
#signal.1.min <- convert.time(data = signal.1, 
#                             unit.in = signal.1$summary$unit,
#                             unit.out = "min")

## convert only time vector
#time.vector.hour <- signal.1$time$signal
#time.vector.min <- convert.time(data = time.vector.hour,
#                                unit.in = "hour",
#                                unit.out = "min")  

## show summaries of the two data sets
#summary(time.vector.hour)
#summary(time.vector.min)

## Nota bene, convert date from d-m-Y to Julian day
#date <- "10.10.2010"
#strptime(x = date, format = "\%d.\%d.\%Y")$yday + 1
#}

\keyword{environmental}
\keyword{seismology}
