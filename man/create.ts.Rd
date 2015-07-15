\name{create.ts}
\alias{create.ts}
\title{Function to create time series from sac-files.}
\description{This function converts a sac-object, either into a data frame or a 
zoo-object, thereby only keeping the signal and, optionally, time part.}
\usage{create.ts(data, unit = "JD", shift = 0, object = "data.frame", 
    keep.time = TRUE)}
\arguments{
  \item{data}{\code{list} object, data set to be converted.}
  \item{unit}{\code{Character} scalar, date format of the output, one out of 
\code{"JD"} (Julian Day), \code{"hour"}, \code{"min"}, \code{"sec"}.}
  \item{shift}{Numeric \code{scalar}, time shift [sec].}
  \item{object}{\code{Character} scalar, object type to be created, one out of
\code{"data.frame"} (data frame) or a \code{"zoo"} (zoo object).Each 
object will be build by two vectors: \code{signal} and \code{time}.}
  \item{keep.time}{\code{Logical} scalar, option to return time vector.}
}
\details{Although the funciton name may imply that a \code{ts} object (time
series object) is returned, this is not the case. A \code{ts} object
only allows definition of a start year and quarter, which is not
appropriate for such high resolution data as seismic data.}
\value{either a \code{data frame} or a \code{zoo} object}

\author{Michael Dietze}



\seealso{\code{\link{read.sac}}, \code{\link{zoo}}}
\examples{
## load example data set
data(sac, envir = environment())

## create data frame object with Julian Day time stamp
t.JD <- create.ts(data = sac)

## create data frame object with Julian Day time stamp
t.JD.signal <- create.ts(data = sac, keep.time = FALSE)

## create zoo object with hourly time stamp
t.hour <- create.ts(data = sac, unit = "hour", object = "zoo")
}
