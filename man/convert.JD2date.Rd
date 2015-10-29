\name{convert.JD2date}
\alias{convert.JD2date}
\title{Function to convert Julian Days to POSIXlt dates and vice versa.}
\description{This function converts time Julian Days to POSIXlt dates and 
vice versa. It uses function from the package \code{date}.}
\usage{convert.JD2date(data, format = "mm-dd", year, timezone = "UTC", 
reverse = FALSE)}
\arguments{
  \item{data}{\code{Numeric} vector, data set to be processed.}
  \item{format}{\code{Character} scalar, output format. One out of 
  \code{"mm-dd"} ( month and day), \code{"POSIXct"} (a POSIXct-object). 
  Default is \code{"ddmm"}. Note that for the latter option a year must be 
  provided or the current year will be set by default.}
  \item{year}{\code{Numeric} scalar, optional year to be used for conversion.}
  \item{timezone}{\code{Character} vector, time zone of the output date.}
  \item{reverse}{\code{Logical} scalar, option to reverse calculation (i.e., 
  convert POSIXct dates to Julian Days).}
}
\value{\code{Numeric} vector of dates.}

\author{Michael Dietze}

\examples{
  ## convert a series of Julian days to POSIXct
  #convert.JD2date(date = 90:100, format = "POSIXct")
  
  ## convert a series of Julian days to POSIXct, with correct years
  #convert.JD2date(date = 90:100, format = "POSIXct", year = 2000)
  
  ## convert a POSICct date to Julian day
  #convert.JD2date(date = as.POSIXct(Sys.Date()), reverse = TRUE)
}

\keyword{environmental}
\keyword{seismology}
