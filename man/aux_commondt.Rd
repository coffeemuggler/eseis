% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aux_commondt.R
\name{aux_commondt}
\alias{aux_commondt}
\title{Identify highest common sampling interval}
\usage{
aux_commondt(data, dt)
}
\arguments{
\item{data}{\code{list} of \code{eseis} objects or vector of sampling 
intervals to be checked for highest common sampling interval}

\item{dt}{\code{Numeric} vector of length one, user-defined common 
sampling frequency for which aggregation factors shall be computed.}
}
\value{
\code{list} object with elements \code{dt} (highest common 
sampling interval) and \code{agg} (aggregation factors for each 
of the input data sets to reach the common sampling interval)
}
\description{
The function compares the sampling intervals of a list of \code{eseis}
objects and identifies the highest common sampling interval (dt) as well 
as the aggregation factors for each \code{eseis} object needed to reach 
this common sampling interval.
}
\examples{

\dontrun{

## TO BE WRITTEN
}
                     
}
\author{
Michael Dietze
}
\keyword{eseis}
