\name{convert.coordinates}
\alias{convert.coordinates}
\title{Convert coordinates between reference systems.}
\description{This function converts coordinates between different reference 
systems.}
\usage{convert.coordinates(data, from, to)}
\arguments{
  \item{data}{\code{Numeric} matrix or data frame, x-, y-coordinates to be 
  converted.}
  \item{from}{\code{Character} scalar, proj4 string of the current reference 
  system.}
  \item{to}{\code{Character} scalar, proj4 string of the target reference system.}
}
\details{No details, yet.}
\value{A \code{Data frame} with transformed coordinates.}

\author{Michael Dietze}



\seealso{\code{\link{read.sac}}, \code{\link{strip.time}},\code{\link{create.ts}}}
\examples{

  xy <- cbind(13, 55)
  
  proj.in <- "+proj=longlat + ellps=WGS84"
  
  proj.out <- "+proj=utm +zone=32 +datum=WGS84"
  
  convert.coordinates(data = xy, 
                      from = proj.in, 
                      to = proj.out)
}

\keyword{environmental}
\keyword{seismology}
