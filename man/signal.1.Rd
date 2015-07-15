\name{signal.1}
\alias{signal.1}
\docType{data}
\title{example data}
\description{Seismic data set, 3 stations with 3 components, trace length 10 min.}
\usage{data(signal.1)}
\format{
  The format is:
 List of 6
 $ summary :List of 2
  ..$ dt  : num 0.005
  ..$ unit: chr "hour"
 $ time    :List of 2
  ..$ signal  : num [1:120001] 11 11 11 11 11 ...
  ..$ spectrum: num(0) 
 $ signal  :List of 3
  ..$ station1:List of 3
  .. ..$ x: num [1:120001] 112 135 140 119 126 ...
  .. ..$ y: num [1:120001] -40.3 -22.3 -19.3 -25.3 -46.3 ...
  .. ..$ z: num [1:120001] 23.9 7.9 3.9 9.9 1.9 ...
}
\details{The dataset was recorded by a Trillium Compact broadband seismometer.}
\examples{
## load example data set
data(signal.1)
}
\keyword{datasets}
