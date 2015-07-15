\name{clip.array}
\alias{clip.array}
\title{Function to clip a data array.}
\description{This function clips (or truncates) a data array. Clipping can include the
temporal dimension (time) or the data structure (i.e. number of stations,
number of components). Entire array components (e.g. \code{metadata},
\code{events}, \code{signal}) can be removed as well.}
\usage{clip.array(data, time.clip, summary)}
\arguments{
  \item{data}{\code{List} object, array structure to be processed.}
  \item{time.clip}{\code{numeric} vector, segment used for clipping the array. Interval 
must refer to the signal time vector (not the spectrum time vector).}
  \item{summary}{\code{List} object, new structure of the summary component. Set to
\code{NULL} to remove.}
}

\value{A list object with the clipped data set.}

\author{Michael Dietze}



\seealso{\code{\link{build.array}}}
\examples{
## load example data set
data(signal.1, envir = environment())

## clip data set to first half of original length, i.e. first 5 minutes
signal.1.clipped <- clip.array(data = signal.1, 
                               time.clip = c(11, 11 + 1/12))
}

\keyword{environmental}
\keyword{seismology}
