\name{merge.array}
\alias{merge.array}
\title{Function to merge data arrays.}
\description{This function merges data arrays. Merging can include the
temporal dimension (time).}
\usage{\method{merge}{array}(data)}
\arguments{
  \item{data}{\code{List} object, data sets to be processed (e.g. 
\code{list(array.1, array.2)}).}
}

\value{A list object with the merged data set.}

\author{Michael Dietze}




\examples{
## load example data set
data(signal.1, envir = environment())

## clip original data set
signal.1.1 <- clip.array(data = signal.1, 
                         time.clip = c(11, 11 + 1/12))
signal.1.2 <- clip.array(data = signal.1, 
                         time.clip = c(11 + 1/12, 11 + 2/12))

## merge data set
signal.1.merged <- merge.array(data = list(signal.1.1, signal.1.2))
}

\keyword{environmental}
\keyword{seismology}
