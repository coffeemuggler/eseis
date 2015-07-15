\name{write.sac}
\alias{write.sac}
\title{Function to write a sac-file.}
\description{This function writes a sac-file.}
\usage{write.sac(data, file, biglong = FALSE)}
\arguments{
  \item{data}{\code{List} object, R-object to be written to a sac file or
multiple sac-files.}
  \item{file}{\code{Character} scalar, optional file name of the sac-file.}
  \item{biglong}{\code{Logical} scalar, biglong option.}
}

\value{One or more sac files, written to disk.}

\author{Michael Dietze}




\examples{
## uncomment to run
data(sac, envir = environment())

# write.sac(data = sac, file = "testfile.sac")

}

\keyword{environmental}
\keyword{seismology}
