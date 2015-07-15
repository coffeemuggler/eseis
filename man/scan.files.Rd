\name{scan.files}
\alias{scan.files}
\title{Function to check presence of files.}
\description{This function scans a given directory sturcture and checks if 
hourly seismic files are present. It lists the presence/absence.}
\usage{scan.files(directory, station, extension)}
\arguments{
  \item{directory}{\code{Character} scalar, directory which is to be scanned.}
  \item{station}{\code{Character} scalar, optional station ID to be scanned.}
  \item{extension}{\code{Character} scalar, optional file extention to which the scan
is to be restricted.}
}

\value{\code{Numeric} matrix of indices of seismic file presence for each
hour (t): 0 - not present, 1 - present.}

\author{Michael Dietze}




\examples{

## uncomment to use
#x <- scan.files(directory = "data/seismic/2013/", extension = "BHZ")
}
