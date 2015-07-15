\name{read.sac}
\alias{read.sac}
\title{Function to read sac-files.}
\description{This function reads sac-files to R. It is a wrapper of the function
\code{JGET.seis}.}
\usage{read.sac(files, unit = "cts", subtract.mean = FALSE, setup, ...)}
\arguments{
  \item{files}{Character \code{scalar} or \code{vector}, name of the files to be read,
with extension. Wildcards may be used (see details and examples).}
  \item{unit}{Character \code{scalar}, unit of the output data set. One out of 
\code{"volts"} (Volts), \code{"cts"} (counts) and \code{"velocity"} 
(velocity). Default is \code{"cts"}. If \code{"velocity"} is selected, 
\code{setup} must be provided.}
  \item{subtract.mean}{\code{Logical} scalar, optionally remove the global mean after 
processing, default is \code{TRUE}.}
  \item{setup}{\code{List} object with station setup information (cf. 
\link{station.setup}), needed to convert counts into m/s.}
  \item{\dots}{Further arguments passed to \code{JGET.seis} and/or \code{deconinst}.}
}
\details{To read more than one file, present in the working directory, wildcards
are supported, e.g. \code{"*BHZ.SAC"}. \cr
A more selctive way might be to first list all possible sac-files in the
working directory (\code{files <- list.files(pattern = "*BHZ.SAC")}) and
then further truncate the file list (e.g. \code{files <- files[1:5]}).}
\value{\code{list} object}

\author{Michael Dietze}



\seealso{\code{\link{JGET.seis}}, 
\code{\link{deconinst}}, 
\code{\link{list.files}}, 
\code{\link{regex}}}
\examples{
## load example data, i.e. station setup list
data(setup.1, envir = environment())

## uncomment to use all further commands for reading user sac-files

## read a single file straightforward
# x <- read.sac(files = "file1.sac")

## read the file with mean removal
# x <- read.sac(files = "file1.sac",
#               subtract.mean = TRUE)

## read the file with deconvolution and units as velocity [m/s]
# x <- read.sac(files = "file1.sac",
#               deconvolve = TRUE,
#               setup = setup.1,
#               unit = "velocity")

## read a series of files
# filelist <- list.files(pattern = "*SAC")
# filelist <- filelist[1:3]
# x <- read.sac(files = filelist)
}
