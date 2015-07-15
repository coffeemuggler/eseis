\name{mseed2sac}
\alias{mseed2sac}
\title{Function to convert from miniseed to sac.}
\description{This function converts files in miniseed format to the sac format. 
It uses the Obspy library (see details.)}
\usage{mseed2sac(file.in, path.out, file.out)}
\arguments{
  \item{file.in}{Character \code{scalar} or \code{vector}, name of the files to be read,
with extension. }
  \item{path.out}{Character \code{scalar} or \code{vector}, optional path to where the
output files are written to.}
  \item{file.out}{Character \code{scalar} or \code{vector}, optional names of the output 
files, with extension. If omitted, the input file names are used and 
extensions \code{BHE}, \code{BHN}, \code{BHZ} will be added.}
}
\details{The conversion uses the Obspy library. See 
https://github.com/obspy/obspy/wiki for how to install it
on diverse platforms. }
\value{Files in sac format are created.}

\author{Michael Dietze}




\examples{
## uncomment to run
# mseed2sac(file.in = "cube1.A7P.pri0")

# mseed2sac(file.in = list.files(path = "../input"))

# mseed2sac(file.in = list.files(pattern = "pri0"))

# mseed2sac(file.in = "cube1.A7P.pri0", 
#           path.out = "../output/",
#           file.out = "cube1.BHE")
}
