\name{cube.GPS}
\alias{cube.GPS}
\title{Function to extract GPS data from cube-files.}
\description{This extracts the GPS tags from cube-files. It uses the library 
CUBEtools (see details). It processes all cube.files in the stated 
directory.}
\usage{cube.GPS(path.cubefiles, file, path.cubetools, keep.files = FALSE)}
\arguments{
  \item{path.cubefiles}{\code{Character} scalar, abolute path to directory which contains the 
cubefiles that will be processed.}
  \item{file}{\code{Character} scalar or vector, names of the cube-files to be 
processed, with extension. If omitted, all files in the 
\code{path.cubefiles} will be processed.}
  \item{path.cubetools}{\code{Character} scalar, abolute path to and name of the CUBEtools
directory.}
  \item{keep.files}{\code{Logical} scalar, option to delete or keep the extracted GPS files
on the disk.}
}

\value{\code{Numeric} matrix with GPS coordinates.}

\author{Michael Dietze}




\examples{

#   path.cubetools = "/home/USER/Downloads/CUBEtools"
#   path.cubefile = "~/data/raw/A7L"
#   
#   GPS.all <- cube.GPS(path.cubetools = path.cubetools, 
#                       path.cubefile = path.cubefile)
# 
#   GPS.one <- cube.GPS(file = "06240000.A7L", 
#                       path.cubetools = path.cubetools, 
#                       path.cubefile = path.cubefile)
#   
#   print(paste("LAT = ", round(mean(GPS.one[,1]), 2), 
#               ", LON = ", round(mean(GPS.one[,2]), 2), 
#               sep = ""))
}

\keyword{environmental}
\keyword{seismology}
