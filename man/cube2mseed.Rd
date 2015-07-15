\name{cube2mseed}
\alias{cube2mseed}
\title{Function to convert cube-format to mseed-format.}
\description{This function converts files in the Omnirecs cube format to the miniseed
format. It uses code provided by Omnirecs (see details).}
\usage{cube2mseed(file.in, path.out, file.out, path)}
\arguments{
  \item{file.in}{Character \code{scalar} or \code{vector}, name of the files to be read,
with extension. }
  \item{path.out}{Character \code{scalar} or \code{vector}, optional path to where the
output files are written to. If omitted, a new directory called 
\code{"output"} will be created since this is necessary for the
Omnirecs code to work.}
  \item{file.out}{Character \code{scalar} or \code{vector}, optional names of the output 
files, without extension. If omitted, the input file names are used.}
  \item{path}{Character \code{scalar}, absoulte location of the cube2mseed file 
code, e.g. \code{~/Downloads/CUBEtools/bin/}.}
}
\details{Omnirecs provides Java-based code to convert cube files to the miniseed
format. The software can be downloaded from 
http://www.omnirecs.de/documents.html. For use in this function
Java must be installed and at least the folders \code{bin} and \code{java}
from the Cube2miniSEED (C2M) directory must be present. Alternatively, 
make the cube tools globally available by adding the path to the 
bin-directory. In Ubuntu this can be done by opening the file .bashrq 
in the home directory and adding the line 
\code{export $PATH:/home/CUBEtools/bin}. In Windows XP this can be done
by adding the path to CUBEtools/bin to environmental variables list.}
\value{Files in miniseed format are created.}

\author{Michael Dietze}




\examples{
## uncomment to run
# cube2mseed(file.in = "cubefile.A1A", 
#            path.out = "output", 
#            file.out = "miniseed",
#            path = "../code/C2M")
}
