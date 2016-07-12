#' eseis
#'
#' Environmental seismology toolbox
#'
#' \tabular{ll}{ Package: \tab MDtools\cr Type: \tab Package\cr Version: \tab
#' 0.3.0\cr Date: \tab 2016-06-17\cr License: \tab GPL-3\cr }
#'
#' @name eseis-package
#' @aliases eseis
#' @docType package
#' @author Michael Dietze
#' @keywords package
#' @importFrom graphics image
#' @importFrom stats ccf spec.taper spectrum filter spec.pgram spec.ar
#' @importFrom methods as
#' @importFrom Rcpp evalCpp
#' @useDynLib eseis
NULL

#' example data
#' 
#' Seismic trace of a rockfall event.
#' 
#' The dataset comprises the seismic signal (vertical component) of a rockfall
#' event, preceeded by an earthquake.
#' 
#' @name rockfall
#' @docType data
#' @format The format is: num [1:98400] 65158 65176 65206 65194 65155 ...
#' @keywords datasets
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## plot signal vector
#' plot(x = t, y = rockfall, type = "l")
#' 
NULL

#' example data
#' 
#' Time vector of a rockfall event.
#' 
#' The dataset comprises the time vector corresponding the to seismic signal
#' of a rockfall event.
#' 
#' @name t
#' @docType data
#' @format The format is: POSIXct[1:98400], format: "2015-04-06 13:16:54" ...
#' @keywords datasets
#' @examples
#' 
#' ## load example data set
#' data(examples)
#' 
NULL



