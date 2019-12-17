#' eseis: Environmental Seismology Toolbox
#' 
#' Environmental seismoloy is a scientific field that studies the seismic 
#' signals, emitted by Earth surface processes. This package eseis provides 
#' all relevant functions to read/write seismic data files, prepare, analyse
#' and visualise seismic data, and generate reports of the processing history.
#'
#' \tabular{ll}{
#' **Package:** \tab eseis \cr
#' **Type:** \tab Package \cr
#' **Version:** \tab 0.4.0 \cr
#' **Date:** \tab 2018-04-25 \cr
#' **License:** \tab GPL-3 \cr
#' }
#' 
#' @name eseis
#' @aliases eseis-package
#' @docType package
#' @author Michael Dietze
#' @keywords package
#' @importFrom graphics image plot plot.default axis axis.POSIXct box mtext
#' @importFrom stats acf ccf spec.taper spectrum filter spec.pgram spec.ar median nextn runif sd quantile splinefun cor nls residuals
#' @importFrom methods as new
#' @importFrom Rcpp evalCpp
#' @importFrom IRISSeismic readMiniseedFile getNetwork getStation getSNCL
#' @importFrom grDevices colorRampPalette dev.off jpeg
#' @importFrom rmarkdown render
#' @importFrom utils combn read.delim write.table read.table sessionInfo download.file browseURL
#' @importFrom XML xmlParse xmlToList
#' @useDynLib eseis
NULL

#' Seismic trace of a rockfall event.
#' 
#' The dataset comprises the seismic signal (vertical component) of 
#' a rockfall event, preceeded by an earthquake. The data have been
#' recorded at 200 Hz sampling frequency with an Omnirecs Cube ext 3
#' data logger.
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
#' ## plot signal vector using base functionality
#' plot(x = rockfall_t, y = rockfall_z, type = "l")
#' 
#' ## plot signal vector using the package plot function
#' plot_signal(data = rockfall_z, time = rockfall_t)
#' 
"rockfall_z"

#' Time vector of a rockfall event.
#' 
#' The dataset comprises the time vector corresponding the to seismic signal
#' of the rockfall event from the example data set "rockfall".
#' 
#' @name rockfall
#' @docType data
#' @format The format is: POSIXct[1:98400], format: "2015-04-06 13:16:54" ...
#' @keywords datasets
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
"rockfall_t"

#' An eseis object of a rockfall event.
#' 
#' The dataset comprises the seismic signal (vertical component) of 
#' a rockfall event, preceeded by an earthquake. The data have been
#' recorded at 200 Hz sampling frequency with an Omnirecs Cube ext 3
#' data logger.
#' 
#' @name rockfall
#' @docType data
#' @format List of 4
#'          $ signal : num [1:98399] 65211 65192 65158 65176 65206 ...
#'          $ meta   :List of 12
#'           ..$ station  : chr "789     "
#'           ..$ network  : chr "XX      "
#'           ..$ component: chr "p0      "
#'           ..$ n        : int 98399
#' @keywords datasets
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
"rockfall_eseis"

#' Seismic traces of a small earthquake
#' 
#' The dataset comprises the seismic signal (all three components) of 
#' a small earthquake. The data have been recorded at 200 Hz sampling 
#' frequency with an Omnirecs Cube ext 3 data logger.
#' 
#' @name earthquake
#' @docType data
#' @format The format is: 
#' List of 3
#'  $ BHE: num [1:8001] -3.95e-07 ...
#'  $ BHN: num [1:8001] -2.02e-07 ...
#'  $ BHZ: num [1:8001] -1.65e-07 ...
#' @keywords datasets
#' @examples
#' 
#' ## load example data set
#' data(earthquake)
#' 
#' ## plot signal vector
#' plot(x = t, y = s$BHZ, type = "l")
#' 
"s"

#' Time vector for seismic traces of a small earthquake
#' 
#' The dataset comprises the time vector associated with the data set
#' \code{earthquake}.
#' 
#' @name earthquake
#' @docType data
#' @format The format is: POSIXct[1:98400], format: "2015-04-06 13:16:54" ...
#' @keywords datasets
#' @examples
#' 
#' ## load example data set
#' data(earthquake)
#' 
#' ## show range of time vector
#' range(t)
#' 
"t"