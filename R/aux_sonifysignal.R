#' Convert seismic signal to sound (sonification)
#' 
#' The function converts a seismic signal to sound and saves it as a 
#' wav file.
#' 
#' @param data \code{eseis} object to be converted to sound file
#' 
#' @param file \code{Character} value, file name under which the sonified 
#' signal is saved.
#' 
#' @param aggregate \code{Numeric} value, factor by which the seismic 
#' file is aggregated before conversion. Aggregation is performed by 
#' linear interpolation.
#' 
#' @param amplification \code{Numeric} value, amplification factor. Default 
#' is \code{10^6}.
#' 
#' @param speed \code{Numeric} value, factor by which sampling rate is 
#' increased to make sound sensible. The higher the speed value, the 
#' higher is the tone. Default is \code{1} (100 Hz seismic signal becomes 
#' 100 Hz sound signal).
#' 
#' @param dt \code{Numeric} value, samplig rate. Only needed if data is 
#' not an \code{eseis} object.
#' 
#' @return Sound file in wav format, written to disk.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## load example data
#' data(rockfall)
#' 
#' ## deconvolve and taper signal
#' s <- signal_deconvolve(data = rockfall_eseis)
#' s <- signal_taper(data = s, p = 0.05)
#' 
#' ## sonify as is (barely sensible, due to too low frequency)
#' aux_sonifysignal(data = s, 
#'                  file = "~/Downloads/r1.wav")
#' 
#' ## sonify at 20-fold speed
#' aux_sonifysignal(data = s, 
#'                  file = "~/Downloads/r1.wav", 
#'                  speed = 20)
#' }
#' 
#' @export aux_sonifysignal
#' 
aux_sonifysignal <- function(
  data,
  file, 
  aggregate = 1,
  amplification = 10^6, 
  speed = 1, 
  dt
) {
  
  ## check if package seewave is installed
  if (requireNamespace("seewave", quietly = TRUE) == FALSE) {
    
    stop("Package seewave is not installed, operation not possible!")
  }
  
  ## get number of samples
  n <- ifelse(test = inherits(x = data, what = "eseis"), 
              yes = data$meta$n, 
              no = length(data))
  
  ## check/set dt
  if(class(data)[1] == "eseis") {
    
    dt <- data$meta$dt
  } else {
    
    if(missing(dt) == TRUE) {
      
      stop("No eseis object and dt missing!")
    }
  }
  
  ## optionally aggregate data set
  if(aggregate == 1) {
    
    s <- as.numeric(data$signal)
  } else {
    
    s <- stats::approx(x = seq(from = 1, to = n), 
                       y = data$signal,
                       xout = seq(from = 1, 
                                  to = n, 
                                  by = 1 / aggregate))$y
  }
  
  ## amplify data set
  s <- as.numeric(s * amplification)
  
  ## save data set
  seewave::savewav(wave = s, 
                   filename = file,
                   f = 1/dt * speed)
}