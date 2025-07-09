#' Clip, interpolate (and optionally smoothen) a spectrogram
#' 
#' The function interpolates the power values in the frequency range defined 
#' by the user, applying a spline interpolator. This function can be used to 
#' extract a given frequency range or frequency band from a PSD. The former is 
#' useful for convenient preparation of the \code{fmi_} function family to 
#' achieve fluvial model inversion workflows. The latter is useful to create 
#' seismic power time series for a given frequency band. 
#' 
#' @param data \code{List} object, spectrogram to be processed. Must be the 
#' output of \code{signal_spectrogram} or of an equivalent structure.
#' 
#' @param f \code{Numeric} vector of length two, frequency range to which 
#' the PSD will be clipped. Alternatively, an empirical frequency vector can 
#' be submitted, too (e.g., when non-uniformly spaced frequencies are used).
#' 
#' @param res \code{Numeric} value, number of frequency values used as output 
#' of the frequency interpolation. If set to \code{1}, 
#' the input frequency range (\code{f}) will be averaged and a 
#' \code{data frame} with the elements \code{time} and \code{power} will 
#' be returned.
#' 
#' @param n \code{Numeric} value, number of values to use in a running average 
#' filter to smoothen the interpolated output. If omitted, the spectrogram 
#' will not be changed apart from the interpolation option.
#' 
#' @param dt \code{Numeric} value, sampling interval. Only required if 
#' \code{data} is no eseis object.
#' 
#' @return A clipped and interpolated spectrogram, or a data frame with 
#' a time series of an averaged frequency band.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## load example data
#' data(rockfall)
#' 
#' ## calculate spectrogram
#' psd <- signal_spectrogram(data = rockfall_eseis)
#' 
#' ## Example 1: interpolate PSD to 5 to 15 Hz with 100 values resolution
#' psd_int <- aux_clipspectrogram(data = psd, f = c(5, 15), res = 100)
#' 
#' ## plot output
#' plot(psd_int)
#' 
#' ## Example 2: interpolate PSD using user defined frequency vector
#' psd_int <- aux_clipspectrogram(data = psd, f = sort(runif(n = 20, 5, 20)))
#' 
#' ## plot output
#' plot(psd_int)
#' 
#' ## Example 3: interpolate non-eseis matrix object
#' psd_int <- aux_clipspectrogram(data = psd$PSD$S, 
#'                                f = c(0, 10), 
#'                                res = 10, 
#'                                dt = 1/50)
#'                                
#' ## Example 4: extract a power time series for a frequency band
#' p_ts <- aux_clipspectrogram(data = psd, f = c(5, 10), res = 1)
#' plot(p_ts, type = "l")
#' 
#' ## Example 5: clip and smoothen the output
#' p_sm <- aux_clipspectrogram(data = psd, f = c(5, 20), res = 50, n = 10)
#' plot(p_sm)
#' 
#' @export aux_clipspectrogram

aux_clipspectrogram <- function(

  data,
  f,
  res = 100,
  n,
  dt
  
) {
  
  ## check for eseis object structure
  if(inherits(x = data, what = "eseis") == FALSE) {
    
    if(missing(f) | missing(res) | missing(dt)) {
      
      stop("The data is no eseis object and f, res and dt are missing!")
    } else {
      
      p <- data
      f_emp <- NA
    }
  } else {
    
    ## assign spectrogram object and frequency vector
    p <- data$PSD$S
    f_emp <- data$PSD$f
  }
  
  ## check/set interpolation frequency vector
  if(length(f) == 2) {
    
    ## check correct order of frequency limits
    if(f[1] >= f[2]) {
      
      stop("Lower frequency is not smaller than higher!")
    }

    ## build frequency vector
    f_int <- seq(from = f[1], to = f[2], length.out = res)
    
  } else {
    
    f_int <- f
  }
  
  ## optionally assign synthetic frequency vector
  if(any(is.na(f_emp))) {
    
    f_emp <- seq(from = 0, to = 1 / dt / 2, length.out = nrow(p))
  }

  ## average or interpolate data set
  if(res == 1) {
    
    ## calculate average power and frequency
    p_avg <- colMeans(p[f_emp >= f[1] & f_emp <= f[2],])

    ## get output time vector
    if(inherits(x = data, what = "eseis") == TRUE) {
      
      t_out <- data$PSD$t
    } else {
      
      t_out <- 1:length(p_avg)
    }
    
    ## create output data set
    data <- data.frame(time = t_out,
                       power = p_avg) 
    
    ## optionally smoothen data set
    if(missing(n) == FALSE) {
      
      data$power <- caTools::runmean(x = data$power, k = n)
    }
    
  } else {
    
    ## clip and interpolate PSD to frequency space of interest
    p_clip <- apply(X = p, MARGIN = 2, FUN = function(x, f_emp, f_int) {
      
      spline(x = f_emp, y = x, xout = f_int)$y
    }, f_emp, f_int)
    
    ## optionally, smoothen data set
    if(missing(n) == FALSE) {
      
      p_clip <- apply(X = p, MARGIN = 2, FUN = function(x, n) {
        
        caTools::runmean(x = x, k = n)
      }, n)
    }
    
    ## prepare output
    if(inherits(x = data, what = "eseis") == TRUE) {
      
      data$PSD$S <- p_clip
      data$PSD$f <- f_int
    } else {
      
      data <- p_clip
    }
  }
  
  ## return output
  return(data)
}
