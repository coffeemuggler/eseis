#' Calculate spectrograms (power spectral density estimates) from time series.
#' 
#' This function creates spectrograms from seismic signals. It supports the
#' standard spectrogram apporach, multitaper, and the Welch method.
#' 
#' @param data \code{List} object, spectrogram to be plotted. Must be output
#' of \code{signal_spectrogram()} or of equivalent structure.
#' 
#' @param col \code{Character} scalar, colour palette to use. Default is 
#' \code{tim.colors}.
#' 
#' @return Graphic output of a spectrogram.
#' @author Michael Dietze
#' @seealso \code{\link{signal_spectrogram}}
#' @keywords eseis
#' @examples
#' 
#' ## TO BE ADDED LATER
#'                      
#' @export plot_spectrogram
plot_spectrogram <- function(
  data,
  col = "tim.colors"
) {
  
  ## check input data
  if(class(data) != "spectrogram") {
    
    if(class(data) != "list" | 
       length(data) != 3 | 
       class(data[[1]]) != "matrix" |
       class(data[[2]])[1] != "POSIXct" | 
       class(data[[3]]) != "numeric") {
         
         stop("Input data is not appropriate!")
       }
  }
  
  image(x = data$t, 
        y = data$f, 
        z = t(data$S), 
        col = do.call(what = col, 
                      args = list(200)))
  
}