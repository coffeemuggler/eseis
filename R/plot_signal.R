#' Plot a seismic signal
#' 
#' This function plots a line graph of a seismic signal. To avoid long plot 
#' times the signal is reduced to a given number of points.
#' 
#' @param data \code{Numeric} vector to plot.
#' 
#' @param time \code{POSIXct} vector, corresponding time vector.
#' 
#' @param n \code{Numeric} value, number of values to which the dataset 
#' is reduced. Default is \code{10000}.
#' 
#' @param \dots Further arguments passed to the plot function.
#' 
#' @return A line plot.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## plot data set straightforward
#' plot_signal(data = rockfall, time = t)
#' 
#' ## plot data set with lower resolution
#' plot_signal(data = rockfall, time = t, n = 100)
#' 
#' @export plot_signal
#' 
plot_signal <- function(
  
  data,
  time,
  n = 10000,
  ...
) {
  
  ## extract additional plot arguments
  args <- list(...)
  
  ## check/set plot arguments
  if ("type" %in% names(args)) {
    type <- args$type
  }
  else {
    
    type = "l"
  }
  
  ## create index vector
  i <- seq(from = 1,
           to = length(data))
  
  ## create bin vector
  b <- round(x = seq(from = 1,
                     to = length(data),
                     length.out = n),
             digits = 0)
  
  ## make bin indices
  bins <- findInterval(x = i,
                       vec = b)
  
  ## find maxima per bin
  b_max = stats::aggregate(x = list(x = i,
                                    y = data),
                           by = list(bins = bins),
                           max)
  
  ## find minima per bin
  b_min = stats::aggregate(x = list(x = i,
                                    y = data),
                           by = list(bins = bins),
                           min)
  
  ## create plot vectors
  x_plot <- rep(b_min$x, 
                each = 2)
  
  y_plot <- as.numeric(rbind(b_min$y, 
                             b_max$y))
  
  ## generate plot
  graphics::plot(time[x_plot], 
                 y_plot, 
                 type = type, 
                 ...)
}