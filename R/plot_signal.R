#' Plot a seismic signal
#' 
#' This function plots a line graph of a seismic signal. To avoid long plot 
#' times the signal is reduced to a given number of points.
#' 
#' The \code{format} argument is based on hints provided by Sebastian 
#' Kreutzer and Christoph Burow. It allows plotting time axis units in 
#' user defined formats. The time format must be provided as character string 
#' using the POSIX standard (see documentation of \code{strptime} for a list 
#' of available keywords), e.g., "%H:%M:%S" for hour:minute:second.
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
#' plot_signal(data = rockfall_z, time = rockfall_t)
#' 
#' ## plot data set with lower resolution
#' plot_signal(data = rockfall_z, time = rockfall_t, n = 100)
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
  
  if ("xlab" %in% names(args)) {
    xlab <- args$xlab
  }
  else {
    
    xlab = "Time"
  }
  
  if ("ylab" %in% names(args)) {
    ylab <- args$ylab
  }
  else {
    
    ylab = "Amplitude"
  }
  
  if ("axes" %in% names(args)) {
    axes <- args$axes
  }
  else {
    
    axes <- TRUE
  }
  
  if ("format" %in% names(args)) {
    format <- args$format
  }
  else {
    
    format <- ""
  }
  
  ## remove keywords
  keywords <- c("format")
  args <- args[!names(args)%in%keywords]
  
  ## account for data sets smaller than n
  if(length(data) < n) {
    
    n <- length(data)
  }
  
  ## get NA values to padd data set
  n_padd <- ceiling(x = length(data) / n) * n - length(data)
  
  ## padd data set
  s_padd <- c(data, rep(NA, 
                        times = n_padd))
  
  t_padd <- c(time, rep(NA, 
                        times = n_padd))
  
  ## convert signal vector to matrix
  S <- matrix(data = s_padd, 
              ncol = n)
  
  ## calculate columnwise min and max
  S_min <- matrixStats::colMins(x = S, 
                                na.rm = TRUE)
  
  S_max <- matrixStats::colMaxs(x = S, 
                                na.rm = TRUE)
  
  ## convert min and max alternatingly to vector
  s_plot <- as.numeric(rbind(S_min, S_max))
  
  ## get time vector subset
  i <- seq(from = 1, 
           to = length(t_padd), 
           by = nrow(S))
  
  ## make pairs of time vector subsets
  t_plot <- rep(t_padd[i], each = 2)
  
  
  ## generate plot
  do.call(what = graphics::plot, 
          args = c(list(t_plot, 
                        s_plot, 
                        type = type, 
                        xlab = xlab,
                        ylab = ylab,
                        axes = FALSE), 
                   args))
  
  ## add box
  graphics::box(which = "plot")
  
  ## optionally add axes
  if(axes == TRUE) {
    
    graphics::axis.POSIXct(side = 1, 
                           x = t_plot, 
                           format = format)
    
    graphics::axis(side = 2)
  }
}