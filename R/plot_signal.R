#' Plot a seismic signal
#' 
#' This function plots a line graph of a seismic signal. To avoid long plot 
#' preparation times the signal is reduced to a given number of points.
#' 
#' The \code{format} argument is based on hints provided by Sebastian 
#' Kreutzer and Christoph Burow. It allows plotting time axis units in 
#' user defined formats. The time format must be provided as character string 
#' using the POSIX standard (see documentation of \code{strptime} for a list 
#' of available keywords), e.g., "%H:%M:%S" (hour:minute:second),
#' "%Y-%m-%d" (year-month-day).
#' 
#' @param data \code{eseis} object or \code{numeric} vector, data set to 
#' be plotted.
#' 
#' @param time \code{POSIXct} vector, corresponding time vector.
#' 
#' @param n \code{Numeric} value, number of values to which the dataset 
#' is reduced. Default is \code{10000}.
#' 
#' @param \dots Further arguments passed to the plot function.
#' 
#' @return A line plot of a seismic wave form.
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
#' plot_signal(data = rockfall_eseis)
#' 
#' ## plot data set with lower resolution
#' plot_signal(data = rockfall_eseis, n = 100)
#' 
#' ## plot data set but not as an eseis object
#' plot_signal(data = rockfall_z, time = rockfall_t)
#' 
#' ## load earthquake data set
#' data(earthquake)
#' 
#' ## plot all three components (after changing plot options)
#' pars <- par(no.readonly = TRUE)
#' par(mfcol = c(3, 1))
#' 
#' plt <- lapply(s, plot_signal, t = t)
#' 
#' par(pars)
#' 
#' @export plot_signal
#' 
plot_signal <- function(
  
  data,
  time,
  n = 10000,
  ...
) {
  
  ## check if input object is of class eseis
  if(class(data)[1] == "eseis") {
    
    ## set eseis flag
    eseis_class <- TRUE
    
    ## store initial object
    eseis_data <- data
    
    ## extract signal vector
    data <- eseis_data$signal
    
    ## create time vector
    time <- seq(from = eseis_data$meta$starttime, 
                by = eseis_data$meta$dt, 
                length.out = eseis_data$meta$n)
    
  } else {
    
    ## set eseis flag
    eseis_class <- FALSE
  }
  
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
  keywords <- c("format", "axes", "xlab", "ylab")
  args <- args[!names(args)%in%keywords]
  
  ## account for data sets smaller than n
  if(length(data) < n) {
    
    n <- length(data)
  }
  
  ## get NA values to pad data set
  n_pad <- ceiling(x = length(data) / n) * n - length(data)
  
  ## pad data set
  s_pad <- c(data, rep(NA, 
                        times = n_pad))
  
  t_pad <- c(time, rep(NA, 
                        times = n_pad))
  
  ## convert signal vector to matrix
  S <- matrix(data = s_pad, 
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
           to = length(t_pad), 
           by = nrow(S))
  
  ## make pairs of time vector subsets
  t_plot <- rep(t_pad[i], each = 2)
  
  
  ## generate plot
  do.call(what = graphics::plot, 
          args = c(list(t_plot, 
                        s_plot, 
                        type = type, 
                        xlab = xlab,
                        ylab = ylab,
                        axes = FALSE), 
                   args))
  
  ## optionally add axes
  if(axes == TRUE) {
    
    graphics::axis.POSIXct(side = 1, 
                           x = t_plot, 
                           format = format)
    
    graphics::axis(side = 2)
    
    graphics::box(which = "plot")
  }
}