plot.signal <-
structure(function(# Function to plot seismic traces.
  ### This function plots seismic traces.
  
  data,
  ### Numeric \code{vector} or \code{matrix} with respective seismic traces.
  
  time,
  ### \code{Numeric} vector with time values.

  dynamic.resolution = 5000,
  ### \code{Numeric} scalar, option to decrease graph resolution for faster
  ### plottig, default is 5000. The value specifies the number of subsamples
  ### that are drawn from the data set. If set to \code{0} the entire data set
  ### is plotted.
  
  shared.limits,
  ### \code{Logical} scalar, option to use the same axes limits for all 
  ### plot panels. Only useful when plotting more than one signal trace.
  ### The parameter affects both, x- and y-axes.
  
  ...
  ### Further arguments passed.
){
  
  ## check/correct data structure
  if(is(data, "matrix") == FALSE) {
    data <- rbind(data)
  }
  
  ## optionally subsample data set for faster plotting
  if(dynamic.resolution > 0) {
    dynamic.resolution <- ifelse(dynamic.resolution > length(time),
                                 length(time), dynamic.resolution)
    n <- seq(from = 1, 
             to = length(time), 
             by =  round(length(time) / dynamic.resolution))
    time <- time[n]
    data <- data[,n]
  }
  
  ## find out time unit
  if(missing(unit) == TRUE) {
    
    ## calculate time difference and infer automatically set unit
    unit <- units(difftime(max(time, na.rm = TRUE), 
                           min(time, na.rm = TRUE)))
    
    ## convert units into appropriate keywords
    if(unit == "seconds") {
      unit <- "%S"
    } else if(unit == "mins") {
      unit <- "%M"
    } else if(unit == "hours") {
      unit <- "%H"
    } else if(unit == "days") {
      unit <- "%d"
    }
  }
  
  ## check/set plot parameters
  if("xlim" %in% names(list(...))) {
    xlim <- list(...)$xlim
  } else {
    xlim <- as.numeric(format(range(t, na.rm = TRUE), unit))
  }

  
  
  
  plot(x = time, y = data, type = "l", ...)

}, ex = function(){
  ## load example data
  data(signal.1, envir = environment())
  
  ## create traces matrix
  s.1 <- rbind(signal.1$signal$station1$x,
               signal.1$signal$station1$y,
               signal.1$signal$station1$z)

  ## straightforward plot of the data set
  plot.signal(data = s.1)

  ## create time and signal vector
  t <- signal.1$time$signal
  s <- signal.1$signal$station1$x
  
  ## plot of the data set with correct time
  plot.signal(time = t, data = s)
  
  ## plot only trace 1
  plot.signal(time = t, data = s.1[1,])
  
  ## plot in very low resolution (200 data points)
  plot.signal(time = t, data = s.1, dynamic.resolution = 200)
  
  ## plot in different colour, line type and headings
  plot.signal(time = t, data = s.1, col = 1:3, lty = 2:4, 
              xlab = rep("Time [min]", 3), ylim = c(-1500, 1500),
              main = c("X-component", "Y-component", "Z-component"))
  
  ## plot minute 3 to 6
  plot.signal(time = t, data = s.1, xlim = c(11.05, 11.1))
  
  ## plot three data sets over each other
  p <- plot.signal(time = t, data = s.1, plot = FALSE, output = TRUE)
  
  plot(p[1,], p[2,], type = "l", ylim =c(-1000, 1500), 
       main = "XYZ-components", xlab = "Time [h]", ylab = "amp")
  lines(p[1,], p[3,], col = 3)
  lines(p[1,], p[4,], col = 4)
})
