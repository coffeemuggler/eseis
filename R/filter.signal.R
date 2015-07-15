filter.signal <-
structure(function(# Function to filter seismic signals
  ### This function filters seismic signals.

  data,
  ### \code{Numeric} matrix or vector, data set(s) to be processed.
  
  f.low,
  ### \code{Numeric} scalar, low frequency cut-off, default is zero.
  
  f.high,
  ### \code{Numeric} scalar, high frequency cut-off, default is (1/2 dt)^-1.  
  ### The parameter dt is taken from the data set. If a time series is
  ### provided, \code{dt} must be specified separately.
  
  subtract.mean = TRUE,
  ###  \code{Logical} scalar, optionally remove the global mean before 
  ### processing, default is \code{TRUE}.
  
  type,
  ### \code{Character} scalar, type of filter, one out of \code{"LP"} (low 
  ### pass), \code{"HP"} (high pass), \code{"BP"} (band pass) and \code{"BR"}
  ### (band rejection).
  
  shape = "butter",
  ### \code{Character} scalar, filter shape, one out of \code{"butter"} (
  ### Butterworth).
  
  order = 2,
  ### \code{Numeric} scalar, order of the filter, default is \code{2}.
  
  dt,
  ### \code{Numeric} scalar, sampling period.
  
  ...
  ### Further arguments to pass.
){
  ## check/set parameters
  if(missing(type) == TRUE) {
    stop("Filter type unspecified!")
  }
  
  if(missing(f.high) == TRUE) {
    f.high <- 2 * dt
  }
  
  if(missing(dt) == TRUE) {
    stop("Sampling frequency unspecified!")
  }
  
  ## check/correct data structure
  if(is(data, "matrix") == FALSE) {
    data <- rbind(data)
  }
  
  ## filter signal with Butterworth filter
  if(shape == "butter") {
    
    ## translate filter type and frequencies for function "butter()"
    if(type == "LP") {
      type <- "low"
      filter.frequencies <- f.high * 2 * dt
    } else if(type == "HP") {
      type <- "high"
      filter.frequencies <- f.low * 2 * dt
    } else if(type == "BP") {
      type <- "pass"
      filter.frequencies <- c(f.low * 2 * dt, f.high * 2 * dt)
    } else if(type == "BR") {
      type <- "stop"
      filter.frequencies <- c(f.low * 2 * dt, f.high * 2 * dt)
    }
    
    ## optionally remove mean
    if(subtract.mean == TRUE) {
      data.mean <- rowMeans(x = data, na.rm = TRUE)
      data <- data - data.mean
    }
    
    ## filter signal
    for(i in 1:nrow(data)) {
      data[i,] <- filter(x = data[i,], 
                         butter(n = order,
                                W = filter.frequencies,
                                type = type))
    }
  } else {
    stop("Filter type not supported!")
  }
  
  return(data)
  ### A filtered data set
  
  ##details<<
  ## At the moment, only Butterworth filters are supported. If needed, further
  ## filter shapes may be implemented.
  
  ##seealso<<
  ## \code{\link{filter}}, \code{\link{butter}}
  
  ##references<<
  
  ##keyword<<
  ## 
}, ex = function(){
  ## load example data set
  data(signal.1, envir = environment())
  
  ## extract 3 traces of station 1 to matrix
  s.1 <- rbind(signal.1$signal$station1$x,
               signal.1$signal$station1$y,
               signal.1$signal$station1$z)
  
  ## filter the data set, i.e. bandpass filter between 1 and 60 Hz
  s.1.filtered <- filter.signal(data = s.1,
                                dt = 0.005,
                                f.low = 1, 
                                f.high = 60, 
                                type = "BP")
  
  ## plot unfiltered and filtered z-component over each other
  time <- signal.1$time$signal
  plot(time, s.1[3,],type = "l", col = "grey70")
  lines(time, s.1.filtered[3,], col = 2)
  ## ATTENTION, REPLACE PLOT EXAMPLE BY NEW FUNCTION plot.signal
})
