trigger.stalta <-
structure(function(# Function to detect events by the STA-LTA-method.
  ### This function provides methods to detect seismic events in signals. It 
  ### uses the ratio of STA (short-time average) and LTA (long-time average)
  ### or the absolute signal level to trigger events. The result is either
  ### appended to the data set or returned as list object.
  data,
  ### \code{List} object, data set to be processed
  stations,
  ### Numeric or character \code{vector}, number or name of the stations to 
  ### be processed. If omitted, als stations are processed.
  component = "z",
  ### Character \code{scalar}, component to be processed. If \code{"sum"} is
  ### specified, the sum of the absolute values of all components are used.
  trigger.ratio,
  ### Numeric \code{vector} of length two, ratio of STA and LTA. First value
  ### specifies trigger-on value, second value specifies trigger-off value.
  window.size,
  ### Numeric \code{vector} of length two, window sizes for STA and LTA in
  ### seconds.
  time.extra,
  ### Numeric \code{vector} of length two, pre- and post-time to be added to
  ### the detected event, in seconds. If omitted, both are set to zero.
  time.min,
  ### Numeric \code{scalar}, optional minimum length of the triggered event.
  square.signal = FALSE,
  ### Logical \code{scalar}, option to use the squared signal for event
  ### triggering, default is \code{FALSE}.
  dt,
  ### Numeric \code{scalar}, sampling period. Must be provided if deviating
  ### from value in \code{data$summary$dt}.
  append = TRUE
  ### Logical \code{scalar}, option to append result to the input data set. If
  ### set \code{FALSE}, a list object is returned with triggered events for
  ### each station.
){
  ## get stations to process
  if(missing(stations) == TRUE) {
    stations <- seq(from = 1, to = length(data$signal))
  }
 
  ## check/set parameters
  if(missing(dt) == TRUE) {dt <- data$summary$dt}
  if(missing(time.extra) == TRUE) {time.extra <- c(0, 0)}
  if(missing(time.min) == TRUE) {time.min <- 0}
  
  ## reassign component IDs
  if(component != "sum") {
    component[component == "x"] <- 1
    component[component == "y"] <- 2
    component[component == "z"] <- 3
    component <- as.numeric(component)
    component.sum = FALSE
  } else {
    component.sum <- TRUE
    component <- 1
  }
  
  ## calculate roughly half the number of samples in window
  n.sta <- round(window.size[1] / (2 * dt), 0)
  n.lta <- round(window.size[2] / (2 * dt), 0)
  
  ## create dummy output data set
  events <- list(numeric(0))
  
  ## loop through all stations
  for(i in 1:length(data$signal)){
    
    ## create signal vector
    signal <- data$signal[[stations[i]]][[component]]
    
    ## optionally, compute absolute component sum
    if(component.sum == TRUE) {
      signal <- colSums(rbind(abs(data$signal[[stations[i]]][[1]]),
                              abs(data$signal[[stations[i]]][[2]]),
                              abs(data$signal[[stations[i]]][[3]])))
    }
    
    ## optionally, compute squared signals
    if(square.signal == TRUE) {
      signal <- signal^2
    }
    
    ## calculate STA-LTA ratio
    signal.sta <- runmean(x = signal, 
                          k = n.sta, 
                          alg = "fast", 
                          endrule = "NA",
                          align = "right")
    
    signal.lta <- runmean(x = signal, 
                          k = n.lta, 
                          alg = "fast", 
                          endrule = "NA",
                          align = "right")
    
    ratio <- signal.sta / signal.lta
    ratio <- ifelse(is.na(ratio) == TRUE, 0, ratio)
    
    ## create output variables
    event <- numeric(length(signal))
    T1 <- 0
    T2 <- 0
    
    ## assign event trigger
    for(i in 1:length(ratio)) {
      
      if(ratio[i] > trigger.ratio[1] | T2 == 1) {
        T1 <- 1
      } else {
        T1 <- 0
      }
      
      if(T1 == 1 & ratio[i] > trigger.ratio[2]) {
        T1 <- 1
        T2 <- 1
        event[i] <- 1
      } else {
        T2 <- 0
      }
    }
    
    ## identify event onsets and ends
    event.on <- ifelse(event[2:length(event)] >
                             event[1:(length(event) - 1)], 1, 0)
    event.off <- ifelse(event[2:length(event)] <
                         event[1:(length(event) - 1)], 1, 0)
    event.on <- seq(1, length(signal))[event.on == 1]
    event.off <- seq(1, length(signal))[event.off == 1]
    
    ## calculate data points before and after event
    n.pre <- round(time.extra[1] / dt, 0)
    n.post <- round(time.extra[2] / dt, 0)
    
    ## optionally, add extra time to event vector
    event.on <- event.on - n.pre
    event.off <- event.off + n.post
    
    ## optionally, add minimum event length time
    for(i in 1:length(event.on)) {
      if(event.off[i] - event.on[i] < time.min / dt) {
        event.off[i] <- event.off[i] + round(time.min / dt, 0)
      }
    }
    
    ## recalculate event vector
    for(i in 1:length(event.on)) {
      event[event.on[i]:event.off[i]] <- 1
    }
     
    events[[length(events) + 1]] <- event
 }


  ## remove dummy output
  events[[1]] <- NULL
  
  ## assign events to data set
  data$event <- events
  
  ## return result
  if(append == TRUE) {
    return(data)
  } else {
    return(events)
  }
  ### A list object.
  
  ##references<<
  
  ##keyword<<
  ## environmental seismology
  
}, ex = function(){
  ## load example data set
  data(signal.1, envir = environment())
  
  ## trigger events
  signal.1.events <- trigger.stalta(data = signal.1, 
                                    trigger.ratio = c(4, 2), 
                                    window.size = c(6, 60))
})
