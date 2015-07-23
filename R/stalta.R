stalta <-
structure(function(# Function to detect events by the STA-LTA-method.
  time,
  ### Numeric \code{vector}, optional, time of the data set to be processed.
  data,
  ### Numeric \code{vector}, data set to be processed.
  ratio.on,
  ### Numeric \code{scalar} sta-lta-ratio for on-trigger.
  ratio.off,
  ### Numeric \code{scalar} sta-lta-ratio for off-trigger.
  t.sta,
  ### Numeric \code{scalar} length of short time average in seconds.
  t.lta,
  ### Numeric \code{scalar} length of long time average in seconds.
  dt,
  ### Numeric \code{scalar}, sampling period.
  extended = FALSE,
  ### Logical \code{scalar}, option to return extended output (sta levels, lta 
  ### levels). Output will be a list in this case. Default is \code{FALSE}.
  envelope = "abs"
  ### Character \code{scalar}, option to calculate the signal envelope prior 
  ### to sta and lta evaluation. Options are \code{"abs"} (absolute, fastest), 
  ### \code{"sqrt"} (root of the square) and \code{"hilbert"} (root of the 
  ### square of the hilbert transform, way slowest). Default is \code{"abs"}.
){

  ## check/set parameters
  if(missing(dt) == TRUE) {
    stop("No sampling period dt provided!")
  }
  
  if(missing(time) == TRUE) {
    print("No time vector provided, output will refer to seconds in data!")
    time <- seq(from = 0, to = length(data)) * dt
  }
  
  ## calculate absolute of signal
  if(envelope == "abs") {
    data <- abs(data)
  } else if(envelope == "sqrt") {
    data <- sqrt(data^2)
  } else {
    data <- envelope(data)
  }

  ## create sample vector
  n <- seq(from = 1, to = length(data))
  
  ## calculate number of samples in windows - half of them for runmean window size
  n.sta <- round(t.sta / (2 * dt), 0)
  n.lta <- round(t.lta / (2 * dt), 0)
  
  ## calculate sta-lta ratio
  signal.sta <- runmean(x = data, 
                        k = n.sta, 
                        alg = "fast", 
                        endrule = "NA",
                        align = "right")
  
  signal.lta <- runmean(x = data, 
                        k = n.lta, 
                        alg = "fast", 
                        endrule = "NA",
                        align = "right")
  
  ratio <- signal.sta / signal.lta
  ratio[is.na(ratio)] <- 0
  
  ## create output variables
  event <- numeric(length = length(data))
  T1 <- 0
  T2 <- 0
  
  ## assign event trigger
  for(i in 1:length(ratio)) {
    
    if(ratio[i] > ratio.on | T2 == 1) {
      T1 <- 1
    } else {
      T1 <- 0
    }
    
    if(T1 == 1 & ratio[i] > ratio.off) {
      T1 <- 1
      T2 <- 1
      event[i] <- 1
    } else {
      T2 <- 0
    }
  }
  
  ## calculate event state switches
  event.diff <- diff(event)
  
  ## create on and off states
  event.on <- n[event.diff == 1]
  event.off <- n[event.diff == -1]
  
  ## calculate event duration
  event.duration <- (event.off - event.on) * dt
  
  ## create event ID vector
  ID <- seq(from = 1, to = length(event.duration))
  
  ## create output matrix
  if(length(ivent.on) > 0) {
    events <- cbind(ID,
                    time[event.on],
                    event.duration)
  } else {
    print("No events detected.")
    events <- cbind(rep(x = NA, times = 3))
    sta <- rep(x = NA, times = length(data))
    lta <- rep(x = NA, times = length(data))
  }
  
  ### return output data set(s)
  if(extended == FALSE) {
    return(events)
  } else {
    return(list(events = events,
                sta = signal.sta,
                lta = signal.lta))
  }
  
}, ex = function(){
  ## load example data set
  data(signal.1, envir = environment())
  
})
