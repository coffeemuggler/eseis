stalta <-
structure(function(# Function to detect events by the STA-LTA-method.

  time,
  ### Numeric \code{vector}, optional, time of the data set to be processed.
  
  data,
  ### Numeric \code{vector} or \code{matrix}, data set to be processed.
  
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
  
  freeze.lta = FALSE,
  ### Logical \code{scalar}, option to freeze lta value at event onset.
  
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
  
  ## homogenise data structure
  if(is(object = data, class2 = "matrix") == FALSE) {
    data <- rbind(data)
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
  n <- matrix(data = rep(seq(from = 1, 
                             to = ncol(data)), 
                         nrow(data)), 
              nrow = nrow(data), 
              byrow = TRUE)
  
  n.time <- matrix(data = rep(seq(from = time[1], 
                                  by = dt,
                                  length.out = ncol(n)), 
                              nrow(data)), 
                   nrow = nrow(data), 
                   byrow = TRUE)
  
  ## calculate number of samples in windows - half is for runmean window size
  n.sta <- round(t.sta / (2 * dt), 0)
  n.lta <- round(t.lta / (2 * dt), 0)
  
  ## calculate sta-lta ratio
  signal.sta <- t(apply(X = data, 
                        MARGIN = 1, 
                        FUN = caTools::runmean, 
                        k = n.sta, 
                        alg = "fast", 
                        endrule = "NA",
                        align = "right"))
  
  signal.lta <- t(apply(X = data, 
                        MARGIN = 1, 
                        FUN = caTools::runmean, 
                        k = n.lta, 
                        alg = "fast", 
                        endrule = "NA",
                        align = "right"))

  ## decision on freezed lta option
  if(freeze.lta == TRUE) {
    
    ## create output data set
    event <- data * 0
    
    ## loop through all signals
    for(h in 1:nrow(data)) {
      
      ## create output variables
      T1 <- 0
      T2 <- 0
      
      ## assign event trigger
      for(i in 1:(length(event[h,]) - 1)) {
        
        ## calculate ratio and optionally remove na-value
        ratio <- signal.sta[h,i] / signal.lta[h,i]
        
        if(is.na(ratio) == TRUE) {
          ratio <- 0
        }
        
        if(ratio > ratio.on | T2 == 1) {
          signal.lta[h,i + 1] <- signal.lta[h,i]
          T1 <- 1
        } else {
          T1 <- 0
        }
        
        if(T1 == 1 & ratio > ratio.off) {
          T1 <- 1
          T2 <- 1
          event[h,i] <- 1
        } else {
          T2 <- 0
        }
      }
    }
    
  } else {
    
    ## calculate ratio
    ratio <- signal.sta / signal.lta
    ratio[is.na(ratio)] <- 0
    
    ## create output variables
    event <- data * 0
    
    for(h in 1:nrow(data)) {
     
      T1 <- 0
      T2 <- 0
      
      ## assign event trigger
      for(i in 1:length(ratio[h,])) {
        
        if(ratio[h,i] > ratio.on | T2 == 1) {
          T1 <- 1
        } else {
          T1 <- 0
        }
        
        if(T1 == 1 & ratio[h,i] > ratio.off) {
          T1 <- 1
          T2 <- 1
          event[h,i] <- 1
        } else {
          T2 <- 0
        }
      } 
    }
  }
  
  ## calculate event state switches
  event.diff <- t(apply(X = event, 
                        MARGIN = 1, 
                        FUN = diff))
  
  ## create output data set
  events <- vector(mode = "list", length = nrow(data))

  for(i in 1:nrow(n)) {
    
    ## asign on and off states
    event.on <- n[i,event.diff[i,] == 1]
    event.off <- n[i,event.diff[i,] == -1]
    
    ## calculate event duration
    event.duration <- (event.off - event.on) * dt
    
    ## create ID vector
    ID <- seq(from = 1, to = length(event.duration))
    
    ## create output data set
    if(length(event.on) > 0) {
      events[[i]] <- data.frame(ID = ID,
                                start = n.time[i, event.on],
                                duration = event.duration)
    } else {
      print("No events detected.")
      events <- data.frame(ID = NA,
                           start = NA,
                           duration = NA)
      sta <- rep(x = NA, times = length(data))
      lta <- rep(x = NA, times = length(data))
      event <- rep(x = NA, times = length(data))
    }
  }
  
  ### return output data set(s)
  if(extended == FALSE) {
    return(events)
  } else {
    return(list(events = events,
                sta = signal.sta,
                lta = signal.lta,
                event = event))
  }
  
}, ex = function(){
  ## load example data set
  data(signal.1, envir = environment())
  
})