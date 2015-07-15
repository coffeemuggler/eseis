detect.events <-
structure(function(# Function to detect seismic events.
  ### This function provides methods to detect seismic events in signals. It 
  ### uses the ratio of STA (short-time average) and LTA (long-time average)
  ### to detect seismic events.
  
  data,
  ### \code{Numeric} vector or matrix, data set to be processed.
  
  dt,
  ### \code{Numeric} scalar, sampling period.
  
  trigger,
  ### \code{Numeric} vector of length two, STA/LTA-ratio values for trigger 
  ### on and trigger off.
  
  window.size,
  ### \code{Numeric} vector of length two, window sizes for STA and LTA in
  ### seconds.
  
  time.extra,
  ### \code{Numeric} vector of length two, pre- and post-time to be added to
  ### the detected event, in seconds. If omitted, both are set to zero.
  
  duration,
  ### \code{Numeric} scalar or vector of length two, optional minimum and  
  ### maximum length of the triggered event. If only scalar is provided this 
  ### is taken for minimum duration of an event.
  
  lta.freeze = FALSE,
  ### \code{Logical} scalar, optionally freeze the lta value after 
  ### trigger-on-value is reached.
  
  square.signal = FALSE,
  ### \code{Logical} scalar, option to use the squared signal for event
  ### triggering, default is \code{FALSE}.
  
  extra = FALSE
  ### \code{Logical} scalar, option to return additional output. If set to
  ### \code{TRUE} the event vectors are written to a list (\code{$events})
  ### and the individual event onsets, offsets and durations as well as 
  ### the STA/LTA ratios are returned as list objects.
  
){
  
  ## check/set parameters
  if(is.matrix(data) == FALSE) {
    data <- rbind(data)
  }
  
  if(missing(dt) == TRUE) {
    stop("Value for dt is missing!")
  }
  
  if(missing(time.extra) == TRUE) {
    time.extra <- c(0, 0)
  }
  
  if(missing(duration) == TRUE) {
    duration <- c(0, ncol(data))
  }
  
  if(length(duration) == 1) {
    duration <- c(duration, ncol(data))
  }
  
  ## optionally, compute squared signals
  if(square.signal == TRUE) {
    data <- data^2
  }
  
  ## calculate roughly half the number of samples in window
  n.sta <- round(window.size[1] / (2 * dt), 0)
  n.lta <- round(window.size[2] / (2 * dt), 0)
  
  ## calculate STA-LTA ratio
  sta <- t(apply(X = data, 
                 MARGIN = 1, 
                 FUN = runmean, 
                 k = n.sta, 
                 alg = "fast", 
                 endrule = "NA", 
                 align = "right"))
  
  lta <- t(apply(X = data, 
                 MARGIN = 1, 
                 FUN = runmean, 
                 k = n.lta, 
                 alg = "fast", 
                 endrule = "NA", 
                 align = "right"))
  
  ratio <- abs(sta / lta)
  ratio <- ifelse(is.na(ratio) == TRUE, 0, ratio)
  
  ## create time vector for duration caluclation and output variable
  t <- seq(from = 1, to = ncol(data))
  E <- data - data
  extra.data <- list(NA)
  
  ## loop through all data sets
  for(i in 1:nrow(data)) {
    
    ## find event onsets
    trigger.on <- ifelse(diff(x = ifelse(ratio[i,] > trigger[1], 1, 0), 
                              lag = 1) == 1, TRUE, FALSE) 
    events.on <- seq(from = 1, to = length(t))[trigger.on] + 1
    
    ## create dummy variable
    events <- rep(FALSE, length(t))
    
    ## trace events until trigger-off-value is reached
    if(length(events.on) > 0) {
      for(j in 1:length(events.on)) {
        t.n <- events.on[j]
        while(ratio[t.n] > trigger[2] & t.n < length(t)) {
          events[t.n] <- TRUE
          t.n <- t.n + 1
          if(lta.freeze == TRUE) {
            ratio[t.n] <- sta[t.n] / lta[events.on[j]]
          }
        }
      }
    }
    
    ## find switches in event trigger state
    events.switch <- diff(x = events, lag = 1) 
    
    ## get event onset time
    events.on <- seq(from = 1, to = length(t))[events.switch == 1] - 
      (time.extra[1] / dt)
    
    ## get event offset time
    events.off <- seq(from = 1, to = length(t))[events.switch == -1] + 
      (time.extra[2] / dt)
    
    ## calculate preliminary event duration
    events.duration <- (events.off - events.on) * dt
    
    ## remove events that do not fit duration limits
    events.on <- events.on[events.duration >= duration[1]]
    events.off <- events.off[events.duration >= duration[1]]
    events.duration <- events.duration[events.duration >= duration[1]]
    
    events.on <- events.on[events.duration <= duration[2]]
    events.off <- events.off[events.duration <= duration[2]]
    events.duration <- events.duration[events.duration <= duration[2]]
    
    ## convert events data to vector
    events.vector <- rep(x = FALSE, times = ncol(data))
    
    if(length(events.duration) > 0) {
      for(j in 1:length(events.duration)) {
        events.vector[events.on[j]:events.off[j]] <- TRUE  
      }
    }
    
    ## write results to output variable
    E[i,] <- events.vector
    
    ## write extra output data to list
    extra.data[[length(extra.data) + 1]] <- 
      list(events.on = events.on,
           events.off = events.off,
           events.duration = events.duration,
           ratio = ratio)
  }
  
  ## remove dummy list content
  extra.data[[1]] <- NULL
  
  ## optionally create extended output
  if(extra == TRUE) {
    return(list(events = E, extra = extra.data))
  } else{
    return(E)
    
  }
  ### Numeric vector or matrix with event states (\code{TRUE}, 
  ### \code{FALSE}). Or list object, containing event onsets,
  ### offsets, duration and the STA/LTA ratio, if extended 
  ### output is used.
  
  ##keyword<<
  ## environmental seismology
  
}, ex = function(){
  ## load example data set
  data(sac, envir = environment())
  
  ## strip time and signal part
  t <- strip.time(data = sac, unit = "sec")
  s <- strip.signal(data = sac)
  
  ## trigger events
  events <- detect.events(data = s, 
                          dt = 1 / 200, 
                          trigger = c(4, 2), lta.freeze = TRUE,
                          window.size = c(1, 4))
  
  ## plot 30 seconds of the signal and overlay events, very simple
  plot(x = t, 
       y = s, 
       type = "l", 
       xlim = c(0, 30), 
       ylim = c(-500, 300), 
       xlab = "t (s)")
  lines(x = t, 
        y = events * 100 - 500)  
})
