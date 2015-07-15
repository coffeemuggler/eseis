envelope <-
structure(function(# Function to create signal envelopes.
  ### This function creates the signal envelopes. It returns cosine tapered
  ### sums of the absolute values of the hilbert-transformed signals.

  data,
  ### Numeric \code{vector} or \code{matrix} with respective seismic traces
  ### to be used for calculating the envelope.
  
  power = 1,
  ### Numeric \code{scalar}, power used prior to signal summation.
  
  p = 0.1
  ### Numeric \code{scalar}, proportion of the signals to be tapered.

  ){  
 
  ## check/set data structure
  if(is.matrix(data) == FALSE) {
    data <- rbind(data)
  }
  
  signal.hilbert <- hilbert(data = data)
  
  signal.abs <- abs(signal.hilbert)^2
  
  signal.sqrtsum <- sqrt(apply(X = signal.abs^power, MARGIN = 2, FUN = sum))
  
  signal.taper <- spec.taper(x = signal.sqrtsum, p = p)
  
  return(signal.taper)
  ### Numeric vector with signal envelope
  

  ##keyword<<
  ## environmental seismology
}, ex = function(){
  
  ## no example present yet
})
