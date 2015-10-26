envelope <-
structure(function(# Function to create signal envelopes.
  ### This function creates the signal envelopes. It returns cosine tapered
  ### sums of the absolute values of the hilbert-transformed signals.

  data,
  ### Numeric \code{vector} or \code{matrix} with respective seismic traces
  ### to be used for calculating the envelope.
  
  sum = FALSE,
  ### Logical \code{scalar}, option to calculate vector sum of the input 
  ### data. Only useful if \code{data} contains more than one component of 
  ### the same record.
  
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
  
  if(sum == TRUE) {
    
    signal.sqrtsum <- sqrt(apply(X = signal.abs^power, MARGIN = 2, FUN = sum))
  } else {
    
    signal.sqrtsum <- signal.abs
  }
  
  signal.taper <- spec.taper(x = signal.sqrtsum, p = p)
  
  return(signal.taper)
  ### Numeric vector with signal envelope
  

  ##keyword<<
  ## environmental seismology
}, ex = function(){
  
  ## no example present yet
})
