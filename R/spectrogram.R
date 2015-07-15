spectrogram <-
structure(function( # Function to spectrograms from time series.
  ### This function creates spectrograms from a seismic trace. It supports the 
  ### standard spectrogram apporach and the Welch method. See details below.

  time,
  ### Numeric \code{vector} with time values.
  
  data,
  ### \code{Numeric} vector, seismic trace to be processed.

  dt,
  ### Numeric \code{scalar}, sampling period.

  unit,
  ### Character \code{scalar}, unit of the target time series. If omitted, the
  ### output is in seconds.

  method = "generic",
  ### method used for computation of the spetrogram. One out of "generic",
  ### "Welch".

  window,
  ### spectrogram resolution (i.e. time window length), seconds.

  overlap,
  ### fraction of overlapping.

  window.sub,
  ### width of the sub-window [seconds], used for the "Welch-method".

  overlap.sub,
  ### percentage of overlapping, used for the "Welch-method".

  n.fft,
  ### number of samples for fft, alternative to window or window.sub,
  ### depending on the chosen method.

  plot = FALSE,
  ### \code{logical} scalar, toggle plot output. Only useful for plotting
  ### single time series. Disabled when multidimensional data is provided.

  ...
  ### further parameters passed to the function.
){
  ## check/set parameters
  x <- data
  if(missing(time) == TRUE) {time <- seq(from = 1, to = length(data))}
  if(missing(dt) == TRUE) {stop("No dt provided!")}
  if(missing(unit) == TRUE) {unit <- "sec"}
  if(missing(window) == TRUE) {stop("window width not specified!")}
  if(missing(overlap) == TRUE) {overlap <- 0.5}
  if(missing(window.sub) == TRUE) {window.sub <- window}
  if(missing(overlap.sub) == TRUE) {overlap.sub <- overlap}
  if(missing(method) == TRUE) {method <- "generic"}
 
  ## calculate window lengths
  l.window <- round(window * (1/dt), 0)
  l.overlap <- round(l.window * overlap, 0)
  l.window.sub <- round(window.sub * (1/dt), 0)
  l.overlap.sub <- round(l.window.sub * overlap.sub, 0)
  
  ## calculate number of windows and sub-windows
  n.window <- floor(length(x) / l.window)
  
  ## rescale time series length
  x.n <- x[1:(l.window * n.window)]
  if(length(x) - length(x.n) != 0) {
    print("Time series has been truncated to n times the window length.")
  }
  x <- x.n
  
  ## calculate step size
  l.step <- l.window - l.overlap
  steps <- seq(from = 1, to = length(x) - l.window, by = l.step)
  
  ## convert time series to matrix
  x.m <- matrix(0, nrow = l.window, ncol = length(steps))
  for (i in 1:length(steps)) {
    x.m[,i] <- x[steps[i]:(steps[i] + l.window - 1)]
  }
    
  ## calculate fft
  x.f <- mvfft(x.m)[1:(nrow(x.m) / 2),]
  x.f <- 10 * log10(sqrt(Re(x.f)^2 + Im(x.f)^2)^2)
    
  if(method == "Welch") {
    
    ## Welch method
    n.window.sub <- floor(nrow(x.m) / l.window.sub)
    l.step.sub <- l.window.sub - l.overlap.sub
    steps.sub <- seq(from = 1, to = nrow(x.m) - l.window.sub, by = l.step.sub)
    x.f.2 <- matrix(nrow = l.window.sub/2, ncol = ncol(x.f))
    
    for(i in 1:ncol(x.f)) {
      x.sub <- x.m[,i]    
      
      x.m.sub <- matrix(0, nrow = l.window.sub, ncol = length(steps.sub))
      for (j in 1:length(steps.sub)) {
        x.m.sub[,j] <- x.sub[steps.sub[j]:(steps.sub[j] + l.window.sub - 1)]
      }
      
      
      x.f.sub <- mvfft(x.m.sub)[1:(nrow(x.m.sub) / 2),]
      x.f.sub <- 10 * log10(sqrt(Re(x.f.sub)^2 + Im(x.f.sub)^2)^2)
      
      x.f.2[,i] <- rowMeans(x.f.sub)
    }
    x.f <- x.f.2
  }
  
  ## assign time and frequency vectors
  f <- seq(0, 1 / (2 * dt), length.out = nrow(x.f))
  t.s <- seq(1, length(x) * dt, length.out = ncol(x.f))
  t <- convert.time(data = t.s, unit.in = "sec", unit.out = unit)
  
  ## assign output to data object
  data <- x.f
  rownames(data) <- f
  colnames(data) <- t
  
  ## optionally plot data set
  if(plot == TRUE) {
    plot.spectrogram(data = x.f)
  }
  
  return(data)
  
  ### OUTPUT
  
  ##details<<
  ## Define/explain the two methods: generic and Welch
  
  ##seealso<<
  ## 
  
  ##references<<
  ## ADD cubetools references
  
  ##keyword<<
  ## environmental seismology
  
}, ex = function(){
  ## load example data set
  data(signal.1, envir = environment())
  
  ## extract time and z-component of station 1
  t <- signal.1$time$signal
  x <- signal.1$signal$station1$z
  
  ## time series with generic, data array with Welch
  signal.1.Welch <- spectrogram(time = t,
                                data = x,
                                dt = 1/200,
                                unit = "hour",
                                window = 6, 
                                window.sub = 2, 
                                component = "z", 
                                method = "Welch", 
                                plot = TRUE) 
})
