build.array <-
structure(function(# Function to build an empty data array.
  ### This function builds an empty structure for storing data from a seismic
  ### array.

  n.stations,
  ### Numeric \code{scalar}, number of stations that form the array.
  
  n.components = 4,
  ### Numeric \code{scalar}, number of signal components per station. Can
  ### be one out of 1 (x), 2 (x, y), 3 (x, y, z) and 4 (x, y, z, x-y-z), 
  ### default is 4.
  
  summary = TRUE,
  ### Logical \code{scalar}, option to build a summarising structure that
  ### contains essential common information about the array, i.e. \code{dt}.
  
  metadata = TRUE,
  ### Logical \code{scalar}, option to build structure for station-wise  
  ### metadata, default is \code{TRUE}.
  
  spectrum = TRUE,
  ### Logical \code{scalar}, option to build structure for spectral data, 
  ### default is \code{TRUE}.
  
  event = TRUE
  ### Logical \code{scalar}, option to build structure for detected events.

  ){
  ## calculate number of list components in array
  n.array <- 2 + spectrum + metadata
  
  ## calculate number list components in time component
  n.time <- 1 + spectrum
  
  ## calculate number of list components in signal component
  n.signal <- n.stations
  
  ## calculate number of list components in signal component
  n.event <- n.stations
  
  ## calculate number of list components in spectrum component
  n.spectrum <- n.stations
  
  ## calculate number of list components in metadata component
  n.metadata <- n.stations
  
  ## calculate number of list components in component components
  n.component = n.components
  
  ## build component structure
  component <- list(numeric(0))
  for(i in 1:n.component) {
    component[[length(component) + 1]] <- numeric(0)
  }
  component[1] <- NULL
  names(component) <- c("x", "y", "z", "xyz")[1:n.components]
  
  ## build metadata structure
  metadata.1 <- list(dt = numeric(0),
                     JD = numeric(0),
                     year = numeric(0),
                     month = numeric(0),
                     day = numeric(0),
                     hour = numeric(0),
                     minute = numeric(0),
                     second = numeric(0),
                     millisecond = numeric(0))
  
  ## build summary structure
  summary.2 <- list(dt = numeric(0),
                    unit = character(0),
                    N = numeric(0),
                    n.stations = numeric(0),
                    f.spectrum = numeric(0))
  
  ## build time structure
  time.2 <- list(numeric(0))
  for(i in 1:n.time) {
    time.2[[length(time.2) + 1]] <- numeric(0)
  }
  time.2[1] <- NULL
  if(length(time.2) == 1) {
    names(time.2) <- "signal"
  } else {
    names(time.2) <- c("signal", "spectrum")
  }
  
  ## build signal structure
  signal.2 <- list(numeric(0))
  for(i in 1:n.signal) {
    signal.2[[length(signal.2) + 1]] <- component
  }
  signal.2[1] <- NULL
  names(signal.2) <- paste("station", 1:n.signal, sep = "")
  
  ## build metadata structure
  metadata.2 <- list(numeric(0))
  for(i in 1:n.metadata) {
    metadata.2[[length(metadata.2) + 1]] <- metadata.1
  }
  metadata.2[1] <- NULL
  names(metadata.2) <- paste("station", 1:n.metadata, sep = "")
  
  ## build spectrum structure
  spectrum.2 <- list(numeric(0))
  for(i in 1:n.spectrum) {
    spectrum.2[[length(spectrum.2) + 1]] <- component
  }
  spectrum.2[1] <- NULL
  names(spectrum.2) <- paste("station", 1:n.spectrum, sep = "")
  
  ## build event structure
  event.2 <- list(numeric(0))
  for(i in 1:n.event) {
    event.2[[length(event.2) + 1]] <- numeric(0)
  }
  event.2[1] <- NULL
  names(event.2) <- paste("station", 1:n.signal, sep = "")
  
  ## build array structure
  array <- list(summary = summary.2,
                time = time.2,
                signal = signal.2,
                metadata = metadata.2,
                spectrum = spectrum.2,
                event = event.2)
  
  ## remove empty components
  if(metadata == FALSE) {array[[3]] <- NULL}
  if(spectrum == FALSE) {array[[4]] <- NULL}
  if(event == FALSE) {array[[5]] <- NULL}
  
  return(array)
  ### A list object.
  
  ##details<<
  ## The array structure is used for convenient and comprehensive analysis of
  ## an array of seismic stations. Most of the package function support this
  ## array object as input data and perform the desired tasks with the entire 
  ## (or parts of it, if specified) data set. The name array may not be 
  ## confused with the R data structure \code{\link{array}}.
  
  ##keyword<<
  ## environmental seismology
  
}, ex = function(){
  ## build a simple array
  array <- build.array(n.stations = 1)
  
  ## build a complete array
  array <- build.array(n.stations = 2, 
                       n.components = 3, 
                       summary = TRUE,
                       metadata = TRUE,
                       spectrum = TRUE,
                       event = TRUE)
  
  ## show struture of the array
  str(array)
})
