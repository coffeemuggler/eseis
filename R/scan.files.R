scan.files <-
structure(function(# Function to check presence of files.
  ### This function scans a given directory sturcture and checks if 
  ### hourly seismic files are present. It lists the presence/absence.
  
  directory,
  ### \code{Character} scalar, directory which is to be scanned.
  
  station,
  ### \code{Character} scalar, optional station ID to be scanned.
  
  extension
  ### \code{Character} scalar, optional file extention to which the scan
  ### is to be restricted.
  
){
  
  ## get subdirectory list
  sub.dir <- dir(path = directory, include.dirs = TRUE)
  
  ## create result data set
  t <- numeric(0)
  y <- logical(0)
  
  ## loop through all subdirectories
  for(i in 1:length(sub.dir)) {
    
    ## update hourly time vector
    t.i <- 0:23
    y.i <- rep(x = NA, times = length(t.i))
    
    ## update file vector
    files.i <- list.files(path = paste(directory, "/", sub.dir[i], sep = ""))
    
    ## restrict file fist to those specified by station
    if(missing(station) == FALSE) {
      files.i <- files.i[grepl(x = files.i, pattern = station)]
    }
    
    ## restrict file fist to those specified by extension
    if(missing(extension) == FALSE) {
      files.i <- files.i[grepl(x = files.i, pattern = extension)]
    }
        
    if(length(files.i) > 0) {
      ## read out hour from data set
      files.hour <- strsplit(x = files.i, split = ".", fixed = TRUE)
      
      hour <- numeric(length(files.hour))
      
      for(j in 1:length(files.hour)) {
        hour[j] <- as.numeric(files.hour[[j]][4])
      }
      
      ## check presence of hour
      for(j in 1:length(t.i)) {
        y.i[j] <- ifelse(min(abs(hour - t.i[j])) > 0, 0, 1)
      }
    } else {
      y.i <- rep(0, length(t.i))
    }
    
    ## update result data set
    y <- c(y, y.i)
    t <- c(t, 0:23 + 24 * (i -1))
  }
  
  return(rbind(t, y))
  ### \code{Numeric} matrix of indices of seismic file presence for each
  ### hour (t): 0 - not present, 1 - present.
  
  ##details<<
  ## 
  
}, ex = function() {
  
  ## uncomment to use
  #x <- scan.files(directory = "data/seismic/2013/", extension = "BHZ")
})
