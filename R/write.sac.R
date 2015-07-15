write.sac <-
structure(function(# Function to write a sac-file.
  ### This function writes a sac-file.
  
  data,
  ### \code{List} object, R-object to be written to a sac file or
  ### multiple sac-files.
  
  file,
  ### \code{Character} scalar, optional file name of the sac-file.
  
  biglong = FALSE
  ### \code{Logical} scalar, biglong option.
){
  
  ## check/set biglong-dependent parameters
  if(biglong == TRUE) {
    ilong <- 8
    ifloat <- 4
  } else {
    ilong <- 4
    ifloat <- 4
  }
  
  ## check/set file names
  if(missing(file) == TRUE) {
    file.out <- character(0)
    for(i in 1:length(data)) {
      file.out <- c(file.out, paste(data[[i]]$sta, "_",
                                    data[[i]]$comp, "_",
                                    data[[i]]$DATTIM$yr, "_",
                                    data[[i]]$DATTIM$jd, "_",
                                    data[[i]]$DATTIM$hr, "_",
                                    data[[i]]$DATTIM$mi, ".sac", 
                                    sep = ""))
    }
  } else {
    file.out <- file
  }
  file <- file.out
  
  ## loop through all data sets
  for(i in 1:length(data)) {
    
    ## fill section.1
    section.1 <- c(data[[i]]$dt,
                   rep(x = -12345, times = 4),
                   data[[i]]$DATTIM$t1,
                   data[[i]]$DATTIM$t2,
                   rep(x = -12345, times = 63))                 
    
    ## fill section.2
    section.2 <- c(data[[i]]$DATTIM$yr,
                   data[[i]]$DATTIM$jd,
                   data[[i]]$DATTIM$hr,
                   data[[i]]$DATTIM$mi,
                   trunc(data[[i]]$DATTIM$sec),
                   (data[[i]]$DATTIM$sec - trunc(data[[i]]$DATTIM$sec)) * 1000,
                   6,
                   -12345,
                   -12345,
                   data[[i]]$N,
                   rep(x = -12345, times = 5),
                   1,
                   rep(x = -12345, times = 24))
    section.2[data[[i]]$HEAD[71:110, 1] == "leven"] <- 6
    
    ## fill section.3
    section.3 <- rep(format("-12345", width = 8, justify = "left"), 
                     times = 21)
    section.3[data[[i]]$HEAD[71:110, 1] == "kcmpnm"] <- format(data[[i]]$comp, 
                                                               width = 8, 
                                                               justify = "left")
    
    ## open binary file
    SAC <- file(description = file[i], open = "wb")
    
    ## write section.1 to sac-file
    writeBin(object = section.1, 
             con = SAC, 
             size = ifloat, 
             endian = .Platform$endian)
    
    ## write section.2 to sac-file
    writeBin(object = section.2, 
             con = SAC, 
             size = ilong, 
             endian = .Platform$endian)
    
    ## write station name to sac-file
    writeChar(object = format(data[[i]]$sta, width = 8, justify = "left"), 
              con = SAC, 
              nchars = 8, 
              eos = NULL)
    
    ## write kvenm[16] to sac-file
    writeChar(object = format("-12345", width = 16, justify = "left"), 
              con = SAC, 
              nchars = 16, 
              eos = NULL)
    
    ## write section.3 to sac-file
    for(j in 1:21) {
      writeChar(object = section.3[j], 
                con = SAC, 
                nchars = 8, 
                eos = NULL)
    }
    
    ## write signal part to sac-file
    writeBin(object = data[[i]]$amp, 
             con = SAC, 
             size = ifloat, 
             endian = .Platform$endian)
    
    ## close connection
    close(SAC)
  }
  ### One or more sac files, written to disk.
  
  
  ##keyword<<
  ## environmental seismology
  
}, ex = function() {
  ## uncomment to run
  data(sac, envir = environment())
  
  # write.sac(data = sac, file = "testfile.sac")
  
})
