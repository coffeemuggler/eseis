read.sac <-
structure(function(# Function to read sac-files.
  ### This function reads sac-files to R. It is a wrapper of the function
  ### \code{JGET.seis}.
  files,
  ### Character \code{scalar} or \code{vector}, name of the files to be read,
  ### with extension. Wildcards may be used (see details and examples).
  unit = "cts",
  ### Character \code{scalar}, unit of the output data set. One out of 
  ### \code{"volts"} (Volts), \code{"cts"} (counts) and \code{"velocity"} 
  ### (velocity). Default is \code{"cts"}. If \code{"velocity"} is selected, 
  ### \code{setup} must be provided.
  subtract.mean = FALSE,
  ### \code{Logical} scalar, optionally remove the global mean after 
  ### processing, default is \code{TRUE}.
  setup,
  ### \code{List} object with station setup information (cf. 
  ### \link{station.setup}), needed to convert counts into m/s.
  ...
  ### Further arguments passed to \code{JGET.seis} and/or \code{deconinst}.
){
  ## check/select files
  if(sum(grepl(pattern = "[*]", x = files)) > 0) {
    files <- list.files(pattern = files)
  }
  
  ## read out additional arguments
  kind <- if("kind" %in% names(list(...))) {list(...)$kind} else {2}
  
  ## check/set RAW-parameter
  if(unit != "volts") {
    RAW = TRUE
    
    if(unit == "velocity" & missing(setup) == TRUE) {
      stop("Unit velocity requires specifying station setup!")
    }
  } else {
    RAW = FALSE
  }
  
  ## read and return files, i.e. call JGET.seis
  data <- JGET.seis(fnames = files, kind = kind, RAW = RAW, ...)
  
  ## convert header part to character matrix
  data[[1]]$HEAD <- as.matrix(data[[1]]$HEAD)
  
  ## optionally convert values
  if(unit == "velocity") {
    for(i in 1:length(data)) {
      data[[i]]$amp <- (data[[i]]$amp * setup$AD) / (setup$gain * setup$s)
    }
  }
  
  ## optionally, remove global mean
  if(subtract.mean == TRUE) {
    
    ## calculate global signal
    amp.global <- numeric(0)
    for(i in 1:length(data)) {
      amp.global <- c(amp.global, data[[i]]$amp)
    }
    
    ## calculate global mean
    mean.global <- mean(amp.global, na.rm = TRUE)
    
    ## subtract global mean
    for(i in 1:length(data)) {
      data[[i]]$amp <- data[[i]]$amp - mean.global
    }
    
  }
  
  return(data)
  ### \code{list} object
  
  ##details<<
  ## To read more than one file, present in the working directory, wildcards
  ## are supported, e.g. \code{"*BHZ.SAC"}. \cr
  ## A more selctive way might be to first list all possible sac-files in the
  ## working directory (\code{files <- list.files(pattern = "*BHZ.SAC")}) and
  ## then further truncate the file list (e.g. \code{files <- files[1:5]}).
  
  ##seealso<<
  ## \code{\link{JGET.seis}}, 
  ## \code{\link{deconinst}}, 
  ## \code{\link{list.files}}, 
  ## \code{\link{regex}}
  
}, ex = function(){
  ## load example data, i.e. station setup list
  data(setup.1, envir = environment())
  
  ## uncomment to use all further commands for reading user sac-files
  
  ## read a single file straightforward
  # x <- read.sac(files = "file1.sac")
  
  ## read the file with mean removal
  # x <- read.sac(files = "file1.sac",
  #               subtract.mean = TRUE)
  
  ## read the file with deconvolution and units as velocity [m/s]
  # x <- read.sac(files = "file1.sac",
  #               deconvolve = TRUE,
  #               setup = setup.1,
  #               unit = "velocity")
  
  ## read a series of files
  # filelist <- list.files(pattern = "*SAC")
  # filelist <- filelist[1:3]
  # x <- read.sac(files = filelist)
})
