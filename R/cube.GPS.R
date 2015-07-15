cube.GPS <-
structure(function(# Function to extract GPS data from cube-files.
  ### This extracts the GPS tags from cube-files. It uses the library 
  ### CUBEtools (see details). It processes all cube.files in the stated 
  ### directory.
  
  path.cubefiles,
  ### \code{Character} scalar, abolute path to directory which contains the 
  ### cubefiles that will be processed.
  
  file,
  ### \code{Character} scalar or vector, names of the cube-files to be 
  ### processed, with extension. If omitted, all files in the 
  ### \code{path.cubefiles} will be processed.
  
  path.cubetools,
  ### \code{Character} scalar, abolute path to and name of the CUBEtools
  ### directory.
  
  keep.files = FALSE
  ### \code{Logical} scalar, option to delete or keep the extracted GPS files
  ### on the disk.
  
){

  if(file.exists(paste(path.cubetools, "/bin/cubeinfo", sep = "")) == FALSE) {
    stop("Cube-file directory does not exist!")
  } else {
    path.cubetools <- paste(path.cubetools, "/bin", sep = "")
  }
  
  ## check input data
  if(file.exists(path.cubefiles) == FALSE) {
    stop("Cube-file directory does not exist!")
  }
  
  ## fill and check file list to be processed
  if(missing(file) == TRUE) {
    file <- list.files(path = path.cubefiles)
  }
  
  if(length(file) < 1) {
    stop("No files found!")
  }
  
  for(i in 1:length(file)) {
    if(file.exists(paste(path.cubefiles, 
                         "/", 
                         file[i], 
                         sep = "")) == FALSE) {
      stop(paste("Cube-file", file[i], "does not exist!"))
    }
  }
  
  ## split path to cube-files
  path.cubefiles.split <- strsplit(x = path.cubefiles, split = "/")
  path.cubefiles.1 <- paste(path.cubefiles.split[[1]][1:(
    length(path.cubefiles.split[[1]]) - 1)], collapse = "/")
  path.cubefiles.2 <- path.cubefiles.split[[1]][length(
    path.cubefiles.split[[1]])]

  ## extract GPS data using the cubeinfo command as system call
  for(i in 1:length(file)) {
    system(paste("export PATH=$PATH:", 
                 path.cubetools, 
                 " ; cd ", 
                 path.cubefiles.1, 
                 "/ ; ", 
                 "cubeinfo --format=GPS --output-dir=", 
                 path.cubefiles.2, 
                 " ", 
                 path.cubefiles.2, 
                 "/", 
                 file[i], 
                 sep = ""))    
  } 
  
  ## infer gps tag file names
  files.gps <- paste(file, 
                     ".gps.txt", 
                     sep = "")
  
  ## create gps variable
  gps <- numeric(0)
  
  ## read and merge all files
  for(i in 1:length(files.gps)) {
    gps.i <- readLines(con = paste(path.cubefiles, 
                                   "/", 
                                   files.gps[i], 
                                   sep = ""))
    gps <- c(gps, gps.i)
  }
  
  ## read out coordinates
  data <- cbind(as.numeric(substr(x = gps, 
                                  start = 50, 
                                  stop = 60)), 
                as.numeric(substr(x = gps, 
                                  start = 64, 
                                  stop = 72)))
  
  ## optionally remove gps-files
  if(keep.files == FALSE) {
    file.remove(paste(paste(path.cubefiles, 
                            "/", 
                            files.gps, 
                            sep = "")))
  }
  
  return(data)
  ### \code{Numeric} matrix with GPS coordinates.
  
  ##details<<
   
  ##keyword<<
  ## environmental seismology
  
}, ex = function() {

#   path.cubetools = "/home/USER/Downloads/CUBEtools"
#   path.cubefile = "~/data/raw/A7L"
#   
#   GPS.all <- cube.GPS(path.cubetools = path.cubetools, 
#                       path.cubefile = path.cubefile)
# 
#   GPS.one <- cube.GPS(file = "06240000.A7L", 
#                       path.cubetools = path.cubetools, 
#                       path.cubefile = path.cubefile)
#   
#   print(paste("LAT = ", round(mean(GPS.one[,1]), 2), 
#               ", LON = ", round(mean(GPS.one[,2]), 2), 
#               sep = ""))
  })
