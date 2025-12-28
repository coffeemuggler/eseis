#' Extract temperature data from cube files.
#' 
#' This function reads auxiliary information stored in Omnirecs/Digos Datacube  
#' files and extracts the temperature data that is stored along with each GPS 
#' tag. Optionally, the data is interpolated to equal intervals.
#' 
#' This feature is ony available for  Omnirecs/Digos Datacube that were 
#' produced since 2015, i.e., whose GPS output files also record the 
#' temperature inside the logger. Generating an ACSII GPS tag file using the 
#' gipptools software requires a few minutes time per daily file.
#' 
#' @param input_dir \code{Character} value, path to directory where all cube 
#' files to be processed as stored. Each set of files from one logger must be 
#' stored in a separate sub-directory named after the cube ID.
#'
#' @param logger_ID \code{Character} vector, logger ID.
#' 
#' @param interval \code{Numeric} value, time interval (minutes) to which  
#' temperature data is interpolated. No interpolation is performed if this 
#' argument is omitted.
#' 
#' @param cpu \code{Numeric} value, fraction of CPUs to use for parallel 
#' processing. If omitted, one CPU is used.
#' 
#' @param gipptools \code{Character} value, path to gipptools or cubetools 
#' directory. 
#' 
#' @return A \code{list} of \code{data frames} with time and temperature 
#' values for each cube data logger.
#' 
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#'
#' ## uncomment to use
#' # t <- aux_gettemperature(input_dir = "input",
#' #                         logger_ID = c("ANN", "ABT"),
#' #                         interval = 15,
#' #                         gipptools = "~/software/gipptools-2015.225/")
#' 
#' @export aux_gettemperature
aux_gettemperature <- function(
  input_dir,
  logger_ID,
  interval,
  cpu,
  gipptools
){
  
  ## Part 1 - checks, tests, adjustments --------------------------------------
  
  ## check/set fraction of CPUs to use
  if(missing(cpu) == TRUE) {
    
    cpu <- NA
  }
  
  ## get cube directories
  input_dir_exist <- list.files(path = input_dir,
                                full.names = TRUE)
  
  ## check/set cube ID
  if(missing(logger_ID) == TRUE) {
    
    logger_ID <- list.files(path = input_dir)
  }
  
  ## check cube ID for consistency
  if(any(nchar(x = logger_ID) != 3) == TRUE) {
    
    warning("[aux_stationinfofile]: The cube_IDs may be incorrect!")
  }
  
  ## compare cube_ID with cube directories
  for(i in 1:length(logger_ID)) {
    
    if(sum(logger_ID[i] == list.files(path = input_dir)) < 1) {
      warning(paste("[aux_stationinfofile]: Cube ID", 
                    logger_ID[i],
                    "not found in input directory!"))
    }
  }
  
  ## Part 2 - preparation steps -----------------------------------------------
  
  ## get files to read GPS data from
  files_cube <- lapply(X = logger_ID, 
                       FUN = function(x, input_dir) {
                         
                         ## get all files in respective cube directory
                         files_i <- list.files(path = paste(input_dir, 
                                                            x, 
                                                            sep = "/"), 
                                               full.names = TRUE)
                         
                         ## remove unwanted files
                         files_i <- files_i[substr(x = files_i, 
                                                   start = nchar(files_i) - 2, 
                                                   stop = nchar(files_i)) == x]
                         
                         ## return output
                         return(files_i)
                       },
                       input_dir = input_dir)
  
  ## convert list content to vector
  files_cube <- unlist(files_cube)
  
  ## create temporary data directory
  dir_temp <- file.path(tempdir(), "output")
  
  if(dir.exists(paths = dir_temp) == FALSE) {
    
    dir.create(path = dir_temp)
  }
  
  ## detect and adjust number of cores to use
  cores <- parallel::detectCores()
  
  if(is.na(cpu) == FALSE) {
    
    n_cpu <- floor(cores * cpu)
    cores <- ifelse(cores < n_cpu, cores, n_cpu)
  } else {
    
    cores <- 1
  }
  
  ## Part 3 - extraction of GPS data ------------------------------------------
  
  ## initiate cluster
  cl <- parallel::makeCluster(getOption("mc.cores", cores))
  
  ## extract GPS data
  invisible(parallel::parLapply(
    cl = cl, 
    X = files_cube, 
    fun = function(X, gipptools, output_dir) {
      
      system(command = paste(gipptools, "/bin/cubeinfo", 
                             " --format=GPS --output-dir=",
                             dir_temp, " ",
                             X,
                             sep = ""))
    }, 
    gipptools = gipptools))
  
  ## stop cluster
  parallel::stopCluster(cl = cl)
  
  ## Part 4 - calculations of GPS data ----------------------------------------
  
  ## get all gps raw files
  gps_files <- list.files(path = dir_temp,
                          pattern = ".gps.txt", 
                          full.names = TRUE)
  
  ## assign gps files to cubes
  gps_files_cube <- lapply(X = logger_ID, FUN = function(x, gps_files) {
    
    gps_files[grepl(x = gps_files, 
                    pattern = x)]
  }, 
  gps_files = gps_files)
  
  ## extract temperature data
  t_cube <- lapply(X = gps_files_cube, FUN = function(x) {
    
    ## create coordinate list
    temp <- numeric(length = 0)
    time <- character(length = 0)
    
    ## append coordinates for all files
    for(i in 1:length(x)) {
      
      ## read each file
      data_i <- try(utils::read.delim(file = x[i],
                                      sep = " ", 
                                      header = FALSE, 
                                      stringsAsFactors = FALSE), 
                    silent = TRUE)
      
      ## append successfully extracted data
      if(class(data_i)[1] != "try-error") {
        
        ## extract time
        time <- c(time, as.character(substr(x = data_i$V5, 
                                            start = 6, 
                                            stop = nchar(data_i$V5[1]))))
        
        ## extract temperature
        temp <- c(temp, as.numeric(substr(x = data_i$V14, 
                                          start = 6, 
                                          stop = nchar(data_i$V14[1]))))
      }
    }
    
    ## convert time string to POSIXct
    time <- gsub(x = time, pattern = "T", replacement = " ")
    
    time <- as.POSIXct(x = time, tz = "UTC")
    
    ## return output
    return(data.frame(time = time,
                      temperature = temp))
  })
  
  ## remove raw gps tag data
  file.remove(unlist(gps_files_cube))
  
  ## optionally interpolate time series
  if(missing(interval) == FALSE) {
    
    interval <- interval * 60
    
    t_cube <- lapply(X = t_cube, FUN = function(x, interval) {
      
      time_int <- stats::approx(x = x$time, 
                                y = x$temperature, 
                                xout = seq(from = min(x$time), 
                                           to = max(x$time), 
                                           by = interval), 
                                method = "linear")
      
      return(data.frame(time = time_int$x,
                        temperature = time_int$y))
    },
    interval = interval)
  }
  
  ## Part 5 - export output data ----------------------------------------------
  
  ## return result
  return(t_cube)
}