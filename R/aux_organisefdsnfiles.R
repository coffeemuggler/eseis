#' Download and organise FDSN data sets
#' 
#' This function downloads and organises seismic files provided by FDSN 
#' data services.
#' 
#' @param start \code{POSIXct} value, start time for data acquisition. If a 
#' character string (or vector) is provided, the function will try to convert 
#' it to POSIXct. Note that the start time will be rounded down to the full
#' hour.
#' 
#' @param stop \code{POSIXct} value, stop time for data acquisition. If a 
#' character string (or vector) is provided, the function will try to convert 
#' it to POSIXct. Note that the stop time will be rounded up to the full
#' hour.
#' 
#' @param network \code{Character} vector, two-character FDSN network ID.
#' 
#' @param station \code{Character} vector, FDSN station ID.
#' 
#' @param component \code{Character} vector, seismic channel or channels to 
#' get. Default is \code{"BHZ"}. Note that the component(s) must exist in the 
#' FDSN data repository. See details for further information.
#' 
#' @param url \code{Character} vector, FDSN URL.
#' 
#' @param dir \code{Character} value, path to directory where downloaded 
#' data is written to.
#' 
#' @param format \code{Character} value, output file format. One out of 
#' \code{"mseed"} and \code{"sac"}. Default is \code{"mseed"}.
#' 
#' @param pattern \code{Character} value, file organisation scheme keyword. 
#' One out of \code{"eseis"} and \code{"seiscomp"}. Default is \code{"eseis"}. 
#' See details and \code{read_data} for further information.
#' 
#' @param buffer \code{Numeric} vector of length two, size of the two-sided 
#' buffer around the data to download, in seconds. Default is 
#' \code{c(300, 300)}. The buffer is useful because some data do not start 
#' at the desired start and end times. The buffer is only used for downloading
#' the data and will be clipped before writing the files.
#' 
#' @param verbose \code{Logical} value, option to enable informative screen 
#' output. Default is \code{FALSE}.
#' 
#' @return Set of seismic files organised for seamless use by eseis functions
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#'
#' \dontrun{
#' 
#' ## example to download and organise 3 hours of data into eseis scheme
#' aux_organisefdsnfiles(start = start, 
#'                       stop = stop, 
#'                       network = "CH", 
#'                       station = "FUORN",
#'                       component = c("BHE", "BHN"), 
#'                       url = url, 
#'                       dir = "~/Downloads/", 
#'                       verbose = TRUE)
#'                       
#' }
#'                         
#' @export aux_organisefdsnfiles

aux_organisefdsnfiles <- function(
  
  start,
  stop,
  network,
  station,
  component,
  url,
  dir,
  format = "mseed",
  pattern = "eseis",
  buffer = c(300, 300),
  verbose = FALSE
  
){
  
  ## Part 1 - checks, tests, adjustments --------------------------------------
  
  ## check/set output directory
  if(missing(dir) == TRUE) {
    
    warning("No output directory provided, using working directory!")
    dir <- getwd()
    
  } else if(dir.exists(dir) == FALSE) {
    
    stop("Output directory does not exist!")
  }
  
  ## check/set time limits
  if(class(start)[1] != "POSIXct"){
    
    start <- try(as.POSIXct(start, tz = "UTC"))
    
    if(class(start)[1] != "POSIXct") {
      
      stop("Cannot convert start time to POSIXct!")
    }
  }
  
  if(class(stop)[1] != "POSIXct"){
    
    stop <- try(as.POSIXct(stop, tz = "UTC"))
    
    if(class(stop)[1] != "POSIXct") {
      
      stop("Cannot convert stop time to POSIXct!")
    }
  }
  
  if(start >= stop) {
    
    stop("Time interval is negative!")
  }
  
  ## check file format keywords
  if(format %in% c("sac", "mseed") == FALSE) {
    
    stop("File format keyword not supported!")
  }
  
  ## check pattern keywords
  if(pattern %in% c("eseis", "seiscomp") == FALSE) {
    
    stop("File organisation pattern keyword not supported!")
  }
  
  ## Part 2 - Preparation steps -----------------------------------------------
  
  ## define file lengths from pattern keyword
  if(pattern == "eseis") {
    
    t_dur <- 3600
  } else if(pattern == "seiscomp") {
    
    t_dur <- 24 * 3600
  }
  
  ## round start and stop times to full hours
  start_h <- as.POSIXct(format(start, "%Y-%m-%d %H:00:00"), tz = "UTC")
  stop_h <- as.POSIXct(format(stop, "%Y-%m-%d %H:00:00"), tz = "UTC") + 3600
  
  ## make vector of time slices to get
  t_get <- seq(from = start_h, to = stop_h, by = t_dur)
  
  ## optionally print file number info
  if(verbose == TRUE) {
    
    print(paste0("A total of ", 
                 length(t_get) * length(component), 
                 " files to get (", 
                 length(t_get), 
                 " time snippets and ",
                 length(component),
                 " component(s) per time snippet)"))
  }
  
  ## Part 3 - Download and save the data sets ---------------------------------

  ## optionally print progress
  if(verbose == TRUE) {
    
    print(paste0("Starting (",
                 Sys.time(),
                 ")... "))
  }
  
  ## loop through all time snippets
  for(i in 1:length(t_get)) {
    
    ## loop through all components
    for(j in 1:length(component)) {
      
      ## try to get file
      s <- try(eseis::read_fdsn(
        start = t_get[i] - buffer[1], 
        duration = t_dur + buffer[1] + buffer[2], 
        component = component[j], 
        network = network, 
        station = station, 
        url = url), silent = TRUE)
      
      ## print progress
      if(verbose == TRUE) {
        
        if(inherits(x = s, what = "try-error") == FALSE) {
          
          print(paste0("  Done file ",
                       i,
                       " (",
                       t_get[i],
                       " - ",
                       component[j],
                       ")"))
        } else {
          
          print(paste0("  File not found: ",
                       i,
                       " (",
                       t_get[i],
                       " - ",
                       component[j],
                       ")"))
        }
      }
      
      ## check if file exists and save it
      if(inherits(x = s, what = "eseis")) {
        
        ## remove buffer
        s <- try(signal_clip(data = s, 
                             limits = c(t_get[i], t_get[i] + t_dur)))
        
        ## extract output directory time info
        dir_y <- format(s$meta$starttime, "%Y")
        dir_j <- format(s$meta$starttime, "%Y/%j")
        
        ## create output directories
        if(dir.exists(paste0(dir, "/", dir_y)) == FALSE) {
          
          dir.create(paste0(dir, "/", dir_y))
        }
        
        if(dir.exists(paste0(dir, "/", dir_j)) == FALSE) {
          
          dir.create(paste0(dir, "/", dir_j))
        }
        
        ## build file name
        name_output <- paste(s$meta$station, 
                             format(s$meta$starttime, "%y.%j.%H.%M.%S"),
                             s$meta$component,
                             sep = ".")
        
        ## append directory names
        file_output <- paste0(dir, "/", dir_j, "/", name_output)
        
        ## expand directory path
        file_output <- paste0(dirname(file_output), "/", 
                              basename(file_output))
        
        ## remove possible double-slashes
        file_output <- gsub(x = file_output, 
                            pattern = "//", 
                            replacement = "/", 
                            fixed = TRUE)
        
        ## check/use file format
        if(format == "mseed") {
          
          try(eseis::write_mseed(data = s, 
                                 file = file_output, 
                                 network = network))
        } else if(format == "sac"){
          
          try(eseis::write_sac(data = s, 
                               file = file_output, 
                               network = network, 
                               unit = "unknown"))
        }
      }
    }
  }
    
  ## optionally print progress
  if(verbose == TRUE) {
    
    print(paste0("Finished (",
                 Sys.time(),
                 ")"))
  }

}