#' Write seismic traces as mseed file to disk.
#'
#' This function converts seismic traces to mseed files and writes them to 
#' disk. It makes use of the Python library 'ObsPy'. Thus, this software 
#' must be installed, to make use of this function.
#' 
#' The ObsPy Python library can be installed following the information 
#' provided here: \code{"https://github.com/obspy/obspy/wiki"}.
#' 
#' Since the ObsPy functionality through R is not able to interpret path 
#' definitions using the tilde symbol, e.g. \code{"~/Downloads"}, this 
#' Linux type definition must be avoided.
#'
#' @param data \code{eseis} object or \code{numeric} vector, data set to 
#' be processed. Most other arguments can be omitted if \code{data} is an
#' \code{eseis} object.
#' 
#' @param file \code{Character} scalar, mseed file name with extension.
#' 
#' @param time \code{POSIXct} vector, time vector corresponding to the 
#' seismic trace. Alternatively, the start time stamp can be provided as
#' \code{POSIXct} value and a value for \code{dt} must be given.
#' 
#' @param component \code{Character} value, component ID, optional.
#' 
#' @param station \code{Character} value, station ID, optional.
#' 
#' @param location \code{Character} vector of length four, station location 
#' data (latitude, longitude, elevation, depth), optional.
#' 
#' @param network \code{Character} value, network ID, optional.
#' 
#' @param dt \code{Numeric} value, sampling period. Only needed if no time
#' vector is provided.
#' 
#' @return A binary file written to disk.
#' 
#' @author Michael Dietze
#' 
#' @examples
#'
#' \dontrun{
#' ## load example data 
#' data("rockfall")
#' 
#' ## write as mseed file
#' write_mseed(data = rockfall_eseis, file = "rockfall.mseed")
#'           
#' }
#'
#' @export write_mseed
#' 
write_mseed <- function(
  data,
  file,
  time,
  component,
  station,
  location,
  network,
  dt
) {
  
  ## if needed, link to ObsPy library
  if(exists("obspy") == FALSE) {
    
    obspy <- try(reticulate::import("obspy"), silent = TRUE)
    
  } else if(class(obspy)[1] != "python.builtin.module") {
    
    obspy <- try(reticulate::import("obspy"), silent = TRUE)
  }
  
  ## check if ObsPy library could be loaded
  if(class(obspy)[1] == "try-error") {
    
    stop(paste0("Obspy library could not be loaded! Try manually with:",
                " obspy <- reticulate::import('obspy')"))
  }
  
  ## check arguments when no eseis object is provided
  if(class(data)[1] != "eseis") {
    
    ## check/set station
    if(missing(station) == TRUE) {
      
      print("Station name missing! Value station set to NULL")
      station <- "NULL"
    }
    
    ## check/set network
    if(missing(network) == TRUE) {
      
      print("Network name missing! Value network set to NULL")
      network <- "NULL"
    }
    
    ## check/set component
    if(missing(component) == TRUE) {
      
      print("Component name missing! Value component set to p0")
      component <- "p0"
    }

    ## check/set location
    if(missing(location) == TRUE) {
      
      print("Location information missing! Values set to -12345")
      location <- rep(x = -12345, times = 4)
    }
    
    ## check/set time vector
    if(missing(time) == TRUE) {
      
      stop("No time information provided. Cannot generate mseed-file.")
    } else if(length(time) == 1) {
      
      if(missing(dt) == TRUE) {
        stop("No sampling period (dt) provided. Cannot generate mseed-file.")
      }
      
      time <- seq(from = time, 
                  by = dt, 
                  length.out = length(data))
    }
    
    ## check/set dt
    if(missing(dt) == TRUE) {
      dt <- as.numeric(mean(diff(time)))
    }
    
    ## round dt
    dt <- signif(x = dt, digits = 10)
    
    ## get first time entry
    start <- time[1]
    
    ## convert data to eseis object
    mseed_out <- eseis::aux_initiateeseis()
    
    ## fill eseis object
    mseed_out$signal <- data
    mseed_out$meta$station <- station
    mseed_out$meta$network <- network
    mseed_out$meta$component <- component
    mseed_out$meta$n <- length(data)
    mseed_out$meta$sensor <- "NA"
    mseed_out$meta$logger <- "NA"
    mseed_out$meta$starttime <- start
    mseed_out$meta$dt <- dt
    mseed_out$meta$latitude <- location[1]
    mseed_out$meta$longitude <- location[2]
    mseed_out$meta$elevation <- location[3]
    mseed_out$meta$depth <- location[4]
    mseed_out$meta$filename <- "NA"
    mseed_out$meta$type <- "waveform"
    
  } else {
    
    ## assign input data to output data
    mseed_out <- data
  }

  ## convert eseis object to ObsPy stream object
  mseed_obspy <- aux_eseisobspy(data = mseed_out)
  
  ## write obspy stream object
  mseed_obspy$write(filename = file, format = "MSEED")
}
