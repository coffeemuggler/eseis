#' Write seismic traces as sac-file to disk.
#'
#' This function converts seismic traces to sac-files and writes them to disk.
#' 
#' For description of the sac file format see
#' https://ds.iris.edu/files/sac-manual/manual/file_format.html. Currently the 
#' following parameters are not supported when writing the sac file: 
#' LAT, LON, ELEVATION, NETWORK.
#'
#' @param data \code{Numeric} vector, seismic trace to be written.
#' 
#' @param file \code{Character} scalar, filename with extension.
#' 
#' @param time \code{POSIXct} vector, time vector corresponding to the 
#' seismic trace. Alternatively, the start time stamp can be provided as
#' \code{POSIXct} value and a value for \code{dt} must be given.
#' 
#' @param station \code{Character} scalar, station ID.
#' 
#' @param network \code{Character} scalar, network ID.
#' 
#' @param component \code{Character} scalar, component ID.
#' 
#' @param dt \code{Numeric} scalar, sampling period. Only needed if no time
#' vector is provided.
#' 
#' @param biglong \code{Logical} scalar, biglong option, default is 
#' \code{FALSE}
#' 
#' @return A binary file written to disk.
#' @author Michael Dietze
#' @examples
#'
#' ## No examples ready, yet.
#'
#' @export write_sac
write_sac <- function(
  data,
  file,
  time,
  station,
  network,
  component,
  dt,
  biglong = FALSE
) {

  ## check/set biglong-dependent parameters
  if(biglong == TRUE) {
    ilong <- 8
    ifloat <- 4
  } else {
    ilong <- 4
    ifloat <- 4
  }
  
  ## check/set station
  if(missing(station) == TRUE) {
    
    warning("Station name missing! station set to NULL")
    station <- "NULL"
  }
  
  ## check/set time vector
  if(missing(time) == TRUE) {
    
    stop("No time information provided. Cannot generate sac-file.")
  } else if(length(time) == 1) {
    
    if(missing(dt) == TRUE) {
      stop("No sampling period (dt) provided. Cannot generate sac-file.")
    }
    
    time <- seq(from = time, by = dt, length.out = length(data))
  }
  
  ## check/set network
  if(missing(network) == TRUE) {
    
    warning("Network name missing! network set to NULL")
    network <- "NULL"
  }
  
  ## check/set component
  if(missing(component) == TRUE) {
    
    warning("Component name missing! component set to p0")
    component <- "p0"
  }
  
  ## round dt
  dt <- signif(x = dt, digits = 10)
  
  ## get first time entry
  start <- time[1]
  
  ## create time data
  yr <- as.numeric(format(start, "%Y"))
  jd <- as.numeric(as.POSIXlt(start)$yday + 1)
  mo <- as.numeric(format(start, "%m"))
  dom <- as.numeric(format(start, "%d"))
  hr <- as.numeric(format(start, "%H"))
  mi <- as.numeric(format(start, "%M"))
  sec   <- as.numeric(format(start, "%S"))
  msec <- as.numeric(format(start, "%OS")) - sec
  t1 <- mi * 60 + sec + msec
  t2 <- length(data) * dt
  off <- 0
  
  ## padd values with zeros
  jd_3 <- ifelse(nchar(jd) < 2, paste("00", jd, sep = ""), jd)
  jd_3 <- ifelse(nchar(jd_3) < 3, paste("0", jd_3, sep = ""), jd_3)
  
  hr_2 <- ifelse(nchar(hr) < 2, paste("0", hr, sep = ""), hr)
  mi_2 <- ifelse(nchar(mi) < 2, paste("0", mi, sep = ""), mi)
  sec_2 <- ifelse(nchar(sec) < 2, paste("0", sec, sep = ""), sec)
  
  
  ## check/set file names
  if(missing(file) == TRUE) {
    
    print("No file name provided. File name will be generated automatically.")
    
    file <- paste(station, "_",
                  yr, "_",
                  jd, "_",
                  hr_2, "_",
                  mi_2, "_",
                  sec_2, "_", 
                  component,
                  ".sac", sep = "")
  }
  
   ## fill section.1
    section.1 <- c(dt,
                   rep(x = -12345, times = 4),
                   t1,
                   t2,
                   rep(x = -12345, times = 63))                 
    
    ## fill section.2
    section.2 <- c(yr,
                   jd,
                   hr,
                   mi,
                   trunc(sec),
                   msec * 1000,
                   6,
                   -12345,
                   -12345,
                   length(data),
                   rep(x = -12345, times = 5),
                   1,
                   rep(x = -12345, times = 24))
    section.2[36] <- 1
    section.2 <- as.integer(section.2)
    
    ## fill section.3
    section.3 <- rep(format("-12345", width = 8, justify = "left"), 
                     times = 21)
    section.3[130] <- format(component, 
                             width = 8, 
                             justify = "left")
    
    ## open binary file
    SAC <- file(description = file, open = "wb")
    
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
    writeChar(object = format(station, width = 8, justify = "left"), 
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
    writeBin(object = data, 
             con = SAC, 
             size = ifloat, 
             endian = .Platform$endian)
    
    ## close connection
    close(SAC)
}
  