#' Write MiniSEED files to disk
#' 
#' The function converts \code{eseis} objects or numeric vectors to MiniSEED 
#' format and writes the file to disk. 
#' 
#' The function was written to use the libmseed software v3.2.3. For 
#' information see https://earthscope.github.io/libmseed/index.html and 
#' https://github.com/EarthScope/libmseed. In essence, only the required C
#' code files were used (see src/libmseed directory of the R package eseis). 
#' The function allows to save mseed v2 and mseed v3 files. Currently, mseed 
#' v3 is rarely used and is not supported by software like snuffler or obspy.
#' The c wrappers were created with the help of ChatGPT based on the examples
#' on the libmseed tutorial website. 
#' 
#' Most conveniently, the function is used with eseis objects as input data. In
#' that case, all relevant meta data is extracted from the eseis objects. 
#' However, one can also save any numeric vector as mseed file (or trace of 
#' a mseed file). In that case, all meta information needs to be provided 
#' explicitly. Relevant meta data are sampling interval (dt), station code 
#' (station), network code (network), seismic component ID (component). In 
#' essence, only dt is required, while the other meta information can be left 
#' out and will be set to blank when writing the mseed file. In addition, the 
#' meta information location can be added, too.
#' 
#' Instead of writing a single data set (trace) to mseed, it is also possible 
#' to save multiple channels to one mseed file, by providing a list of eseis 
#' objects. When providing a list of numeric vectors instead, also the meta 
#' information must be provided for each data vector, by defining for example 
#' dt as a numeric vector of the sampling intervals of all data vectors.
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed. See details for further information on 
#' writing multiple datasets into one MiniSEED file.
#' 
#' @param file \code{Character} value, file name of the mseed file
#'
#' @param encoding \code{Character} value, encoding type for the data set. 
#' One out of \code{"STEIM2"}, \code{"STEIM1"}, \code{"INT32"}, 
#' \code{"INT16"}, \code{"FLOAT32"}. Default is \code{"STEIM2"}. Note that the 
#' default encoding assumes interge values as data. Hence, velocity values in 
#' m/s would be converted to integers. See details for further information.
#' 
#' @param size \code{Numeric} value, binary record length in bytes. Default is 
#' \code{512}.
#' 
#' @param version \code{Numeric} or \code{character} value, version of the 
#' mseed file to write. Default (and still widely supported) is mseed v2 
#' (\code{2} or \code{"2"}). It is also possible to write mseed v3 files by
#' setting \code{version = 2}.
#' 
#' @param dt \code{Numeric} value, sampling period. Only needed if \code{data} 
#' is no \code{eseis} object or list of \code{eseis} objects.
#' 
#' @param starttime \code{POSIXct} value, start time of the data set. Only 
#' needed if \code{data} is no \code{eseis} object or list of \code{eseis} 
#' objects.
#' 
#' @param station \code{Character} value, seismic station ID. Only needed 
#' if \code{data} is no \code{eseis} object or list of \code{eseis} objects. 
#' If omitted, no station information will be added to the MiniSEED file.
#' 
#' @param network \code{Character} value, seismic station ID. Must be a 
#' two-character ID. Only needed if \code{data} is no \code{eseis} object or 
#' list of \code{eseis} objects. If  omitted, no network information will be 
#' added to the MiniSEED file. 
#' 
#' @param location \code{Character} value, location information. Only needed 
#' if \code{data} is no \code{eseis} object or list of \code{eseis} objects. 
#' If omitted, no location information will be added to the MiniSEED file.
#' 
#' @param component \code{Character} value, seismic component code. Should be a 
#' three characters long. Only needed if \code{data} is no \code{eseis} object
#' or list of \code{eseis} objects. If  omitted, no component information 
#' will be added to the MiniSEED file.
#' 
#' @param overwrite \code{Logical} value, option to overwrite an existing 
#' MiniSEED file if it exists. If set to \code{FALSE}, the exported data set 
#' will be appended to an existing MinISEED file. Default is \code{TRUE}.
#' 
#' @return A binary MiniSEED file written to disk.
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

write_mseed <- function(
    
    data,
    file,
    encoding = "STEIM2", 
    size = 512,
    version = 2,
    dt,
    starttime,
    station,
    network,
    location,
    component,
    overwrite = TRUE
    
) {
  
  ## check encoding keyword
  if(encoding %in% c("STEIM2", "STEIM1", "INT32", 
                     "INT16", "FLOAT32") == FALSE) {
    stop("Keyword for encoding not supported!")
  }
  
  ## check version entry
  version <- as.numeric(version)
  if(version %in% c(2, 3) == FALSE) {
    stop("The mseed version is not implemented!")
  }
  
  ## expand file path
  file <- paste0(dirname(file), "/", basename(file))
  
  ## remove double slashes
  file <- gsub(x = file, pattern = "//", replacement = "/", fixed = TRUE)
  
  ## Normalize input into a list of traces
  if (inherits(data, "eseis")) {
    data_list <- list(data)
  } else if (is.list(data)) {
    data_list <- data
  } else {
    data_list <- list(data)
  }
  
  ## get number of data sets to handle
  n <- length(data_list)
  
  ## sampling rate information
  if(missing(dt) == FALSE) {
    if(length(dt) == 1) {dt <- rep(dt, n)}
  }
  
  ## starttime information
  if(missing(starttime) == FALSE) {
    if(length(starttime) == 1) {starttime <- rep(starttime, n)}
    starttime <- try(as.POSIXct(starttime, tz = "UTC"))
  }
  
  ## station information
  if(missing(station) == FALSE) {
    if(length(station) == 1) {station <- rep(station, n)}
  } else {
    station <- rep("", n)
  }
  
  ## network information
  if(missing(network) == FALSE) {
    if(length(network) == 1) {network <- rep(network, n)}
  } else {
    network <- rep("", n)
  }
  
  ## location information
  if(missing(location) == FALSE) {
    if(length(location) == 1) {location <- rep(location, n)}
  } else {
    location <- rep("", n)
  }
  
  ## check/set consistent location place holder for mseed v2
  if(version == 2) {
    location <- ifelse(nchar(location) == 0, "00", location)
  }
  
  ## component information
  if(missing(component) == FALSE) {
    if(length(component) == 1) {component <- rep(component, n)}
  } else {
    component <- rep("", n)
  }
  
  ## create vectorised meta information structures
  samprate  <- numeric(n)
  start_vec <- character(n)
  sid_vec   <- character(n)
  
  ## loop over all traces
  for (i in seq_len(n)) {
    
    ## assign data vector
    x <- data_list[[i]]
    
    ## check if eseis objects or vectors are used
    if (inherits(x, "eseis")) {
      
      # fill meta data from eseis object
      samprate[i]  <- 1 / x$meta$dt
      start_vec[i] <- format(x$meta$starttime, 
                             "%Y-%m-%dT%H:%M:%OS9", 
                             tz = "UTC")
    
      ## prepare channel code, trim white space, padd with spaces, separate 
      chn_i <- trimws(x$meta$component)
      chn_i <- ifelse(nchar(chn_i) == 1, paste0(chn_i, "  "), chn_i)
      chn_i <- ifelse(nchar(chn_i) == 2, paste0(chn_i, " "), chn_i)
      chn_i <- paste(strsplit(x = chn_i, split = "")[[1]], collapse = "_")
       
      ## prepare mseed conform FDSN string
      sid_vec[i] <- paste0("FDSN:",
                           trimws(x$meta$network), "_",
                           trimws(x$meta$station), "_",
                           trimws(location[i]), "_",
                           chn_i)
      
      ## optionally convert signal vector to output data type
      if(encoding == "FLOAT32") {
        data_list[[i]] <- as.double(x$signal)
      } else {
        data_list[[i]] <- as.integer(x$signal)
      }
    } else {
      
      # fill meta data from user-provided input arguments
      samprate[i] <- 1 / dt[i]
      start_vec[i] <- format(starttime[i], "%Y-%m-%dT%H:%M:%OS9", tz = "UTC")
      
      ## prepare channel code, trim white space, padd with spaces, separate 
      chn_i <- trimws(component[i])
      chn_i <- ifelse(nchar(chn_i) == 1, paste0(chn_i, "  "), chn_i)
      chn_i <- ifelse(nchar(chn_i) == 2, paste0(chn_i, " "), chn_i)
      chn_i <- paste(strsplit(x = chn_i, split = "")[[1]], collapse = "_")
      
      ## prepare mseed conform FDSN string
      sid_vec[i] <- paste0("FDSN:",
                           trimws(network[i]), "_",
                           trimws(station[i]), "_",
                           trimws(location[i]), "_",
                           trimws(component[i]))

      ## optionally convert signal vector to output data type
      if(encoding == "FLOAT32") {
        data_list[[i]] <- as.double(x)
      } else {
        data_list[[i]] <- as.integer(x)
      }
    }
  }
  
  ## optionally delete mseed file before creating the new (otherwise append)
  if(overwrite == TRUE) {unlink(file, force = TRUE)}
  
  ## call C code to write MiniSEED file
  if(version == 3) {
    
    ## write a mseed v3 file
    .Call("writeMiniSEED_v3_v3",
          data_list,
          samprate,
          start_vec,
          sid_vec,
          as.integer(size),
          as.character(file),
          encoding)
  } else {
    
    ## write a mseed v2 file
    .Call("writeMiniSEED_v3_v2",
          data_list,
          samprate,
          start_vec,
          sid_vec,
          as.integer(size),
          as.character(file),
          encoding)
  }
  
  ## gulp output
  invisible(file)
}