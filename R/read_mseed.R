#' Read mseed file.
#'
#' The function reads mseed files.
#' 
#' When a file contains more than one trace per channel, the function assumes
#' the sampling rate is identical and only extracts the sampling rate of the 
#' first trace as output sampling rate. In that case, the output is not one 
#' \code{eseis} object (or list object in case option \code{eseis = FALSE}), 
#' but a list of \code{eseis} objects named by their component code.  
#' 
#' The mseed data format is read using the C library libmseed v.3.2.3
#' (https://github.com/earthscope/libmseed). The C wrapper for R has been 
#' build with the help of ChatGPT. 
#'
#' @param file \code{Character} value, input file name, with extension.
#'
#' @param signal \code{Logical} value, option to import the signal vector,
#' default is \code{TRUE}.
#'
#' @param time \code{Logical} value, option to create the time vector. The
#' timezone is automatically set to \code{"UTC"}, default is \code{TRUE}.
#'
#' @param meta \code{Logical} value, option to append the meta data part,
#' default is \code{TRUE}.
#'
#' @param header \code{Logical} value, option to append the header part,
#' default is \code{TRUE}.
#'
#' @param eseis \code{Logical} value, option to read data to an \code{eseis}
#' object (recommended, see documentation of
#' \code{aux_initiateeseis}), default is \code{TRUE}
#'
#' @param type \code{Character} value, type keyword of the data. One out of
#' \code{"waveform"}, \code{"envelope"}, \code{"fft"}, \code{"spectrum"},
#' \code{"spectrogram"}, \code{"other"}, \code{hilbert}, \code{hvratio}.
#' Default is \code{"waveform"}.
#'
#' @return \code{List} object, optionally of class \code{eseis}
#'
#' @author Michael Dietze
#'
#' @examples
#'
#'\dontrun{
#'
#' ## read mseed file with default options
#' x <- read_mseed(file = "input.miniseed")
#'
#' ## read mseed file, only signal trace, not as eseis object
#' x <- read_mseed(file = "input.miniseed",
#'                 time = FALSE,
#'                 meta = FALSE,
#'                 header = FALSE,
#'                 eseis = FALSE)
#'                 
#' }
#'
#' @export read_mseed

read_mseed <- function(
    file,
    signal = TRUE,
    time = TRUE,
    meta = TRUE,
    header = TRUE,
    eseis = TRUE,
    type = "waveform"
) {
  
  ## collect function arguments
  eseis_arguments <- list(file = file,
                          signal = signal,
                          time = time,
                          meta = meta,
                          header = header,
                          eseis = eseis,
                          type = type)
  
  ## get start time
  t_0 <- Sys.time()
  
  ## check if file exists
  if(file.exists(file) == FALSE) {
    
    stop("File does not exist!")
  } else {
    
    ## expand file path
    file <- paste0(dirname(file), "/", basename(file))
  }
  
  ## try to parse mseed file
  mseed <- try(.Call("parseMiniSEED_v3", file, PACKAGE = "eseis"),
               silent = TRUE)
  
  ## restructure parsed mseed object by channels
  data <- try(lapply(X = mseed, FUN = function(msd) {
    
    ## separate data and meta data part
    dta <- msd[[2]]
    
    ## collect all information by segment for trace list
    trc <- try(lapply(X = dta, FUN = function(sgm) {
      
      list(start = as.POSIXct(sgm$start / 1e9,
                              origin = "1970-01-01", 
                              tz = "UTC"),
           stop = as.POSIXct(sgm$end / 1e9,
                             origin = "1970-01-01", 
                             tz = "UTC"),
           n = sgm$npts,
           dt = 1 / sgm$samprate,
           data = sgm$data)
    }))
    
    ## extract station information
    si <- msd$sid
    
    ## remove FDSN part
    si <- gsub(x = si, pattern = "FDSN:", replacement = "", fixed = TRUE)
    
    ## split into individual information but paste together channel info
    si <- strsplit(x = si, split = "_", fixed = TRUE)[[1]]
    si <- c(si[1:3], paste(si[4:length(si)], collapse = ""))
    
    ## extract sampling period from segments
    dt <- do.call(c, lapply(X = trc, FUN = function(trc) {trc$dt}))
    
    ## handle case of several segments per trace
    if(length(dt) > 1) {
      
      ## issue warning if dt values are not identical
      if(sd(dt, na.rm = TRUE) > 0) {
        
        warning("Sample interval not constant!")
      }
      
      ## use sampling interval of the first segment 
      dt <- dt[1]
    }
    
    ## extract number of samples
    n <- do.call(c, lapply(X = trc, FUN = function(trc) {trc$n}))
    
    ## get start and end times for each trace
    tme_sgm <- do.call(rbind, lapply(X = trc, FUN = function(trc) {
      
      data.frame(start = trc$start,stop = trc$stop)
    }))
    
    ## get global start and end time for channel
    tme_glob <- data.frame(start = min(tme_sgm$start, na.rm = TRUE),
                           stop = max(tme_sgm$stop, na.rm = TRUE))
    
    ## build time and signal vector for segment pasting
    tme <- seq(from = tme_glob$start, to = tme_glob$stop, by = dt)
    sgn <- rep(NA, length(tme))
    
    ## paste segments into signal vector
    for(i in 1:length(trc)) {
      
      ## build indices for pasting
      i_ok <- which(tme >= tme_sgm$start[i] & tme <= tme_sgm$stop[i])
      
      ## paste in signal segments
      sgn[i_ok] <- trc[[i]]$data
    }
    
    ## optionally replace time vector
    if(time == FALSE) {
      tme <- NA
    }
    
    ## create header part, nothing to put but kept for consistency
    if(header == TRUE) {
      
      hdr <- list(NA)
    } else {
      
      hdr <- list(NA)
    }
    
    ## create meta information object
    if(meta == TRUE & eseis == TRUE) {
      
      mta <- list(station = si[2],
                  network = si[1],
                  component = si[4],
                  n = length(sgn),
                  sensor = NA,
                  logger = NA,
                  gain = NA,
                  starttime = tme_glob[1,1],
                  dt = dt,
                  latitude = NA,
                  longitude = NA,
                  elevation = NA,
                  depth = NA,
                  filename = file,
                  type = eseis_arguments$type)
    } else {
      
      mta <- NA
    }
    
    ## build output object
    if(eseis == TRUE) {
      
      ## calculate function call duration
      eseis_duration <- as.numeric(difftime(time1 = Sys.time(),
                                            time2 = t_0,
                                            units = "secs"))
      
      ## fill eseis object
      data_out <- eseis::aux_initiateeseis()
      data_out$signal <- sgn
      data_out$meta <- mta
      data_out$header <- hdr
      data_out$history[[length(data_out$history) + 1]] <-
        list(time = Sys.time(),
             call = "read_mseed()",
             arguments = eseis_arguments,
             duration = eseis_duration)
      names(data_out$history)[length(data_out$history)] <-
        as.character(length(data_out$history))
      
    } else {
      
      ## fill data object
      data_out <- list(signal = sgn,
                       time = tme,
                       meta = mta,
                       header = hdr)    
    }
    
    ## return data set
    return(data_out)
  }))
  
  ## extract channel names
  if(eseis == TRUE) {
    
    names(data) <- do.call(c, lapply(X = data, FUN = function(x) {
      x$meta$component
    }))
  }
  
  ## in case that only one channel is imported, reduce list hierarchy
  if(length(data) == 1) {
    data <- data[[1]]
  }
  
  ## return output
  return(data)
}
