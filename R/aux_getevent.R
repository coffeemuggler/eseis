#' Load seismic data based on a time stamp
#' 
#' The function loads seismic data from a data directory structure (see 
#' \code{aux_organisecubefiles()}) based on the event start time 
#' and duration.
#' 
#' This and all other aux-functions are primarily written for internal 
#' use amongst the GFZ Geomorphology Section group members and their  
#' usual data handling scheme. Thus, they may be of limited use when 
#' adopted for other scopes. However, many of these functions are 
#' internally consistent in usage. For example, data set recorded by 
#' Omnirecs Cube data loggers can be conherently processed with the 
#' aux-function family and will result in the envisioned data structure.
#' 
#' The function assumes complete data sets, i.e., not a single hourly 
#' data set must be missing or causes problems during import. The time 
#' vector is loaded only once, from the first station and its first 
#' component. Thus, it is assumed that all loaded seismic signals are
#' of the same sampling frequency and length.
#' 
#' @param start \code{POSIXct} value, start time of the data to import.
#' 
#' @param duration \code{numeric} value, duration of the data to import
#' in seconds.
#' 
#' @param station \code{character} value, seismic station ID, which must
#' correspond to the ID in the file name of the data directory structure 
#' (cf. \code{aux_organisecubefiles()}).
#' 
#' @param component \code{character} value, seismic component, which must
#' correspond to the component name in the file name of the data directory  
#' structure (cf. \code{aux_organisecubefiles()}). Default is 
#' \code{"BHZ"} (vertical component of a sac file).
#' 
#' @param format \code{character} value, seismic data format. One out of 
#' \code{"sac"} and \code{"mseed"}. Default is \code{"sac"}.
#' 
#' @param dir \code{character} value, path to the seismic data directory.
#' 
#' @param simplify \code{logical} value, option to simplify import output
#' when possible. This basically means that if only data from one station 
#' is loaded, the list object will have one level less. Default is 
#' \code{TRUE}.
#' 
#' @param eseis \code{Logical} scalar, option to read data to an \code{eseis}
#' object (recommended, see documentation of 
#' \code{aux_initiateeseis}), default is \code{FALSE}
#' 
#' @return A \code{list} object containing either a set of \code{eseis}
#' objects or a data set with the time vector (\code{$time}) 
#' and a list of seismic stations (\code{$station_ID}) with their seismic
#' signals as data frame (\code{$signal}). If \code{simplify = TRUE} (the 
#' default option) and only one seismic station is provided, the output  
#' object only contains \code{$time} and \code{$signal}.
#' 
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' \dontrun{
#' 
#' ## define event onset and duration
#' start <- as.POSIXct("2015-04-06 13:22:30", tz = "UTC")
#' duration <- 50
#' 
#' ## load the z-component data from a rockfall event
#' data <- aux_getevent(start = start, 
#'                       duration = duration,
#'                       station = "LAU05",
#'                       component = "BHZ",
#'                       dir = "eseis/lauterbrunnen/data/sac/")
#' }
#'                      
#' @export aux_getevent
aux_getevent <- function(
  start,
  duration,
  station, 
  component = "BHZ",
  format = "sac",
  dir,
  simplify = TRUE,
  eseis = FALSE
) {
  
  ## check/set arguments ------------------------------------------------------
  
  ## check start time format
  if(class(start)[1] != "POSIXct") {
    
    stop("Start date is not a POSIXct format!")
  }
  
  ## set default value for data directory
  if(missing(dir) == TRUE) {
    
    dir <- ""
  }
  
  ## composition of the file name patterns ------------------------------------
  
  ## calculate end date
  stop <- start + duration
  
  ## create hour sequence
  hours_seq <- seq(from = start, 
                   to = stop, 
                   by = 3600)
  
  ## extract hour(s) for start and end
  hour <- format(x = hours_seq, 
                 format = "%H")
  
  ## create year sequence
  year_seq <- format(x = hours_seq, 
                     format = "%Y")
  
  ## create JD sequence
  JD_seq <- as.character(eseis::time_convert(input = hours_seq, 
                                             output = "JD"))
  
  ## padd JDs with zeros
  JD_seq_pad <- JD_seq
  
  JD_seq_pad <- ifelse(test = nchar(JD_seq_pad) == 1, 
                   yes = paste("00", JD_seq_pad, sep = ""), 
                   no = JD_seq_pad)
  
  JD_seq_pad <- ifelse(test = nchar(JD_seq_pad) == 2, 
                   yes = paste("0", JD_seq_pad, sep = ""), 
                   no = JD_seq_pad)
  
  ## create directory string for hourly sequence
  files_hourly <- paste(dir,
                        year_seq, "/",
                        JD_seq_pad,
                        sep = "")
  
  ## make file list for JDs
  files <- lapply(X = files_hourly, 
                  FUN = list.files,
                  full.names = TRUE)
  
  ## isolate hours of interest
  for(i in 1:length(files)) {
    
    files[[i]] <- files[[i]][grepl(x = files[[i]], 
                                   pattern = paste(JD_seq[i], 
                                                   hour[i], 
                                                   sep = "."))]
  }
  
  ## convert list to vector
  files <- unlist(files)
  
  ## regroup files by station
  files_station <- vector(mode = "list", 
                          length = length(station))
  
  for(i in 1:length(files_station)) {
    
    files_station[[i]] <- files[grepl(x = files, 
                                      pattern = station[i])]
  }
  
  ## Data import section ------------------------------------------------------
  
  ## create data object
  data <- vector(mode = "list", 
                 length = length(files_station))
  
  ## process files station-wise
  for(i in 1:length(data)) {
    
    data[[i]] <- as.data.frame(do.call(cbind, lapply(
      X = component, 
      FUN = function(x, files_station, i, format, start, stop) {
        
        ## isolate component of interest
        files_cmp <- files_station[[i]]
        files_cmp <- files_cmp[grepl(x = files_cmp, 
                                     pattern = x)]
        
        ## import files based on specified format
        if(format == "sac") {
          
          x <- eseis::read_sac(file = files_cmp, 
                               eseis = TRUE,
                               append = TRUE)
        } else if(format == "mseed") {
          
          x <- eseis::read_mseed(file = files_cmp, 
                                 eseis = TRUE,
                                 append = TRUE)
        }
        
        ## clip signal at start and end time
        x <- eseis::signal_clip(data = x, 
                                limits = c(start, stop))
        
        ## return processed seismic signal
        return(x)
      }, 
      files_station = files_station,
      i = i,
      format = format,
      start = start,
      stop = stop)))
    
    names(data[[i]]) <- component
    
    for(j in 1:length(data[[i]])) {
      
      class(data[[i]][[j]]) <- "eseis"
    }
  }
  
  ## Data cleaning and output section -----------------------------------------
  
  ## optionally convert data structures
  if(eseis == FALSE) {
    
    ## generate time vector
    time <- seq(from = data[[1]][[1]]$meta$starttime, 
                by = data[[1]][[1]]$meta$dt, 
                length.out = data[[1]][[1]]$meta$n)
    
    ## extract signal vectors
    for(i in 1:length(data)) {
      
      data[[i]] <- lapply(X = data[[i]], FUN = function(X) {
        
        X$signal
      })
      
      ## assign names to vectors
      names(data[[i]]) <- component[i]
    }
    
    ## assign names to vectors
    names(data) <- station

    ## create output data set
    data_out <- list(time = time, 
                     signal = data)
    
  } else {
    
    names(data) <- station
    
    data_out <- data
  }
  
  ## optionally simplify data structure
  if(simplify == TRUE) {
    
    if(length(data_out) == 1) {
      
      data_out <- data_out[[1]]
    }
  }
  
  ## return output data set
  return(data_out)
}