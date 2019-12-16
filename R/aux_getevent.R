#' Load seismic data of a user-defined event
#' 
#' The function loads seismic data from a data directory structure (see 
#' \code{aux_organisecubefiles()}) based on the event start time, duration,
#' component and station ID.
#' 
#' The function assumes complete data sets, i.e., not a single hourly 
#' data set must be missing. The time 
#' vector is loaded only once, from the first station and its first 
#' component. Thus, it is assumed that all loaded seismic signals are
#' of the same sampling frequency and length.
#' 
#' @param start \code{POSIXct} value, start time of the data to import.
#' 
#' @param duration \code{Numeric} value, duration of the data to import,
#' in seconds.
#' 
#' @param station \code{Character} value, seismic station ID, which must
#' correspond to the ID in the file name of the data directory structure 
#' (cf. \code{aux_organisecubefiles}).
#' 
#' @param component \code{Character} value, seismic component, which must
#' correspond to the component name in the file name of the data directory  
#' structure (cf. \code{aux_organisecubefiles}). Default is 
#' \code{"BHZ"} (vertical component of a sac file).
#' 
#' @param format \code{Character} value, seismic data format. One out of 
#' \code{"sac"} and \code{"mseed"}. Default is \code{"sac"}.
#' 
#' @param dir \code{Character} value, path to the seismic data directory.
#' 
#' @param simplify \code{Logical} value, option to simplify output
#' when possible. This basically means that if only data from one station 
#' is loaded, the list object will have one level less. Default is 
#' \code{TRUE}.
#' 
#' @param eseis \code{Logical} value, option to read data to an \code{eseis}
#' object (recommended, see documentation of 
#' \code{aux_initiateeseis}), default is \code{TRUE}
#' 
#' @param try \code{Logical} value, option to run the fuction in try-mode, 
#' i.e., to let it return \code{NA} in case an error occurs during data
#' import. Default is \code{FALSE}.
#' 
#' @return A \code{list} object containing either a set of \code{eseis}
#' objects or a data set with the time vector (\code{$time}) 
#' and a list of seismic stations (\code{$station_ID}) with their seismic
#' signals as data frame (\code{$signal}). If \code{simplify = TRUE} (the 
#' default option) and only one seismic station is provided, the output  
#' object containseither just one eseis object or the vectors for 
#' \code{$time} and \code{$signal}.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## set seismic data directory
#' dir_data <- paste0(system.file("extdata", package="eseis"), "/")
#' 
#' ## load the z component data from a station
#' data <- aux_getevent(start = as.POSIXct(x = "2017-04-09 01:20:00", 
#'                                         tz = "UTC"), 
#'                       duration = 120,
#'                       station = "RUEG1",
#'                       component = "BHZ",
#'                       dir = dir_data)                       
#' ## plot signal
#' plot_signal(data = data)
#' 
#' ## load data from two stations
#' data <- aux_getevent(start = as.POSIXct(x = "2017-04-09 01:20:00", 
#'                                         tz = "UTC"), 
#'                      duration = 120,
#'                      station = c("RUEG1", "RUEG2"),
#'                      component = "BHZ",
#'                      dir = dir_data)
#' 
#' ## simplify data structure
#' data <- lapply(X = data, FUN = function(data) {data[[1]]})
#' 
#' ## plot both signals
#' par(mfcol = c(2, 1))
#' lapply(X = data, FUN = plot_signal)
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
  eseis = TRUE,
  try = FALSE
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

  ## get system time zone
  tz_system <- Sys.timezone()
  
  ## get desired time zone
  tz_start <- format(start, 
                     format = "%Z")
  
  if(tz_system != tz_start) {
    
    ## set system time zone to input data time zone
    Sys.setenv(TZ = format(start, 
                           format = "%Z"))

    ## create information message
    tz_message <- paste("System time zone changed to event time zone. ",
                        "Undo with Sys.setenv(TZ = '",
                        tz_system,
                        "')",
                        sep = "")
    
    ## inform about time zone change
    print(tz_message)
  }
  
  ## composition of the file name patterns ------------------------------------
  
  ## calculate end date
  stop <- start + duration
  
  ## create hour sequence
  if(as.numeric(difftime(time1 = stop, 
                         time2 = start, 
                         units = "secs")) < 3600) {
    
    hours_seq <- seq(from = start, 
                     to = stop + 3600, 
                     by = 3600)
  } else {
    
    hours_seq <- seq(from = start, 
                     to = stop, 
                     by = 3600)
  }
  
  ## extract hour(s) for start and end
  hour <- format(x = hours_seq, 
                 format = "%H")
  
  ## create year sequence
  year_seq <- format(x = hours_seq, 
                     format = "%Y")
  
  ## create JD sequence
  JD_seq <- as.character(eseis::time_convert(input = hours_seq, 
                                             output = "JD"))
  
  ## pad JDs with zeros
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
    
    if(try == TRUE) {
      
      data[[i]] <- try(as.data.frame(do.call(cbind, lapply(
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
        stop = stop))))
        
        try(names(data[[i]]) <- component)
        
        try(for(j in 1:length(data[[i]])) {
          
          class(data[[i]][[j]])[1] <- "eseis"
        })
    } else {
      
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
        
        class(data[[i]][[j]])[1] <- "eseis"
      }
    }
  }
  
  ## Data cleaning and output section -----------------------------------------

  ## optionally convert data structures
  if(try == TRUE) {
    
    if(eseis == FALSE) {
      
      ## generate time vector
      time <- try(seq(from = data[[1]][[1]]$meta$starttime, 
                  by = data[[1]][[1]]$meta$dt, 
                  length.out = data[[1]][[1]]$meta$n))
      
      if(class(time)[1] == "try-error") {
        
        time <- NA
      }
      
      ## extract signal vectors
      for(i in 1:length(data)) {
        
        data[[i]] <- try(lapply(X = data[[i]], FUN = function(X) {
          
          X$signal
        }))
        
        ## assign names to vectors
        try(names(data[[i]]) <- component[i])
        
        ## account for try-errors
        if(class(data[[i]])[1] == "try-error") {
          
          data <- NA
        }
      }
      
      ## assign names to vectors
      try(names(data) <- station)
      
      ## create output data set
      data_out <- list(time = time, 
                       signal = data)
      
    } else {

      names(data) <- station
      
      ## account for try-errors
      data <- lapply(X = data, FUN = function(data) {
        
        if(class(data)[1] == "try-error") {
          
          return(NA)
        } else {
          
          return(data)
        }
      })
      
      data_out <- data
    }
  } else {
    
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
  }
  
  ## optionally simplify data structure
  if(simplify == TRUE) {
    
    if(length(data_out) == 1) {

      data_out <- data_out[[1]]
    }
    
    if(length(data_out) == 1) {
      
      data_out <- data_out[[1]]
    }
  }
  
  ## return output data set
  return(data_out)
}