#' Load seismic data of a user-defined event
#' 
#' The function loads seismic data from a data directory structure (see 
#' \code{aux_organisecubefiles()}) based on the event start time, duration,
#' component and station ID.
#' 
#' The data to be read needs to be adequately structured. The data directory
#' must contain SAC files organised by year (e.g.2022) then by Julian Day
#' in full three digits (e.g. 001) and then by a dedicated SAC file name, 
#' containing the station ID, two-digit year, three-digit Julian Day, start 
#' time hour, minute and second, three channel ID and the file extension SAC. 
#' All these items need to be separated by stops (e.g. 
#' sac/2022/001/LAU01.22.001.08.00.00. BHZ.SAC). This data structure will be 
#' most conveniently created by the functions \code{aux_organisecubefiles()} 
#' or \code{aux_organisecentaurfiles()}, or by manually written R code. 
#' 
#' The function assumes complete data sets, i.e., not a single hourly 
#' data set must be missing. The time 
#' vector is loaded only once, from the first station and its first 
#' component. Thus, it is assumed that all loaded seismic signals are
#' of the same sampling frequency and length.
#' 
#' @param start \code{POSIXct} value, start time of the data to import. If
#' lazy users only submit a text string instead of a POSIXct obejct, the 
#' function will try to convert that text string.
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
#' \code{"sac"} and \code{"mseed"}. If omitted, the function will try to 
#' identify the right format automatically.
#' 
#' @param dir \code{Character} value, path to the seismic data directory.
#' See details for further info on data structure.
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
#' @param try \code{Logical} value, option to run the function in try-mode, 
#' i.e., to let it return \code{NA} in case an error occurs during data
#' import. Default is \code{FALSE}.
#' 
#' @param verbose \code{Logical} value, option to show messages during 
#' function execution. Default is \code{FALSE}.
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
#' ## plot both signals
#' par(mfcol = c(2, 1))
#' lapply(X = data, FUN = plot_signal)
#'                      
#' @export aux_getevent
#' 
aux_getevent <- function(
    start,
    duration,
    station, 
    component = "BHZ",
    format,
    dir,
    simplify = TRUE,
    eseis = TRUE,
    try = FALSE,
    verbose = FALSE
) {
  
  ## check/set arguments ------------------------------------------------------
  
  
  ## check start time format
  if(class(start)[1] != "POSIXct") {
    
    start <- try(as.POSIXct(start, tz = "UTC"), silent = TRUE)
    
    if(class(start)[1] != "POSIXct") {
      
      stop("Start date is not a POSIXct format!")
    }
  }
  
  ## check/set file format
  if(missing(format) == TRUE) {
    
    format <- "unknown"
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
    if(verbose == TRUE) {
      
      print(tz_message)
    }
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
  JD_seq <- format(x = hours_seq, 
                   format = "%j")
  
  ## create JD and hour string
  JD_hour_seq <- format(x = hours_seq, 
                        format = "%j.%H.")
  
  ## create directory string for hourly sequence
  files_hourly <- paste(dir,
                        year_seq, "/",
                        JD_seq,
                        sep = "")
  
  ## make file list for JDs
  files <- try(lapply(X = files_hourly, 
                      FUN = list.files,
                      full.names = TRUE), 
               silent = TRUE)
  
  ## check for file presence
  if(class(files)[1] == "try-error") {
    
    stop("No files exist for this time window!")
  }
  
  ## convert list to vector
  files <- do.call(c, files)
  
  ## remove dulicates
  files <- unique(files)
  
  ## isolate hours of interest
  files <- lapply(X = JD_hour_seq, FUN = function(x, files) {
    
    files[grepl(x = files, pattern = x)]
  }, files)
  
  ## convert list to vector
  files <- do.call(c, files)
  
  ## remove duplicates
  files <- unique(files)
  
  ## check for file presence
  if(length(files) < 1) {
    
    stop("No files exist for this time window!")
  }
  
  ## check if station IDs are available
  for(i in 1:length(station)) {
    
    if(sum(grepl(pattern = station[i], x = files)) < 1) {
      
      stop(paste("Station", station[i], "not present!"))
    }
  }
  
  ## regroup files by station
  files_station <- vector(mode = "list", 
                          length = length(station))
  
  for(i in 1:length(files_station)) {
    
    files_station[[i]] <- files[grepl(x = files, 
                                      pattern = station[i])]
  }
  
  ## check for file presence
  if(length(unlist(files_station)) < 1) {
    
    stop("No files of that station(s) exist for this time window!")
  }
  
  ## Data import section ------------------------------------------------------
  
  ## create data object
  data <- vector(mode = "list", 
                 length = length(files_station))
  
  ## find out file format
  if(format == "unknown") {
    
    x_format <- suppressWarnings(try(eseis::read_sac(
      file = files_station[[1]][1], 
      eseis = TRUE), 
      silent = TRUE))
    if(class(x_format)[1] == "eseis") {
      
      format <- "sac"
    } else {
      
      x_format <- suppressWarnings(try(eseis::read_mseed(
        file = files_station[[1]][1], 
        eseis = TRUE,
        append = TRUE), 
        silent = TRUE))
      
      if(class(x_format)[1] == "eseis") {
        
        format <- "mseed"
      } else {
        
        stop("Cannot determine file format automatically!")
      }
    }
  }
  
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
            
            x <- suppressWarnings(try(eseis::read_sac(
              file = files_cmp, 
              eseis = TRUE,
              append = TRUE), silent = TRUE))
          } else if(format == "mseed") {
            
            x <- suppressWarnings(try(eseis::read_mseed(
              file = files_cmp, 
              eseis = TRUE,
              append = TRUE), silent = TRUE))
            
          }
          
          ## clip signal at start and end time
          if(verbose == TRUE)  {
            
            x <- eseis::signal_clip(data = x, 
                                    limits = c(start, stop))
          } else {
            
            x <- suppressWarnings(eseis::signal_clip(data = x, 
                                                     limits = c(start, stop)))
          }
          
          ## return processed seismic signal
          return(x)
        }, 
        files_station = files_station,
        i = i,
        format = format,
        start = start,
        stop = stop))),
      silent = TRUE)

try(names(data[[i]]) <- component)

try(for(j in 1:length(data[[i]])) {
  
  class(data[[i]][[j]])[1] <- "eseis"
}, silent = TRUE)
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
            
            x <- suppressWarnings(try(eseis::read_sac(
              file = files_cmp, 
              eseis = TRUE,
              append = TRUE), silent = TRUE))
          } else if(format == "mseed") {
            
            x <- suppressWarnings(try(eseis::read_mseed(
              file = files_cmp, 
              eseis = TRUE,
              append = TRUE), silent = TRUE))
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
                      length.out = data[[1]][[1]]$meta$n), 
                  silent = TRUE)
      
      if(class(time)[1] == "try-error") {
        
        time <- NA
      }
      
      ## extract signal vectors
      for(i in 1:length(data)) {
        
        data[[i]] <- try(lapply(X = data[[i]], FUN = function(X) {
          
          X$signal
        }), silent = TRUE)
        
        ## assign names to vectors
        try(names(data[[i]]) <- component[i], silent = TRUE)
        
        ## account for try-errors
        if(class(data[[i]])[1] == "try-error") {
          
          data <- NA
        }
      }
      
      ## assign names to vectors
      try(names(data) <- station, silent = TRUE)
      
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
    
    ## case of one station and one component
    if(length(data_out) == 1) {
      
      data_out <- as.list(data_out[[1]])
      
    }
    
    if(length(data_out) == 1) {
      
      data_out <- data_out[[1]]
    }
    
    ## case of several stations and/or components
    if(length(station) > 1) {
      
      data_out <- lapply(X = data_out, FUN = function(x) {
        
        ## check if several components are present per station
        if(length(x) == 1) {
          
          as.list(x[[1]])
        } else {
          
          as.list(x)
        }
      })
    }
  } else {
    
    data_out <- lapply(X = data_out, FUN = function(x) {
      
      as.list(x)
    })
  }
  
  ## return output data set
  return(data_out)
}