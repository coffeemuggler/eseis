#' Read mseed files.
#'
#' This function reads mseed files. If \code{append = TRUE}, all
#' files will be appended to the first one in the order as they are provided. 
#' In the append-case the function returns a either a list with the elements 
#' \code{signal}, \code{time}, \code{meta} and \code{header} or a list of the 
#' class \code{eseis} (see documentation of 
#' \code{aux_initiateeseis()}). If \code{append = FALSE} and more than one file 
#' is provided, the function returns a list of the length of the input files, 
#' each containing the above elements. \cr\cr The mseed data format is read 
#' using the function \code{readMiniseedFile} from the 
#' package \code{IRISSeismic}.
#'
#' @param file \code{Character} vector, input file name(s), with extension.
#' 
#' @param append \code{Logical} value, option to append single files to one
#' continuous file, keeping only the hedaer information of the first file,
#' default is \code{TRUE}.
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
#' ## read more than one mseed files and append traces
#' x <- read_mseed(file = c("input_1.miniseed", "input_2.miniseed"))
#' }
#'
#' @export read_mseed
read_mseed <- function(
  file,
  append = TRUE,
  signal = TRUE,
  time = TRUE,
  meta = TRUE,
  header = TRUE,
  eseis = TRUE,
  type = "waveform"
) {
  
  ## collect function arguments
  eseis_arguments <- list(file = file,
                          append = append,
                          signal = signal,
                          time = time,
                          meta = meta,
                          header = header,
                          eseis = eseis,
                          type = type)
  
  ## get start time
  t_0 <- Sys.time()
  
  ## check/select files
  if(sum(grepl(pattern = "[*]", x = file)) > 0) {
    
    file <- list.files(pattern = file)
  }
  
  ## convert files to list
  file <- as.list(file)

  ## read all mseed files
  data <- lapply(X = file, 
                 FUN = IRISSeismic::readMiniseedFile,
                 sensor = "", 
                 scale = 1, 
                 scaleunits = "")
  
  ## extract sampling periods
  dt <- lapply(X = data, FUN = function(x) {
    
    x@traces[[1]]@stats@delta
  })
  
  ## extract number of samples
  n <- lapply(X = data, FUN = function(x) {
    
    unlist(lapply(X = x@traces, FUN = function(y) {
      
      y@stats@npts
    }))
  })
  
  ## extract global start and end times per trace
  time_limits <- lapply(X = data, FUN = function(x) {
    
    lapply(X = x@traces, FUN = function(y) {
      
      list(start = as.POSIXct(as.numeric(y@stats@starttime), 
                              origin = "1970-01-01", 
                              tz = "UTC"),
           stop = as.POSIXct(as.numeric(y@stats@endtime),
                             origin = "1970-01-01", 
                             tz = "UTC"))
    })
  })
  
  ## convert time data back to POSIXct with UTC time zone
  time_limits_global <- lapply(X = time_limits, FUN = function(x) {
    as.POSIXct(range(unlist(x),
                     na.rm = TRUE), 
               origin = "1970-01-01", 
               tz = "UTC")
  })
  
  ## create time vectors
  time_list <- vector(mode = "list", length = length(data))
  
  for(i in 1:length(time_list)) {
    
    if(length(time_limits[[i]]) == 1) {
      
      time_list[[i]] <- seq(from = time_limits[[i]][[1]]$start,
                       by = dt[[i]],
                       length.out = n[[i]])
    } else {
      
      warning(paste("Trace of file", file[i], "may contain NA-values!"))
      time_list[[i]] <- seq(from = time_limits_global[[i]][1],
                       to = time_limits_global[[i]][2],
                       by = dt[[i]])
    }
  }
  
  ## create empty signal vectors
  signal_list <- lapply(X = time_list, FUN = function(x) {
    
    rep(NA, length(x))
  })
  
  ## paste signal parts into signal vectors
  for(i in 1:length(signal_list)) {
    
      for(j in 1:length(time_limits[[i]])) {
        
        i_time_ok <- time_list[[i]] >= time_limits[[i]][[j]]$start & 
          time_list[[i]] <= time_limits[[i]][[j]]$stop
        
        if(sum(i_time_ok) != length(data[[i]]@traces[[j]]@data)) {
          
          print("read_mseed: possible mismatch of time and data length.")
        }
        
        signal_list[[i]][i_time_ok] <- 
          data[[i]]@traces[[j]]@data[1:sum(i_time_ok)]
      }
  }
  
  ## optionally assign header part
  if(header == TRUE) {
    
    header_list <- data
    
    for(i in 1:length(header_list)) {
      
      for(j in 1:length(header_list[[i]]@traces)) {
        
        header_list[[i]]@traces[[j]]@data <- 0
      }
    }
  } else {
    
    header_list <- vector(mode = "list", length = length(data))
  }
  
  ## optionally extract meta information
  meta_list <- vector(mode = "list", length = length(data)) 
  
  if(meta == TRUE) {
    
    for(i in 1:length(meta_list)) {
      
      meta_list[[i]] <- list(station = data[[i]]@traces[[1]]@stats@station,
                        network = data[[i]]@traces[[1]]@stats@network,
                        component = data[[i]]@traces[[1]]@stats@channel,
                        n = length(signal_list[[i]]),
                        sensor = data[[i]]@traces[[1]]@Sensor,
                        logger = NA,
                        starttime = data[[i]]@traces[[1]]@stats@starttime,
                        dt = data[[i]]@traces[[1]]@stats@delta,
                        latitude = data[[i]]@traces[[1]]@stats@latitude,
                        longitude = data[[i]]@traces[[1]]@stats@longitude,
                        elevation = data[[i]]@traces[[1]]@stats@elevation,
                        depth = data[[i]]@traces[[1]]@stats@depth,
                        filename = data[[i]]@url,
                        type = eseis_arguments$type)
    }
  }
  
  ## create output object
  if(eseis == TRUE) {
    
    data_out <- lapply(X = 1:length(file), 
                        FUN = function(X) {
                          eseis::aux_initiateeseis()
                        })
  } else {
    
    data_out <- vector(mode = "list", 
                        length = length(file))
  }
  
  for(i in 1:length(data_out)) {
    
    if(eseis == TRUE) {
      
      ## calculate function call duration
      eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                            time2 = t_0, 
                                            units = "secs"))
      
      ## fill eseis object
      data_out[[i]]$signal <- signal_list[[i]]
      data_out[[i]]$meta <- meta_list[[i]]
      data_out[[i]]$header <- header_list[[i]]
      data_out[[i]]$history[[length(data_out[[j]]$history) + 1]] <- 
        list(time = Sys.time(),
             call = "read_mseed()",
             arguments = eseis_arguments,
             duration = eseis_duration)
      names(data_out[[i]]$history)[length(data_out[[i]]$history)] <- 
        as.character(length(data_out[[i]]$history))
      
    } else {
      
      ## fill data object
      data_out[[i]] <- list(signal = signal_list[[i]],
                            time = time_list[[i]],
                            meta = meta_list[[i]],
                            header = header_list[[i]])    }
  }
  
  ## optionally append data
  if(append == TRUE) {
    
    data_append <- as.numeric(unlist(signal_list))

    time_append <- as.POSIXct(unlist(time_list),
                              origin = "1970-01-01", 
                              tz = "UTC")
    
    filenames_append <- unlist(lapply(X = meta_list, FUN = function(x) {
      x$filename
    }))
    
    data_out <- data_out[[1]]
    
    data_out$signal <- data_append
    
    if(eseis == FALSE) {
      
      data_out$time <- time_append
    }
    
    data_out$meta$n <- length(data_append)
    
    data_out$meta$filename <- filenames_append
  }

  ## return data set
  return(data_out)
}
