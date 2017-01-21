#' Read sac-files.
#'
#' This function reads sac-files.
#'
#' @param file \code{Character} vector, input file name(s), with extension. 
#' Wildcards may be used (see details and examples).
#' 
#' @param append \code{Logical} scalar, option append single files to one
#' continuous file, keeping only the hedaer information of the first file.
#' 
#' @param signal \code{Logical} scalar, option to import the signal vector, 
#' default is \code{TRUE}.
#' 
#' @param time \code{Logical} scalar, option to create the time vector. The 
#' timezone is automatically set to \code{"UTC"}, default is \code{TRUE}.
#' 
#' @param meta \code{Logical} scalar, option to append the meta data part, 
#' default is \code{TRUE}.
#' 
#' @param header \code{Logical} scalar, option to append the header part, 
#' default is \code{TRUE}.
#' 
#' The function reads one or more mseed-files. If \code{append = TRUE}, all
#' files will be appended to the first one in the order as they are provided. 
#' In the append-case the function returns a list of length 4 with the elements 
#' \code{signal}, \code{time}, \code{meta} and \code{header}. If 
#' \code{append = FALSE} and nmore than one file is provided, the function
#' returns a list of the length of the input files, each containing the above
#' elements. \cr\cr The mseed data format is read using the function 
#' \code{readMiniseedFile} from the package \code{IRISSeismic}.
#' 
#' @return \code{List} object.
#' @author Michael Dietze
#' @examples
#'
#' ## Not run - uncomment to run
#' ## read mseed file with default options
#' # x <- read_mseed(file = "input.miniseed")
#' 
#' ## read mseed file, only signal trace
#' # x <- read_mseed(file = "input.miniseed", 
#' #                 time = FALSE, 
#' #                 meta = FALSE, 
#' #                 header = FALSE)
#'                 
#' ## read more than one mseed files and append traces
#' # x <- read_mseed(file = c("input_1.miniseed", "input_2.miniseed"))
#'
#' @export read_mseed
read_mseed <- function(
  file,
  append = TRUE,
  signal = TRUE,
  time = TRUE,
  meta = TRUE,
  header = TRUE
) {
  
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
        
        signal_list[[i]][time_list[[i]] >= time_limits[[i]][[j]]$start & 
                    time_list[[i]] <= time_limits[[i]][[j]]$stop] <- 
          data[[i]]@traces[[j]]@data
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
                        latitude = NA,
                        longitude = NA,
                        elevation = NA,
                        filename = data[[i]]@url)
    }
  }
  
  ## assign all data to output object
  data_out <- vector(mode = "list", length = length(data))
  
  for(i in 1:length(data_out)) {
    
    data_out[[i]] <- list(signal = signal_list[[i]],
                          time = time_list[[i]],
                          meta = meta_list[[i]],
                          header = header_list[[i]])
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
    data_out$time <- time_append
    
    data_out$meta$n <- length(data_append)
    
    data_out$meta$filename <- filenames_append
  }

  ## return data set
  return(data_out)
}
