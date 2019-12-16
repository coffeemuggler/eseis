#' Download seismic data from IRIS data base
#' 
#' This function accesses the IRIS internet data base of seismic signals and 
#' downloads seismic data based on the provided SNCL string and time 
#' information. The downloaded data is converted to the same structure as 
#' would be expected from \code{read_sac} or \code{read_mseed}.
#' 
#' The function makes use of the package 'IRISSeismic'. It requires a working 
#' internet connection to perform the download.
#'
#' @param start \code{POSIXct} value, start time of the data to query. 
#' 
#' @param duration \code{Numeric} value, length of the data to query, in 
#' seconds.
#' 
#' @param sncl \code{Character} vector, SNCL string used to identify station 
#' and component of interest. These strings should match the time criteria. 
#' Typically, the SNCL string can be taken from the output of the function
#' \code{aux_getirisstations}.
#' 
#' @param quality \code{Character} value, quality level of the data. One out 
#' of \code{"D"} (The state of quality control of the data is indeterminate), 
#' \code{"R"} (Raw Waveform Data with no Quality Control), 
#' \code{"Q"} (Quality Controlled Data, some processes have been applied to 
#' the data), \code{"M"} (Data center modified, time-series values have not 
#' been changed), \code{"B"}. Default is \code{"D"}.
#' 
#' @param ID_iris \code{Character} value, IRIS ID. Default is 
#' \code{"IrisClient"}.
#' 
#' @param eseis \code{Logical} scalar, option to read data to an \code{eseis}
#' object (recommended, see documentation of 
#' \code{aux_initiateeseis}), default is \code{TRUE}
#' 
#' @return \code{List} with downloaded seismic data. For each element in 
#' \code{sncl}, a list element is created, which in turn contains a list with 
#' the typical seismic data organisation as, for example, created by 
#' \code{read_sac}.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' sncl <- aux_getIRISstation(start = as.POSIXct("2010-01-01 22:22:22", 
#'                                                tz = "UTC"), 
#'                             duration = 120, 
#'                             location = c(53, 13), 
#'                             radius = 0.7, 
#'                             component = "BHZ")
#' 
#' s <- aux_getIRISdata(start = as.POSIXct("2010-01-01 22:22:22", 
#'                                            tz = "UTC"), 
#'                         duration = 120,
#'                         sncl = sncl$sncl[1])
#'                         
#' plot_signal(data = s[[1]])
#' }
#'                      
#' @export aux_getIRISdata
#' 
aux_getIRISdata <- function (
  
  start, 
  duration, 
  sncl,
  quality = "D",
  ID_iris = "IrisClient",
  eseis = TRUE
) {
  
  ## collect function arguments
  eseis_arguments <- list(start = start,
                          duration = duration, 
                          sncl = sncl,
                          quality = quality,
                          ID_iris = ID_iris,
                          eseis = eseis)
  
  ## generate new IRIS client ID
  iris <- new(ID_iris)
  
  ## create output data set
  if(eseis == TRUE) {
    
    data_list <- lapply(X = 1:length(sncl), 
                     FUN = function(X) {
                       eseis::aux_initiateeseis()
                     })
  } else {
    
    data_list <- vector(mode = "list", 
                     length = length(sncl))
  }
  
  ## process data download sequentially
  for(i in 1:length(sncl)) {
    
    ## download data
    data_i <- invisible(try(IRISSeismic::getSNCL(obj = iris,  
                                                 sncl = as.character(sncl[i]),
                                                 starttime = start, 
                                                 quality = quality,
                                                 endtime = start + duration), 
                            silent = TRUE))
    
    if(class(data_i)[1] == "try-error") {
      
      data_i <- NA
    } else {
      
      signal <- data_i@traces[[1]]@data
      
      dt <- data_i@traces[[1]]@stats@delta
      
      n <- data_i@traces[[1]]@stats@npts
      
      t_start = as.POSIXct(as.numeric(data_i@traces[[1]]@stats@starttime), 
                           origin = "1970-01-01", 
                           tz = "UTC")
      
      time <- seq(from = t_start, 
                  by = dt, 
                  length.out = n)
      
      meta <- list(station = data_i@traces[[1]]@stats@station,
                   network = data_i@traces[[1]]@stats@network,
                   component = data_i@traces[[1]]@stats@channel,
                   n = n,
                   sensor = data_i@traces[[1]]@Sensor,
                   logger = NA,
                   starttime = t_start,
                   dt = dt,
                   latitude = data_i@traces[[1]]@stats@latitude,
                   longitude = data_i@traces[[1]]@stats@longitude,
                   elevation = data_i@traces[[1]]@stats@elevation,
                   depth = data_i@traces[[1]]@stats@depth,
                   filename = data_i@url)
      
      header <- NA
      
      if(eseis == TRUE) {
        
        ## calculate function call duration
        step_process <- length(data_list[[i]]$history)
        data_list[[i]]$history[[step_process]]$duration <- 
          as.numeric(difftime(time1 = Sys.time(), 
                              time2 = data_list[[i]]$
                                history[[step_process]]$time, 
                              units = "secs"))
        
        ## fill eseis object
        data_list[[i]]$signal <- signal
        data_list[[i]]$meta <- meta
        data_list[[i]]$header <- header
        data_list[[i]]$history[[length(data_list[[i]]$history) + 1]] <- 
          list(time = Sys.time(),
               call = "aux_getirisdata()",
               arguments = eseis_arguments)
        names(data_list[[i]]$history)[length(data_list[[i]]$history)] <- 
          as.character(length(data_list[[i]]$history))

      } else {
        
        ## fill data object
        data_list[[i]] <- list(signal = signal,
                            time = time,
                            meta = meta,
                            header = header)
      }
    }
  } 

  ## return results
  return(data_list)
}