#' Download seismic data from IRIS data base
#' 
#' This function accesses the IRIS internet data base of seismic signals and 
#' downloads seismic data based on the provided SNCL string and time 
#' information. The downloaded data is converted to the same structure as 
#' would be expected from \code{read_sac()} or \code{read_mseed()}.
#' 
#' The function makes use of the functionalities provided by the package 
#' IRISSeismic. It requires a working internet connection to perform the 
#' download.
#'
#' @param start \code{Posixct} value, start time of the data to query. 
#' 
#' @param duration \code{Numeric} value, length of the data to query, in 
#' seconds.
#' 
#' @param sncl \code{Character} vector, SNCL string used to identify station 
#' and component of interest. These strings should match the time criteria. 
#' Typically, the SNCL string can be taken from the output of the function
#' \code{aux_getirisstations}.
#' 
#' @param ID_iris \code{Character} value, IRIS ID. Default is 
#' \code{"IrisClient"}.
#' 
#' @return \code{List} with downloaded seismic data. For each element in 
#' \code{sncl}, a list element is created, which in turn contains a list with 
#' the typical seismic data organisation as, for example, created by 
#' \code{read_sac()}.
#' 
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' \dontrun{
#' 
#' sncl <- aux_getirisdata(start = as.POSIXct("2010-01-01 22:22:22", 
#'                                            tz = "UTC"), 
#'                         duration = 120, 
#'                         location = c(53, 13), 
#'                         radius = 1, 
#'                         component = "BHZ")
#' 
#' data <- aux_getirisdata(start = as.POSIXct("2010-01-01 22:22:22", 
#'                                            tz = "UTC"), 
#'                         duration = 120,
#'                         sncl = sncl$sncl[1])
#'                         
#' s <- data[[1]]$signal
#' t <- data[[1]]$time
#' 
#' plot_signal(data = s, 
#'             time = t)
#' }
#'                      
#' @export aux_getirisdata
#' 
aux_getirisdata <- function (
  
  start, 
  duration, 
  sncl,
  ID_iris = "IrisClient"
) {
  
  ## generate new IRIS client ID
  iris <- new(ID_iris)
  
  ## create output data set
  data <- vector(mode = "list", 
                 length = length(sncl))
  
  ## process data download sequentially
  for(i in 1:length(sncl)) {
    
    ## download data
    data_i <- invisible(try(IRISSeismic::getSNCL(obj = iris,  
                                                 sncl = sncl[i], 
                                                 starttime = start, 
                                                 endtime = start + duration), 
                            silent = TRUE))
    
    if(class(data_i) == "try-error") {
      
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
                   filename = data_i@url)
      
      header <- NA
      
      data[[i]] <- list(signal = signal,
                        time = time,
                        meta = meta,
                        header = header)
    }
  } 
  
  ## return results
  return(data)
}