#' Convert ObsPy object to eseis object
#' 
#' The function converts an ObsPy stream object to an eseis object. The 
#' functionality is mainly useful when running ObsPy through R using the 
#' package 'reticulate'.
#' 
#' @param data \code{obspy} stream object, \code{list} element, created by 
#' running ObsPy through R using the package 'reticulate'.
#' 
#' @param simplify \code{Logical} value, option to simplify output
#' when possible. This basically means that if there is only trace object 
#' in the ObsPy stream, the list object will have one level less. Default is 
#' \code{TRUE}.
#' 
#' @return \code{eseis} object.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## load ObsPy library with package 'reticulate'
#' ## (requires ObsPy to be installed on the computer)
#' obspy <- reticulate::import("obspy")
#' 
#' ## set seismic data directory
#' dir_data <- paste0(system.file("extdata", package="eseis"), "/")
#' 
#' ## read miniseed file to stream object via ObsPy
#' x <- obspy$read(pathname_or_url = "dir_data/2017/99/RUEG1.17.99.00.00.00.BHZ.SAC")
#' 
#' ## convert ObsPy stream object to eseis object
#' y <- aux_obspyeseis(data = x)
#' 
#' ## plot eseis object
#' plot_signal(y)
#' }
#' 
#' @export aux_obspyeseis
#'  
aux_obspyeseis <- function(
  
  data,
  simplify = TRUE
  
) {
  
  ## collect function arguments
  eseis_arguments <- list(data = data)
  
  ## check if input data is obspy stream object
  if(sum(!is.na(match(x = names(data), 
                      table = c("traces", "remove", "plot")))) == 0) {
    
    stop("Input object seems to be no obspy stream object!")
  }
  
  ## get start time
  t_0 <- Sys.time()
  
  ## get number of traces in stream
  ID_trace <- unlist(lapply(X = data$traces, FUN = function(traces) {
    
    traces$stats$channel
  }))
  
  ## create output data set
  data_out <- lapply(X = data$traces, FUN = function(traces) {
    
    ## create empty eseis object
    eseis_i <- aux_initiateeseis()
    
    ## add signal vector
    eseis_i$signal <- traces$data
    
    ## add meta data
    eseis_i$meta$station <- traces$stats$station
    eseis_i$meta$network <- traces$stats$network
    eseis_i$meta$component <- traces$stats$channel
    eseis_i$meta$n <- length(traces$data)
    eseis_i$meta$sensor <- "NA"
    eseis_i$meta$logger <- "NA"
    t_meta <- as.character(traces$stats$starttime)
    eseis_i$meta$starttime <- as.POSIXct(x = substr(x = t_meta,
                                              start = 1, 
                                              stop = nchar(t_meta) - 1), 
                                   format = "%Y-%m-%dT%H:%M:%OS", 
                                   tz = "UTC")
    eseis_i$meta$dt <- traces$stats$delta
    eseis_i$meta$latitude <- "NA"
    eseis_i$meta$longitude <- "NA"
    eseis_i$meta$elevation <- "NA"
    eseis_i$meta$filename <- "NA"
    eseis_i$meta$type <- "waveform"
    
    ## add header information
    eseis_i$header <- traces$stats
    
    ## calculate function call duration
    eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                          time2 = t_0, 
                                          units = "secs"))
    
    eseis_i$history[[length(eseis_i$history) + 1]] <- 
      list(time = Sys.time(),
           call = "obspyeseis()",
           arguments = eseis_arguments,
           duration = eseis_duration)
    names(eseis_i$history)[length(eseis_i$history)] <- 
      as.character(length(eseis_i$history))
    
    ## return eseis object
    return(eseis_i)
  })
  
  ## assign trace IDs as list names
  names(data_out) <- ID_trace
  
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




