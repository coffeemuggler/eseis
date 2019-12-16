#' Initiate an eseis object
#' 
#' The function generates an empty eseis object, starting with processing 
#' step 0. The object contains no data and the history only contains the 
#' system information.
#' 
#' The S3 object class \code{eseis} contains the data vector (\code{$signal}), 
#' a meta information list (\code{$meta}) with all essential seismic meta data - 
#' such as sampling interval, station ID, component, start time of the stream 
#' or file name of the input file - a list with header data of the seismic 
#' source file (\code{$header}), and a history list (\code{$history}), which 
#' records all data manipulation steps of an (\code{eseis}) object. The element 
#' (\code{$meta}) will be used by functions of the package to look for 
#' essential information to perform data manipulations (e.g., the sampling 
#' interval). Thus, working with (\code{eseis}) objects is convenient and less 
#' prone to user related errors/bugs, given that the meta information is 
#' correct and does not change during the processing chain; package functions 
#' will update the meta information whenever necessary (e.g., 
#' \code{signal_aggregate}). The element \code{$header} will only be
#' present if a seismic source file has been imported.
#' 
#' The history element is the key feature for transparent and reproducable 
#' research using this R package. An \code{eseis} object records a history of 
#' every function it has been subject to, including the time stamp, the 
#' function call, all used function arguments and their associated values, 
#' and the overall processing duration in seconds. The history is updated 
#' whenever an \code{eseis} object is manipulated with one of the functions 
#' of this package (with a few exceptions, mainly from the aux_... category). 
#' 
#' @return \code{S3} list object of class \code{eseis}.
#' 
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## initiate eseis object
#' aux_initiateeseis()
#'                      
#' @export aux_initiateeseis
aux_initiateeseis <- function(
  
) {
  
  ## build eseis object
  eseis <- list(signal = numeric(length = 0),
                meta = list(station = NA,
                            network = NA,
                            component = NA,
                            n = NA,
                            sensor = NA,
                            logger = NA,
                            gain = NA,
                            starttime = as.POSIXct(x = NA, 
                                                   tz = "UTC"),
                            dt = NA,
                            latitude = NA,
                            longitude = NA,
                            elevation = NA,
                            depth = NA,
                            filename = NA,
                            type = NA),
                header = list(),
                history = list("1" = list(system = sessionInfo())))
  
  ## provide eseis class name
  class(eseis)[1] <- "eseis"
  
  ## return eseis object
  return(eseis)
}