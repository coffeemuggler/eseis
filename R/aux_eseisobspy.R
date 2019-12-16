#' Convert eseis object to ObsPy stream object
#' 
#' The function converts an eseis object to an ObsPy stream object. The 
#' functionality is mainly useful when running ObsPy through R using the 
#' package 'reticulate'. Currently, only single traces (i.e., single eseis 
#' objects) can be converted. Thus, to convert multiple traces, these need 
#' to be converted individually and added to the first trace using ObsPy 
#' functionalities.
#' 
#' @param data \code{eseis} object, \code{list} element.
#' 
#' @return \code{ObsPy} stream object as defined by the architecture of 
#' package 'reticulate'.
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
#' ## load example data set
#' data(rockfall)
#' 
#' ## convert example eseis object to ObsPy stream object
#' x <- aux_eseisobspy(data = rockfall_eseis)
#' 
#' ## filter data set using ObsPy
#' x_filter <- obspy$traces[[1]]$filter(type = "bandpass", 
#'                                      freqmin = 0.5, 
#'                                      freqmax = 1.0)
#'                                      
#' ## plot filtered trace using ObsPy plotting routine
#' x$traces[[1]]$plot()
#' 
#' }
#' 
#' @export aux_eseisobspy
#'  
aux_eseisobspy <- function(
  
  data
  
) {
  
  ## check if input data is obspy stream object
  if(class(data)[1] != "eseis") {
    
    stop("Input object seems to be no or not a single eseis object!")
  }
  
  ## if needed, link to ObsPy library
  if(exists("obspy")[1] == FALSE) {
    
    obspy <- try(reticulate::import("obspy"), silent = TRUE)
    
  } else if(class(obspy)[1] != "python.builtin.module") {
    
    obspy <- try(reticulate::import("obspy"), silent = TRUE)
  }
  
  ## check if ObsPy library could be loaded
  if(class(obspy)[1] == "try-error") {
    
    stop(paste0("Obspy library could not be loaded! Try manually with:",
                " obspy <- reticulate::import('obspy')"))
  }
  
  ## link to numpy library
  numpy <- reticulate::import("numpy")
  
  ## import dummy data set as ObsPy stream object
  data_out <- obspy$read(
    pathname_or_url = paste0(system.file("extdata", package="eseis"), "/",
                             "2017/099/RUEG1.17.99.00.00.00.BHZ.SAC"))
  
  ## adjust signal vector
  data_out$traces[[1]]$data <- numpy$array(data$signal)
  
  ## store original digits option
  option_old <- options()$digits.secs

  ## set options to six digits  
  options(digits.secs = 6)
  
  ## extract data set start and end time
  t_start <- data$meta$starttime
  t_end <- t_start + data$meta$n * data$meta$dt
  
  ## convert POSIXct time to seconds with 6 digits
  t_start_sec <- strsplit(x = format(x = t_start, 
                                     format = "%OS", 
                                     tz = "UTC"), 
                          split = ".", 
                          fixed = TRUE)[[1]][2]
  
  t_end_sec <- strsplit(x = format(x = t_end, 
                                   format = "%OS", 
                                   tz = "UTC"), 
                        split = ".", 
                        fixed = TRUE)[[1]][2]
  
  ## restore digits option  
  options(digits.secs = option_old)
  
  ## convert POSIXct time to ObsPy time format
  t_start_obspy <- paste0(format(x = t_start, 
                          format = "%Y-%m-%dT%H:%M:%S", 
                          tz = "UTC"), ".", t_start_sec, "Z")
  
  t_end_obspy <- paste0(format(x = t_end, 
                        format = "%Y-%m-%dT%H:%M:%S", 
                        tz = "UTC"), ".", t_end_sec, "Z")
  
  ## adjust meta data
  data_out$traces[[1]]$stats$network <- trimws(data$meta$network)
  data_out$traces[[1]]$stats$station <- trimws(data$meta$station)
  data_out$traces[[1]]$stats$location <- ""
  data_out$traces[[1]]$stats$channel <- trimws(data$meta$component)
  data_out$traces[[1]]$stats$starttime <- t_start_obspy
  data_out$traces[[1]]$stats$sampling_rate <- 1/data$meta$dt
  data_out$traces[[1]]$stats$delta <- data$meta$dt
  data_out$traces[[1]]$stats$npts <- data$meta$n
  data_out$traces[[1]]$stats$calib <- "1.0"
  
  ## adjust format type
  data_out$traces[[1]]$stats$"_format" <- "NA"
  
  ## adjust file header information
  data_out$traces[[1]]$stats$sac <- "NA"
  
  ## return output data set
  return(data_out)
}