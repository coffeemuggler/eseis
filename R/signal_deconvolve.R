#' Deconvolve a signal vector.
#' 
#' The function removes the instrument response from a signal vector.
#' 
#' The function requires a set of input parameters, apart from the signal 
#' vector. These parameters are contained in and read from the function 
#' \code{list_sensor()} and \code{list_logger()}. Poles and zeros are used 
#' to build the transfer function. The value s is the generator constant in 
#' Vs/m. The value k is the normalisation factor. AD is the analogue-digital 
#' conversion factor n Volts per count. If the signal was recorded with a gain 
#' value other than 1, the resulting signal needs to be corrected for this, 
#' as well. As of v. 0.8.0, the function also supports deconvolution using 
#' the station XML scheme. However, that feature is implemented through the 
#' python toolbox Obspy, which needs to be installed separately.
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param xml station XML file to use, either an \code{obspy} object or path 
#' to a \code{XML} file. If provided, all other station parameters will be 
#' ignored and the station XML file will be used. Currently, the station XML
#' approach is only supported through Obspy and when \code{eseis} objects are 
#' provided. In that case, lists of \code{eseis} cannot be supported.
#' 
#' @param use_metadata \code{Logical} value, option to take keywords for 
#' \code{sensor}, \code{logger} and \code{gain} from eseis object meta data
#' element instead of using explicitly provided arguments. Default is 
#' \code{FALSE}.
#' 
#' @param dt \code{Numeric} value, sampling rate. Only needed if \code{data} 
#' is not an \code{eseis} object
#' 
#' @param sensor \code{Character} value or \code{list} object, 
#' seismic sensor name. Must be present in the sensor library 
#' (\code{list_sensor}) or parameters must be added manually (see 
#' examples). Default is \code{"TC120s"}.
#' 
#' @param logger \code{Character} value, seismic logger name. Must be 
#' present in the logger library (\code{list_logger}) or parameters must be
#' added manually. Default is \code{"Cube3extBOB"}.
#' 
#' @param gain \code{Numeric} value, signal gain level of the logger. Default 
#' is \code{1}.
#' 
#' @param xml_use \code{Logical} value, only needed internally, will be set 
#' automatically.
#' 
#' @param url \code{Character} value, URL of the FDSN data provider. Should 
#' be of the form \code{"http://service.iris.edu"}, i.e., without further 
#' URL parts. URLs can be submitted as a vector. See \code{aux_getxml} for 
#' further details. Only needed when XML file is to be downloaded. 
#' 
#' @param p \code{Numeric} value, proportion of signal to be tapered. Default
#' is\code{10^-6}.
#' 
#' @param waterlevel \code{Numeric} value, waterlevel value for frequency
#' division, default is \code{10^-6}.
#' 
#' @param na.replace \code{Logical} value, option to replace NA values in the 
#' data set by zeros. Default is \code{FALSE}. Attention, the zeros will 
#' create artifacts in the deconvolved data set. However, \code{NA} values will 
#' result in no deconvolution at all.
#' 
#' @param verbose \code{Logical} value, option to allow messages and warnings 
#' to be shown. Default is \code{FALSE}.
#' 
#' @return \code{Numeric} vector or list of vectors, deconvolved signal.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## deconvolve signal with minimum effort
#' rockfall_decon <- signal_deconvolve(data = rockfall_eseis)
#' 
#' ## plot time series
#' plot_signal(data = rockfall_decon, 
#'      main = "Rockfall, deconvolved signal", 
#'      ylab = "m/s")
#'  
#' ## add new logger manually
#' logger_new <- list_logger()[[1]]
#' 
#' ## add logger data
#' logger_new$ID <- "logger_new"
#' logger_new$name <- "logger_new"
#' logger_new$AD <- 2.4414e-07
#' 
#' ## deconvolve signal with new logger
#' rockfall_decon <- signal_deconvolve(data = rockfall_eseis,
#'                                     sensor = "TC120s", 
#'                                     logger = logger_new)
#'
#' ## Change the setup of a logger, here: Centaur AD is changed due to 
#' ## other than default Vpp value, according to AD = V / (2^24).
#' 
#' ## extract default Centaur logger
#' Centaur_10V <- list_logger()[[2]]
#' 
#' ## replace AD value
#' Centaur_10V$AD <- 20/(2^24)
#' 
#' \dontrun{
#' 
#' ## working with station XML files:
#' 
#' ## download and import example data set
#' s <- read_fdsn(start = "2023-06-10", 
#'                duration = 600, 
#'                station = "DAVA", 
#'                network = "OE",
#'                component = "BHZ")
#' 
#' ## download and save station XML file
#' xml <- aux_getxml(xml = "OE.DAVA.XML",
#'                   start = "2023-06-10",
#'                   duration = 600,
#'                   network = "OE",
#'                   station = "DAVA",
#'                   component = "BHZ",
#'                   url = "http://service.iris.edu")
#' 
#' ## deconvolve data set with downloaded XML file
#' s_d <- signal_deconvolve(data = s, 
#'                          xml = "OE.DAVA.XML")
#'                          
#' ## alternatively, deconvolve data set by online XML file (no saving)
#' s_d <- signal_deconvolve(data = s, 
#'                          xml = TRUE,
#'                          url = "http://service.iris.edu")
#'                          
#' }
#'
#' @export signal_deconvolve
#' 
signal_deconvolve <- function(
    data,
    xml,
    sensor = "TC120s",
    logger = "Cube3BOB",
    gain = 1,
    use_metadata = FALSE,
    dt,
    url,
    xml_use,
    p = 10^-6,
    waterlevel = 10^-6,
    na.replace = FALSE,
    verbose = FALSE
) {
  
  ## check/set dt
  if(missing(dt) == TRUE) {
    dt <- NULL
  }
  
  ## check/set url
  if(missing(url) == TRUE) {
    url <- NULL
  }
  
  ## check/set xml
  if(missing(xml) == TRUE) {
    xml <- NULL
  }
  
  ## check/set xml argument
  if(is.null(xml) == FALSE) {
    
    ## load obspy
    obspy <- try(reticulate::import("obspy"), silent = TRUE)
    
    ## set xml_use flag
    xml_use <- TRUE
    
    ## case A, xml argument is no Obspy XML object
    if(class(xml)[1] != "obspy.core.inventory.inventory.Inventory") {
      
      ## case A1, try to download xml file to temporary directory
      if(xml == TRUE) {
        
        xml <- eseis::aux_getxml(xml = paste0(tempdir(), "/",
                                              s$meta$network, ".", 
                                              s$meta$station, ".",
                                              s$meta$component, ".XML"), 
                                 start = s$meta$starttime, 
                                 duration = s$meta$n * s$meta$dt, 
                                 network = s$meta$network, 
                                 station = s$meta$station, 
                                 component = s$meta$component,
                                 url = url)
        
      } else if(!is.null(xml)) {
        
        ## case A2, download and save xml file
        xml <- eseis::aux_getxml(xml = xml, 
                                 start = s$meta$starttime, 
                                 duration = s$meta$n * s$meta$dt, 
                                 network = s$meta$network, 
                                 station = s$meta$station, 
                                 component = s$meta$component,
                                 url = url)
        
      } else {
        
        stop("xml must be TRUE or a local XML file!")
        
      }

      ## case B, try to read local xml file
    } else {
      
      xml <- obspy$read_inventory(path_or_file_object = path.expand(xml))
    }
  } else {
    
    xml_use <- FALSE
    xml <- NULL
  }
  
  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = signal_deconvolve, 
                       xml = xml,
                       sensor = sensor,
                       logger = logger,
                       gain = gain,
                       use_metadata = use_metadata,
                       dt = dt,
                       url = url,
                       xml_use = xml_use,
                       p = p,
                       waterlevel = waterlevel,
                       na.replace = na.replace)
    
    ## return output
    return(data_out)
    
  } else {
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "",
                            xml_use = xml_use,
                            xml = xml,
                            dt = dt,
                            sensor = ifelse(test = use_metadata == TRUE,
                                            yes = data$meta$sensor,
                                            no = sensor),
                            logger = ifelse(test = use_metadata == TRUE,
                                            yes = data$meta$logger,
                                            no = logger),
                            gain = ifelse(test = use_metadata == TRUE,
                                          yes = data$meta$gain,
                                          no = gain),
                            p = p,
                            waterlevel = waterlevel)
    
    ## case of keyword-provided sensor characteristics
    if(class(sensor)[1] == "character") {
      
      sensor <- try(list_sensor()[[sensor]], silent = TRUE)
      
      if(class(sensor)[1] == "try-error") {
        stop("Sensor keyword not in library! Consider adding manually.")
      }
      
    } else if(class(sensor)[1] == "list") {
      
      if(length(match(x = names(sensor), 
                      table = names(list_sensor()[[1]]))) != 10) {
        
        stop("Sensor specification incorrect! See examples.")
      }
    } else {
      
      stop("Sensor specification incorrect! See examples.")
    }  
    
    ## assign poles, zeros, sensitivity and normalisation factor
    poles <- sensor$poles
    zeros <- sensor$zeros
    s <- sensor$s
    k <- sensor$k
    
    ## get logger information
    if(class(logger)[1] == "character") {
      
      logger <- try(list_logger()[[logger]], silent = TRUE)
      
      if(class(logger)[1] == "try-error") {
        
        stop("Logger keyword not in library! Consider adding manually.")
      }      
    } else if(class(logger)[1] == "list") {
      
      if(length(match(x = names(logger), 
                      table = names(list_logger()[[1]]))) != 7) {
        
        stop("Logger specification incorrect! See examples.")
      }
    } else {
      
      stop("Logger specification incorrect! See examples.")
    }
    
    ## assign AD ratio
    AD <- logger$AD
    
    ## extract eseis object signal vector
    if(class(data)[1] == "eseis") {
      
      ## set eseis flag
      eseis_class <- TRUE
      
      ## store initial object
      eseis_data <- data
      
      ## extract signal vector
      data <- eseis_data$signal
      
      ## extract dt
      dt <- eseis_data$meta$dt
    } else {
      
      eseis_class <- FALSE
    }
    
    if(eseis_arguments$xml_use == TRUE) {
      
      s_o <- aux_eseisobspy(data = eseis_data)
      
      
      s_o$remove_response(inventory=xml)
      
      data_out <- aux_obspyeseis(data = s_o)$signal
      
    } else {
      
      ## check for non-missing dt
      if(is.null(dt) == TRUE) {
        
        stop("No dt provided.")
      }
      
      ## apply gain correction
      data <- data / gain
      
      ## detrend data set
      data_detrend <- signal_detrend(data = data)
      
      ## remove mean from data set
      data_demean <- signal_demean(data = data_detrend)
      rm(data_detrend)
      
      ## taper data set
      data_taper <- signal_taper(data = data_demean, 
                                 p = p)
      rm(data_demean)
      
      ## add zeros to reach even power of two number
      data_pad <- signal_pad(data = data_taper)
      rm(data_taper)
      
      ## optionally replace NA values by zero
      if(na.replace == TRUE) {
        
        data_pad[is.na(data_pad)] <- 0
      }
      
      ## make frequency vector
      if ((length(data_pad)%%2) == 1) {
        f <- c(seq(0, (length(data_pad) - 1) / 2), 
               seq(-(length(data_pad) - 1) / 2, -1)) / (length(data_pad) * dt)
      } else {
        f = c(seq(0, length(data_pad) / 2), 
              seq(-length(data_pad) / 2 + 1, -1)) / (length(data_pad) * dt)
      }  
      
      ## calculate Fourier transform
      x_fft <- fftw::FFT(x = data_pad)
      
      ## get polynomial form of poles and zeros
      poles_poly <- Re(signal::poly(x = poles))
      zeros_poly <- Re(signal::poly(x = zeros))
      
      ## calculate complex circular frequency vector
      w_comp <- complex(real = 0, imaginary = 1) * (2 * pi * f)
      
      ## calculate transfer function
      h <- outer(w_comp, (length(zeros_poly) - 1):0, "^") %*% zeros_poly / 
        outer(w_comp, (length(poles_poly) - 1):0, "^") %*% poles_poly * k
      rm(w_comp)
      
      ## calculate waterlevel factor
      gamma <- max(Re(h * Conj(h))) * waterlevel
      
      ## deconvolve signal  
      data_decon <- x_fft * Conj(h) / Re(h * Conj(h) + gamma)
      rm(x_fft)
      
      ## invert deconvolved signal
      data_inverse <- Re(fftw::IFFT(x = data_decon, 
                                    inverse = TRUE))
      rm(data_decon)
      
      ## correct for A/D-ratio and sensitivity factor
      data_inverse <- (data_inverse * AD) / s
      
      ## truncate signal to original length
      data_inverse <- data_inverse[1:length(data)]
      
      ## remove mean from output signal
      data_out <- signal_demean(data_inverse)
      rm(data_inverse)
      
      ## remove garbage
      gc()
    }
    
    ## optionally update eseis object
    if(eseis_class == TRUE) {
      
      ## assign aggregated signal vector
      eseis_data$signal <- data_out
      
      ## calculate function call duration
      eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                            time2 = eseis_t_0, 
                                            units = "secs"))
      
      ## update object history
      eseis_data$history[[length(eseis_data$history) + 1]] <- 
        list(time = Sys.time(),
             call = "signal_deconvolve()",
             arguments = eseis_arguments,
             duration = eseis_duration)
      names(eseis_data$history)[length(eseis_data$history)] <- 
        as.character(length(eseis_data$history))
      
      ## assign eseis object to output data set
      data_out <- eseis_data
    }
    
    ## return output signal
    return(data_out)    
  }
}
