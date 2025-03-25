#' Download and import seismic data from an FDSN service provider
#' 
#' The function implements download and import of seismic data from FDSN data
#' providers via the fdsnws-dataselect service (see 
#' \code{https://www.fdsn.org/webservices/}). It is basically a wrapper for 
#' the query approach. 
#' 
#' The FDSN (International Federation of Digital Seismograph Networks) 
#' provides access to a large number of seismic stations worldwide. The data 
#' are organised by network, station, component and further arguments. In 
#' order to use the eseis function \code{read_fdsn}, one must know at least 
#' the former three criteria for the data of interest. A list of networks is 
#' available here: \code{https://www.fdsn.org/networks/}. The function expects 
#' the 2-digit network code, the 3- or 4-digit station code, a single seismic 
#' component ID, and the URL to the data archive. Additional query arguments 
#' can be added (and must be added to point at a single seismic trace to 
#' download and import). A complete list of query arguments is available 
#' here: \url{https://www.fdsn.org/webservices/fdsnws-dataselect-1.1.pdf}.
#' 
#' For each network listed there, one can find the URL that gives access to 
#' the data (if existing) under "Data Access". Note that the function only 
#' requires the first URL part, e.g., \code{https://geofon.gfz-potsdam.de}. 
#' 
#' @param start \code{POSIXct} value, start time of the data to import. If
#' lazy users only submit a text string instead of a POSIXct object, the 
#' function will try to convert that text string, assuming UTC as time zone. 
#' 
#' @param duration \code{Numeric} value, duration of the data to import,
#' in seconds.
#' 
#' @param station \code{Character} vector, optional 3-4-digit FDSN station ID.
#' 
#' @param network \code{Character} vector, optional 2-digit FDSN network ID. 
#' 
#' @param component \code{Character} vector, seismic component to search for. 
#' If omitted, the function will look for \code{"BHZ"} by default.
#' 
#' @param url \code{Chracter} vector, URL of the FDSN data provider. Should be 
#' of the form \code{"http://service.iris.edu"}, i.e., without further URL 
#' parts. URLs can be submitted as a vector. If omitted, the function will 
#' look in the two most comprehensive providers, i.e. 
#' \code{url = c("http://service.iris.edu", "http://eida-federator.ethz.ch")}. 
#' See details for further information.
#' 
#' @param eseis \code{Logical} value, option to read data to an \code{eseis}
#' object (recommended, see documentation of 
#' \code{aux_initiateeseis}), default is \code{TRUE}
#' 
#' @param \dots Additional query arguments sent to the FDSN data provider. See
#' details for available argument names and conventions.
#' 
#' @return An \code{eseis} object or a \code{list} with the time  
#' (\code{$time}) and \code{$signal} vectors as well as meta information.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## read and plot 10 min of data from Ecuador, specifying the component
#' s <- read_fdsn(start = "2020-05-16 22:42:00",
#'                duration = 360, 
#'                station = "IMBA", 
#'                network = "EC", 
#'                component = "HHZ")
#' plot(s)
#' 
#' ## read and plot 10 min of data from Germany, specifying the URL
#' s <- read_fdsn(start = "2017-03-21 04:38:00",
#'                duration = 360, 
#'                station = "RGN", 
#'                network = "GE", 
#'                url = "https://geofon.gfz-potsdam.de")
#' plot(s)
#' 
#' }
#'                      
#' @export read_fdsn

read_fdsn <- function(
    
  start,
  duration,
  station,
  network,
  component = "BHZ",
  url,
  eseis = TRUE,
  ...
  
){
  
  ## check start time format
  if(class(start)[1] != "POSIXct") {
    
    start <- try(as.POSIXct(start, tz = "UTC"), silent = TRUE)
    
    if(class(start)[1] != "POSIXct") {
      
      stop("Start date is not a POSIXct format!")
    }
  }
  
  ## define end time based on duration
  stop <- start + duration
  
  ## extract further arguments
  dots <- list(...)  
  
  ## check/set location argument
  if ("location" %in% names(dots)) {
    
    location <- paste0("location=", dots$location, "&")
  }
  else {
    
    location <- ""
  }
  
  ## check/set quality argument
  if ("quality" %in% names(dots)) {
    
    quality <- paste0("quality=", dots$quality, "&")
  }
  else {
    
    quality <- ""
  }
  
  ## check/set minimumlength argument
  if ("minimumlength" %in% names(dots)) {
    
    minimumlength <- paste0("minimumlength=", dots$minimumlength, "&")
  }
  else {
    
    minimumlength <- ""
  }
  
  ## check/set longestonly argument
  if ("longestonly" %in% names(dots)) {
    
    longestonly <- paste0("longestonly=", dots$longestonly, "&")
  }
  else {
    
    longestonly <- ""
  }
  
  ## check/set urls
  if(missing(url) == TRUE) {
    
    url <- c("http://service.iris.edu", "http://eida-federator.ethz.ch")
  }
  
  ## make parameter list
  pars <- list(network = network, 
               station = station, 
               start = start, 
               stop = stop, 
               channel = component, 
               location = location,
               quality = quality,
               minimumlength = minimumlength,
               longestonly = longestonly)
  
  ## build query url
  s  <- lapply(url, FUN = function(url, pars) {
    
    ## optionally remove superfluent URL parts
    url_get <- gsub(x = url, 
                    pattern = "/fdsnws/dataselect/1", 
                    replacement = "")
    
    ## optionally remove superfluent URL parts
    url_get <- gsub(x = url, 
                    pattern = "/fdsnws", 
                    replacement = "")
    
    ## optionally remove superfluent URL parts
    url_get <- gsub(x = url, 
                    pattern = "//", 
                    replacement = "")
    
    ## build query URL string
    url_get <- paste0(url[1], "/fdsnws/dataselect/1/query?",
                      "network=", network, "&",
                      "station=", station, "&",
                      location, quality, minimumlength, longestonly,
                      "starttime=", format(start, "%Y-%m-%dT%H:%M:%S"), "&",
                      "endtime=", format(stop, "%Y-%m-%dT%H:%M:%S"), "&",
                      "channel=", component, "&")
    
    ## create temporary file name
    f_temp <- paste0(tempdir(), "/", 
                     "temp_eseis_", 
                     format(Sys.time(), "%Y-%m-%d-%H-%M-%S_"), 
                     round(x = runif(n = 1, min = 1000000, max = 9999999), 
                           digits = 0), 
                     ".mseed")
    
    ## download file
    s_tmp <- try(utils::download.file(url = url_get, quiet = TRUE, 
                                      destfile = f_temp), 
                 silent = TRUE)
    
    ## read downloaded file
    s <- suppressWarnings(try(eseis::read_mseed(file = f_temp), 
                              silent = TRUE))
    
    ## remove temporary file
    try(if(file.exists(f_temp) == TRUE) {
      
      invisible(unlink(f_temp))
    }, silent = TRUE)
    
    ## return output
    return(s)
  }, pars)
  
  ## remove empty list elements
  i_ok <- do.call(c, lapply(X = s, FUN = function(s) {
    
    class(s)[1] == "eseis"
  }))
  s <- s[i_ok]
  s <- s[[1]]
  
  ## optionally remove eseis object structure
  if(eseis == FALSE) {
    
    s <- list(signal = s$signal, 
              time = seq(from = s$meta$starttime, 
                         by = s$meta$dt, 
                         length.out = s$meta$n),
              meta = s$meta, 
              header = s$header)
  }
  
  ## return output
  return(s)
}