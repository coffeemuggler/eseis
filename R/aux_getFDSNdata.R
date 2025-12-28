#' Download seismic data from FDSN data base
#' 
#' The function accesses the specified FDSN internet data base and 
#' downloads seismic data based on the network and station IDs and time
#' constraints.
#' 
#' A convenient way to get all the required input data is using the 
#' function \code{aux_getFDSNstation} before. It will return all the 
#' information in a structured way.
#' 
#' The FDSN web service URL must be provided only with its base path, e.g.
#' \code{"http://service.iris.edu"}.
#' 
#' @param start \code{POSIXct} value, start time of the data to query. 
#' 
#' @param duration \code{Numeric} value, length of the data to query, in 
#' seconds.
#' 
#' @param network \code{Character} value, two-character FDSN network ID.
#' 
#' @param station \code{Character} value, FDSN station ID.
#' 
#' @param component \code{Character} value, seismic component or channel to 
#' get.
#' 
#' @param url \code{Character} value, FDSN URL. See details for further 
#' information.
#' 
#' @param link_only \code{Logical} value, return only FDSN link instead of
#' downloading and importing the data.
#' 
#' @param eseis \code{Logical} scalar, option to read data to an \code{eseis}
#' object (recommended, see documentation of 
#' \code{aux_initiateeseis}), default is \code{TRUE}
#' 
#' @return \code{List} object with imported seismic data for each provided 
#' set of input arguments. 
#' 
#' @author Michael Dietze
#' 
#' @seealso aux_get_FDSNstation, read_mseed
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## get stations < 0.6 degrees away from Piz Chengalo collapse
#' x <- aux_getFDSNstation(centre = c(46.3, 9.6),
#'                         radius = 0.6,
#'                         access = TRUE)
#' 
#' ## sort statiions by distance
#' x <- x[order(x$distance),]
#' 
#' ## download available data
#' d <- aux_getFDSNdata(start = "2017-08-23 07:30:00",
#'                      duration = 180, 
#'                      component = "BHZ",
#'                      network = x$network, 
#'                      station = x$station, 
#'                      url = x$url)
#' 
#' ## remove stations without available data
#' x <- x[!unlist(lapply(d, is.null)),]
#' d <- d[!unlist(lapply(d, is.null))]
#' 
#' ## generate plots of the three nearest stations
#' par(mfcol = c(3, 1))
#' 
#' for(i in 1:3) {
#' 
#'   plot_signal(data = d[[i]],
#'               main = paste(x$ID[i], 
#'                            " | ",
#'                            round(x$distance[i], 2),
#'                            "distance (DD)"))
#' } 
#' }
#'                      
#' @export aux_getFDSNdata

aux_getFDSNdata <- function(
    
  start,
  duration,
  component,
  network,
  station,
  url,
  link_only = FALSE,
  eseis = TRUE
  
) {
  
  ## optionally convert start time to POSIXct value
  if(inherits(x = start, what = "POSIXct") == FALSE){
    
    start <- try(as.POSIXct(start, tz = "UTC"))
    
    if(class(start)[1] != "POSIXct") {
      
      stop("Cannot convert start time to POSIXct!")
    }
  }
  
  ## remove accidentally appended URL parts
  url <- gsub(x = url, pattern = "/fdsnws/station/1/query?", 
              replacement = "")
  url <- gsub(x = url, pattern = "/fdsnws/station/1/", replacement = "")
  url <- gsub(x = url, pattern = "/fdsnws/station/1", replacement = "")
  url <- gsub(x = url, pattern = "/fdsnws/station/", replacement = "")
  url <- gsub(x = url, pattern = "/fdsnws/station", replacement = "")
  url <- gsub(x = url, pattern = "/fdsnws/", replacement = "")
  url <- gsub(x = url, pattern = "/fdsnws", replacement = "")
  
  ## append accidentally missing http:// parts
  url <- ifelse(test = grepl(pattern = "http://", x = url, fixed = TRUE), 
                yes = url, 
                no = paste0("http://", url))
  
  ## generate fdsnws-link
  data <- paste0(url,
                 "/fdsnws/dataselect/1/query?net=", 
                 network, "&sta=",
                 station, "&channel=",
                 component, "&starttime=",
                 format(x = start, 
                        format = "%Y-%m-%dT%H:%M:%S", 
                        tz = attr(x = start,  which = "tzone")), "&endtime=",
                 format(x = start + duration, 
                        format = "%Y-%m-%dT%H:%M:%S", 
                        tz = attr(x = start, which = "tzone")))
  
  ## if desired, download and import the file
  if(link_only == FALSE) {
    
    ## download seed file
    
    ## create temporary download file name
    mseed_temp <- paste0(tempdir(), "/temp_mseed_", 
                         format(Sys.time(), "%y%m%d%H%M%S"),
                         round(runif(1, 1e8, 9e8)))
    
    ## download files in temporary directory
    dump <- invisible(try(download.file(
      url = data, destfile = mseed_temp), silent = TRUE))
    
    ## read mseed file
    data <- try(eseis::read_mseed(file = mseed_temp, eseis = eseis), 
                silent = TRUE)
    
    ## remove temporary mseed file
    dump <- invisible(try(invisible(unlink(mseed_temp, recursive = TRUE)),
                          silent = TRUE))
  }
  
  ## return output (either link or data set)
  return(data)
}
