#' Download seismic data from FDSN data base
#' 
#' The function accesses the specified FDSN internet data base(s) and 
#' downloads seismic data based on the network and station IDs and time
#' constraints.
#' 
#' A convenient way to get all the required input data is using the 
#' function \code{aux_getFDSNstation} before. It will return all the 
#' information in a structured way.
#' 
#' It is possible to use the function to process more than one data set. In 
#' this case, the arguments \code{network}, \code{station} and \code{url} 
#' must match pairwise. The arguments \code{start}, \code{duration} and 
#' \code{channel} will be treated as constants if not also provided as 
#' vectors. 
#' 
#' @param start \code{POSIXct} value, start time of the data to query. 
#' 
#' @param duration \code{Numeric} value, length of the data to query, in 
#' seconds.
#' 
#' @param channel \code{Character} value, seismic channel to get. Default is
#' \code{"BHZ"}.
#' 
#' @param network \code{Character} vector, two-character FDSN network ID.
#' 
#' @param station \code{Character} vector, FDSN station ID.
#' 
#' @param url \code{Character} vector, FDSN URL.
#' 
#' @param link_only \code{Logical} vector, return only FDSN link instead of
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
#' d <- aux_getFDSNdata(start = as.POSIXct(x = "2017-08-23 07:30:00", 
#'                                         tz = "UTC"),
#'                      duration = 180, 
#'                      network = x$network_ID, 
#'                      station = x$station_code, 
#'                      url = x$network_url)
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
#' 
aux_getFDSNdata <- function(
  
  start,
  duration,
  channel = "BHZ",
  network,
  station,
  url,
  link_only = FALSE,
  eseis = TRUE
  
) {
  
  ## check/set arguments
  n <- length(station)
  
  if(length(n) > 1) {
    
    ## check if crucial arguments are of the same length
    if(sd(c(length(network), 
            length(station), 
            length(url))) != 0) {
      
      stop("aux_getIRISdata: input vectors not of the same length!")
    }
    
    ## check/adjust length of other arguments
    start <- rep(x = start,
                 length.out = n)
    
    duration <- rep(x = duration, 
                    length.out = n)
    
    channel <- rep(x = channel, 
                   length.out = n)
    eseis <- rep(x = eseis, 
                 length.out = n)
  }
  
  ## build input argument data frame
  input_dataframe <- data.frame(start = start,
                                duration = duration,
                                channel = channel,
                                network = network,
                                station = station,
                                url = url,
                                eseis = eseis, 
                                stringsAsFactors = FALSE)
  
  ## convert data frame to list
  input_list <- vector(mode = "list", 
                       length = nrow(input_dataframe))
  
  for(i in 1:length(input_list)) {
    
    input_list[[i]] <- input_dataframe[i,]
  }
  
  if(link_only == TRUE) {
    
    data_out <- lapply(X = input_list, FUN = function(input) {
      
      ## generate FDSN link
      link_data <- paste(input$url,
                         "/fdsnws/dataselect/1/query?",
                         "net=", 
                         input$network,
                         "&sta=",
                         input$station,
                         "&channel=",
                         input$channel,
                         "&starttime=",
                         format(x = input$start, 
                                format = "%Y-%m-%dT%H:%M:%S", 
                                tz = attr(x = input$start, 
                                          which = "tzone")),
                         "&endtime=",
                         format(x = input$start + input$duration, 
                                format = "%Y-%m-%dT%H:%M:%S", 
                                tz = attr(x = input$start, 
                                          which = "tzone")), 
                         sep = "")
      })
  } else {
    
    data_out <- lapply(X = input_list, FUN = function(input) {
      
      ## generate FDSN link
      link_data <- paste(input$url,
                         "/fdsnws/dataselect/1/query?",
                         "net=", 
                         input$network,
                         "&sta=",
                         input$station,
                         "&channel=",
                         input$channel,
                         "&starttime=",
                         format(x = input$start, 
                                format = "%Y-%m-%dT%H:%M:%S", 
                                tz = attr(x = input$start, 
                                          which = "tzone")),
                         "&endtime=",
                         format(x = input$start + input$duration, 
                                format = "%Y-%m-%dT%H:%M:%S", 
                                tz = attr(x = input$start, 
                                          which = "tzone")), 
                         sep = "")
      
      ## generate station meta data link
      link_meta <- paste(input$url,
                         "/fdsnws/station/1/query?",
                         "net=", 
                         input$network,
                         "&sta=",
                         input$station,
                         "&channel=",
                         input$channel,
                         sep = "")
      
      ## get station meta data
      s_meta <- try(XML::xmlToList(XML::xmlParse(link_meta, 
                                                 error = ""))[-(1:3)], 
                    silent = TRUE)
      
      ## create temporary download file name
      mseed_temp <- paste("temp_mseed_", 
                          round(x = runif(n = 1, 
                                          min = 10000000, 
                                          max = 99999999), 
                                digits = 0),
                          sep = "")
      
      ## download files in temporary directory
      dump <- invisible(try(download.file(url = link_data, 
                                          destfile = mseed_temp), 
                            silent = TRUE))
      
      ## read mseed file
      s <- try(eseis::read_mseed(file = mseed_temp, 
                                 eseis = eseis), 
               silent = TRUE)
      
      ## remove temporary mseed file
      dump <- invisible(try(file.remove(mseed_temp),
                            silent = TRUE))
      
      ## fill meta information and return data
      if(class(s)[1] != "try-error") {
        
        s$meta$latitude <- as.numeric(s_meta$Network$Station$Latitude)
        s$meta$longitude <- as.numeric(s_meta$Network$Station$Longitude)
        s$meta$elevation <- as.numeric(s_meta$Network$Station$Elevation)
        
        return(s)
      }
    
    })
    
    ## assign data set names
    names(data_out) <- paste(input_dataframe$network, 
                             input_dataframe$station, 
                             sep = "-")
  }
  
  ## return output
  return(data_out)
}