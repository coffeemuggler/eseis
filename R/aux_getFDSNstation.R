#' Query FDSN data base for stations
#' 
#' This function queries as series of data bases for seismic stations that 
#' match a set of criteria for seismic data. The criteria include signal time 
#' stamp and location, and component. The returned data can be used to 
#' download data using the function \code{aux_FDSNdata}.
#' 
#' The function requires a working internet connection to perform the query.
#' It uses the following FDSN data bases by default: 
#' 
#' \itemize{
#'   \item \code{orfeus} \code{"http://www.orfeus-eu.org"}
#'   \item \code{geofon} \code{"http://geofon.gfz-potsdam.de/"}
#'   \item \code{bgr} \code{"http://eida.bgr.de"}
#'   \item \code{sss} \code{"http://eida.ethz.ch"}
#' }
#' 
#' Other FDSN data base addresses can be provided in the same way as the 
#' addresses in the above list. They need to be provided as character 
#' vector. For a list of addresses see 
#' \code{"http://www.fdsn.org/webservices/datacenters/"} and 
#' \code{"http://docs.obspy.org/packages/obspy.clients.fdsn.html#module-obspy.clients.fdsn"}.
#'
#' @param centre \code{Numeric} vector of length two, center coordinates 
#' of the location to search data for (\code{c(latitude, longitude)}). 
#' Units must be decimal degrees.
#' 
#' @param radius \code{Numeric} value, radius within which to search for 
#' seismic stations. Unit must be decimal degrees.
#' 
#' @param start \code{POSIXct} value, start time of the data to query. If 
#' omitted, stations are queried for the full time available.
#' 
#' @param access \code{Logical} value, access type of the data. If omitted,
#' all data sets are returned, if set \code{TRUE}, only data with access 
#' flag \code{"open"} are returned.
#' 
#' @param url \code{Character} vector, optional other FDSN base web 
#' addresses to search for stations. See details for default addresses and 
#' their format.
#' 
#' @return \code{Data frame} with query results. The data frame contains 
#' information for all seismic stations fulfilling the defined criteria.
#' 
#' @author Michael Dietze
#' 
#' @seealso aux_get_FDSNdata, aux_getIRISstation
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' x <- aux_getFDSNstation(start = as.POSIXct(x = "2010-01-01 22:22:22", 
#'                                            tz = "UTC"), 
#'                         centre = c(45, 10), 
#'                         radius = 1)
#'                            
#' ## optionally plot station locations on a map (requires RgoogleMaps)
#' center <- c(mean(x$station_latitude), 
#'             mean(x$station_longitude))
#' 
#' zoom <- min(RgoogleMaps::MaxZoom(range(x$station_latitude), 
#'                                  range(x$station_longitude)))
#'                                  
#' Map <- RgoogleMaps::GetMap(center = center,
#'                            zoom = zoom, 
#'                            maptype = "terrain")
#'                            
#' RgoogleMaps::PlotOnStaticMap(MyMap = Map, 
#'                              lat = x$station_latitude, 
#'                              lon = x$station_longitude, 
#'                              pch = 15, 
#'                              col = 4)
#' }
#'                      
#' @export aux_getFDSNstation
#' 
aux_getFDSNstation <- function (
  
  centre, 
  radius,
  start, 
  access,
  url
) {
  
  ## convert radial coordinates to rectangular ones
  location <- c(centre[1] - radius,
                centre[1] + radius,
                centre[2] - radius,
                centre[2] + radius)
  
  ## build link list
  if(missing(url) == TRUE) {
    
    url <- c("http://eida.bgr.de",
             "http://eida.ethz.ch",
             "http://www.orfeus-eu.org",
             "http://geofon.gfz-potsdam.de")
  }
  
  ## create output object
  station_data_all <- vector(mode = "list", 
                             length = length(url))
  
  ## get FDSn data for the queries
  for(k in 1:length(station_data_all)) {
    
    link_stations <- paste(url[k],
                                    "/fdsnws/station/1/query?",
                                    "minlatitude=",
                                    location[1],
                                    "&maxlatitude=",
                                    location[2],
                                    "&minlongitude=",
                                    location[3],
                                    "&maxlongitude=",
                                    location[4],
                                    sep = "")
    
    ## get data and convert XML file to data frame
    meta <- try(XML::xmlToList(XML::xmlParse(link_stations, 
                                             error = "")), 
                silent = TRUE)
    
    ## check if query returned any results
    if(class(meta)[1] != "try-error") {
      
      ## remove irrelevant header and tail data
      networks <- meta[-(1:3)]
      networks <- networks[-length(networks)]
      
      ## create temporary output object
      station_data <- vector(mode = "list", 
                             length = length(networks))
      
      ## loop through all detected networks
      for(i in 1:length(networks)) {
        
        ## get number of stations per network
        n <- length(networks[[i]]) - 2
        
        ## define helper variables
        station_name <- character(length = n)
        station_code <- character(length = n)
        station_start <- character(length = n)
        station_access <- logical(length = n)
        station_lat <- numeric(length = n)
        station_lon <- numeric(length = n)
        station_elevation <- numeric(length = n)
        
        ## assign station information
        for(j in 1:n) {
          
          station_name[j] <- networks[[i]][[j + 1]]$Site$Name
          station_code[j] <- networks[[i]][[j + 1]]$.attrs[1]
          station_start[j] <- networks[[i]][[j + 1]]$CreationDate
          station_access[j] <- networks[[i]][[j + 1]]$.attrs[3] == "open"
          station_lat[j] <- networks[[i]][[j + 1]]$Latitude
          station_lon[j] <- networks[[i]][[j + 1]]$Longitude
          station_elevation[j] <- networks[[i]][[j + 1]]$Elevation
        }
        
        ## convert vectors to data frame
        station_data[[i]] <- data.frame(
          network_name = rep(x = networks[[i]]$Description, 
                             times = n),
          network_ID = rep(x = networks[[i]]$.attrs[1], 
                           times = n),
          network_url = rep(x = url[k], 
                           times = n),
          station_name = station_name,
          station_code = station_code,
          station_start = station_start,
          station_access = as.logical(station_access),
          station_latitude = as.numeric(station_lat),
          station_longitude = as.numeric(station_lon),
          station_elevation = as.numeric(station_elevation), 
          stringsAsFactors = FALSE)
        
      }
      
      ## combine data sets of networks
      station_data_all[[k]] <- do.call(rbind, 
                                       station_data)
    }
  }
  
  ## combine all data sets
  station_data_all <- do.call(rbind, 
                              station_data_all) 
  
  ## handle insuccessful query result
  if(is.null(station_data_all) == TRUE) {
    
    print("No stations matching the criteria found.")
    
    return()
  }
  
  ## convert start time to POSIXct values
  station_data_all$station_start <-
    as.POSIXct(x = station_data_all$station_start, 
               format = "%Y-%m-%dT%H:%M:%S", 
               tz = "UTC")
  
  ## create station code for data download
  ID <- paste(station_data_all$network_ID, 
              station_data_all$station_code, 
              sep = "")
  
  ## append ID
  station_data_all <- cbind(ID, 
                            station_data_all)
  
  ## remove duplicates
  station_data_all <- station_data_all[!duplicated(station_data_all$ID),]

  ## calculate distance to centre
  distance <- 
    sqrt((station_data_all$station_latitude - centre[1])^2 + 
           (station_data_all$station_longitude - centre[2])^2)
  
  ## append distance
  station_data_all <- cbind(distance, 
                            station_data_all)

  ## optionally remove stations with inappropriate start time
  if(missing(start) == FALSE) {
    
    station_data_all <-
      station_data_all[station_data_all$station_start <= start,]
  }
  
  ## handle insuccessful query result
  if(nrow(station_data_all) < 1) {
    
    print("No stations passing the time criterium.")
    
    return()
  }

  ## optionally remove stations with inappropriate access option
  if(missing(access) == FALSE) {
    
    station_data_all <-
      station_data_all[station_data_all$station_access == access,]
  }
  
  ## handle insuccessful query result
  if(nrow(station_data_all) < 1) {
    
    print("No stations passing the access type criterium.")
    
    return()
  }
  
  ## remove stations outside search radius
  station_data_all <-
    station_data_all[station_data_all$distance <= radius,]
  
  ## handle insuccessful query result
  if(nrow(station_data_all) < 1) {
    
    print("No stations passing the radius criterium.")
    
    return()
  }
  

  ## return output
  return(station_data_all)
}
