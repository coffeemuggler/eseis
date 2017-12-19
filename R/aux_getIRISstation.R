#' Query IRIS data base for stations
#' 
#' This function queries the IRIS data base for seismic stations that match
#' a set of criteria for seismic data. The criteria include signal time stamp 
#' and location, component and a search radius. The returned SNCL strings can 
#' be used to download data using the function \code{aux_getIRISdata}.
#' 
#' The function makes use of the package  IRISSeismic. It requires a working 
#' internet connection to perform the query.
#'
#' @param start \code{POSIXct} value, start time of the data to query. 
#' 
#' @param duration \code{Numeric} value, length of the data to query, in 
#' seconds.
#' 
#' @param location \code{Numeric} vector of length two, coordinates of the 
#' seismic source, in decimal degrees (i.e., latitude and longitude).
#' 
#' @param radius \code{Numeric} value, search radius for the query, in 
#' decimal degrees. Default is \code{10} (about 1100 km).
#' 
#' @param component \code{Character} value, signal component to check for. 
#' One out of \code{"BHE"}, \code{"BHN"} and \code{"BHZ"}. Currently, only  
#' one component can be defined per search. Default is \code{"BHZ"}.
#' 
#' @param ID_iris \code{Character} value, IRIS ID. Default is 
#' \code{"IrisClient"}.
#' 
#' @return \code{Data frame} with query results. The data frame contains 
#' information for all seismic stations fulfilling the defined criteria.
#' 
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' \dontrun{
#' 
#' x <- aux_getIRISstation(start = as.POSIXct("2010-01-01 22:22:22", 
#'                          tz = "UTC"), 
#'                          duration = 3 * 3600, 
#'                          location = c(53, 13), 
#'                          radius = 1, 
#'                          component = "BHZ")
#' }
#'                      
#' @export aux_getIRISstation
#' 
aux_getIRISstation <- function (
  
  start, 
  duration, 
  location, 
  radius = 10, 
  component = "BHZ",
  ID_iris = "IrisClient"
) {
  
  ## generate new IRIS client ID
  iris <- new(ID_iris)
  
  ## query data base for networks
  networks <- IRISSeismic::getNetwork(obj = iris,
                                      network = "*", 
                                      station = "*", 
                                      location = "*", 
                                      channel = component, 
                                      starttime = start, 
                                      endtime = start + duration,
                                      lat = location[1],
                                      long = location[2],
                                      maxradius = radius)

  ## identify suitable stations within networks  
  stations <- lapply(X = networks$network, 
                     FUN = function(X, 
                                    obj, 
                                    component, 
                                    start, 
                                    duration, 
                                    location, 
                                    radius) {
                       
                       IRISSeismic::getStation(obj = obj,
                                               network = X,
                                               station = "*",
                                               location = "*",
                                               channel = component,
                                               starttime = start,
                                               endtime = start + duration,
                                               lat = location[1],
                                               long = location[2],
                                               maxradius = radius)
                     },
                     obj = iris,
                     component = component,
                     start = start,
                     duration = duration,
                     location = location,
                     radius = radius)
  
  ## convert list to data frame
  stations <- do.call(rbind, stations)
  
  ## generate SNCL string
  sncl <- paste(stations$network, 
                stations$station, 
                "", 
                component, 
                sep = ".")

  ## calculate distance from source coordinates
  distance <- sqrt((location[1] - stations$latitude)^2 + 
                 (location[2] - stations$longitude)^2)
  
  ## append SNCL and distance to stations data set
  stations <- cbind(stations, sncl, distance)
  
  ## return result
  return(stations)
}