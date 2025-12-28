#' Query FDSN data bases for station information
#' 
#' The function queries as series of data bases for seismic stations that 
#' match a set of criteria for seismic data. Criteria include time interval,
#' location, and component. The returned data can be used to download data 
#' using the function \code{aux_FDSNdata}.
#' 
#' @details
#' The function is a wrapper for FDSN Web Services, explicitly making 
#' fdsnws-station available to R. The XML query and wrangling part of the 
#' function is based on handy suggestions by Copilot.
#' 
#' Definition of the search area can be done either by perimeter around a 
#' central coordinate or rectangular bounding box. In the latter case, make 
#' sure to notice that the box coordinates are not provided as min and max
#' latitude, followed by longitude, but the other way around, implying the 
#' definition of xmin, xmax, ymin, ymax coordinates. All coordinates are 
#' considered to be in decimal degree units.
#' 
#' The function uses the following FDSN data bases by default (i.e., without 
#' explicitly providing any URLs manually): 
#' 
#' \itemize{
#'   \item \code{orfeus} \code{"http://www.orfeus-eu.org"}
#'   \item \code{geofon} \code{"http://geofon.gfz-potsdam.de/"}
#'   \item \code{bgr} \code{"http://eida.bgr.de"}
#'   \item \code{sss} \code{"http://eida.ethz.ch"}
#' }
#' 
#' Other FDSN data base addresses can be provided in the same format as the 
#' addresses in the above list. They need to be provided as character 
#' vector. For lists of addresses see 
#' \code{"http://www.fdsn.org/webservices/datacenters/"},
#' \code{"https://www.orfeus-eu.org/data/eida/webservices/"} and 
#' \code{"http://docs.obspy.org/packages/obspy.clients.fdsn.html#module-obspy.clients.fdsn"}.
#' 
#' Empty query returns will be indicated on the screen 
#' (\code{Document is empty}). If no data at all matches the query, an 
#' empty data frame is returend. 
#'
#' @param centre \code{Numeric} vector of length two, center coordinates 
#' of the location to search data for (\code{c(latitude, longitude)}). 
#' Units must be decimal degrees.
#' 
#' @param radius \code{Numeric} value, radius within which to search for 
#' seismic stations. Unit must be decimal degrees.
#' 
#' @param bbox \code{Numeric} vector of length four, bounding box for the 
#' rectangular search area, containing the four limiting values for 
#' \code{xmin, xmax, ymin, ymax}. Note that here, the order is not latitude 
#' and longitude, but x-values and y-values. The argument is only used if no
#' information is given for \code{centre} and \code{radius}.
#' 
#' @param start \code{POSIXct} value, start time of the data to query. If 
#' omitted, time limits are not applied.
#' 
#' @param stop \code{POSICct} value, stop time of the data to query. If 
#' omitted, time limits are not applied.
#' 
#' @param url \code{Character} vector, optional other FDSN base web 
#' addresses to search for stations. See details for default addresses and 
#' their format.
#' 
#' @param level \code{Character} value, keyword defining at which level the 
#' information about the stations of interest will be collected. Either 
#' \code{"station"} (identification at station level) or \code{"channel"} (
#' identification at channel level). Instead of \code{"channel"}, one can also 
#' use the keyword \code{"component"}. Note that any channel exclusion options 
#' require to use \code{"channel"}. See details for further information. 
#' Default is \code{"station"}.
#' 
#' @param component \code{Character} vector, keyword(s) indicating the spatial 
#' component (or channel) of the data to retrieve. Wildcards can be defined as
#' asterisk (\code{"*"}) or period (\code{"."}), e.g. \code{"*H."}. If more 
#' than one value is provided, the sequence of expressions is used like a 
#' logical OR-case, e.g \code{c("BHE", "BHN")} for both horizontal components. 
#' 
#' @return \code{Data frame} with query results. The data frame contains 
#' information for all seismic stations fulfilling the defined criteria. 
#' Information provided at channel level will only be gathered when the 
#' argument \code{level} is set accordingly. The specific URL that leads to 
#' the data for later download, is also appended to the data frame.
#' 
#' @author Michael Dietze
#' 
#' @seealso aux_get_FDSNdata, aux_organisefdsnfiles
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## get all stations within 1 degree perimeter around Piz Chengalo
#' x <- aux_getFDSNstation(centre = c(46.3, 9.6), radius = 1)
#' 
#' ## sort the stations by distance
#' x <- x[order(x$distance),]
#' 
#' ## plot distance
#' plot(1:nrow(x), x$distance)
#' 
#' ## get all stations within bounding box
#' x <- aux_getFDSNstation(bbox = c(9, 11, 45, 47))
#' 
#' ## get all stations in operation during the Piz Chengalo failure
#' x <- aux_getFDSNstation(bbox = c(9, 11, 45, 47), start = "2017-09-23")
#' range(x$stop)
#' 
#' ## manually define FDSN URL
#' x <- aux_getFDSNstation(bbox = c(9, 11, 45, 47), 
#'                         url = "http://service.iris.edu")
#' 
## query all stations at channel (component) level
#' x <- aux_getFDSNstation(bbox = c(9, 10, 45, 46), 
#'                         level = "component", 
#'                         component = c("BH*", "HH*"))
#' 
#' }
#'                      
#' @export aux_getFDSNstation
#' 
aux_getFDSNstation <- function (
    
  centre, 
  radius,
  bbox,
  start, 
  stop,
  url,
  level = "station",
  component
  
) {
  
  ## Part 0 - Checks and preparations -----------------------------------------
  
  ## check/set spatial limits
  if(missing(bbox) & (missing(centre) | missing(radius))) {
    stop("Neither centre and raduis nor bbox defined!")
  }
  
  if(missing(centre) == FALSE & missing(radius) & missing(bbox)) {
    stop("No search radius or bbox defined!")
  }
  
  if(missing(bbox) == FALSE) {
    if(bbox[1] > bbox[2] | bbox[3] > bbox[4]) {
      stop("Order of bbox values according to xmin, xmax, ymin, ymax!")
    }
  }
  
  ## assign search option
  if(missing(centre) == FALSE) {
    opt_search <- "circular"
  } else {
    opt_search <- "rectangular"
    centre <- c(mean(bbox[3:4]), mean(bbox[1:2]))
  }
  
  ## check/set time limits
  if(missing(start)) {
    start <- as.POSIXct("1000-01-01", tz = "UTC")
  }
  
  if(missing(stop)) {
    stop <- as.POSIXct("5000-01-01", tz = "UTC")
  }
  
  if(class(start)[1] != "POSIXct"){
    
    start <- try(as.POSIXct(start, tz = "UTC"))
    
    if(class(start)[1] != "POSIXct") {
      
      stop("Cannot convert start time to POSIXct!")
    }
  }
  
  if(class(stop)[1] != "POSIXct"){
    
    stop <- try(as.POSIXct(stop, tz = "UTC"))
    
    if(class(stop)[1] != "POSIXct") {
      
      stop("Cannot convert stop time to POSIXct!")
    }
  }

  if(start >= stop) {
    
    stop("Time interval is negative!")
  }
  
  ## check or build link list
  if(missing(url) == TRUE) {
    
    url <- c("http://service.iris.edu",
             "http://eida.bgr.de",
             "http://eida.ethz.ch",
             "http://geofon.gfz-potsdam.de")
  } else {
    
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
  }
  
  ## check level keyword
  if(level == "channel") {
    level <- "component"
  }
  
  if(level %in% c("station", "component") == FALSE) {
    stop("Keyword for level not supported!")
  }
  
  ## check/set component keyword
  if(missing(component) == FALSE) {
    
    ## check if level keyword is correct
    if(level != "component") {
      
      ## otherwise, set level to component
      level <- "component"
      warning("Keyword for level adjusted due to use of component keyword!")
    }
  } else {
    
    ## set wildcard for all components to keep
    component <- "..."
  }
  
  ## define keyword for url query
  if(level == "station") {
    level_query <- "station"
  } else {
    level_query <- "channel"
  }
  
  ## convert radial coordinates to rectangular ones for initial search
  if(opt_search == "circular") {
    
    location <- c(centre[1] - radius,
                  centre[1] + radius,
                  centre[2] - radius,
                  centre[2] + radius)
  } else {
    
    location <- c(bbox[3:4], bbox[1:2])
  }
  
  ## Part 1 - Retrieval and processing of the data ----------------------------

  ## create output data set
  D <- vector(mode = "list", length = length(url))
  
  ## create dummy NA-output data set
  D_empty <- data.frame(network = NA,
                        station = NA,
                        latitude = NA,
                        longitude = NA,
                        elevation = NA,
                        depth = NA,
                        start = NA,
                        stop = NA,
                        component = NA,
                        azimuth = NA,
                        dip = NA,
                        url = NA)
  
  ## collect data from all catalogues
  for(i in 1:length(url)) {
    
    ## build query url
    link_service <- paste(url[i],
                          "/fdsnws/station/1/query?",
                          "minlatitude=",
                          location[1],
                          "&maxlatitude=",
                          location[2],
                          "&minlongitude=",
                          location[3],
                          "&maxlongitude=",
                          location[4],
                          "&level=",
                          level_query,
                          sep = "")
    
    ## parse XML file of catalogue
    catalogue <- try(XML::xmlTreeParse(link_service, useInternalNodes = TRUE), 
                     silent = TRUE)
    
    ## proceed if catalogue is not empty
    if(inherits(catalogue, "try-error") == FALSE) {
      
      ## get root node of data set
      catalogue_rootnode <- XML::xmlRoot(catalogue)
      
      ## extract namespace prefixes
      namespace <- XML::xmlNamespaceDefinitions(catalogue_rootnode, 
                                                recursive = TRUE)
      namespace <- sapply(namespace, function(namespace) namespace$uri)
      
      ## extract station or channel information
      if(level == "station") {
        
        ## Option A - station level information extraction --------------------
        
        ## assign a keyword to the default namespace a name so XML can use it
        if("" %in% names(namespace)) {
          names(namespace)[names(namespace) == ""] <- "sta"
        }
        
        ## get all node sets from data set
        stations <- XML::getNodeSet(doc = catalogue, 
                                    path = "//sta:Station", 
                                    namespaces = namespace)
        
        ## check if any stations are returned and if so, proceed
        if(length(stations) > 0) {
          
          D[[i]] <- do.call(rbind, lapply(stations, function(x) {
            
            ## get network code
            network = XML::xmlGetAttr(XML::xmlParent(x), "code")
            
            ## get station ID
            station = XML::xmlGetAttr(x, "code")
            
            ## get latitude
            latitude = as.numeric(XML::xpathSApply(doc = x, 
                                                   path = "sta:Latitude",  
                                                   XML::xmlValue, 
                                                   namespaces = namespace))
            
            ## get longitude
            longitude = as.numeric(XML::xpathSApply(doc = x, 
                                                    path = "sta:Longitude", 
                                                    XML::xmlValue, 
                                                    namespaces = namespace))
            
            ## get elevation
            elevation = as.numeric(XML::xpathSApply(doc = x, 
                                                    path = "sta:Elevation", 
                                                    XML::xmlValue, 
                                                    namespaces = namespace))
            
            ## get start time
            starttime = XML::xmlGetAttr(x, "startDate", default = NA)
            
            ## get stop time
            stoptime = XML::xmlGetAttr(x, "endDate", default = NA)
            
            
            ## organise information as data frame
            info <- data.frame(network = network,
                               station = station,
                               latitude = latitude,
                               longitude = longitude,
                               elevation = elevation,
                               depth = NA,
                               start = starttime,
                               stop = stoptime,
                               component = NA,
                               azimuth = NA,
                               dip = NA,
                               url = url[i])
            
            ## return output
            return(info)
          }))
        } else {
          
          ## build NA data set in case of no identified data sets
          D[[i]] <- D_empty
        }
      } else {
        
        ## Option B - channel level information extraction --------------------
        
        ## assign a keyword to the default namespace a name so XML can use it
        if("" %in% names(namespace)) {
          names(namespace)[names(namespace) == ""] <- "sta"
        }
        
        ## get all node sets from data set
        channels <- XML::getNodeSet(doc = catalogue, 
                                    path = "//sta:Channel", 
                                    namespaces = namespace)
        
        ## check if any stations are returned and if so, proceed
        if(length(channels) > 0) {
          
          D[[i]] <- do.call(rbind, lapply(channels, function(x) {
            
            ## get network code
            network <- XML::xmlGetAttr(XML::xmlParent(XML::xmlParent(x)), 
                                        "code", default = NA)
            ## get station ID
            station <- XML::xmlGetAttr(XML::xmlParent(x), "code", default = NA)
            
            ## get component
            cmp <- XML::xmlGetAttr(x, "code", default = NA)
            
            ## get latitude
            latitude  = as.numeric(XML::xpathSApply(x, "sta:Latitude",  
                                                    XML::xmlValue, 
                                                    namespaces = namespace))
            
            ## get longitude
            longitude  = as.numeric(XML::xpathSApply(x, "sta:Longitude",  
                                                     XML::xmlValue, 
                                                    namespaces = namespace))
            
            ## get elevation
            elevation  = as.numeric(XML::xpathSApply(x, "sta:Elevation",  
                                                     XML::xmlValue, 
                                                    namespaces = namespace))
            
            ## get depth
            depth  = as.numeric(XML::xpathSApply(x, "sta:Depth",  
                                                 XML::xmlValue, 
                                                    namespaces = namespace))
            
            ## get azimuth
            azimuth  = as.numeric(XML::xpathSApply(x, "sta:Azimuth",  
                                                   XML::xmlValue, 
                                                    namespaces = namespace))

            ## get dip
            dip  = as.numeric(XML::xpathSApply(x, "sta:Dip",  
                                               XML::xmlValue, 
                                                    namespaces = namespace))


            ## get start time
            starttime = XML::xmlGetAttr(XML::xmlParent(x), 
                                        "startDate", 
                                        default = NA)
            
            ## get stop time
            stoptime = XML::xmlGetAttr(XML::xmlParent(x), 
                                       "endDate",
                                       default = NA)
            
            ## organise information as data frame
            info <- data.frame(network = network,
                               station = station,
                               latitude = latitude,
                               longitude = longitude,
                               elevation = elevation,
                               depth = depth,
                               start = starttime,
                               stop = stoptime,
                               component = cmp,
                               azimuth = azimuth,
                               dip = dip,
                               url = url[i])
            
            ## return output
            return(info)
          }))
        } else {
          
          ## build NA data set in case of no identified data sets
          D[[i]] <- D_empty
        }
        
      }
      
    } else {
      D[[i]] <- D_empty
    }
  }
  
  ## merge data from all catalogues
  D <- do.call(rbind, D)
  
  ## remove potential empty or NA-only entries
  i_ok <- apply(X = D, MARGIN = 1, FUN = function(x){
    sum(is.na(x)) < length(x)
  })
  D <- D[i_ok,]
  
  ## Part 2 - Postprocessing of information -----------------------------------
  
  ## check that result contains any data
  if(nrow(D) > 0) {
    
    ## homogenise date format in time stamps
    D$start <- gsub(x = D$start, pattern = "Z", replacement = "")
    D$stop <- gsub(x = D$stop, pattern = "Z", replacement = "")
    D$start <- gsub(x = D$start, pattern = "T", replacement = " ")
    D$stop <- gsub(x = D$stop, pattern = "T", replacement = " ")
    
    
    ## convert time stamps to POSIX format
    D$start <- as.POSIXct(x = D$start, 
                          format = "%Y-%m-%d %H:%M:%S", 
                          tz = "UTC")
    D$stop <- as.POSIXct(x = D$stop, 
                         format = "%Y-%m-%d %H:%M:%S", 
                         tz = "UTC")
    
    ## remove NA start cases and set NA stop cases to far future 
    D <- D[!is.na(D$start),]
    D$stop[is.na(D$stop)] <- as.POSIXct("5000-01-01", tz = "UTC")
    
    ## append distance to data set
    D$distance <- sqrt((centre[1] - D$latitude)^2 + 
                         (centre[2] - D$longitude)^2)
    
    ## Part 3 - Application of rejection criteria -----------------------------
    
    ## keep stations that did not stop before desired start time
    D <- D[D$stop > start,]
    
    ## remove stations outside search radius
    if(opt_search == "circular") {
      
      D <- D[D$distance <= radius,]
    } else {
      
      D <- D[D$longitude >= bbox[1] & D$longitude <= bbox[2] &
               D$latitude >= bbox[3] & D$latitude <= bbox[4],]
    }

    ## keep only stations with desired components
    if(level == "component") {
      
      i_ok <- apply(X = t(component), MARGIN = 2, FUN = function(x, y) {
        
        grepl(x = D$component, pattern = x)
      }, D)
      
      i_ok <- apply(X = i_ok, MARGIN = 1, FUN = function(x) {any(x > 0)})
      
      D <- D[i_ok,]

    } else {
      D$component <- NA
    }
    
    ## remove duplicate entries, based on network, station, component
    dpl_index <- paste(D$network, D$station, D$component, sep = "_")
    D <- D[!duplicated(x = dpl_index),]
    
  }
  
  ## return output
  return(D)
}
