#' Download and/or read station XML file
#' 
#' This function either downloads an online station XML file and (optionally) 
#' saves it and/or reads an already existing local station XML file.
#' 
#' Currently, the function uses Obspy python code. Hence, both python and the 
#' package 'obspy' need to be installed in order to use the function.
#' 
#' @param xml \code{Character} value, local XML file to import. If omitted,
#' the function expects information for the other arguments listed below to
#' download (into a temporary directory) and read the station XML file. In 
#' case both, the below information and a XML path is provided, the downloaded
#' data will be saved under the provided XML file name.
#' 
#' @param start \code{POSIXct} value, start time for which the XML 
#' information shall be collected. If omitted, XML information for all 
#' available time spans will be downloaded.
#' 
#' @param duration \code{Numeric} value, duration until when the XML 
#' information shall be collected. If omitted, XML information for all 
#' available time spans will be downloaded.
#' 
#' @param network \code{Character} value, seismic network to which the station
#' belongs, for which the XML data will be downloaded.
#' 
#' @param station \code{Character} value, seismic station ID for which the 
#' XML data will be downloaded. If omitted, XML information for all stations
#' of the network will be collected.
#' 
#' @param component \code{Character} value, component for which the XML data
#' will be downloaded. If omitted, XML data will be collected for all 
#' components available for the specified stations.
#' 
#' @param url \code{Chracter} vector, URL of the FDSN data provider. Should be 
#' of the form \code{"http://service.iris.edu"}, i.e., without further URL 
#' parts. URLs can be submitted as a vector.
#' 
#' @param level \code{Character} value, level of station XML information 
#' depth. Default is \code{"response"}.
#' 
#' @return \code{Obspy} object with station meta info inventory.
#' 
#' @author Michael Dietze
#'
#' @keywords eseis
#'
#' @examples
#' 
#' \dontrun{
#' 
#' x <- aux_getxml(start = "2010-10-10", 
#'                 duration = 60,
#'                 network = "GE",
#'                 station = "BRNL",
#'                 component = "BHZ",
#'                 url = "http://service.iris.edu")
#' 
#' }
#' 
#' @export aux_getxml

aux_getxml <- function(
    xml, 
    start,
    duration, 
    network, 
    station, 
    component, 
    url, 
    level = "response"
){
  
  ## load obspy
  obspy <- try(reticulate::import("obspy"), silent = TRUE)
  
  if(class(obspy)[1] != "python.builtin.module") {
    
    stop("Could not load obspy!")
  }
  
  ## check if path to local XML file is provided
  if(missing(xml) == FALSE) {
    
    if(file.exists(xml) == TRUE) {
      
      ## case: xml file present and to be imported to R
      inventory <- obspy$read_inventory(path.expand(xml))
      save_xml <- FALSE
      
    } else {
      
      ## case: no xml file present, so download, save and import to R
      inventory <- NA
      save_xml <- TRUE
    }
    
  } else {
    
    ## case: no xml file present, so download and import to R but don not save
    inventory <- NA
    save_xml = FALSE
  }
  
  ## check start time format
  if(missing(start) == TRUE & class(inventory)[1] == "logical") {
    
    start <- NA
    stop <- NA
    
  } else {
    
    if(class(start)[1] != "POSIXct") {
      
      start <- try(as.POSIXct(start, tz = "UTC"), silent = TRUE)
      
      if(class(start)[1] != "POSIXct") {
        
        stop("Start date is not a POSIXct format!")
      }
    }  
    
    if(missing(duration) == TRUE & class(inventory)[1] == "logical") {
      
      stop("Argument duration missing!")
    } else {
      
      if(class(duration)[1] != "numeric" & class(inventory)[1] == "logical") {
        
        stop("Duration must be a numeric value!")
      } else {
        
        ## define end time based on duration
        stop <- start + duration
      }
    }
  }
  
  ## check if network is provided
  if(missing(network) == TRUE & class(inventory)[1] == "logical") {
    
    stop("No network code provided!")
  }
  
  if(class(inventory)[1] == "logical") {
    
    ## optionally remove superfluent URL parts
    url_get <- gsub(x = url, 
                    pattern = "/fdsnws/dataselect/1", 
                    replacement = "")
    
    ## optionally remove superfluent URL parts
    url_get <- gsub(x = url, 
                    pattern = "/fdsnws", 
                    replacement = "")
    
    ## initiate client object
    client <- obspy$clients$fdsn$Client("http://service.iris.edu/")
    
    ## convert start and endtime data to obspy standard
    os_starttime <- obspy$UTCDateTime(format(start, "%Y-%m-%dT%H:%M:%S"))
    os_endtime <- obspy$UTCDateTime(format(stop, "%Y-%m-%dT%H:%M:%S"))
    
    ## build temporary file name
    if(save_xml == TRUE) {
      
      xml_local <- xml
    } else {
      
      xml_local <- paste0(tempdir(), "/",
                          network, ".", 
                          ifelse(test = missing(station), 
                                 yes = "", 
                                 no = paste0(station, ".")),
                          component, ".XML")
    }
    
    ## download xml file
    download.file(url = paste0(url_get, 
                               "/fdsnws/station/1/query?network=",
                               network,
                               "&station=",
                               ifelse(test = missing(station), 
                                      yes = "*", 
                                      no = station),
                               "&level=", level,
                               "&format=XML",
                               "&channel=", 
                               ifelse(test = missing(component), 
                                      yes = "*",
                                      no = component)),
                  destfile = xml_local, 
                  quiet = TRUE)
    
    ## read xml file via obspy
    inventory <- obspy$read_inventory(path.expand(xml_local))
    
    if(save_xml == FALSE) {
      
      ## remove temporary file
      try(if(file.exists(xml_local) == TRUE) {
        
        invisible(unlink(xml_local))
      }, silent = TRUE)  
    }
  }
  
  ## return output
  return(inventory)
}