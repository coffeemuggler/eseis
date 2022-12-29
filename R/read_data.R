#' Load seismic data from an archive
#' 
#' The function loads seismic data from a data directory structure (see 
#' \code{aux_organisecubefiles()}) based on the event start time, duration,
#' component and station ID.
#' 
#' The data to be read needs to be adequately structured. The data directory
#' must contain mseed or SAC files. These files will either be identified 
#' automatically or can be defined explicitly by the parameter \code{format}.
#' 
#' Data organisation must follow a consistent scheme. The default scheme, 
#' \code{eseis} (Dietze, 2018 DOI: 10.5194/esurf-6-669-2018) requires hourly
#' files organised in a directory for each Julian Day, and in each calendar 
#' year. The file name must be entirely composed of station ID, 2-digit year, 
#' Julian Day, hour, minute, second and channel name. Each item must be 
#' separated by a ".", e.g. \code{2013/203/IGB01.13.203.16.00.00.BHZ} for a 
#' file from 2013, Julian Day 203, from station IGB01, covering one hour from 
#' 16:00:00 UTC, and containing the BHZ component. Each Julian Day directory 
#' can contain files from different components and stations. The respective 
#' pattern string to describe that file organisation is 
#' \code{"%Y/%j/%STA.%y.%j.%H.%M.%S.%CMP"}. The percent sign indicates a wild 
#' card, where \code{%Y} is the 4-digit year, \code{%j} the 3-digit Julian 
#' Day, \code{%STA} the station ID, \code{%y} the 2-digit year, \code{%H} the 
#' 2-digit hour, \code{%M} the 2-digit minute, \code{%S} the 2-digit second 
#' and \code{%CMP} the component ID. The files can have a further file 
#' extension which does not need to be explicitly defined in the pattern 
#' string. The slashes in the above pattern string define subdirectories.
#' 
#' An alternative organisation scheme is the one used by SeisComP, indicated 
#' by the keywork \code{"seiscomp"} or the pattern string 
#' \code{"%Y/%NET/%STA/%CMP/%NET.%STA.%LOC.%CMP.%TYP.%Y.%j"}. The wild card 
#' \code{"NET"} means the network ID, \code{"LOC"} the location abbreviation 
#' and \code{"TYP"} the data type. The other wild cards are as defined above. 
#' Hence, the SeisComP scheme consists of directories of the calendar year, 
#' the network to which the data belongs, the station it has been recorded by, 
#' and the component it belongs to. The files in that latter directory must be 
#' daily files.
#' 
#' @param start \code{POSIXct} value, start time of the data to import. If
#' lazy users only submit a text string instead of a POSIXct object, the 
#' function will try to convert that text string, assuming UTC as time zone. 
#' 
#' @param duration \code{Numeric} value, duration of the data to import,
#' in seconds.
#' 
#' @param station \code{Character} value, seismic station ID, which must
#' correspond to the ID in the file name of the data directory structure 
#' (cf. \code{aux_organisecubefiles}).
#' 
#' @param component \code{Character} value, seismic component, which must
#' correspond to the component name in the file name of the data directory  
#' structure (cf. \code{aux_organisecubefiles}). Default is 
#' \code{"BHZ"} (vertical component of a sac file).
#' 
#' @param format \code{Character} value, seismic data format. One out of 
#' \code{"sac"} and \code{"mseed"}. If omitted, the function will try to 
#' identify the right format automatically.
#' 
#' @param dir \code{Character} value, path to the seismic data directory.
#' See details for further info on data structure.
#' 
#' @param pattern \code{Character} value, either keyword or pattern string 
#' with wildcards, describing the data organisation. Supported keywords are 
#' \code{"eseis"} and \code{"seiscomp"}. See details for keyword definition 
#' and format of pattern strings. Default option is \code{eseis}.
#' 
#' @param simplify \code{Logical} value, option to simplify output
#' when possible. This basically means that if only data from one station 
#' is loaded, the list object will have one level less. Default is 
#' \code{TRUE}.
#' 
#' @param interpolate \code{Logical} value, option to interpolate possible 
#' gaps in the resulting data stream. If enabled, NA values will be identified 
#' and linearly interpolated using the function \code{signal_fill}. Default 
#' is \code{FALSE}, i.e. NA gaps will remain in the imported data set.
#' 
#' @param eseis \code{Logical} value, option to read data to an \code{eseis}
#' object (recommended, see documentation of 
#' \code{aux_initiateeseis}), default is \code{TRUE}
#' 
#' @param try \code{Logical} value, option to run the function in try-mode, 
#' i.e., to let it return \code{NA} in case an error occurs during data
#' import. Default is \code{FALSE}.
#' 
#' @param verbose \code{Logical} value, option to show messages during 
#' function evaluation. Default is \code{FALSE}.
#' 
#' @return A \code{list} object containing either a set of \code{eseis}
#' objects or a data set with the time vector (\code{$time}) 
#' and a list of seismic stations (\code{$station_ID}) with their seismic
#' signals as data frame (\code{$signal}). If \code{simplify = TRUE} (the 
#' default option) and only one seismic station is provided, the output  
#' object containseither just one eseis object or the vectors for 
#' \code{$time} and \code{$signal}.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## set seismic data directory
#' dir_data <- paste0(system.file("extdata", package="eseis"), "/")
#' 
#' ## load the z component data from a station
#' data <- read_data(start = as.POSIXct(x = "2017-04-09 01:20:00", 
#'                                         tz = "UTC"), 
#'                       duration = 120,
#'                       station = "RUEG1",
#'                       component = "BHZ",
#'                       dir = dir_data)
#' ## plot signal
#' plot_signal(data = data)
#' 
#' ## load data from two stations
#' data <- aux_getevent(start = as.POSIXct(x = "2017-04-09 01:20:00", 
#'                                         tz = "UTC"), 
#'                      duration = 120,
#'                      station = c("RUEG1", "RUEG2"),
#'                      component = "BHZ",
#'                      dir = dir_data)
#' 
#' ## plot both signals
#' par(mfcol = c(2, 1))
#' lapply(X = data, FUN = plot_signal)
#'                      
#' @export read_data

read_data <- function(
    start,
    duration,
    station, 
    component = "BHZ",
    format,
    dir,
    pattern = "eseis",
    simplify = TRUE,
    interpolate = FALSE,
    eseis = TRUE,
    try = TRUE,
    verbose = FALSE
) {
  
  ## check start time format
  if(class(start)[1] != "POSIXct") {
    
    start <- try(as.POSIXct(start, tz = "UTC"), silent = TRUE)
    
    if(class(start)[1] != "POSIXct") {
      
      stop("Start date is not a POSIXct format!")
    }
  }
  
  ## check/set file format
  if(missing(format) == TRUE) {
    
    format <- "unknown"
  }
  
  ## convert keyword to pattern
  if(pattern == "eseis") {
    
    pattern <- "%Y/%j/%STA.%y.%j.%H.%M.%S.%CMP"
  } else if(pattern == "seiscomp") {
    
    pattern <- "%Y/%NET/%STA/%CMP/%NET.%STA.%LOC.%CMP.%TYP.%Y.%j"
  }
  
  ## convert start time to UTC
  t_0 <- as.POSIXct(as.POSIXlt(start, tz = "UTC"))
  
  ## calculate stop time
  t_1 <- t_0 + duration
  
  ## acount for full hour crossing
  if(t_1 - t_0 < 3600) {
    
    t_1 <- t_1 + 3600
  }
  
  ## handle different file organisation patterns
  if(grepl(x = pattern, pattern = "%H", fixed = TRUE)) {
    
    ## build time series  of files to read
    t <- seq(from = t_0, to = t_1 - 1/100000, by = 3600)

  } else {
    
    t <- seq(from = t_0, to = t_1 - 1/100000, by = 24 * 3600)

  }
  
  ## convert time to full hours
  t <- as.POSIXct(x = format(t, format = "%Y-%m-%d %H:00:00"), 
                  tz = "UTC")
  
  ## assign shortened object names
  sta <- station
  cmp <- component

  ## organise seismic data  
  s <- lapply(X = sta, FUN = function(sta, cmp, dir, t, pt, fm, try, ip) {
    
    ## organise files by component
    s <- lapply(X = cmp, FUN = function(cmp, sta, dir, t, pt, fm, try, ip) {
      
      s <- lapply(X = t, FUN = function(t, cmp, sta, dir, pt, fm, try, ip) {
        
        ## separate directory string by subdirectories
        dir_in <- try(strsplit(x = pt, split = "/", fixed = TRUE)[[1]],
                      silent = TRUE)
        
        ## convert sub directory time tags to required time characters
        dir_sub <- try(format(t, format = dir_in[-length(dir_in)]),
                       silent = TRUE)
        
        ## build full directory paths
        dir_get <- try(paste0(dir, "/", paste(dir_sub, collapse = "/"), "/"),
                       silent = TRUE)
        
        ## remove possible double slashes
        dir_get <- try(gsub(x = dir_get, pattern = "//", replacement = "/"),
                       silent = TRUE)
        
        ## separate file name items
        file_get <- try(strsplit(x = dir_in[length(dir_in)], 
                                 split = ".", 
                                 fixed = TRUE)[[1]], silent = TRUE)
        
        ## replace wild cards by required actual strings
        try(file_get[file_get == "%STA"] <- sta)
        try(file_get[file_get == "%CMP"] <- cmp)
        
        ## build required file name items for time wild cards
        file_get <- try(sapply(X = file_get, FUN = function(file_get, t) {
          
          if(grepl(x = file_get, pattern = "%", fixed = TRUE) == TRUE) {
            
            format(t, format = file_get)
          } else {
            
            file_get
          }
        }, t), silent = TRUE)
        
        ## paste items to full file name
        file_get <- try(paste(file_get, collapse = "."))
        
        ## check if file exists
        file_all <- try(list.files(dir_get), silent = TRUE)
        file_dir <- try(grep(x = file_all, pattern = file_get), silent = TRUE)
        
        ## paste direcotry and file name
        try(file_read <- paste0(dir_get, file_all[file_dir]))
        
        ## try to read file
        if(fm == "sac") {
          
          s <- try(eseis::read_sac(file = file_read), silent = TRUE)
        } else if(fm == "mseed") {
          
          s <- try(eseis::read_mseed(file = file_read), silent = TRUE)
        } else if(fm == "unknown") {
          
          s <- try(eseis::read_sac(file = file_read), silent = TRUE)
          
          if(class(s)[1] != "eseis") {
            
            s <- try(eseis::read_mseed(file = file_read), silent = TRUE)
          } 
        }

        ## optionally handle try-error case
        if(class(s)[1] != "eseis") {
          
          if(try == TRUE) {
            
            s <- aux_initiateeseis()
            s$signal <- NULL
            s$meta$n <- 0
            s$meta$starttime <- start
            s$meta$dt <- NA
            
          } else {
            
            stop("Cannot read file!")
          }
        }
        
        ## return output
        return(s)
        
      }, cmp, sta, dir, pt, fm, try, ip)
      
      ## get meta data from collected files
      s_meta <- do.call(rbind, lapply(X = s, FUN = function(s) {
        
        return(data.frame(start = s$meta$starttime, 
                          stop = s$meta$starttime + s$meta$n * s$meta$dt - 
                            s$meta$dt/100,
                          n = s$meta$n,
                          dt = s$meta$dt))
      }))
      
      ## check that files have common sampling rate
      if(length(unique(na.omit(s_meta$dt))) > 1) {
        
        stop("Files do not have common sampling period!")
      }
      
      ## make merged time series
      t_s <- seq(from = min(s_meta$start), 
                 to = max(s_meta$stop), 
                 by = unique(s_meta$dt))
      
      ## make merged data vector
      s_s <- rep(NA, length(t_s))
      
      ## fill data vector
      for(i in 1:length(s)) {
        
        s_s[t_s >= s_meta$start[i] & t_s <= s_meta$stop[i]] <- s[[i]]$signal
      }
      
      ## paste data into first imported object
      s <- s[[1]]
      s$signal <- s_s
      s$meta$n <- length(s_s)
      
      ## clip file to target time period
      s <- eseis::signal_clip(data = s, limits = c(start, start + duration))
      
      ## optionally fill gaps
      if(ip == TRUE) {
        
        s <- eseis::signal_fill(data = s)
      }
      
      return(s)
    }, sta, dir, t, pt, fm, try, ip)
    
    names(s) <- cmp
    
    return(s)
  }, cmp, dir, t, pt = pattern, fm = format, try, ip = interpolate)
  names(s) <- sta
  
  ## optionally simplify file structure
  if(simplify == TRUE) {
    
    s <- lapply(X = s, FUN = function(s) {
      
      if(class(s)[1] == "list" & length(s) == 1) {
        
        s <- s[[1]]
      }
    })
    
    if(class(s) == "list" & length(s) == 1) {
      
      s <- s[[1]]
    }
  }
  
  ## optionally remove eseis object
  if(eseis == FALSE) {
    
    
  }
  
  ## return output
  return(s)
}
