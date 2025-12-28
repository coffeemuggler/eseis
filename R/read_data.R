#' Load seismic data from an archive
#' 
#' The function loads seismic data from a data directory structure (see 
#' \code{aux_organisecubefiles}) based on the event start time, duration,
#' component and station ID. The data to be read needs to be adequately 
#' structured. The data directory must contain mseed or SAC files. These 
#' files will either be identified automatically or can be defined 
#' explicitly by the parameter \code{format}.
#' 
#' Data organisation must follow a consistent scheme. The default scheme, 
#' \code{eseis} (Dietze, 2018 ESurf) requires 
#' hourly files organised in a directory for each Julian Day, and in each  
#' calendar year. The file name must be entirely composed of 
#' station ID, 2-digit year, Julian Day, hour, minute, second and 
#' channel name. Each item must be separated by a full stop, 
#' e.g. \code{"2013/203/IGB01.13.203.16.00.00.BHZ"} for a 
#' file from 2013, Julian Day 203, from station IGB01, covering one hour from 
#' \code{"16:00:00 UTC"}, and containing the BHZ component. Each Julian Day directory 
#' can contain files from different components and stations. The respective 
#' pattern string to describe that file organisation is 
#' \code{"\%Y/\%j/\%STA.\%y.\%j.\%H.\%M.\%S.\%CMP"}. The percent sign indicates  
#' a wild card, where \code{\%Y} is the 4-digit year, \code{\%j} the 3-digit Julian 
#' Julian Day, \code{\%STA} the station ID, \code{\%y} the 2-digit year, 
#' \code{\%H} the 2-digit hour, \code{\%M} the 2-digit minute, \code{\%S} the 
#' 2-digit second and \code{\%CMP} the component ID. The files can have a 
#' further file extension which does not need to be explicitly defined in the 
#' pattern string. The slashes in the above pattern string define 
#' subdirectories.
#' 
#' An alternative organisation scheme is the one used by SeisComP, indicated 
#' by the keyword \code{"seiscomp"} or the pattern string 
#' \code{"\%Y/\%NET/\%STA/\%CMP/\%NET.\%STA.\%LOC.\%CMP.\%TYP.\%Y.\%j"}. 
#' The wild card \code{"NET"} means the network ID, \code{"LOC"} the location 
#' abbreviation and \code{"TYP"} the data type. The other wild cards are as 
#' defined above. Hence, the SeisComP scheme consists of directories of the 
#' calendar year, the network to which the data belongs, the station it has 
#' been recorded by, and the component it belongs to. The files in that 
#' latter directory must be daily files.
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
#' structure (cf. \code{aux_organisecubefiles}). Information can be restricted 
#' to a single character, denoting the last character of the station component 
#' code, i.e. \code{"Z"} would use the first and last component of 
#' \code{c("BHZ", "HHE", "HHZ")}. Default is \code{"BHZ"} (vertical component 
#' of a seismic file).
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
#' @param \dots Further arguments to describe data structure, only needed for 
#' pattern type \code{seiscomp}. These arguments can be one or more of the 
#' following: \code{"network"}, \code{"type"}, \code{"location"}. If omitted, 
#' the function will identify all files in the SeisComP data archive that 
#' fulfill the criteria. If other than data files (\code{type = "D"}) or 
#' files from another network are in the archive, these may lead to crashes  
#' of the function.
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
#' data <- read_data(start = as.POSIXct(x = "2017-04-09 01:20:00", 
#'                                      tz = "UTC"), 
#'                   duration = 120,
#'                   station = c("RUEG1", "RUEG2"),
#'                   component = "BHZ",
#'                   dir = dir_data)
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
    ...
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
  
  ## extract additional arguments
  args <- list(...)
  
  ## check/set network argument
  if ("network" %in% names(args)) {
    
    network <- args$network
    
  } else {
    
    if(pattern == "%Y/%NET/%STA/%CMP/%NET.%STA.%LOC.%CMP.%TYP.%Y.%j") {
      
      stop("Argument network missing, cannot assess directory structure!")
    } else {
      
      network <- ""
    }
  }
  
  ## check/set type argument
  if ("type" %in% names(args)) {
    
    type <- args$type
    
  } else {
    
    type <- "__"
  }
  
  ## check/set location argument
  if ("location" %in% names(args)) {
    
    location <- args$location
    
  } else {
    
    location <- "__"
    
  }
  
  ## convert start time to UTC
  t_0 <- as.POSIXct(as.POSIXlt(start, tz = "UTC"))
  
  ## calculate stop time
  t_1 <- t_0 + duration
  
  ## handle different file organisation patterns
  if(grepl(x = pattern, pattern = "%H", fixed = TRUE)) {
    
    ## account for full hour crossing
    if(t_1 - t_0 < 3600) {t_1 <- t_1 + 3600}
    
    ## build hourly time series  of files to read
    t <- seq(from = t_0, to = t_1 - 1/100000, by = 3600)
    
    ## convert time to full hours
    t <- as.POSIXct(x = format(t, format = "%Y-%m-%d %H:00:00"), 
                    tz = "UTC")
    
  } else {
    
    ## account for full day crossing
    if(t_1 - t_0 < 24 * 3600) {t_1 <- t_1 + 24 * 3600}
    
    ## build daily time series of files to read
    t <- seq(from = t_0, to = t_1 - 1/100000, by = 24 * 3600)
    
    ## convert time to full day
    t <- as.POSIXct(x = format(t, format = "%Y-%m-%d 00:00:00"), 
                    tz = "UTC")
  }

  ## assign shortened object names
  sta <- station
  cmp <- ifelse(test = nchar(component) == 1, #
                yes = paste0("*", component), 
                no = component)
  pars <- list(dir = dir, 
               pt = pattern, 
               fm = format, 
               try = try, 
               ip = interpolate,
               network = network,
               type = type,
               location = location)
  
  ## organise seismic data  
  s <- lapply(X = sta, FUN = function(sta, cmp, t, pars) {
    
    ## organise files by component
    s <- lapply(X = cmp, FUN = function(cmp, sta, t, pars) {
      
      s <- lapply(X = t, FUN = function(t, cmp, sta, pars) {
        
        ## separate directory string by subdirectories
        dir_in <- try(strsplit(x = pars$pt, split = "/", fixed = TRUE)[[1]],
                      silent = TRUE)
        
        ## remove file name pattern
        dir_sub <- try(dir_in[1:(length(dir_in) - 1)], silent = TRUE)
        
        ## optionally change network, station, component tags
        try(dir_sub[grepl(x = dir_sub, pattern = "%NET", fixed = TRUE)] <- 
          pars$network, silent = TRUE)
        try(dir_sub[grepl(x = dir_sub, pattern = "%STA", fixed = TRUE)] <- 
          sta, silent = TRUE)
        try(dir_sub[grepl(x = dir_sub, pattern = "%CMP", fixed = TRUE)] <- 
          cmp, silent = TRUE)
        
        ## convert sub directory time tags to required time characters
        dir_sub <- try(format(t, format = dir_sub),
                       silent = TRUE)
        
        ## build full directory paths
        dir_get <- try(paste0(pars$dir, "/", 
                              paste(dir_sub, collapse = "/"), "/"),
                       silent = TRUE)
        
        ## remove possible double slashes
        dir_get <- try(gsub(x = dir_get, pattern = "//", replacement = "/"),
                       silent = TRUE)
        
        ## separate file name items
        file_get <- try(strsplit(x = dir_in[length(dir_in)], 
                                 split = ".", 
                                 fixed = TRUE)[[1]], silent = TRUE)
        
        ## replace wild cards by required actual strings
        try(file_get[file_get == "%STA"] <- sta, silent = TRUE)
        try(file_get[file_get == "%CMP"] <- cmp, silent = TRUE)
        try(file_get[file_get == "%NET"] <- pars$network, silent = TRUE)
        try(file_get[file_get == "%LOC"] <- pars$location, silent = TRUE)
        try(file_get[file_get == "%TYP"] <- pars$type, silent = TRUE)
        
        ## build required file name items for time wild cards
        file_get <- try(sapply(X = file_get, FUN = function(file_get, t) {
          
          if(grepl(x = file_get, pattern = "%", fixed = TRUE) == TRUE) {
            
            format(t, format = file_get)
          } else {
            
            file_get
          }
        }, t), silent = TRUE)
        
        ## paste items to full file name
        file_get <- try(paste(file_get, collapse = "."), silent = TRUE)
        
        ## list all files in data directory
        file_all <- try(list.files(dir_get), silent = TRUE)
        
        ## handle missing location value
        if(grepl(x = file_get, pattern = ".__.", fixed = TRUE)) {
          
          ## split file name
          file_get_spl <- try(strsplit(x = file_get, 
                                       split = ".__.", 
                                       fixed = TRUE)[[1]], silent = TRUE)
          
          ## check patterns
          i_ok <- try(sapply(X = file_get_spl, FUN = function(x, file_all) {
            
            grepl(x = file_all, pattern = x)
          }, file_all), silent = TRUE)
          i_ok <- try(rowSums(i_ok) == length(file_get_spl), silent = TRUE)
          
          ## check if file exists
          file_read <- try(file_all[i_ok], 
                          silent = TRUE)
        } else {
          
          ## check if file exists
          file_dir <- try(grep(x = file_all, pattern = file_get), 
                          silent = TRUE)
          file_read <- try(file_all[file_dir], silent = TRUE)
        }
        
        ## handle unsuccessful attempt
        try(if(length(file_read) != 1) {
          
          warning(paste0("no or too many files found for ", t))
          
          s <- eseis::aux_initiateeseis()
          s$signal <- NULL
          s$meta$n <- 0
          s$meta$starttime <- start
          s$meta$dt <- NA
          
          return(s)
        }, silent = TRUE)
        
        ## paste direcotry and file name
        try(file_read <- paste0(dir_get, file_read))
        
        ## try to read file
        if(pars$fm == "sac") {
          
          s <- try(eseis::read_sac(file = file_read), silent = TRUE)
        } else if(pars$fm == "mseed") {
          
          s <- try(eseis::read_mseed(file = file_read), silent = TRUE)
        } else if(pars$fm == "unknown") {
          
          s <- try(suppressWarnings(eseis::read_sac(file = file_read)), 
                   silent = TRUE)

          if(class(s)[1] != "eseis") {
            
            s <- try(suppressWarnings(eseis::read_mseed(file = file_read)), 
                     silent = TRUE)
          } else if(class(s)[1] == "eseis" & s$meta$n < 1) {
            
            s <- try(suppressWarnings(eseis::read_mseed(file = file_read)), 
                     silent = TRUE)
          }
        }
        
        ## optionally handle try-error case
        if(class(s)[1] != "eseis") {
          
          warning(paste0("could not read file for ", t))
          
          s <- aux_initiateeseis()
          s$signal <- NULL
          s$meta$n <- 0
          s$meta$starttime <- start
          s$meta$dt <- NA
        }
        
        ## return output
        return(s)
        
      }, cmp, sta, pars)
      
      ## get meta data from collected files
      s_meta <- try(do.call(rbind, lapply(X = s, FUN = function(s) {
        
        return(data.frame(start = s$meta$starttime, 
                          stop = s$meta$starttime + s$meta$n * s$meta$dt - 
                            s$meta$dt/100,
                          n = s$meta$n,
                          dt = s$meta$dt))
      })), silent = TRUE)
      
      ## check that files have common sampling rate
      try(if(length(unique(na.omit(s_meta$dt))) > 1) {
        
        warning("Files do not have common sampling period!")
        
        dt <- unique(na.omit(s_meta$dt))[1]
      }, silent = TRUE)
      
      ## make merged time series
      t_s <- suppressWarnings(try(seq(from = min(s_meta$start, na.rm = TRUE), 
                                      to = max(s_meta$stop, na.rm = TRUE), 
                                      by = na.omit(unique(s_meta$dt))), 
                                  silent = TRUE))
      
      ## make merged data vector
      s_s <- try(rep(NA, length(t_s)), silent = TRUE)
      
      ## fill data vector
      try(for(i in 1:length(s)) {
        
        if(!is.na(s_meta$stop[i])) {
          
          s_s[t_s >= s_meta$start[i] & t_s <= s_meta$stop[i]] <- 
            s[[i]]$signal
        }
      }, silent = TRUE)
      
      ## paste data into first imported object
      s <- try(s[[1]], silent = TRUE)
      try(s$signal <- s_s, silent = TRUE)
      try(s$meta$n <- length(s_s), silent = TRUE)
      
      ## clip file to target time period
      s <- try(eseis::signal_clip(data = s, 
                                  limits = c(start, start + duration)), 
               silent = TRUE)
      
      ## optionally fill gaps
      try(if(pars$ip == TRUE) {
        
        s <- eseis::signal_fill(data = s)
      })
      
      ## handle error case
      if(class(s)[1] == "try-error") {
        
        if(try == TRUE) {
          
          s <- NA
          
        } else {
          
          stop(paste0("Failure during import of ", sta, ", ", cmp))
        }
      }
      
      ## optionally remove eseis object structure
      if(eseis == FALSE) {
        
        s <- list(signal = s$signal, 
                  time = seq(from = s$meta$starttime, 
                             by = s$meta$dt, 
                             length.out = s$meta$n),
                  meta = s$meta, 
                  header = s$header)
      }
      
      return(s)
    }, sta, t, pars)
    
    names(s) <- do.call(c, lapply(X = s, FUN = function(x) {x$meta$component}))
    
    return(s)
  }, cmp, t, pars)
  names(s) <- sta
  
  ## optionally simplify file structure
  if(simplify == TRUE) {
    
    if(class(s)[1] == "list" & length(s) == 1) {
      
      s <- s[[1]]
    }
    
    s <- lapply(X = s, FUN = function(s) {
      
      if(class(s)[1] == "list" & length(s) == 1) {
        
        s <- s[[1]]
        
      } else {
        
        s <- s
      }
      
      return(s)
    })
    
    if(class(s)[1] == "list" & length(s) == 1) {
      
      s <- s[[1]]
    }
    
  }
  
  ## return output
  return(s)
}
