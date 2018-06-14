#' Reorganise seismic files recorded by Nanometrics Centaur loggers
#' 
#' This function optionally converts mseed files to sac files and 
#' organises these in a coherent directory structure, by year, Julian day, 
#' (station, hour and channel). It depends on the cubetools or gipptools  
#' software package (see details). The function is at an experimental stage  
#' and only used for data processing at the GFZ Geomorphology section, 
#' currently.
#' 
#' The function assumes that the Nanometrics Centaur data logger directory 
#' contains only hourly mseed files. These hourly files are organised in a 
#' coherent directory structure which is organised by year and Julian day. 
#' In each Julian day directory the hourly files are placed and named according 
#' to the following scheme: 
#' STATIONID.YEAR.JULIANDAY.HOUR.MINUTE.SECOND.CHANNEL.\cr
#' The function requires that the software cubetools 
#' (\code{http://www.omnirecs.de/documents.html}) or gipptools 
#' (\code{http://www.gfz-potsdam.de/en/section/geophysical-deep-sounding/infrastructure/geophysical-instrument-pool-potsdam-gipp/software/gipptools/}) 
#' are installed. \cr Specifying an input directory 
#' (\code{input_dir}) is mandatory. This input directory must only contain the 
#' subdirectories with mseed data for each Centaur logger. The subdirectory 
#' must be named after the four digit Centaur ID and contain only mseed files,
#' regardless if further subdirectories are used (e.g., for calendar days). 
#' 
#' In the case a six-channel Centaur is used to record signals from two
#' sensors, in the station info file (cf. \code{aux_stationinfofile()})
#' the logger ID field must contain the four digit logger ID and the 
#' channel qualifiers, e.g., "AH" (first three channels) or "BH" (last three channels), 
#' separated by an underscore.
#' 
#' @param stationfile \code{Character} value, file name of the station info 
#' file, with extension. See \code{aux_stationinfofile}.
#'
#' @param input_dir \code{Character} value, path to directory where all 
#' files to be processed as stored. Each set of files from one logger must be 
#' stored in a separate sub-directory named after the logger ID (which in 
#' turn must be the four digit number of the logger).
#' 
#' @param output_dir \code{Character} value, path to directory where output 
#' data is written to.
#' 
#' @param format \code{Character} value, output file format. One out of 
#' \code{"mseed"} and \code{"sac"}. Default is \code{"sac"}.
#' 
#' @param channel_name \code{Character} value, output file extension. One out 
#' of \code{"bh"} and \code{"p"}. Default is \code{"bh"}.
#' 
#' @param cpu \code{Numeric} value, fraction of CPUs to use for parallel 
#' processing. If omitted, one CPU is used.
#' 
#' @param gipptools \code{Character} value, path to gipptools or cubetools 
#' directory. 
#' 
#' @param file_key \code{Character} value, file name extension of the files
#' to process. Only files with this extension will be processed. Default is 
#' \code{"miniseed"}.
#' 
#' @param network \code{Character} value, optional seismic network code.
#'
#' @return A set of hourly seismic files written to disk.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## basic example with minimum effort
#' aux_organisecentaurfiles(stationfile = "output/stationinfo.txt", 
#'                          input_dir = "input", 
#'                          gipptools = "software/gipptools-2015.225/")
#' 
#' }
#'                         
#' @export aux_organisecentaurfiles
#' 
aux_organisecentaurfiles <- function(
  stationfile, 
  input_dir, 
  output_dir, 
  format = "sac",
  channel_name = "bh",
  cpu, 
  gipptools,
  file_key = "miniseed",
  network) {
  
  ## Part 1 - checks, tests, adjustments --------------------------------------
  
  ## check/set network ID
  if(missing(network) == TRUE) {
    
    network <- "NA"
  }

  ## check/set output directory
  if(missing(output_dir) == TRUE) {
    
    output_dir <- file.path(tempdir(), "output")
    print(paste("Output will be written to", output_dir))
  }
  
  ## check if output directory exists and, if necessary create it
  if(dir.exists(paths = output_dir) == FALSE) {
    
    dir.create(path = output_dir)
    print("[aux_organisecubefiles]: Output directory did not exist, created.")
  }
  
  ## create temporary output directories
  if(dir.exists(paths = paste(output_dir, 
                              "/mseed_hour/", 
                              sep = "")) == FALSE) {
    
    dir.create(path = paste(output_dir, 
                            "/mseed_hour/", 
                            sep = ""), 
               showWarnings = FALSE)
  }
  
  ## check/set fraction of CPUs to use
  if(missing(cpu) == TRUE) {
    
    cpu <- NA
  }
  
  ## save root directory
  dir_general <- getwd()
  
  ## set path to input files
  path_data <- paste(getwd(), 
                     input_dir, 
                     sep = "/")
  
  ## read station info data
  stations <- read.table(file = stationfile, 
                         header = TRUE,
                         stringsAsFactors = FALSE, 
                         colClasses = "character")
  
  ## check for Centaur signatures
  stations_centaur <- stations[nchar(as.character(stations$logger_ID)) > 3,]
  
  ## optionally process 6-channel qualifiers
  centaur_six <- strsplit(x = as.character(stations_centaur$logger_ID), 
                          split = "_")

  centaur_six_ID <- unlist(lapply(X = centaur_six, FUN = function(X) {
    
    X[1]
  }))
  
  centaur_six_chn <- unlist(lapply(X = centaur_six, FUN = function(X) {
    
    ifelse(test = length(X) > 1, yes = X[2], no = NA)
  }))
  
  stations_centaur <- cbind(stations, 
                            centaur_six_ID, 
                            centaur_six_chn)
  
  ## identify files to process
  files <- list.files(pattern = file_key, 
                      full.names = TRUE, 
                      recursive = TRUE)
  
  ## remove uncomplete files
  files <- files[!grepl(x = files, 
                        pattern = ".part", 
                        fixed = TRUE)]
  
  ## detect and adjust number of cores to use
  cores <- parallel::detectCores()
  
  if(is.na(cpu) == FALSE) {
    
    n_cpu <- floor(cores * cpu)
    cores <- ifelse(cores < n_cpu, cores, n_cpu)
  } else {
    
    cores <- 1
  }
  
  ## initiate cluster
  cl <- parallel::makeCluster(getOption("mc.cores", cores))
  
  ## import and export all mseed files as sac files or mseed files
  invisible(parallel::parLapply(
    cl = cl, 
    X = files, 
    fun = function(X, 
                   files, 
                   output_dir, 
                   stations_centaur, 
                   channel_name,
                   format,
                   gipptools,
                   network) {
      
      ## read mseed file
      x <- eseis::read_mseed(file = X)
      
      ## extract logger ID from file name
      x_raw <- strsplit(x = x$meta$filename, split = "/", fixed = TRUE)[[1]]
      x_raw <- strsplit(x = x_raw[length(x_raw)], split = "_")[[1]]
      x_name <- x_raw[3]
      x_chn <- substr(x = strsplit(x = x_raw[1], 
                                   split = ".", 
                                   fixed = TRUE)[[1]][3], 
                      start = 1, 
                      stop = 2)
      x_ID <- paste(x_name, x_chn, sep = "_")
      
      x_ID <- ifelse(test = sum(match(x = stations_centaur$logger_ID, 
                                      table = x_ID), 
                                na.rm = TRUE) > 0,
                     yes = x_ID,
                     no = x_name)
      
      ## extract file information
      stat_info <- 
        stations_centaur$ID[stations_centaur$logger_ID == x_ID]
      comp_info <- substr(x = x$meta$component, start = 3, stop = 3)
      date_info <- x$meta$starttime
      
      ## check/set network name
      if(missing(network) == TRUE) {
        
        if(x$meta$network == "XX") {
          
          x$meta$network <- ""
        }
      } else {
        
        x$meta$network <- network
      }

      ## optionally change component names
      if(channel_name == "bh") {
        
        comp_info[comp_info == "Z"] <- "BHZ"
        comp_info[comp_info == "Y"] <- "BHN"
        comp_info[comp_info == "X"] <- "BHE"
        
        comp_info[comp_info == "Z"] <- "BHZ"
        comp_info[comp_info == "N"] <- "BHN"
        comp_info[comp_info == "E"] <- "BHE"
        
      }
      
      if(channel_name == "p") {
        
        comp_info[comp_info == "Z"] <- "p0"
        comp_info[comp_info == "X"] <- "p1"
        comp_info[comp_info == "Y"] <- "p2"
        
        comp_info[comp_info == "Z"] <- "p0"
        comp_info[comp_info == "E"] <- "p1"
        comp_info[comp_info == "N"] <- "p2"
        
      }
      
      ## extract date elements
      JD_info <- eseis::time_convert(input = date_info, output = "JD")
      JD_info <- ifelse(test = nchar(x = JD_info) == 1, 
                        yes = paste("0", JD_info, sep = ""),
                        no = JD_info)
      JD_info <- ifelse(test = nchar(x = JD_info) == 2, 
                        yes = paste("0", JD_info, sep = ""),
                        no = JD_info)
      year_info <- format(date_info, "%y")
      hour_info <- format(date_info, "%H")
      min_info <- format(date_info, "%M")
      sec_info <- format(date_info, "%S")
      
      ## optionally assign further meta information
      if(is.na(x$meta$latitude) == TRUE) {
        x$meta$latitude <- 
          stations_centaur$y[stations_centaur$logger_ID == x_ID]
        if(is.null(x$meta$latitude) == TRUE) {
          
          x$meta$latitude <- NA
        }
      }
      
      if(is.na(x$meta$longitude) == TRUE) {
        x$meta$longitude <- 
          stations_centaur$x[stations_centaur$logger_ID == x_ID]
        if(is.null(x$meta$longitude) == TRUE) {
          
          x$meta$longitude <- NA
        }
      }

      if(is.na(x$meta$elevation) == TRUE) {
        x$meta$elevation <- 
          stations_centaur$z[stations_centaur$logger_ID == x_ID]
        if(is.null(x$meta$elevation) == TRUE) {
          
          x$meta$elevation <- NA
        }
      }
      
      ## build file name
      name_output <- paste(stat_info, 
                           year_info,
                           JD_info,
                           hour_info,
                           min_info,
                           sec_info,
                           comp_info,
                           sep = ".")
      
      if(format == "sac") {
        
        ## write sac file
        eseis::write_sac(data = x$signal, 
                         time = x$time, 
                         file = paste(output_dir,
                                      "/mseed_hour/",
                                      name_output, 
                                      ".SAC", 
                                      sep = ""), 
                         component = comp_info, 
                         unit = 1,
                         station = as.character(stat_info), 
                         location = c(x$meta$latitude, 
                                      x$meta$longitude, 
                                      x$meta$elevation, 
                                      NA), 
                         network = x$meta$network, 
                         dt = x$meta$dt)
        
      } else if(format == "mseed") {
        
        ## copy mseed file to temporary directory
        file.copy(from = X, to = paste(output_dir, 
                                       "/mseed_hour/", 
                                       name_output, 
                                       "_raw",
                                       sep = ""))
        
        ## edit and resave mseed file
        system(command = paste(gipptools, "/bin/mseed2mseed", 
                               " --set-station=",
                               stat_info,
                               " --set-channel=",
                               comp_info,
                               " ./",
                               paste(output_dir, 
                                     "/mseed_hour/", 
                                     name_output,
                                     "_raw", 
                                     sep = ""),
                               " > ",
                               paste(output_dir, 
                                     "/mseed_hour/", 
                                     name_output, 
                                     sep = ""),
                               sep = ""))
        
        ## remove temporary file
        file.remove(paste(output_dir, 
                          "/mseed_hour/", 
                          name_output, 
                          "_raw",
                          sep = ""))
      }
      
    }, 
    output_dir = output_dir,
    stations_centaur = stations_centaur,
    channel_name = channel_name,
    format = format,
    gipptools = gipptools,
    network = network))
  
  ## stop cluster
  parallel::stopCluster(cl = cl)
  
  ## make new file list
  files_new <- list.files(path = paste(output_dir, 
                                       "/mseed_hour", 
                                       sep = ""))
  
  ## extract date information from hourly files
  files_year <- unlist(lapply(X = files_new, FUN = function(X) {
    
    strsplit(x = X, split = ".", fixed = TRUE)[[1]][2]
  }))
  
  files_JD <- unlist(lapply(X = files_new, FUN = function(X) {
    
    strsplit(x = X, split = ".", fixed = TRUE)[[1]][3]
  }))
  
  ## extract year of the files, this only works for data after the year 2000
  year <- as.character(paste("20", files_year, sep = ""))
  
  ## extract Julian day of the files and add leading zeros
  JD <- as.character(files_JD)
  JD[nchar(JD) == 1] <- paste("00", JD[nchar(JD) == 1], sep = "")
  JD[nchar(JD) == 2] <- paste("0", JD[nchar(JD) == 2], sep = "")
  
  ## merge year and JD
  year_JD <- paste(year, JD, sep = "/")
  
  ## loop through all files
  for(i in 1:length(year_JD)) {
    
    ## optionally create directories based on year and Julian day
    if(dir.exists(paths = paste(output_dir, 
                                year_JD[i], 
                                sep = "/")) == FALSE) {
      
      dir.create(path = paste(output_dir, 
                              year_JD[i], 
                              sep = "/"), 
                 showWarnings = FALSE, 
                 recursive = TRUE)
    }
    
    ## move file to directory
    file.copy(from = paste(output_dir, 
                           "/mseed_hour", 
                           files_new[i],
                           sep = "/"), 
              to = paste(output_dir, 
                         year_JD[i],
                         files_new[i], 
                         sep = "/"))
    
    ## delete original file
    file.remove(paste(output_dir, 
                      "/mseed_hour", 
                      files_new[i],
                      sep = "/"))
  }
  
  ## remove temporary directory
  invisible(file.remove(paste(output_dir, 
                              "/mseed_hour",
                              sep = "")))
}
