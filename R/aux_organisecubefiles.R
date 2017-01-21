#' Convert cube files to mseed or sac files and organise in directory structure.
#' 
#' This function converts cube files to hourly mseed or sac files and 
#' organises these in a coherent directory structure, by year, Julian day, 
#' (station, hour and channel). It depends on the cubetools or gipptools  
#' software package (see details). The function is at an experimental stage  
#' and only used for data processing at the GFZ Geomorphology section, 
#' currently.
#' 
#' The function converts seismic data from the cube file format to either 
#' mseed (cf. \code{read_mseed}) or sac (cf. \code{read_sac}) and cuts the 
#' daily cube files to hourly files. These hourly files are organised in a 
#' coherent directory structure which is organised by year and Julian day. 
#' In each Julian day directory the hourly files are placed and named after 
#' the following scheme: STATIONID.YEAR.JULIANDAY.HOUR.MINUTE.SEC.CHANNEL.\cr
#' The function requires that the software cubetools 
#' (\code{http://www.omnirecs.de/documents.html}) or gipptools 
#' (\code{http://www.gfz-potsdam.de/en/section/geophysical-deep-sounding/infrastructure/geophysical-instrument-pool-potsdam-gipp/software/gipptools/}) 
#' are installed. \cr Specifying an input directory 
#' (\code{input_dir}) is mandatory. This input directory must only contain the 
#' subdirectories with the cube files to process, each set of cube files must 
#' be located in a separate subdirectory and these subdiretories must 
#' have the same name as specified by the logger IDs (\code{logger_ID}). An 
#' appropriate structure would be something like: \cr 
#' \enumerate{
#'   \item input
#'   \enumerate{
#'     \item A1A
#'       \enumerate{
#'         \item file1.A1A
#'         \item file2.A1A
#'       }
#'     \item A1B
#'       \enumerate{
#'         \item file1.A1B
#'         \item file2.A1B
#'       }
#'    }
#' }
#' 
#' @param stationfile \code{Character} value, file name of the station info 
#' file, with extension. See \code{aux_stationinfofile}.
#'
#' @param input_dir \code{Character} value, path to directory where all cube 
#' files to be processed as stored. Each set of files from one logger must be 
#' stored in a separate sub-directory named after the cube ID.
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
#' @return A set of hourly seismic files written to disk.
#' 
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#'
#' ## uncomment to use
#' 
#' ## basic example with minimum effort
#' # aux_organisecubefiles(stationfile = "output/stationinfo.txt", 
#' #                       input_dir = "input", 
#' #                       output_dir = "output", 
#' #                       gipptools = "software/gipptools-2015.225/")
#'                         
#' @export aux_organisecubefiles
aux_organisecubefiles <- function(
  stationfile,
  input_dir,
  output_dir,
  format = "sac",
  channel_name = "bh",
  cpu,
  gipptools
){
  
  ## Part 1 - checks, tests, adjustments --------------------------------------
  
  ## check/set output directory
  if(missing(output_dir) == TRUE) {
    
    ## set default output directory
    output_dir <- paste(getwd(), "", sep = "")
    output_dir_flag <- FALSE
  } else {
    
    output_dir_flag <- TRUE
  }
  
  ## check if output directory exists and, if necessary create it
  if(dir.exists(paths = output_dir) == FALSE & output_dir_flag == TRUE) {
    
    dir.create(path = output_dir)
    print("[aux_organisecubefiles]: Output directory did not exist, created.")
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
                         header = TRUE)
  
  ## create list of logger directories to process
  list_logger <- list.files(path = path_data, 
                            full.names = TRUE)
  
  ## compare cube_ID with cube directories
  for(i in 1:nrow(stations)) {
    
    if(sum(stations$logger_ID[i] == list.files(path = input_dir)) < 1) {
      stop(paste("[aux_organisecubefiles]: ", 
                 stations$ID[i], " (", 
                 stations$logger_ID[i], ") ",
                 "not found in input directory!", sep = ""))
    }
  }
  
  ## create temporary output directories
  if(dir.exists(paths = paste(output_dir, 
                              "/mseed_raw/", 
                              sep = "")) == FALSE) {
    
    dir.create(path = paste(output_dir, 
                            "/mseed_raw/", 
                            sep = ""), 
               showWarnings = FALSE)
  }
  
  if(dir.exists(paths = paste(output_dir, 
                              "/mseed_hour/", 
                              sep = "")) == FALSE) {
    
    dir.create(path = paste(output_dir, 
                            "/mseed_hour/", 
                            sep = ""), 
               showWarnings = FALSE)
  }
  
  ## identify files to process
  files <- unlist(x = lapply(X = list_logger, 
                             FUN = list.files, 
                             full.names = TRUE))
  
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
  
  ## convert cube to mseed files
  invisible(parallel::parLapply(
    cl = cl, 
    X = list_logger, 
    fun = function(X, gipptools, output_dir) {
      
      system(command = paste(gipptools, "/bin/cube2mseed", 
                             " --output-dir=",
                             output_dir,
                             "/mseed_raw ",
                             X,
                             "/",
                             sep = ""))
    }, 
    gipptools = gipptools,
    output_dir = output_dir))
  
  ## create list of generated mseed files
  files_mseed_raw <- list.files(path = paste(output_dir, 
                                             "/mseed_raw/",
                                             sep = ""))
  
  ## create hourly from daily mseed files 
  invisible(parallel::parLapply(
    cl = cl, 
    X = files_mseed_raw, 
    fun = function(X, gipptools, output_dir) {
      
      system(command = paste(gipptools, "/bin/mseedcut", 
                             " --file-length=HOUR --force-overwrite",
                             " --output-dir=",
                             output_dir,
                             "/mseed_hour ",
                             output_dir,
                             "/mseed_raw/",
                             X,
                             sep = ""))                                                       
    },
    gipptools = gipptools,
    output_dir = output_dir))
  
  ## remove daily files
  files_mseed_raw_full <- list.files(path = paste(output_dir, 
                                                  "/mseed_raw/",
                                                  sep = ""), 
                                     full.names = TRUE)
  invisible(file.remove(files_mseed_raw_full))
  invisible(file.remove(paste(output_dir, 
                              "/mseed_raw", 
                              sep = "")))
  
  ## create hourly mseed file lists
  files_mseed_hour <- list.files(path = paste(output_dir,
                                              "/mseed_hour",
                                              sep = ""))
  
  ## import and export all mseed files as sac files
  invisible(parallel::parLapply(
    cl = cl, 
    X = files_mseed_hour, 
    fun = function(X, 
                   files_mseed_hour, 
                   output_dir, 
                   stations, 
                   channel_name,
                   format,
                   gipptools) {
      
      ## read mseed file
      x <- eseis::read_mseed(file = paste(output_dir, 
                                          "/mseed_hour/", 
                                          X, 
                                          sep = ""),
                             signal = TRUE, 
                             time = TRUE, 
                             meta = TRUE, 
                             header = FALSE)
      
      ## extract file information
      stat_info <- substr(x = x$meta$station, start = 3, stop = 5)
      comp_info <- x$meta$component
      date_info <- x$meta$starttime
      
      ## re-assign station information
      stat_info <- as.character(stations[grepl(x = stations$logger_ID, 
                                               pattern = stat_info),1])
      
      ## optionally change component names
      if(channel_name == "bh") {
        
        comp_info[comp_info == "p0"] <- "BHZ"
        comp_info[comp_info == "p1"] <- "BHN"
        comp_info[comp_info == "p2"] <- "BHE"
      }
      
      ## extract date elements
      JD_info <- eseis::time_convert(input = date_info, output = "JD")
      year_info <- format(date_info, "%y")
      hour_info <- format(date_info, "%H")
      min_info <- format(date_info, "%M")
      sec_info <- format(date_info, "%S")
      
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
                         station = stat_info, 
                         location = c(x$meta$latitude, 
                                      x$meta$longitude, 
                                      x$meta$elevation, 
                                      NA), 
                         network = x$meta$network, 
                         dt = x$meta$dt)
      } else if(format == "mseed") {
        
        ## edit and resave mseed file
        system(command = paste(gipptools, "/bin/mseed2mseed", 
                               " --set-station=",
                               stat_info,
                               " --set-channel=",
                               comp_info,
                               " ./",
                               paste(output_dir, 
                                     "/mseed_hour/", 
                                     X, 
                                     sep = ""),
                               " > ",
                               paste(output_dir, 
                                     "/mseed_hour/", 
                                     name_output, 
                                     sep = ""),
                               sep = ""))
      }
      
    }, 
    output_dir = output_dir,
    stations = stations,
    channel_name = channel_name,
    format = format,
    gipptools = gipptools))
  
  ## remove old mseed files
  invisible(file.remove(paste(output_dir, "/mseed_hour", 
                              files_mseed_hour,
                              sep = "/")))
  
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
  
  ## stop cluster
  parallel::stopCluster(cl = cl)
}