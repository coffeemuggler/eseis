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
  if(dir.exists(paths = "mseed_raw/") == FALSE) {
    
    dir.create(path = "mseed_raw/", 
               showWarnings = FALSE)
  }
  
  if(dir.exists(paths = "mseed_hour/") == FALSE) {
    
    dir.create(path = "mseed_hour/", 
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
    fun = function(X, gipptools) {
      
      system(command = paste(gipptools, "bin/cube2mseed", 
                             " --verbose --output-dir=",
                             "mseed_raw ",
                             X,
                             "/",
                             sep = ""))
    }, 
    gipptools = gipptools))
  
  ## create list of generated mseed files
  files_mseed_raw <- list.files(path = "mseed_raw/")
  
  ## create hourly from daily mseed files 
  invisible(parallel::parLapply(
    cl = cl, 
    X = files_mseed_raw, 
    fun = function(X, gipptools) {
      
      system(command = paste(gipptools, "bin/mseedcut", 
                             " --file-length=HOUR --force-overwrite",
                             " --output-dir=",
                             "mseed_hour",
                             " mseed_raw/",
                             X,
                             sep = ""))                                                       
    },
    gipptools = gipptools))
  
  ## remove daily files
  files_mseed_raw_full <- list.files(path = "mseed_raw/", 
                                     full.names = TRUE)
  invisible(file.remove(files_mseed_raw_full))
  invisible(file.remove("mseed_raw"))
  
  ## create hourly mseed file list
  files_mseed_hour <- list.files(path = "mseed_hour", 
                                 pattern = "*.pri*")
  
  ## extract sac file name elements
  files_cubeID <- toupper(x = substr(x = files_mseed_hour, 
                                     start = 3, 
                                     stop = 5))
  files_year <- substr(x = files_mseed_hour, start = 6, stop = 7)
  files_month <- substr(x = files_mseed_hour, start = 8, stop = 9)
  files_day <- substr(x = files_mseed_hour, start = 10, stop = 11)
  files_hour <- substr(x = files_mseed_hour, start = 12, stop = 13)
  files_min <- substr(x = files_mseed_hour, start = 14, stop = 15)
  files_sec <- substr(x = files_mseed_hour, start = 16, stop = 17)
  files_JD <- strptime(x =  paste(files_day, 
                                  files_month,
                                  files_year), "%d %m %y")$yday + 1
  files_JD_unique <- sort(unique(files_JD))
  
  ## optionally convert component keywords and assign file
  if(channel_name == "bh") {
    
    files_component <- substr(x = files_mseed_hour, 
                              start = 19, 
                              stop = 22)
    files_component[files_component == "pri0"] <- "BHZ"
    files_component[files_component == "pri1"] <- "BHN"
    files_component[files_component == "pri2"] <- "BHE"
  } else {
    
    files_component <- paste("p", substr(x = files_mseed_hour, 
                                         start = 22, 
                                         stop = 22), sep = "")
  }
  
  ## account for inappropriate logger ID converison
  files_cubeID[nchar(files_cubeID) == 2] <- paste("0", 
                                                  files_cubeID[nchar(
                                                    files_cubeID) == 2], 
                                                  sep = "")
  
  ## convert logger ID to station ID
  files_stationID <- character(length = length(files_cubeID))
  station_ID <- as.character(stations$ID)
  
  for(i in 1:nrow(stations)) {
    
    files_stationID[files_cubeID == stations$logger_ID[i]] <- station_ID[i]
  }
  
  ## generate file names
  if(format == "mseed") {
    
    file_names <- paste(files_stationID, 
                        files_year, 
                        files_JD, 
                        files_hour, 
                        files_min, 
                        files_sec, 
                        files_component,
                        sep = ".")
  } else {
    
    file_names <- paste(files_stationID, 
                        files_year, 
                        files_JD, 
                        files_hour, 
                        files_min, 
                        files_sec, 
                        files_component, 
                        "SAC", 
                        sep = ".")
  }
  
  ## because I am still not able to use the apply-family correctly, this
  ## merging step needs to be done, here.
  files_mseed_edit <- paste(files_stationID,
                            files_component,
                            files_mseed_hour,
                            file_names,
                            sep = "__")
  
  ## edit mseedfiles, i.e. change station ID and component name
  invisible(parallel::parLapply(
    cl = cl, 
    X = files_mseed_edit, 
    fun = function(X, gipptools) {
      
      ## separate function arguments
      x_sep <- strsplit(x = X, 
                        split = "__", 
                        fixed = TRUE)
      
      station <- x_sep[[1]][1]
      channel <- x_sep[[1]][2]
      file <- x_sep[[1]][3] 
      file_name <- x_sep[[1]][4] 
      
      ## set station and channel ID
      system(command = paste(gipptools, "bin/mseed2mseed", 
                             " --set-station=",
                             station,
                             " --set-channel=",
                             channel,
                             " --output-dir=mseed_hour",
                             " mseed_hour/",
                             file,
                             sep = ""))
      
      ## generate renamed file name
      file_new <- paste(tolower(x = station),
                        substr(x = file,
                               start = 6, stop = nchar(file) - 4),
                        tolower(x = channel),
                        sep = "")
      
      ## rename file to output name
      file.rename(from = paste("mseed_hour/",
                               file_new,
                               sep = ""),
                  to = paste("mseed_hour/",
                             file_name,
                             sep = ""))
    }, 
    gipptools = gipptools))
  
  ## remove original files
  invisible(file.remove(paste("mseed_hour", 
                              files_mseed_hour, 
                              sep = "/")))
  
  ## extract year of the files, this only works for data after the year 2000
  year <- as.character(paste("20", files_year, sep = ""))
  
  ## extract Julian day of the files and add leading zeros
  JD <- as.character(files_JD)
  JD[nchar(JD) == 1] <- paste("00", JD[nchar(JD) == 1], sep = "")
  JD[nchar(JD) == 2] <- paste("0", JD[nchar(JD) == 2], sep = "")
  
  ## merge year and JD
  year_JD <- paste(year, JD, sep = "/")
  
  ## create file list of mseed files with new name
  files_mseed_hour_new <- list.files(path = "mseed_hour")
  
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
    file.copy(from = paste("mseed_hour", 
                           files_mseed_hour_new[i],
                           sep = "/"), 
              to = paste(output_dir, 
                         year_JD[i],
                         file_names[i], 
                         sep = "/"))
    
    ## delete original file
    file.remove(paste("mseed_hour", 
                      files_mseed_hour_new[i],
                      sep = "/"))
  }
  
  ## remove temporary directory
  invisible(file.remove("mseed_hour"))
  
  ## optionally convert mseed to sac
  if(format == "sac") {
    
    ## get all mseed files
    files_mseed <- unlist(lapply(
      X = unique(year), FUN = function(X, output_dir) {
        
        list.files(path = paste(output_dir, X, sep = "/"), 
                   full.names = TRUE, 
                   recursive = TRUE)
      },
      output_dir = output_dir))
    
    ## import and export all mseed files as sac files
    invisible(parallel::parLapply(
      cl = cl, 
      X = files_mseed, 
      fun = function(X) {
        
        ## read mseed file
        x <- eseis::read_mseed(file = X,
                               signal = TRUE, 
                               time = TRUE, 
                               meta = TRUE, 
                               header = FALSE)
        
        ## write sac file
        eseis::write_sac(data = x$signal, 
                         time = x$time, 
                         file = X, 
                         component = x$meta$component, 
                         unit = 1,
                         station = x$meta$station, 
                         location = c(x$meta$latitude, 
                                      x$meta$longitude, 
                                      x$meta$elevation, 
                                      NA), 
                         network = x$meta$network, 
                         dt = x$meta$dt)
      }))
  }
  
  ## stop cluster
  parallel::stopCluster(cl = cl)
}