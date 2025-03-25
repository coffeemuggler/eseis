#' Create station info file from cube files.
#' 
#' This function reads GPS tags from Omnirecs/Digos Datacube files and creates 
#' a station info file from additional input data. It depends on the gipptools 
#' software package (see details).
#' 
#' A station info file is an ASCII table that contains all relevant information
#' about the individual stations of a seismic network. The variables contain a 
#' station ID (containing not more than 5 characters), station name (an longer 
#' description of the station), latitude, longitude, elevation, deployment 
#' depth, sensor type, logger type, sensor ID, logger ID, gain (signal 
#' preamplification by the logger), dt (sampling interval), start and stop 
#' time of the station records.
#' 
#' The start and stop times can be automatically collected from the meta data 
#' stored along in each Cube file. For that, the function has to select the 
#' first and last file in a data record of a station. This is automatically 
#' done if the option \code{order = "margin"} is selected and the number of 
#' files per station to process is two, hence \code{n = 2}.
#' 
#' Automatically, the resulting ASCII file will have all 14 columns as defined 
#' above. One has to delete unwanted columns (or add additional ones) from 
#' the text file, manually after the file has been generated. 
#' 
#' The function requires the software gipptools 
#' (\code{http://www.gfz-potsdam.de/en/section/geophysical-deep-sounding/infrastructure/geophysical-instrument-pool-potsdam-gipp/software/gipptools/}) 
#' is installed. Note that GPS tag extraction may take several minutes per 
#' cube file. Hence, depending on the number of files and utilised CPUs the 
#' processing may take a while.
#' 
#' Specifying an input directory (\code{input}) is mandatory. This input 
#' directory must only contain the subdirectories with the cube files to 
#' process, each set of cube files must be located in a separate subdirectory 
#' and these subdirectories must have the same name as specified by the logger 
#' IDs (\code{logger_ID}). An appropriate structure would be something like:
#'  
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
#' @param file \code{Character} value, file name of the output station info 
#' file, with extension but without path.
#'
#' @param input \code{Character} value, path to directory where all cube 
#' files to be processed as stored. Each set of files from one logger must be 
#' stored in a separate sub-directory named after the cube ID.
#'
#' @param output \code{Character} value, path to directory where output 
#' data is written to.
#' 
#' @param gipptools \code{Character} value, path to gipptools or cubetools 
#' directory. 
#' 
#' @param ID \code{Character} vector, seismic station ID. Each value  
#' must not contain more than 5 characters. Longer entries will be clipped. If  
#' omitted, a default ID will be created.
#' 
#' @param name \code{Character} vector, seismic station name. If  
#' omitted, the station ID is used as name.
#' 
#' @param z \code{Numeric} vector, elevation of the seismic stations
#' 
#' @param d \code{Numeric} vector, deployment depth of the seismic
#'  sensor
#' 
#' @param sensor_type \code{Character} vector, sensor types
#' 
#' @param logger_type \code{Character} vector, logger types
#' 
#' @param sensor_ID \code{Character} vector, sensor IDs
#' 
#' @param logger_ID \code{Character} vector, logger IDs
#' 
#' @param gain \code{Numeric} or \code{character} vector, gain values. If 
#' omitted, the information will be extracted from Cube files, if possible. 
#' 
#' @param dt \code{Numeric} or \code{character} vector, sampling intervals. If 
#' omitted, the information will be extracted from Cube files, if possible.
#' 
#' @param start \code{POSIXct} or \code{character} vector, time when station 
#' started operating.  If omitted, the information will be extracted from Cube 
#' files, if possible.
#' 
#' @param stop \code{POSIXct} or \code{character} vector, time when station 
#' stopped operating. If omitted, the information will be extracted from Cube 
#' files, if possible.
#' 
#' @param n \code{Numeric} value, number of cube file to process for GPS 
#' coordinate extraction. If omitted, all files are processed.
#' 
#' @param order \code{Character} value, keyword indicating how files will be 
#' chosen to extract meta data. One out of \code{"margin"} (first and last 
#' Cube file, necessary to extract start and end time of the record), 
#' \code{"random"} (random order of files) and \code{"ascending"} (files in 
#' ascending order). Default is \code{"margin"}.
#' 
#' @param unit \code{Character} value, coordinates unit of the location. One 
#' out of \code{"dd"} (decimal degrees) and \code{"utm"} (metric in UTM zone). 
#' Default is \code{"dd"}.
#' 
#' @param quantile \code{Numeric} value, quantile size to which the extracted 
#' coordinate sample size is restricted. This is mainly used to remove 
#' coordinate outliers, due to spurious GPS signals. Default is 
#' \code{0.95}. Set to \code{1} to omit any sample rejection.
#' 
#' @param cpu \code{Numeric} value, fraction of CPUs to use for parallel 
#' processing. If omitted, one CPU is used.
#' 
#' @param write_file \code{Logical} value, option to write station info file 
#' to disk. Default is \code{TRUE}. 
#' 
#' @param write_raw \code{Logical} value, option to write (keep) raw ASCII 
#' GPS data. Default is \code{FALSE}. 
#' 
#' @param write_data \code{Logical} value, option to write gps raw data as 
#' rda-file. File name will be the same as for \code{file}. Default is
#' \code{FALSE}.
#' 
#' @return A set of files written to disk and a data frame with seismic 
#' station information.
#' 
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#'
#' \dontrun{
#' 
#' ## basic example with minimum effort
#' aux_stationinfofile(file = "stationinfo.txt", 
#'                     input = "path/to/cube/dirs", 
#'                     output = "path/to/stationfile/", 
#'                     gipptools = "software/gipptools-2024.354", 
#'                     logger_ID = c("A1A", "A1B"))
#' 
#' ## example with more adjustments
#' aux_stationinfofile(file = "stationinfo.txt", 
#'                     input = "path/to/cube/dirs", 
#'                     output = "path/to/stationfile/", 
#'                     gipptools = "software/gipptools-2024.354",
#'                     ID = c("STAN", "STAS"),
#'                     name = c("Station North", "Station South"), 
#'                     z = c(1000, 1100),
#'                     d = c(0.5, 0.5),
#'                     sensor_type = c("TC120s", "TC120s"), 
#'                     logger_type = c("Cube3extBOB", "Centaur"), 
#'                     sensor_ID = c("4711", "0815"), 
#'                     logger_ID = c("A1A", "A1B"), 
#'                     gain = c(32, 16), 
#'                     dt = c(1/100, 1/200),
#'                     n = 3, 
#'                     order = "margin", 
#'                     unit = "utm",
#'                     cpu = 0.5, 
#'                     write_raw = TRUE,
#'                     write_data = TRUE)
#' }
#' 
#' @export aux_stationinfofile

aux_stationinfofile <- function(
    file,
    input,
    output,
    gipptools,
    ID,
    name,
    z,
    d,
    sensor_type,
    logger_type,
    sensor_ID,
    logger_ID,
    gain,
    dt,
    start,
    stop,
    n,
    order = "margin",
    unit = "dd",
    quantile = 0.95,
    cpu,
    write_file = TRUE,
    write_raw = FALSE,
    write_data = FALSE
){
  
  ## Part 1 - checks, tests, adjustments --------------------------------------
  
  ## get start time
  t_1 <- Sys.time()
  
  ## check/set file name
  if(missing(file) == TRUE) {
    
    file <- "station_info.txt"
  }
  
  ## check/set output directory
  if(missing(output) == TRUE) {
    
    output <- file.path(tempdir(), "output")
    print(paste("Output will be written to", output))
  }
  
  ## set/correct path definitions
  input <- paste0(dirname(input), "/", basename(input), "/")
  output <- paste0(dirname(output), "/", basename(output), "/")
  gipptools <- paste0(dirname(gipptools), "/", 
                      basename(gipptools), "/")
  
  ## check if output directory exists and, if necessary create it
  if(dir.exists(paths = output) == FALSE) {
    
    dir.create(path = output)
    print("[aux_stationinfofile]: Output directory did not exist, created.")
  }
  
  ## check/set number of files to process
  if(missing(n) == TRUE) {
    
    n <- "all"
  }
  
  ## check/set fraction of CPUs to use
  if(missing(cpu) == TRUE) {
    
    cpu <- NA
  }
  
  ## get cube directories
  input_exist <- list.files(path = input, full.names = TRUE)
  
  ## check/set cube ID
  if(missing(logger_ID) == TRUE) {
    
    logger_ID <- list.files(path = input)
  }
  
  ## check cube ID for consistency
  if(any(nchar(x = logger_ID) != 3) == TRUE) {
    
    warning("[aux_stationinfofile]: The cube_IDs may be incorrect!")
  }
  
  ## compare cube_ID with cube directories
  for(i in 1:length(logger_ID)) {
    
    if(sum(logger_ID[i] == list.files(path = input)) < 1) {
      warning(paste("[aux_stationinfofile]: Cube ID", 
                    logger_ID[i],
                    "not found in input directory!"))
    }
  }
  
  ## check/set station ID
  if(missing(ID) == TRUE) {
    
    ID <- paste("STA", 1:length(logger_ID), sep = "")
  }
  
  ## check/set station name
  if(missing(name) == TRUE) {
    
    name <- ID
  }
  
  ## check/set station elevation
  if(missing(z) == TRUE) {
    
    z <- rep(x = NA, times = length(ID))
  }
  
  ## check/set station depth
  if(missing(d) == TRUE) {
    
    d <- rep(x = NA, times = length(ID))
  }
  
  ## check/set sensor type
  if(missing(sensor_type) == TRUE) {
    
    sensor_type <- rep(x = NA, times = length(ID))
  }
  
  ## check/set logger type
  if(missing(logger_type) == TRUE) {
    
    logger_type <- rep(x = NA, times = length(ID))
  }
  
  ## check/set sensor ID
  if(missing(sensor_ID) == TRUE) {
    
    sensor_ID <- rep(x = NA, times = length(ID))
  }
  
  ## check keyword for file order
  if(order %in% c("margin", "random", "ascending") == FALSE) {
    
    stop("Keyword for argument order is not supported!")
  }
  
  ## check/set order and n consistence
  if(order == "margin" & n < 2) {
    
    n <- 2
    warning("Order type margin needs at least two files, n set to 2!")
  }
  
  ## check/get options for gain, dt, start, end
  if(missing(gain) == TRUE) {
    
    get_gain <- TRUE
    gain <- rep(NA, length(ID))
  } else {
    
    get_gain <- FALSE
    if(length(gain) != length(ID)) {
      stop("Gain vector length does not match number of stations!")
    }
  }
  
  if(missing(dt) == TRUE) {
    
    get_dt <- TRUE
    dt <- rep(NA, length(ID))
  } else {
    
    get_dt <- FALSE
    if(length(dt) != length(ID)) {
      stop("Sample interval vector length does not match number of stations!")
    }
  }
  
  if(missing(start) == TRUE) {
    
    get_start <- TRUE
    start <- rep(NA, length(ID))
  } else {
    
    get_start <- FALSE
    if(length(start) != length(ID)) {
      stop("Start time vector length does not match number of stations!")
    }
  }
  
  if(missing(stop) == TRUE) {
    
    get_stop <- TRUE
    stop <- rep(NA, length(ID))
  } else {
    
    get_stop <- FALSE
    if(length(stop) != length(ID)) {
      stop("Stop time vector length does not match number of stations!")
    }
  }
  
  ## build preliminary station info file
  station_info <- data.frame(ID = ID,
                             name = name,
                             x = numeric(length = length(ID)),
                             y = numeric(length = length(ID)),
                             z = z,
                             d = d,
                             sensor_type = sensor_type,
                             logger_type = logger_type,
                             sensor_ID = sensor_ID,
                             logger_ID = logger_ID,
                             gain = gain,
                             dt = dt,
                             start = start,
                             stop = stop)
  
  ## convert data frame row-wise to list
  station_info <- lapply(X = 1:nrow(station_info), FUN = function(i, x){x[i,]}, 
                         x = station_info)
  
  ## Part 2 - preparation steps -----------------------------------------------
  
  ## get files to read GPS data from
  files_cube <- lapply(X = station_info, 
                       FUN = function(x, input, n, order) {
                         
                         ## get all files in respective cube directory
                         files_i <- list.files(path = paste(input, 
                                                            x$logger_ID, 
                                                            sep = "/"), 
                                               full.names = TRUE)
                         
                         ## remove unwanted files
                         files_i <- files_i[substr(x = files_i, 
                                                   start = nchar(files_i) - 2, 
                                                   stop = nchar(files_i)) == 
                                              x$logger_ID]
                         
                         ## keep only specified number of files
                         if(n != "all" & n <= length(files_i)) {
                           
                           if(order == "ascending") {
                             
                             files_i <- files_i[1:n]
                           } else if(order == "random") {
                             
                             files_i <- files_i[sample(x = 1:length(files_i), 
                                                       size = n, 
                                                       replace = FALSE)]
                           } else if(order == "margin") {
                             
                             files_i <- c(files_i[1], tail(files_i, 1))
                           }
                         }
                         
                         ## return output
                         return(files_i)
                       },
                       input = input, n = n, order = order)
  
  ## convert list content to vector
  files_cube <- do.call(c, files_cube)
  
  ## create raw gps file output directory
  if(dir.exists(paths = paste(output, 
                              "/gps_raw", 
                              sep = "")) == FALSE) {
    
    dir.create(path = paste(output, "/gps_raw", sep = ""), 
               showWarnings = FALSE)
  }
  
  ## detect and adjust number of cores to use
  cores <- parallel::detectCores()
  
  if(is.na(cpu) == FALSE) {
    
    n_cpu <- floor(cores * cpu)
    cores <- ifelse(cores < n_cpu, cores, n_cpu)
  } else {
    
    cores <- 1
  }
  
  ## estimate process duration, based on a Lenovo X260 Intel I7 2.6 GHz CPU
  t_0 <- 150 / 3600 * 1.1 # i.e. 150 s per file + 10 % extra time
  t_duration_estimate <- t_0 * length(files_cube)
  
  ## print notification
  print(paste0("[eseis::aux_stationinfofile]: ", length(ID), 
               " loggers and ", length(files_cube), " files to process"))
  
  ## Part 3 - extraction of GPS data ------------------------------------------
  
  ## initiate cluster
  cl <- parallel::makeCluster(getOption("mc.cores", cores))
  
  ## extract GPS data
  invisible(parallel::parLapply(
    cl = cl, X = files_cube, 
    fun = function(X, gipptools, output) {
      
      system(command = paste(gipptools, "/bin/cubeinfo", 
                             " --format=GPS --output-dir=",
                             output, "/gps_raw ",
                             X,
                             sep = ""))
    }, gipptools = gipptools, output = output))
  
  ## stop cluster
  parallel::stopCluster(cl = cl)
  
  ## Part 4 - calculations of GPS data ----------------------------------------
  
  ## get all gps raw files
  gps_files <- list.files(path = paste(output, "/gps_raw", sep = ""), 
                          full.names = TRUE)
  
  ## assign gps files to cubes
  gps_files_cube <- lapply(X = station_info, FUN = function(x, gps_files) {
    
    gps_files[grepl(x = gps_files, 
                    pattern = x[10])]
  }, 
  gps_files = gps_files)
  
  ## extract GPS data
  gps_cube <- lapply(X = gps_files_cube, FUN = function(x) {
    
    ## create coordinate list
    lat <- numeric(length = 0)
    lon <- numeric(length = 0)
    
    ## append coordinates for all files
    for(i in 1:length(x)) {
      
      ## read each file
      data_i <- try(utils::read.delim(file = x[i],
                                      sep = " ", 
                                      header = FALSE, 
                                      stringsAsFactors = FALSE), 
                    silent = TRUE)
      
      ## append successfully extracted data
      if(class(data_i)[1] != "try-error") {
        
        ## identify required fields
        v_lat <- which(grepl(x = data_i[1,], pattern = "lat="))
        v_lon <- which(grepl(x = data_i[1,], pattern = "lon="))
        
        ## extract latitude
        lat <- c(lat, as.numeric(substr(x = data_i[,v_lat], 
                                        start = 5, 
                                        stop = nchar(data_i[,v_lat][1]))))
        
        ## extract longitude
        lon <- c(lon, as.numeric(substr(x = data_i[,v_lon], 
                                        start = 5, 
                                        stop = nchar(data_i[,v_lon][1]))))
      }
    }
    
    ## return output
    return(cbind(lat, lon))
  })
  
  ## remove outliers
  gps_cube <- lapply(X = gps_cube, FUN = function(gps_cube, qt) {
    
    lat_median_diff <- abs(median(x = gps_cube[,1]) - gps_cube[,1])
    lon_median_diff <- abs(median(x = gps_cube[,2]) - gps_cube[,2])
    
    lat_diff_quantile <- stats::quantile(x = lat_median_diff, qt)
    lon_diff_quantile <- stats::quantile(x = lon_median_diff, qt)
    
    gps_cube[lat_median_diff > lat_diff_quantile,1] <- NA
    gps_cube[lon_median_diff > lon_diff_quantile,1] <- NA
    
    gps_cube <- gps_cube[stats::complete.cases(gps_cube),]
    
    return(gps_cube)
  }, qt = quantile)
  
  ## optionally convert decimal degrees to UTM coordinates
  if(unit == "utm") {
    
    gps_cube <- lapply(X = gps_cube, FUN = function(x) {
      
      ## infer UTM zone
      utm_zone <- (floor((x[,2] + 180)/6) %% 60) + 1
      
      ## estimate UTM coordinates
      proj_utm <- paste("+proj=utm +zone=", 
                        floor(median(utm_zone, na.rm = TRUE)), 
                        " ellps=WGS84", 
                        sep = "")
      
      ## convert coordinates
      eseis::spatial_convert(data = x, 
                             from = "+proj=longlat +datum=WGS84",
                             to = proj_utm)
    })
  }
  
  ## calculate average coordinates
  gps_cube_mean <- lapply(X = gps_cube,
                          FUN = colMeans)
  
  ## convert station info file and average gps data to data frames
  station_info <- as.data.frame(do.call(rbind, station_info), 
                                stringsAsFactors = FALSE)
  gps_cube_mean <- do.call(rbind, gps_cube_mean)
  
  ## add coordinates to station info file
  station_info$x <- gps_cube_mean[,2]
  station_info$y <- gps_cube_mean[,1]
  
  ## optionally remove raw gps data
  if(write_raw == FALSE) {
    
    unlink(gps_files, recursive = TRUE)
    unlink(paste(output, "gps_raw", sep = "/"), recursive = TRUE)
  }
  
  ## Part 5 - get gain, dt, start, and end times ------------------------------
  
  ## check if any data is requested
  if(any(c(get_gain, get_dt, get_start, get_stop)) == TRUE) {
    
    ## extract cube meta information
    cubedata <- lapply(X = files_cube, FUN = function(x) {
      
      y <- try(eseis::aux_cubeinfo(file = x, gipptools = gipptools), 
               silent = TRUE)
      
      ## extract ID from file name
      ID_get <- try(strsplit(x = y$`file name`, 
                             split = ".", 
                             fixed = TRUE)[[1]][2], silent = TRUE)
      if(inherits(ID_get, "try-error")) {ID_get <- NA}
      
      ## extract gain
      gain <- try(strsplit(x = y$`amplification factor`, 
                           split = " ", 
                           fixed = TRUE)[[1]][1], silent = TRUE)
      if(inherits(gain, "try-error")) {gain <- NA}
      
      ## extract dt
      dt <- try(1 / as.numeric(strsplit(x = y$`sample rate`, 
                                        split = " ", 
                                        fixed = TRUE)[[1]][1]), silent = TRUE)
      if(inherits(dt, "try-error")) {dt <- NA}
      
      ## extract start time
      start <- try(as.POSIXct(y$`approx. recorder start`, tz = "UTC"), 
                   silent = TRUE)
      if(length(start) < 1) {start <- try(1/"x", silent = TRUE)}
      if(inherits(start, "try-error")) {start <- as.POSIXct(NA)}
      
      ## extract stop time
      stop <- try(as.POSIXct(y$`approx. recorder stop`, tz = "UTC"), 
                  silent = TRUE)
      if(length(stop) < 1) {stop <- try(1/"x", silent = TRUE)}
      if(inherits(stop, "try-error")) {stop <- as.POSIXct(NA)}
      
      ## build output data set
      z <- data.frame(ID = ID_get, 
                      gain = as.numeric(gain), 
                      dt = as.numeric(dt), 
                      start = start, 
                      stop = stop)
      
      return(z)
    })
    
    ## convert list to data frame
    cubedata <- try(do.call(rbind, cubedata), silent = TRUE)
    
    ## replace NA values in stop times by start times
    stop_tmp <- cubedata$start
    i_ok <- which(!is.na(cubedata$stop))
    stop_tmp[i_ok] <- cubedata$stop[i_ok]
    cubedata$stop <- stop_tmp
    
    ## organise by Cube ID
    id_unique <- unique(x = cubedata$ID)
    
    ## get statistics by ID
    cubedata_grp <- lapply(X = id_unique, FUN = function(x, cubedata) {
      
      y <- cubedata[cubedata$ID %in% x,]
      
      z <- data.frame(ID = x, 
                      gain = median(y$gain, na.rm = TRUE),
                      dt = median(y$dt, na.rm = TRUE), 
                      start = min(y$start, na.rm = TRUE), 
                      stop = max(y$stop, na.rm = TRUE))
      
      return(z)
    }, cubedata = cubedata)
    
    ## convert list to data frame
    cubedata_grp <- do.call(rbind, cubedata_grp)
    
    ## sort by output ID order
    cubedata_grp <- cubedata_grp[order(station_info$logger_ID),]
    
    ## replace dummy stop times by NA
    cubedata_grp$stop[cubedata_grp$start == cubedata_grp$stop] <- NA
    
    ## assign meta data
    station_info$gain <- cubedata_grp$gain
    station_info$dt <- cubedata_grp$dt
    station_info$start <- cubedata_grp$start
    station_info$stop <- cubedata_grp$stop
  }
  
  ## Part 6 - export output data ----------------------------------------------
  
  ## optionally save station info file
  if(write_file == TRUE) {
    
    utils::write.table(station_info, 
                       file = paste0(output, "/", file), 
                       col.names = TRUE, 
                       row.names = FALSE, 
                       quote = TRUE,
                       sep = "\t")
  }
  
  ## optionally save GPS data set
  if(write_data == TRUE) {
    
    trysave <- try(save(gps_cube, 
                        file = paste(output, 
                                     "/",
                                     file,
                                     ".rda",
                                     sep = "")), silent = TRUE)
    
    if(class(trysave)[1] == "try-error") {
      
      warning("Unable to save raw gps data file!")
    }
  }
  
  ## get end time
  t_2 <- Sys.time()
  
  ## calculate duration
  duration_processing <- difftime(time1 = t_2, 
                                  time2 = t_1, 
                                  units = "hours")
  
  ## generate notification
  note_2 <- paste0("[eseis::aux_stationinfofile]: ",
                   "Finished station info file in ", 
                   round(x = duration_processing, digits = 2), " h.")
  
  ## print notification
  print(note_2)
  
  ## return result
  return(station_info)
}
