#' Create station info file from cube files.
#' 
#' This function reads GPS tags from Omnirecs/Digos Datacube files and creates 
#' a station info file from additional input data. It depends on the cubetools 
#' or gipptools  software package (see details).
#' 
#' A station info file is an ASCII file that contains all relevant information
#' about the individual stations of a seismic network. The variables contain a 
#' station ID (containing not more than 5 characters), station name, latitude, 
#' longitude, elevation, deployment depth, sensor type, logger type, sensor 
#' ID and logger ID.\cr The function requires that the software cubetools 
#' (\code{http://www.omnirecs.de/documents.html}) or gipptools 
#' (\code{http://www.gfz-potsdam.de/en/section/geophysical-deep-sounding/infrastructure/geophysical-instrument-pool-potsdam-gipp/software/gipptools/}) 
#' are installed. Note that GPS tag extraction may take several minutes per 
#' cube file. Hence, depending on the number of files and utilised CPUs the 
#' processing may take a while.\cr Specifying an input directory 
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
#' @param name \code{Character} value, file name of the output station info 
#' file, without extention (will be added as *.txt).
#'
#' @param input_dir \code{Character} value, path to directory where all cube 
#' files to be processed as stored. Each set of files from one logger must be 
#' stored in a separate sub-directory named after the cube ID.
#'
#' @param output_dir \code{Character} value, path to directory where output 
#' data is written to.
#' 
#' @param station_ID \code{Character} vector, seismic station ID. Each value  
#' must not contain more than 5 characters. Longer entries will be clipped. If  
#' omitted, a default ID will be created.
#' 
#' @param station_name \code{Character} vector, seismic station name. If  
#' omitted, the station ID is used as name.
#' 
#' @param station_z \code{Numeric} vector, elevation of the seismic stations.
#' 
#' @param station_d \code{Numeric} vector, deployment depth of the seismic sensor.
#' 
#' @param sensor_type \code{Character} vector, sensor type.
#' 
#' @param logger_type \code{Character} vector, logger type.
#' 
#' @param sensor_ID \code{Character} vector, sensor ID.
#' 
#' @param logger_ID \code{Character} vector, logger ID.
#' 
#' @param unit \code{Character} value, coordinates unit of the location. One 
#' out of \code{"dd"} (decimal degrees) and \code{"utm"} (metric in UTM zone). 
#' Default is \code{"dd"}.
#' 
#' @param n \code{Numeric} value, number of cube file to process for GPS 
#' coordinate extraction. If omitted, all files are processed.
#' 
#' @param random \code{Logical} value, option to draw \code{n} cube files 
#' randomly instead of ordered by date. Default is \code{TRUE}.
#' 
#' @param quantile \code{Numeric} value, quantile size to which the extracted 
#' coordinate sample size is restricted. This is mainly used to remove 
#' coordinate outliers, due to spurious GPS signals. Default is 
#' \code{0.95}. Set to \code{1} to omit any sample rejection.
#' 
#' @param cpu \code{Numeric} value, fraction of CPUs to use for parallel 
#' processing. If omitted, one CPU is used.
#' 
#' @param gipptools \code{Character} value, path to gipptools or cubetools 
#' directory. 
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
#' aux_stationinfofile(name = "stationinfo", 
#'                     input_dir = "input", 
#'                     logger_ID = c("864", "876", "AB1"),
#'                     gipptools = "software/gipptools-2015.225")
#' 
#' ## example with more adjustments
#' aux_stationinfofile(name = "stationinfo",
#'                     input_dir = "input",
#'                     logger_ID = c("864", "876", "AB1"),
#'                     station_name = c("KTZ01", "KTZ02", "KTZ03"), 
#'                     station_z = c(30, 28, 29), 
#'                     station_d = rep(0.5, 3), 
#'                     sensor_type = rep("TC120s", 3), 
#'                     logger_type = rep("Cube3ext", 3), 
#'                     unit = "utm", 
#'                     n = 1, 
#'                     cpu = 0.9,
#'                     gipptools = "software/gipptools-2015.225", 
#'                     write_raw = TRUE, 
#'                     write_data = TRUE)
#' 
#' }
#' 
#' @export aux_stationinfofile
aux_stationinfofile <- function(
  name,
  input_dir,
  output_dir,
  station_ID,
  station_name,
  station_z,
  station_d,
  sensor_type,
  logger_type,
  sensor_ID,
  logger_ID,
  unit = "dd",
  n,
  quantile = 0.95,
  random = TRUE,
  cpu,
  gipptools,
  write_file = TRUE,
  write_raw = FALSE,
  write_data = FALSE
){
  
  ## Part 1 - checks, tests, adjustments --------------------------------------
  
  ## get start time
  t_1 <- Sys.time()
  
  ## check/set file name
  if(missing(name) == TRUE) {
    
    name <- "station_info"
  }
  
  ## check/set output directory
  if(missing(output_dir) == TRUE) {
    
    output_dir <- file.path(tempdir(), "output")
    print(paste("Output will be written to", output_dir))
  }
  
  ## check if output directory exists and, if necessary create it
  if(dir.exists(paths = output_dir) == FALSE) {
    
    dir.create(path = output_dir)
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
  input_dir_exist <- list.files(path = input_dir, 
                                full.names = TRUE)
  
  ## check/set cube ID
  if(missing(logger_ID) == TRUE) {
    
    logger_ID <- list.files(path = input_dir)
  }
  
  ## check cube ID for consistency
  if(any(nchar(x = logger_ID) != 3) == TRUE) {
    
    warning("[aux_stationinfofile]: The cube_IDs may be incorrect!")
  }
  
  ## compare cube_ID with cube directories
  for(i in 1:length(logger_ID)) {
    
    if(sum(logger_ID[i] == list.files(path = input_dir)) < 1) {
      warning(paste("[aux_stationinfofile]: Cube ID", 
                    logger_ID[i],
                    "not found in input directory!"))
    }
  }
  
  ## check/set station ID
  if(missing(station_ID) == TRUE) {
    
    station_ID <- paste("STA", 1:length(logger_ID), sep = "")
  }
  
  ## check/set station name
  if(missing(station_name) == TRUE) {
    
    station_name <- station_ID
  }
  
  ## check/set station elevation
  if(missing(station_z) == TRUE) {
    
    station_z <- rep(x = NA, 
             times = length(station_ID))
  }
  
  ## check/set station depth
  if(missing(station_d) == TRUE) {
    
    station_d <- rep(x = NA, 
                     times = length(station_ID))
  }

  ## check/set sensor type
  if(missing(sensor_type) == TRUE) {
    
    sensor_type <- rep(x = NA, 
             times = length(station_ID))
  }
  
  ## check/set logger type
  if(missing(logger_type) == TRUE) {
    
    logger_type <- rep(x = NA, 
             times = length(station_ID))
  }
  
  ## check/set sensor ID
  if(missing(sensor_ID) == TRUE) {
    
    sensor_ID <- rep(x = NA, 
             times = length(station_ID))
  }

  ## build preliminary station info file
  station_info <- data.frame(ID = station_ID,
                             name = station_name,
                             x = numeric(length = length(station_ID)),
                             y = numeric(length = length(station_ID)),
                             station_z = station_z,
                             station_d = station_d,
                             sensor_type = sensor_type,
                             logger_type = logger_type,
                             sensor_ID = sensor_ID,
                             logger_ID = logger_ID)
  
  ## convert data frame row-wise to list
  station_info <- apply(X = station_info, MARGIN = 1, FUN = list)
  station_info <- lapply(X = station_info, FUN = function(x) {x[[1]]})
  
  ## Part 2 - preparation steps -----------------------------------------------
  
  ## get files to read GPS data from
  files_cube <- lapply(X = station_info, 
                       FUN = function(x, input_dir, n, random) {
                         
                         ## get all files in respective cube directory
                         files_i <- list.files(path = paste(input_dir, 
                                                            x[10], 
                                                            sep = "/"), 
                                               full.names = TRUE)
                         
                         ## remove unwanted files
                         files_i <- files_i[substr(x = files_i, 
                                                   start = nchar(files_i) - 2, 
                                                   stop = nchar(files_i)) == 
                                              x[10]]
                         
                         ## keep only specified number of files
                         if(n != "all" & n <= length(files_i)) {
                           
                           if(random == FALSE) {
                             
                             files_i <- files_i[1:n]
                           } else {
                             
                             files_i <- files_i[sample(x = 1:length(files_i), 
                                                       size = n, 
                                                       replace = FALSE)]
                           }
                         }
                         
                         ## return output
                         return(files_i)
                       },
                       input_dir = input_dir,
                       n = n,
                       random = random)
  
  ## convert list content to vector
  files_cube <- unlist(files_cube)
  
  ## create raw gps file output directory
  if(dir.exists(paths = paste(output_dir, 
                              "/gps_raw", 
                              sep = "")) == FALSE) {
    
    dir.create(path = paste(output_dir, "/gps_raw", sep = ""), 
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
  
  ## generate notification
  note_1 <- paste("[eseis::aux_stationinfofile]: ",
                  "Started create station info file.\n",
                  "   Job started:", Sys.time(), ".\n", 
                  "   ", length(station_ID), "loggers to process.\n",
                  "   ", length(files_cube), "files to process.\n",
                  "   ", round(x = t_duration_estimate, 
                               digits = 2), "h estimated duration.",
                collapse = "")
  
  ## print notification
  cat(note_1)

  ## Part 3 - extraction of GPS data ------------------------------------------

  ## initiate cluster
  cl <- parallel::makeCluster(getOption("mc.cores", cores))
  
  ## extract GPS data
  invisible(parallel::parLapply(
    cl = cl, 
    X = files_cube, 
    fun = function(X, gipptools, output_dir) {
      
      system(command = paste(gipptools, "/bin/cubeinfo", 
                             " --format=GPS --output-dir=",
                             output_dir, "/gps_raw ",
                             X,
                             sep = ""))
      }, 
    gipptools = gipptools,
    output_dir = output_dir))
  
  ## stop cluster
  parallel::stopCluster(cl = cl)
  
  ## Part 4 - calculations of GPS data ----------------------------------------
  
  ## get all gps raw files
  gps_files <- list.files(path = paste(output_dir, "/gps_raw", sep = ""), 
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
        
        ## extract latitude
        lat <- c(lat, as.numeric(substr(x = data_i$V8, 
                                        start = 5, 
                                        stop = nchar(data_i$V8[1]))))
        
        ## extract longitude
        lon <- c(lon, as.numeric(substr(x = data_i$V9, 
                                        start = 5, 
                                        stop = nchar(data_i$V9[1]))))
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
      
      ## convert coordinates
      rgdal::project(xy = x, 
                     proj = paste("+proj=utm +zone=", 
                                  floor(median(utm_zone, na.rm = TRUE)), 
                                  " ellps=WGS84", 
                                  sep = ""))
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
    
    file.remove(gps_files)
    file.remove(paste(output_dir, "gps_raw", sep = "/"))
  }
  
  ## Part 5 - export output data ----------------------------------------------
  
  ## optionally save station info file
  if(write_file == TRUE) {
    
    utils::write.table(station_info, 
                       file = paste(output_dir, 
                                    "/",
                                    name,
                                    ".txt",
                                    sep = ""), 
                       col.names = TRUE, 
                       row.names = FALSE, 
                       quote = TRUE,
                       sep = "\t")
  }
  
  ## optionally save GPS data set
  if(write_data == TRUE) {
    
    save(gps_cube, 
         file = paste(output_dir, 
                      "/",
                      name,
                      ".rda",
                      sep = ""))    
  }

  ## get end time
  t_2 <- Sys.time()
  
  ## calculate duration
  duration_processing <- difftime(time1 = t_2, 
                                  time2 = t_1, 
                                  units = "hours")
  
  ## generate notification
  note_2 <- paste("[eseis::aux_stationinfofile]: ",
                  "Finished create station info file.\n",
                  "   Job done: ", Sys.time(), ".\n", 
                  "   Duration: ", round(x = duration_processing, 
                                         digits = 2), " h.",
                  collapse = "")
  
  ## print notification
  cat(note_2)
  
  ## return result
  return(station_info)
}