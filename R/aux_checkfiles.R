#' Check structured seismic files for consistency
#' 
#' The function checks seismic files organised by \code{aux_organisecubefiles} 
#' or \code{aux_organisecentaurfiles} for completeness. The tests include 
#' agreement of file name and seismic file meta data.
#' 
#' @param dir \code{Character} value, path to directorythat contains the 
#' seismic files to check.
#' 
#' @param station \code{Character} value, ID of the station of which the files 
#' will be checked. ID must match the IDs as used in the file names.
#' 
#' @param component \code{Character} value, seismic component, which must
#' correspond to the component name in the file name of the data directory  
#' structure (cf. \code{aux_organisecubefiles}). Default is 
#' \code{"BHZ"} (vertical component of a sac file).
#' 
#' @param method \code{Character} value, method used to check files. One out 
#' of \code{"fast"} (only file names are used to check the files, the actual 
#' files are not read and examined) and \code{"thorough"} (files are imported
#' to R and their meta data will be used for checking, as well). Option 
#' \code{"thorough"} is many times slower. Default is \code{"thorough"}.
#' 
#' @param period \code{Character} value, aggregation period, i.e., the time 
#' period used to generate plots. One out of \code{"total"},\code{"yearly"}, 
#' \code{"monthly"}, \code{"weekly"}, \code{"daily"}. Default is 
#' \code{"weekly"}.
#' 
#' @param format \code{Character} value, seismic data format. One out of 
#' \code{"sac"} and \code{"mseed"}. Default is \code{"sac"}.
#' 
#' @param duration_set \code{Numeric} value, anticipated length of the seismic 
#' time series of the files to test in seconds. Default is \code{3600} (one 
#' hour).
#' 
#' @param plot \code{Logical} value, option to visualise the output of the 
#' function. Default is \code{TRUE}.
#' 
#' @return \code{Data frame} containing check results and meta data.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## set seismic data directory
#' dir_data <- paste0(system.file("extdata", package="eseis"), "/")
#' 
#' ## check data archive for record completeness
#' chk <- aux_checkfiles(dir = dir_data, 
#'                       station = "RUEG1", 
#'                       component = "BHZ",
#'                       plot = TRUE)
#' 
#' }
#'
#' @export aux_checkfiles
#' 
aux_checkfiles <- function(
  
  dir,
  station,
  component = "BHZ",
  method = "thorough",
  period = "weekly",
  format = "sac",
  duration_set = 3600,
  plot = TRUE
) {
  
  ## get system time zone
  tz_system <- Sys.timezone()
  
  if(tz_system != "UTC") {
    
    ## set system time zone to input data time zone
    Sys.setenv(TZ = "UTC")
    
    ## create information message
    tz_message <- paste("System time zone changed to event time zone. ",
                        "Undo with Sys.setenv(TZ = '",
                        tz_system,
                        "')",
                        sep = "")
    
    ## inform about time zone change
    print(tz_message)
  }
  
  ## check/set station length
  if(length(station) > 1) {
    
    station <- station[1]
    
    print("Only one station ID can be processed. Using first entry")
  }
  
  ## check/set component length
  if(length(component) > 1) {
    
    component <- component[1]
    
    print("Only one component can be processed. Using first entry")
  }
  
  
  ## make list of files with file names only
  files_check <- list.files(path = dir, 
                                 full.names = FALSE, 
                                 recursive = TRUE)
  
  ## keep only matching stations
  files_check <- files_check[grepl(x = files_check, 
                                   pattern = station)]
  
  ## keep only matching components
  files_check <- files_check[grepl(x = files_check, 
                                   pattern = component)]
  
  ## GATHER INFORMATION -------------------------------------------------------
  
  ## convert file names to meta data
  file_meta <- lapply(X = files_check, FUN = function(x) {
    
    x_meta_1 <- strsplit(x = x, 
                         split = "/")[[1]]
    
    x_meta_2 <- x_meta_1[length(x_meta_1)]
    
    x_meta_3 <- strsplit(x = x_meta_2, 
                         split = ".", 
                         fixed = TRUE)[[1]]
    
    x_meta_4 <-  paste(x_meta_3[2:6], collapse = "-")
    
    x_meta_1 <- x_meta_1[-length(x_meta_1)]
    
    x_meta_out <- c(x_meta_2,
                    x_meta_1, 
                    x_meta_3, 
                    x_meta_4)
  })

  ## convert list to data frame
  file_meta <- as.data.frame(do.call(rbind, file_meta), 
                             stringsAsFactors = FALSE)

  ## append file name and path data  
  file_meta <- cbind(files_check, file_meta)
  file_meta$files_check <- as.character(file_meta$files_check)
  
  ## assign variable names
  names(file_meta) <- c("path_file",
                        "name_file",
                        "year_path",
                        "jd_file", 
                        "id_file", 
                        "year_file", 
                        "jd_file", 
                        "hour_file", 
                        "minute_file",
                        "second_file", 
                        "component_file", 
                        "format_file",
                        "starttime")
  
  ## convert time string to POSIXct type
  file_meta$starttime <- as.POSIXct(x = file_meta$starttime, 
                                    format = "%y-%j-%H-%M-%S", 
                                    tz = "UTC")
  
  ## optionally read seismic files
  if(method == "thorough") {
    
    ## try to read file
    file_meta_detail <- lapply(
      X = files_check, FUN = function(f, dir, format) {
        
        if(format == "sac") {
          
          x <- try(eseis::read_sac(file = paste0(dir, "/", f)))
        } else if(format == "mseed") {
          
          x <- try(eseis::read_mseed(file = paste0(dir, "/", f)))
        }
        
        ## proceed if successful
        if(class(x) == "eseis") {
          
          ## extract meta data
          meta_out <- data.frame(readable_detail = TRUE,
                                 station_detail = trimws(x$meta$station),
                                 component_detail = trimws(x$meta$component),
                                 n_detail = x$meta$n,
                                 starttime_detail = x$meta$starttime,
                                 dt_detail = x$meta$dt)
        } else {
          
          ## account for unsuccessful file import
          meta_out <- data.frame(readable_detail = FALSE,
                                 station_detail = "NA",
                                 component_detail = "NA",
                                 n_detail = NA,
                                 starttime_detail = as.POSIXct(NA),
                                 dt_detail = NA)
        }
        
        ## return output
        return(meta_out)
        
      }, dir, format)
    
    ## convert list to data frame
    file_meta_detail <- do.call(rbind, file_meta_detail)
    
  } else {
    
    file_meta_detail <- data.frame(
      readable_detail = rep(NA, length(files_check)),
      station_detail = rep(NA, length(files_check)),
      component_detail = rep(NA, length(files_check)),
      n_detail = rep(NA, length(files_check)),
      starttime_detail = as.POSIXct(rep(NA, length(files_check))),
      dt_detail = rep(NA, length(files_check)))
  }
  
  ## append detailed information
  file_meta <- cbind(file_meta, file_meta_detail)
  
  
  ## get range of data set
  t_range <- c(min(file_meta$starttime, na.rm = TRUE), 
               max(file_meta$starttime, na.rm = TRUE) + duration_set)
  
  ## make sequence of expected start times
  t_expect <- c(t_range[1],
                seq(from = as.POSIXct(x = format(t_range[1],
                                                 "%Y-%m-%d %H:00:00"),
                                      format = "%Y-%m-%d %H:00:00",
                                      tz = "UTC") + duration_set, 
                    to = as.POSIXct(x = format(t_range[2] - duration_set, 
                                               "%Y-%m-%d %H:00:00"),
                                    format = "%Y-%m-%d %H:00:00", 
                                    tz = "UTC"),
                    by = duration_set))
  
  ## match starttimes
  t_match <- match(x = t_expect, 
                   table = file_meta$starttime)
  
  ## create index vector
  t_fill <- na.omit(seq(from = 1, to = length(t_match)) * (t_match * 0 + 1))

  ## create data frame with expected files
  file_expect <- data.frame(starttime_expect = t_expect,
                            present = rep(NA, length(t_expect)),
                            path = rep(NA, length(t_expect)),
                            file = rep(NA, length(t_expect)),
                            component = rep(NA, length(t_expect)),
                            format = rep(NA, length(t_expect)),
                            starttime_file = rep(NA, length(t_expect)),
                            readable_detail = rep(NA, length(t_expect)),
                            station_detail = rep(NA, length(t_expect)),
                            component_detail = rep(NA, length(t_expect)),
                            n_detail = rep(NA, length(t_expect)),
                            starttime_detail = rep(NA, length(t_expect)),
                            dt_detail = rep(NA, length(t_expect)), 
                            stringsAsFactors = FALSE)
  
  ## fill data frame
  file_expect$present <- !is.na(t_match)
  file_expect$path[t_fill] <- as.character(file_meta$path_file)
  file_expect$file[t_fill] <- as.character(file_meta$name_file)
  file_expect$component[t_fill] <- as.character(file_meta$component_file)
  file_expect$format[t_fill] <- as.character(file_meta$format_file)
  file_expect$starttime_file[t_fill] <- file_meta$starttime
  file_expect$readable_detail[t_fill] <- file_meta$readable_detail
  file_expect$station_detail[t_fill] <- as.character(file_meta$station_detail)
  file_expect$component_detail[t_fill] <- as.character(file_meta$component_detail)
  file_expect$n_detail[t_fill] <- file_meta$n_detail
  file_expect$starttime_detail[t_fill] <- file_meta$starttime_detail
  file_expect$dt_detail[t_fill] <- file_meta$dt_detail

  ## correct time formats
  file_expect$starttime_file <- as.POSIXct(file_expect$starttime_file, 
                                           origin = "1970-01-01")
  file_expect$starttime_detail <- as.POSIXct(file_expect$starttime_detail, 
                                           origin = "1970-01-01")
  
  ## build data set
  d <- file_expect
  
  ## PERFORM CHECKS -----------------------------------------------------------
  
  ## check if file start as expected
  if(method == "thorough") {
    
    starttime_ok <- d$starttime_expect == d$starttime_detail
  } else {
    
    starttime_ok <- d$starttime_expect == d$starttime_file
  }
  
  ## add test data to output data set
  d_out <- cbind(d, starttime_ok)
  
  ## correct data for missing files
  d_out$readable_detail[is.na(d_out$readable_detail)] <- FALSE
  d_out$starttime_ok[is.na(d_out$starttime_ok)] <- FALSE
  
  ## OPTIONALLY GENERATE PLOT -------------------------------------------------
  
  if(plot == TRUE) {
    
    ## create sorting period time format
    if(period == "total") {
      
      t_sort <- rep(TRUE, nrow(d_out))
    } else if(period == "yearly") {
      
      t_sort <- format(d_out$starttime_expect, format = "%Y")
    } else if(period == "monthly") {
      
      t_sort <- format(d_out$starttime_expect, format = "%Y-%m")
    } else if(period == "weekly") {
      
      t_sort <- format(d_out$starttime_expect, format = "%Y-%W")
    } else if(period == "daily") {
      
      t_sort <- format(d_out$starttime_expect, format = "%Y-%j")
    }
    
    ## get unique values
    t_unique <- unique(t_sort)
    
    ## add sorting time stamp to data set
    d_sort <- cbind(d_out, t_sort)
    
    ## organise data by specified period
    d_plot <- lapply(X = t_unique, FUN = function(t_unique, d_sort) {
      
      return(d_sort[d_sort$t_sort == t_unique,])
    }, d_sort)

    ## get time ranges
    t_plot <- lapply(X = d_plot, FUN = function(d_plot, duration_set) {
      
      range(d_plot$starttime_expect) + c(0, duration_set)
    }, duration_set)
    t_plot <- do.call(rbind, t_plot)
    t_plot <- as.POSIXct(c(min(t_plot[,1]), 
                           max(t_plot[,2])), 
                         origin = "1970-01-01")
    
    ## generate plot
    plot(NA, 
         xlim = t_plot, 
         ylim = c(1, length(d_plot)), 
         ann = FALSE, 
         axes = FALSE)
    
    box(which = "plot")
    
    axis.POSIXct(side = 1, 
                 x = t_plot)
    
    axis(side = 2, 
         at = 1:length(d_plot), 
         labels = t_unique)
    
    for(i in 1:length(d_plot)) {
    
      ## create sorting period time format
      if(period == "total") {
        
        t_plot_2 <- d_plot[[i]]$starttime_file
      } else if(period == "yearly") {
        
        t_plot_2 <- as.numeric(x = format(x = d_plot[[i]]$starttime_file, 
                                          format = "%m"))
      } else if(period == "monthly") {
        
        t_plot_2 <- as.numeric(x = format(x = d_plot[[i]]$starttime_file, 
                                          format = "%d"))
      } else if(period == "weekly") {
        
        t_plot_2 <- as.numeric(x = format(x = d_plot[[i]]$starttime_file, 
                                          format = "%w"))
      } else if(period == "daily") {
        
        t_plot_2 <- as.numeric(x = format(x = d_plot[[i]]$starttime_file, 
                                          format = "%H"))
      }
      
        
      lines(x = c(min(d_plot[[i]]$starttime_file, na.rm = TRUE), 
                   max(d_plot[[i]]$starttime_file, na.rm = TRUE) + 
                    duration_set), 
            y = c(i, i),
            col = "red",
            lwd = 0.8)
      
      segments(x0 = d_plot[[i]]$starttime_file, 
               y0 = i, 
               x1 = d_plot[[i]]$starttime_file + duration_set, 
               y1 = i, lwd = 1.5)
    }
  }
  
  return(d_out)
}