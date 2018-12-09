#' Read sac files.
#'
#' This function reads sac files.
#'
#' The function reads one or more sac-files. If \code{append = TRUE}, all
#' files will be appended to the first one in the order as they are provided. 
#' In the append-case the function returns a either a list with the elements 
#' \code{signal}, \code{time}, \code{meta} and \code{header} or a list of the 
#' class \code{eseis} (see documentation of 
#' \code{aux_initiateeseis}). If \code{append = FALSE} and more than one file 
#' is provided, the function returns a list of the length of the input files, 
#' each containing the above elements. \cr\cr The sac data format is 
#' implemented as descibed on the IRIS website 
#' (https://ds.iris.edu/files/sac-manual/manual/file_format.html).
#' 
#' @param file \code{Character} vector, input file name(s), with extension. 
#' 
#' @param append \code{Logical} value, option append single files to one
#' continuous file, keeping only the header information of the first file,
#' default is \code{TRUE}.
#' 
#' @param signal \code{Logical} value, option to import the signal vector, 
#' default is \code{TRUE}.
#' 
#' @param time \code{Logical} value, option to create the time vector. The 
#' timezone is automatically set to \code{"UTC"}, default is \code{TRUE}.
#' 
#' @param meta \code{Logical} value, option to append the meta data part, 
#' default is \code{TRUE}.
#' 
#' @param header \code{Logical} value, option to append the header part, 
#' default is \code{TRUE}.
#' 
#' @param eseis \code{Logical} value, option to read data to an \code{eseis}
#' object (recommended, see documentation of 
#' \code{aux_initiateeseis}), default is \code{TRUE}
#' 
#' @param get_instrumentdata \code{Logical} value, option to fill meta 
#' information (sensor name, logger name, logger gain) from SAC user fields 
#' (field 127-129, KUSER0-KUSER2). Default is \code{FALSE}.
#' 
#' @param endianness \code{Logical} value, endianness of the sac file. One
#' out of \code{"little"}, \code{"big"} and \code{"swap"}. Default 
#' is \code{"little"}.
#' 
#' @param biglong \code{Logical} value, number coding format. Default 
#' is \code{FALSE}.
#' 
#' @param type \code{Character} value, type keyword of the data. One out of 
#' \code{"waveform"}, \code{"envelope"}, \code{"fft"}, \code{"spectrum"}, 
#' \code{"spectrogram"}, \code{"other"}, \code{hilbert}, \code{hvratio}. 
#' Default is \code{"waveform"}.
#' 
#' @return \code{List} object, optionally of class \code{eseis}.
#' 
#' @author Michael Dietze
#' 
#' @examples
#'
#' \dontrun{
#' ## read one file
#' file1 <- "~/Data/sac/EXMP01.14.213.01.00.00.BHE.SAC"
#' 
#' sac1 <- read_sac(file = file1)
#' 
#' ## read two (or more files) without meta and header parts
#' file2 <- c("~/Data/sac/EXMP01.14.213.01.00.00.BHE.SAC",
#'            "~/Data/sac/EXMP01.14.213.02.00.00.BHE.SAC")
#' 
#' sac2 <- read_sac(file = file2, 
#'                  meta = FALSE, 
#'                  header = FALSE,
#'                  eseis = FALSE)
#' }
#'
#' @export read_sac
read_sac <- function(
  file,
  append = TRUE,
  signal = TRUE,
  time = TRUE,
  meta = TRUE,
  header = TRUE,
  eseis = TRUE,
  get_instrumentdata = FALSE,
  endianness = "little",
  biglong = FALSE,
  type = "waveform"
) {
  
  ## collect function arguments
  eseis_arguments <- list(file = file,
                          append = append,
                          signal = signal,
                          time = time,
                          meta = meta,
                          header = header,
                          eseis = eseis,
                          endianness = endianness,
                          biglong = biglong,
                          type = type)
  
  ## check/select files
  if(sum(grepl(pattern = "[*]", x = file)) > 0) {
    
    file <- list.files(pattern = file)
  }
  
  ## define object lengths
  if(biglong == TRUE) {
    
    length_object <- c(2, 4, 8, 4, 8)
  } else {
    
    length_object <- c(2, 4, 4, 4, 8)
  }
  
  ## define object names
  names(length_object) <- c("short", "int", "long", "float", "double")
  
  ## define object structure
  name_sac <- c("DELTA", "DEPMIN", "DEPMAX", "SCALE", "ODELTA", 
                "B", "E", "O", "A", "INTERNAL", 
                "T0", "T1", "T2", "T3", "T4", 
                "T5", "T6", "T7", "T8", "T9", 
                "F", "RESP0", "RESP1", "RESP2", "RESP3", 
                "RESP4", "RESP5", "RESP6", "RESP7", "RESP8", 
                "RESP9", "STLA", "STLO", "STEL", "STDP", 
                "EVLA", "EVLO", "EVEL", "EVDP", "MAG", 
                "USER0", "USER1", "USER2", "USER3", "USER4", 
                "USER5", "USER6", "USER7", "USER8", "USER9", 
                "DIST", "AZ", "BAZ", "GCARC", "INTERNAL", 
                "INTERNAL", "DEPMEN", "CMPAZ", "CMPINC", "XMINIMUM", 
                "XMAXIMUM", "YMINIMUM", "YMAXIMUM", "UNUSED", "UNUSED", 
                "UNUSED", "UNUSED", "UNUSED", "UNUSED", "UNUSED", 
                "NZYEAR", "NZJDAY", "NZHOUR", "NZMIN", "NZSEC", 
                "NZMSEC", "NVHDR", "NORID", "NEVID", "NPTS", 
                "INTERNAL", "NWFID", "NXSIZE", "NYSIZE", "UNUSED", 
                "IFTYPE", "IDEP", "IZTYPE", "UNUSED", "IINST", 
                "ISTREG", "IEVREG", "IEVTYP", "IQUAL", "ISYNTH", 
                "IMAGTYP", "IMAGSRC", "UNUSED", "UNUSED", "UNUSED", 
                "UNUSED", "UNUSED", "UNUSED", "UNUSED", "UNUSED", 
                "LEVEN", "LPSPOL", "LOVROK", "LCALDA", "UNUSED", 
                "KSTNM", "KEVNM", 
                "KHOLE", "KO", "KA", 
                "KT0", "KT1", "KT2", 
                "KT3", "KT4", "KT5", 
                "KT6", "KT7", "KT8", 
                "KT9", "KF", "KUSER0", 
                "KUSER1", "KUSER2", "KCMPNM", 
                "KNETWK", "KDATRD", "KINST")
  
  type_sac <- c(rep("float", times = 70), 
                rep("long", times = 40), 
                rep("char", times = 23))
  
  length_sac <- c(rep(x = length_object[4], times = 70),
                  rep(x = length_object[3], times = 40),
                  rep(x = length_object[5], times = 23))
  length_sac[112] <- 16
  
  
  if(eseis == TRUE) {
    
    data_list <- lapply(X = 1:length(file), 
                        FUN = function(X) {
                          eseis::aux_initiateeseis()
                        })
  } else {
    
    data_list <- vector(mode = "list", 
                        length = length(file))
  }
  
  for(j in 1:length(data_list)) {
    
    ## get start time
    t_0 <- Sys.time()
    
    file_read <- file(description = file[j], 
                      open = "rb")
    
    header <- character(length = length(name_sac))
    
    for (i in 1:70) {
      header[i] <- readBin(con = file_read, 
                           what = numeric(), 
                           n = 1, 
                           size = length_sac[i], 
                           signed = TRUE, 
                           endian = endianness)
    }
    
    for (i in 71:110) {
      header[i] <- readBin(con = file_read, 
                           what = integer(), 
                           n = 1, 
                           size = length_sac[i], 
                           signed = TRUE, 
                           endian = endianness)
    }
    
    for (i in 111:length(name_sac)) {
      header[i] = readChar(con = file_read, 
                           length_sac[i], 
                           useBytes = FALSE)
    }
    
    dt <- signif(x = as.numeric(header[1]), digits = 6)
    
    location_station <- as.numeric(header[32:35])
    location_station <- ifelse(test = location_station == -12345, 
                               yes = NA, 
                               no = location_station)
    
    time_JD <- eseis::time_convert(input = as.numeric(header[72]),
                                   output = "yyyy-mm-dd", 
                                   year = as.numeric(header[71]))
    time_hour <- ifelse(test = nchar(header[73]) == 1, 
                        yes = paste("0", header[73], sep = ""),
                        no = header[73])
    time_min <- ifelse(test = nchar(header[74]) == 1, 
                       yes = paste("0", header[74], sep = ""),
                       no = header[74])
    time_sec <- as.numeric(header[75]) + as.numeric(header[76]) / 1000
    time_sec <- ifelse(test = nchar(header[75]) == 1, 
                       yes = paste("0", header[75], sep = ""),
                       no = header[75])
    
    time_start <- as.POSIXct(strptime(x = paste(time_JD, " ", 
                                                time_hour, ":",
                                                time_min, ":",
                                                time_sec, sep = ""),
                                      format = "%Y-%m-%d %H:%M:%OS"), 
                             tz = "UTC")
    
    name_station <- ifelse(test = header[111] == "-12345  ",
                           yes = NA,
                           no = trimws(header[111]))
    
    name_network <- ifelse(test = header[131] == "-12345  ",
                           yes = NA,
                           no = trimws(header[131]))
    
    name_sensor <- ifelse(test = get_instrumentdata == TRUE,
                          yes = trimws(header[127]),
                          no = NA)
    
    name_logger <- ifelse(test = get_instrumentdata == TRUE,
                          yes = trimws(header[128]),
                          no = NA)
    
    gain_logger <- ifelse(test = get_instrumentdata == TRUE,
                          yes = as.numeric(trimws(header[129])),
                          no = NA)
    
    channel <- ifelse(test = header[130] == "-12345  ",
                      yes = NA,
                      no = trimws(header[130]))
    
    n_data <- as.numeric(header[80])
    
    meta <- list(station = name_station,
                 network = name_network,
                 component = channel,
                 n = n_data,
                 sensor = name_sensor,
                 logger = name_logger,
                 gain = gain_logger,
                 starttime = time_start,
                 dt = dt,
                 latitude = location_station[1],
                 longitude = location_station[2],
                 elevation = location_station[3],
                 depth = location_station[4],
                 filename = file,
                 type = eseis_arguments$type)
    
    if(signal == TRUE) {
      
      data_sac <- readBin(con = file_read, 
                          what = numeric(), 
                          n = n_data, 
                          size = length_object[4], 
                          endian = endianness, 
                          signed = TRUE)
      
    } else {
      
      data_sac <- NA
    }
    
    close(con = file_read)
    
    ## optionally create time vector
    if(time == TRUE) {
      time_sac <- data.frame(seq(from = time_start, 
                                 by = dt, 
                                 length.out = length(data_sac)))
      
    }
    
    if(eseis == TRUE) {
      
      ## calculate function call duration
      eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                            time2 = t_0, 
                                            units = "secs"))
      
      ## fill eseis object
      data_list[[j]]$signal <- data_sac
      data_list[[j]]$meta <- meta
      data_list[[j]]$header <- header
      data_list[[j]]$history[[length(data_list[[j]]$history) + 1]] <- 
        list(time = Sys.time(),
             call = "read_sac()",
             arguments = eseis_arguments,
             duration = eseis_duration)
      names(data_list[[j]]$history)[length(data_list[[j]]$history)] <- 
        as.character(length(data_list[[j]]$history))
      
    } else {
      
      ## fill data object
      data_list[[j]] <- list(signal = data_sac,
                             time = time_sac,
                             meta = meta,
                             header = header)
    }
  }
  
  ## optionally append data sets to first one
  if(append == TRUE) {
    
    ## concatanate signal vectors
    data_append <- as.numeric(unlist(lapply(X = data_list, FUN = function(x) {
      x$signal
    })))
    
    ## concatanate time vectors
    time_append <- do.call(rbind, lapply(X = data_list, FUN = function(x) {
      x$time
    }))
    
    ## assign first data set to output data set
    data_out <- data_list[[1]]
    
    ## insert appended signal vector in output data set
    data_out$signal <- data_append
    
    if(eseis == TRUE) {
      
      ## update processing duration
      step_process <- length(data_out$history)
      data_out$history[[step_process]]$duration <- 
        as.numeric(difftime(time1 = Sys.time(), 
                            time2 = data_out$history[[step_process]]$time, 
                            units = "secs"))
    } else {
      
      ## insert appended time vector in output data set
      data_out$time <- time_append[,1]
    }
    
    ## update data set length
    data_out$meta$n <- length(data_append)
    
  } else {
    
    ## assign initial data set to output data set
    data_out <- data_list
  }
  
  ## return data set
  return(data_out)
}
