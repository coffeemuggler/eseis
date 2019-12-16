#' List all header parameters of a sac file.
#' 
#' The function returns a data frame with all header values of a sac file. It 
#' may be used for advanced modifications by the user.
#' 
#' @return \code{List} object, parameters supported by a sac file.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## show sac parameters
#' list_sacparameters()
#'                      
#' @export list_sacparameters
list_sacparameters <- function(
) {
  
  ## define sac parameters
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
  
 value_sac <- c("mean(diff(time))", "min(data)", "max(data)", "1", "-12345",
                 "min(time)", "max(time)", "-12345", "-12345", "-12345",
                 "-12345", "-12345", "-12345", "-12345", "-12345",
                 "-12345", "-12345", "-12345", "-12345", "-12345",
                 "-12345", "-12345", "-12345", "-12345", "-12345", 
                 "-12345", "-12345", "-12345", "-12345", "-12345",
                 "-12345", "location[1]", "location[2]", "location[3]", "location[4]", 
                 "0", "0", "-12345", "-12345", "-12345",
                 "-12345", "-12345", "-12345", "-12345", "-12345",
                 "-12345", "-12345", "-12345", "-12345", "-12345", 
                 "0", "-12345", "-12345", "-12345", "-12345",
                 "-12345", "mean(data)", "0", "0", "-12345",
                 "-12345", "-12345", "-12345", "-12345", "-12345", 
                 "-12345", "-12345", "-12345", "-12345", "-12345", 
                 "from time", "from time", "from time", "from time", "from time", 
                 "from time", "6", "-12345", "-12345", "length(data)",
                 "-12345", "-12345", "-12345", "-12345", "-12345", 
                 "1", "unit", "9", "-12345", "-12345",
                 "-12345", "-12345", "-12345", "-12345", "-12345", 
                 "-12345", "-12345", "-12345", "-12345", "-12345", 
                 "-12345", "-12345", "-12345", "-12345", "-12345", 
                 "1", "1", "1", "0", "-12345",
                 "station", "-12345",
                 "XX", "-12345", "-12345",
                 "-12345", "-12345", "-12345", 
                 "-12345", "-12345", "-12345", 
                 "-12345", "-12345", "-12345", 
                 "-12345", "-12345", "-12345", 
                 "-12345", "-12345", "component",
                 "network", "-12345", "-12345") 
 
 type_sac <- c(rep("float", times = 70), 
               rep("long", times = 40), 
               rep("char", times = 23))
 
  sacparameters <- data.frame(parameter = name_sac,
                              value = value_sac,
                              type = type_sac)
  ## return output
  return(sacparameters)
}
 