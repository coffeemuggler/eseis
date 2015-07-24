convert.JD2date <- function(# Function to convert Julian Days to POSIXlt dates and vice versa.
  ### This function converts time Julian Days to POSIXlt dates and vice versa.
  ### It uses function from the package \code{date}.

  date,
  ### \code{Numeric} vector, data set to be processed.
  
  format = "mm-dd",
  ### \code{Character} scalar, output format. One out of \code{"mm-dd"} ( 
  ### month and day), \code{"POSIXct"} (a POSIXct-object). Default is 
  ### \code{"ddmm"}. Note that for the latter option a year must be provided 
  ### or the current year will be set by default.
  
  year,
  ### \code{Numeric} scalar, optional year to be used for conversion.
  
  timezone = "UTC",
  ### \code{Character} vector, time zone of the output date.
  
  reverse = FALSE
  ### \code{Logical} scalar, option to reverse calculation (i.e., convert 
  ### POSIXct dates to Julian Days).
){
  
  ## check/set parameters
  if(missing(year) == TRUE) {
    year <- rep(x = as.numeric(strptime(Sys.Date(), 
                                        format = "%Y-%m-%d")$year + 1900),
                length(date))
  }
  
  ## decide conversion direction
  if(reverse == FALSE) {
    
    ## check input data
    if(sum(JD > 366) > 0) {
      stop("Input data is beyond Julian day 366!")
    }
    
    ## create date object
    t <- date::as.date(x = date)
    t <- date::date.mdy(sdate = t)
    t$year <- year
    
    ## convert to output format
    if(format == "mm-dd") {
      
      ## check/add zero to month
      t.mm <- as.character(t$month)
      t.mm[nchar(t.mm) == 1] <- paste("0", t.mm[nchar(t.mm) == 1], sep = "")

      ## check/add zero to day
      t.dd <- as.character(t$day)
      t.dd[nchar(t.dd) == 1] <- paste("0", t.dd[nchar(t.dd) == 1], sep = "")
      
      ## create output date
      t.out <- paste(t.mm, "-", t.dd, sep = "")
      
    } else if(format == "POSIXct") {
      
      t.out <- as.POSIXct(x = strptime(x = paste(t$year, 
                                                 t$month, 
                                                 t$day, 
                                                 sep = "-"),
                                       format = "%Y-%m-%d"), 
                          tz = timezone)
      
    }
    
  } else {
    
    t.out <- strptime(x = date, format = "%Y-%m-%d")$yday + 1
  }
  
  return(t.out)
  ### \code{Numeric} vector of dates.
  
 
}
