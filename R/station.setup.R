station.setup <-
structure(function(# Function to set a seismic station setup.
  ### This function sets the setup parameters of seismic stations. A
  ### station is considered here as the combination of sensor and data logger.
  ID,
  ### \code{Character} scalar, ID or name of the station.
  type.sensor,
  ### \code{Character} scalar, type of the sensor
  type.logger,
  ### \code{Character} scalar, ID or name of the logger.
  coordinates,
  ### \code{Numeric} vector, x-, y- and z-coordinate of the station location.
  poles,
  ### \code{Complex} vector, real and imaginary parts of the pole values (e.g.
  ### \code{c(-0.037-0.037i, -343+0i)}).
  zeros,
  ### \code{Complex} vector, real and imaginary parts of the zero values.
  k,
  ### \code{Numeric} scalar, normalisation factor
  s,
  ### \code{Numeric} scalar, Ground motion sensitivity.
  gain,
  ### \code{Numeric} scalar, Gain of the logger.
  AD
  ### \code{Numeric} scalar, analog-digital conversion factor of the logger.
){
  ## create output data set
  data <- list(ID = ID,
               type.sensor = character(0),
               type.logger = character(0),
               coordinates = numeric(0),
               poles = complex(0),
               zeros = complex(0),
               k = numeric(0),
               s = numeric(0),
               gain = numeric(0),
               AD = numeric(0))
  
  ## set ID
  if(missing(ID) != TRUE) {
    data$ID <- ID
  }
  
  ## set sensor type
  if(missing(type.sensor) != TRUE) {
    data$type.sensor <- type.sensor
  }
  
  ## set logger type
  if(missing(type.logger) != TRUE) {
    data$type.logger <- type.logger
  }
  
  ## set coordinates
  if(missing(coordinates) != TRUE) {
    data$coordinates <- coordinates
  }
  
  ## set poles
  if(missing(poles) != TRUE) {
    data$poles <- poles
  }

  ## set zeros
  if(missing(zeros) != TRUE) {
    data$zeros <- zeros
  }

  ## set k
  if(missing(k) != TRUE) {
    data$k <- k
  }
  
  ## set s
  if(missing(s) != TRUE) {
    data$s <- s
  }
  
  ## set gain
  if(missing(gain) != TRUE) {
    data$gain <- gain
  }
  
  ## set AD
  if(missing(AD) != TRUE) {
    data$AD <- AD
  }
  
  return(data)
  ### A list object with the station setup
  
  ##references<<
  
  ##keyword<<
  ## environmental seismology
  
}, ex = function(){

  ## Generate setup for a station with a Trillium Compact 120 s seismometer
  ## and an Omnirecs Data Cube 3 ext logger.
  station.01 <- station.setup(ID = "Station 01",
                              type.sensor = "Trillium Compact 120 s",
                              type.logger = "Data Cube 3 ext",
                              coordinates = c(5395031, 5651928, 600),
                              poles = c(-3.691e-02+3.702e-02i,
                                        -3.691e-02-3.702e-02i,
                                        -3.430e+02+0.000e+00i,
                                        -3.700e+02+4.670e+02i,
                                        -3.700e+02-4.670e+02i,
                                        -8.360e+02+1.522e+03i,
                                        -8.360e+02-1.522e+03i,
                                        -4.900e+03+4.700e+03i,
                                        -4.900e+03-4.700e+03i,
                                        -6.900e+03+0.000e+00i,
                                        -1.500e+04+0.000e+00i),
                              zeros = c(0+0i, 
                                        0+0i, 
                                        -392+0i, 
                                        -1960+0i, 
                                        -1490+1740i, 
                                        -1490-1740i),
                              k = 4.34493e+17,
                              s = 754.3,
                              gain = 1,
                              AD = 2.441406e-07)
})
