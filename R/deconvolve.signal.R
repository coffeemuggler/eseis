deconvolve.signal <-
structure(function(# Function to deconvolve a signal.
  ### This function deconvolves the signal from the instrument response.

  data,
  ### \code{list} object, data set to be converted. The signal part must be in
  ### counts. ATTENTION, CURRENTLY ONLY MATRICES ARE SUPPORTED!!!!!
  
  output = "velocity",
  ### \code{Character} scalar, output data type. One out of \code{"velocity"},
  ### \code{"displacement"} or \code{"acceleration"}, default is 
  ### \code{"velocity"}.
  
  setup,
  ### \code{List} object with station setup information (cf. 
  ### \code{station.setup}), needed to convert counts into m/s. If
  ### missing, the necessary paramters need to be specified manually.
  
  detrend = TRUE,
  ### \code{Logical} scalar, option to detrend data set prior to 
  ### deconvolution, default is \code{FALSE}.
  
  subtract.mean = TRUE,
  ###  \code{Logical} scalar, optionally remove the global mean before 
  ### processing, default is \code{TRUE}.
  
  waterlevel = 10^-6,
  ### \code{Numeric} scalar, providing the waterlevel value for frequency
  ### division, default is \code{10^-6}.
  
  dt,
  ### \code{Numeric} scalar, sampling rate.
  
  k,
  ### \code{Numeric} scalar, optional k-factor. Needed if setup object
  ### is missing.
  
  s,
  ### \code{Numeric} scalar, optional s-factor. Needed if setup object
  ### is missing.
  
  gain,
  ### \code{Numeric} scalar, optional gain-factor. Needed if setup object
  ### is missing.
  
  AD,
  ### \code{Numeric} scalar, optional AD-factor. Needed if setup object
  ### is missing.
  
  bilinear = "butter",
  ### \code{Numeric} list object with external, user-defined bilinear 
  ### transformed polynomial coefficients, cf. \code{bilinear}.
  ### This can be used to override the bilinear transformation by R. 
  ### Alternatively, a simplified second order Butterworth filter can 
  ### be used if \code{bilinear = "butter"} is specified.
  
  unit = "f"
  ### \code{Character} scalar, unit of the instrument parameters. Either
  ### \code{"f"} (rad/s) or \code{"w"} (pulsation frequencies), default 
  ### is \code{"f"}.
  
){
  
  ## check/set parameters
  if(missing(setup) == TRUE) {
    if(missing(k) == TRUE) {
      stop("Neither setup nor k present!")
    }
    if(missing(s) == TRUE) {
      stop("Neither setup nor s present!")
    }
    if(missing(gain) == TRUE) {
      stop("Neither setup nor gain present!")
    }
    if(missing(AD) == TRUE) {
      stop("Neither setup nor AD present!")
    }
  } else {
    k <- setup$k
    s <- setup$s
    gain <- setup$gain
    AD <- setup$AD
  }
  
  ## re-adjust data structure
  X <- data
  if(is.matrix(X) == FALSE) {
    X <- rbind(X)
  }
  
  ## optionally detrend data set
  if(detrend == TRUE) {
    X.trend <- t(X)
    A <- cbind(matrix(0, nrow(X.trend), 1), matrix(1, nrow(X.trend), 1))
    A[(1:nrow(X.trend)), 1] <- as.matrix(1:nrow(X.trend))/nrow(X.trend)
    X <- t(X.trend - A %*% qr.solve(A, X.trend))
  }
    
  ## subtract mean from signals
  if(subtract.mean == TRUE) {
    X <- X - apply(X, 1, mean, na.rm = TRUE)
  }
  
  ## taper signal
  X.taper <- apply(X, 1, spec.taper, p = 10^-8)
  
  ## calculate fft
  X.fft <- fft(X.taper)
  
  ## create frequency vector
  if(output == "velocity") {
    n.f <- ncol(X)
    if(n.f%%2 == 1) {
      f <- c(seq(from = 0, to = (n.f - 1) / 2, by = 1), 
             seq(from = -(n.f - 1) / 2, to = -1, by = 1)) / 
        (n.f * dt)
    } else {
      f <- c(seq(from = 0, to = n.f / 2, by = 1), 
             seq(from = -n.f / 2 + 1, to = -1, by = 1)) / 
        (n.f * dt)
    }
  } else if(output == "displacement" | output == "acceleration") {
    f <- seq(from = 1, 
             to = ncol(X)) * 1 / (ncol(X) * dt)
  } else {
    stop("Output type not supported!")
  }
  
  ## optinoal bilinear transformation
  if(missing(bilinear) == TRUE) {
    ## create ZPG-object
    ZPG <- Zpg(zero = Re(signal::poly(setup$zeros)),
               pole = Re(signal::poly(setup$poles)),
             gain = k)
    
    ## perform bilinear transformation
    B <- bilinear(ZPG, T = dt)
  } else if(bilinear == "butter") {
    ## use simplified conversion with Butterworth filter
    B <- butter(n = 2, 
                W = 0.1 / (n.f * 2 * dt), 
                type = "high")
    B <- list(zero = B$b,
              pole = B$a)
  } else {
    B <- bilinear
  }

  ## calculate z-plane frequency response
  transfer <- freqz(B$zero, 
                    B$pole, 
                    n = n.f, 
                    Fs = 1 / dt)

  ## calculate waterlevel factor
  gamma <- max(Re(transfer$h * Conj(transfer$h))) * waterlevel

  ## loop through all signal traces
  X.decon <- X
  for(i in 1:nrow(X)) {
    ## deconvolution
    TFdeconv <- X.fft[,i] * Conj(transfer$h) / Re(transfer$h * 
                  Conj(transfer$h) + gamma)
    
    ## invert fft
    X.inverse <- Re(fft(TFdeconv, inverse = TRUE) / length(TFdeconv))
    
    X.inverse <- (X.inverse * AD) / s
    
    ## remove mean
    X.decon[i,] <- X.inverse - mean(X.inverse)
  }
  
  return(X.decon)
  ### A deconvolved data set of the same structure as the input data set.
  
  ##seealso<<
  ## \code{read.sac}, \code{zoo}
  
  ##details<<

  
}, ex = function(){
  ## load example data set and station setup
  data(sac, envir = environment())

  ## extract signal and time parts
  x <- strip.signal(data = sac)
  t <- strip.time(data = sac, unit = "min")
  
  ## deconvolve instrument response
  x.decon <- deconvolve.signal(data = x, 
                               detrend = TRUE, 
                               dt = 1 / 200, 
                               k = 4.34493e+17, 
                               s = 754.3, 
                               gain = 1, 
                               AD = 2.441406e-07, 
                               bilinear = "butter")
  
  ## compare signals
  plot.signal(time = t, 
              data = rbind(x, x.decon), 
              main = c("Raw signal", "Deconvolved signal"),
              xlab = "Time (min)")
  
  ## compare spectrograms
  par(mfcol = c(1, 2))
  S.1 <- spectrogram(time = t, 
                     data = x, 
                     dt = 1 / 200, 
                     method = "generic", 
                     window = 10)
  S.2 <- spectrogram(time = t, 
                     data = x.decon, 
                     dt = 1 / 200, 
                     method = "generic", 
                     window = 10)
  
  plot.spectrogram(data = S.1, legend = "dB")
  plot.spectrogram(data = S.2, legend = "dB")
})
