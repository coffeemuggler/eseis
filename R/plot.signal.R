plot.signal <-
structure(function(# Function to plot seismic traces.
  ### This function plots seismic traces.
  
  time,
  ### Numeric \code{vector} with time values.

  data,
  ### Numeric \code{vector} or \code{matrix} with respective seismic traces.
  
  mtext,
  ### \code{Character} scalar or vector, optional text below the plot title
  
  dynamic.resolution = 5000,
  ### \code{Numeric} scalar, option to decrease graph resolution for faster
  ### plottig, default is 5000. The value specifies the number of subsamples
  ### that are drawn from the data set. If set to \code{0} the entire data set
  ### is plotted.
  
  shared.limits,
  ### \code{Logical} scalar, option to use the same axes limits for all 
  ### plot panels. Only useful when plotting more than one signal trace.
  ### The parameter affects both, x- and y-axes.
  
  plot = TRUE,
  ### \code{Logical} scalar, option to prevent plot output. Only useful in
  ### combination with \code{output = TRUE}.
  
  output = FALSE,
  ### \code{Logical} scalar, option to return the data used for plotting, i.e.
  ### a two-column matrix (t, amplitude).
  
  ...
  ### Further arguments passed.
){

  ## check/correct data structure
  if(is(data, "matrix") == FALSE) {
    signal <- rbind(data)
  } else {
    signal <- data
  }

  ## check/set time vector
  if(missing(time) == TRUE) {
    t <- seq(from = 1, to = ncol(signal))
  } else {
    t <- time
  }
   
  ## check/set plot parameters
  if("xlim" %in% names(list(...))) {
    xlim <- list(...)$xlim
  } else {
    xlim <- range(t, na.rm = TRUE)
  }
  
  ## truncate data set to plot limits
  signal <- signal[,(t >= xlim[1] & t <= xlim[2])]
  t <- t[(t >= xlim[1] & t <= xlim[2])]

  ## re-adjust signal data structure
  if(is(signal, "matrix") == FALSE) {
    signal <- rbind(signal)
  }

  if("ylim" %in% names(list(...))) {
    ylim <- matrix(data = rep(x = list(...)$ylim, 
                              times = nrow(signal)),
                   nrow = nrow(signal), 
                   byrow = TRUE)
  } else {
    if(missing(shared.limits) == FALSE) {
      ylim <- matrix(data = rep(range(signal, na.rm = TRUE), 
                                times = nrow(signal)),
                     nrow = nrow(signal), 
                     byrow = TRUE)
    } else {
      ylim <- matrix(data = t(apply(signal, 1, range, na.rm = TRUE)), 
                     nrow = nrow(signal))
    }
  }
  
  if("xlab" %in% names(list(...))) {
    xlab <- list(...)$xlab
    if(length(xlab) == 1) {
      xlab <- rep(xlab, nrow(signal))
    }
  } else {
    xlab <- rep("Time", nrow(signal))
  }
  
  if("ylab" %in% names(list(...))) {
    ylab <- list(...)$ylab
    if(length(ylab) == 1) {
      ylab <- rep(ylab, nrow(signal))
    }
  } else {
    ylab <- rep("Amplitude", nrow(signal))
  }
  
  if("main" %in% names(list(...))) {
    main <- list(...)$main
    if(length(main) == 1) {
      main <- rep(main, nrow(signal))
    }
  } else {
    main <- rep("", nrow(signal))
  }
  
  if(missing(mtext) == FALSE) {
    if(length(mtext) == 1) {
      mtext <- rep(x = mtext, times = nrow(signal))
    }
  } else {
    mtext <- rep("", nrow(signal))
  }
  
  if("lty" %in% names(list(...))) {
    lty <- list(...)$lty
    if(length(lty) == 1) {
      lty <- rep(lty, nrow(signal))
    }
  } else {
    lty <- rep(1, nrow(signal))
  }  
  
  if("lwd" %in% names(list(...))) {
    lwd <- list(...)$lwd
    if(length(lwd) == 1) {
      lwd <- rep(lwd, nrow(signal))
    }
  } else {
    lwd <- rep(1, nrow(signal))
  }
    
  if("col" %in% names(list(...))) {
    col <- list(...)$col 
    if(length(col) == 1) {
      col <- rep(col, nrow(signal))
    }
  } else {
    col <- rep(x = 1, times = nrow(signal))
  }
  
  if("cex" %in% names(list(...))) {
    cex <- list(...)$cex
    if(length(cex) == 1) {
      cex <- rep(cex, nrow(signal))
    }
  } else {
    cex <- rep(1, nrow(signal))
  }

    
  ## optionally subsample data set for faster plotting
  if(dynamic.resolution > 0) {
    dynamic.resolution <- ifelse(dynamic.resolution > length(t),
                                 length(t), dynamic.resolution)
    n <- seq(1, length(t), by =  round(length(t) / dynamic.resolution))
    t <- t[n]
    signal <- signal[,n]
  }
  
  ## re-adjust signal data structure
  if(is(signal, "matrix") == FALSE) {
    signal <- rbind(signal)
  }

  ## plot data set
  par.setting <- par()$mfcol
  par(mfcol = c(nrow(signal), 1))
  
  for(i in 1:nrow(signal)) {
    if(plot == TRUE) {
      
      ## prepare plot area
      plot(NA, xlim = xlim, ylim = ylim[i,], xlab = xlab[i], ylab = ylab[i], 
           cex = cex[i], main = main[i])
      
      ## plot signal traces
      lines(t, signal[i,], lty = lty, lwd = lwd, col = col[i])
      
      ## add mtext
      mtext(text = mtext[i], cex = cex * 0.8)
    }
  }
  
  par(mfcol = par.setting)
  
  if(output == TRUE) {
      M <- rbind(t, signal)
      return(M)
  }
  ### A plot of an array of seismic signals through time and, optionally,
  ### a numeric matrix of the data to be plotted.
  
  ##keyword<<
  ## environmental seismology
}, ex = function(){
  ## load example data
  data(signal.1, envir = environment())
  
  ## create traces matrix
  s.1 <- rbind(signal.1$signal$station1$x,
               signal.1$signal$station1$y,
               signal.1$signal$station1$z)

  ## straightforward plot of the data set
  plot.signal(data = s.1)

  ## create time vector
  t <- signal.1$time$signal
  
  ## plot of the data set with correct time
  plot.signal(time = t, data = s.1)
  
  ## plot only trace 1
  plot.signal(time = t, data = s.1[1,])
  
  ## plot in very low resolution (200 data points)
  plot.signal(time = t, data = s.1, dynamic.resolution = 200)
  
  ## plot in different colour, line type and headings
  plot.signal(time = t, data = s.1, col = 1:3, lty = 2:4, 
              xlab = rep("Time [min]", 3), ylim = c(-1500, 1500),
              main = c("X-component", "Y-component", "Z-component"))
  
  ## plot minute 3 to 6
  plot.signal(time = t, data = s.1, xlim = c(11.05, 11.1))
  
  ## plot three data sets over each other
  p <- plot.signal(time = t, data = s.1, plot = FALSE, output = TRUE)
  
  plot(p[1,], p[2,], type = "l", ylim =c(-1000, 1500), 
       main = "XYZ-components", xlab = "Time [h]", ylab = "amp")
  lines(p[1,], p[3,], col = 3)
  lines(p[1,], p[4,], col = 4)
})
