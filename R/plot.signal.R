plot.signal <-
structure(function(# Function to plot seismic traces.
  ### This function plots seismic traces.
  
  data,
  ### Numeric \code{vector} or \code{matrix} with respective seismic traces.
  
  time,
  ### \code{Numeric} vector with time values.

  unit,
  ### \code{Character} scalar, time unit. One out of \code{"sec"}, 
  ### \code{"min"}, \code{"hour"}, \code{"day"}, \code{"JD"}. If omitted,
  ### an automatic unit is used.
  
  t.rel = TRUE,
  ### \code{Logical} scalar, option to enable/disable relative time units
  ### (i.e. x-axis begins with zero). Default is \code{TRUE}.
  
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
    x <- rbind(data)
  } else {
    x <- data
  }

  ## check/set time vector
  if(missing(time) == TRUE) {
    
    ## assign time vector for missing time information
    t.plot <- c(1, ncol(x))
    
    ## assign time unit keyword
    unit <- "unknown"
    
    ## assign x-axis label
    x.lab <- "Time (unknown units)"
    
  } else {
    
    ## assign time vector
    t <- time
    
    ## convert time vector to time object
    t.time <- strptime(x = range(t), format = "%Y-%m-%d %H:%M:%S")

    ## calculate cumulative time vector
    t.cum <- (t.time$yday + 1) * 86400 + t.time$hour * 3600 + 
      t.time$min * 60 + t.time$sec

    ## calculate zero time value
    t.cum.zero <- t.cum - min(t.cum)
    
    ## set time unit
    if(missing(unit) == TRUE) {
      
      ## calculate time difference and infer automatically set unit
      unit <- units(difftime(max(t, na.rm = TRUE), 
                             min(t, na.rm = TRUE)))

      ## convert units into appropriate keywords
      if(unit == "seconds") {
        unit <- "sec"
      } else if(unit.t == "mins") {
        unit <- "min"
      } else if(unit.t == "hours") {
        unit <- "hour"
      } else if(unit.t == "days") {
        unit <- "day"
      }
    }
    
    ## generate conversion factors and x-labels
    if(unit == "sec") {
      f <- 1
      x.lab <- "Time (s)"
    } else if(unit == "min") {
      f <- 60
      x.lab <- "Time (min)"
    } else if(unit == "hour") {
      f <- 3600
      x.lab <- "Time (h)"
    } else if(unit == "day") {
      f <- 86400
      x.lab <- "Time (d)"
    } else if(unit == "day") {
      f <- 86400
      x.lab <- "Time (d)"
    } else if(unit == "unknown") {
      f <- 1
      x.lab <- "Time (unknown units)"
    }  else {
      stop("Time unit not supported!")
    }

    ## optionally remove zero time
    if(t.rel == TRUE) {
      t.plot <- t.cum.zero / f
    } else {
      t.plot <- t.cum / f
    }
  }
  
  ## check/set plot parameters
  if("xlim" %in% names(list(...))) {
    xlim <- list(...)$xlim
  } else {
    xlim <- range(t.plot, na.rm = TRUE)
  }

  ## truncate data set to plot limits
  t.ID <- seq(from = t.plot[1], to = t.plot[2], length.out = length(t))
  x <- x[,(t.ID >= xlim[1] & t.ID <= xlim[2])]
  t.plot <- t.ID[(t.ID >= xlim[1] & t.ID <= xlim[2])]
  
  ## re-adjust signal data structure
  if(is(x, "matrix") == FALSE) {
    x <- rbind(x)
  }

  if("ylab" %in% names(list(...))) {
    ylab <- list(...)$ylab
    if(length(ylab) == 1) {
      ylab <- rep(ylab, nrow(x))
    }
  } else {
    ylab <- rep("Amplitude", nrow(x))
  }
  
  if("main" %in% names(list(...))) {
    main <- list(...)$main
    if(length(main) == 1) {
      main <- rep(main, nrow(x))
    }
  } else {
    main <- rep("", nrow(x))
  }
  
  if(missing(mtext) == FALSE) {
    if(length(mtext) == 1) {
      mtext <- rep(x = mtext, times = nrow(x))
    }
  } else {
    mtext <- rep("", nrow(x))
  }
  
  if("lty" %in% names(list(...))) {
    lty <- list(...)$lty
    if(length(lty) == 1) {
      lty <- rep(lty, nrow(x))
    }
  } else {
    lty <- rep(1, nrow(x))
  }  
  
  if("lwd" %in% names(list(...))) {
    lwd <- list(...)$lwd
    if(length(lwd) == 1) {
      lwd <- rep(lwd, nrow(x))
    }
  } else {
    lwd <- rep(1, nrow(x))
  }
    
  if("col" %in% names(list(...))) {
    col <- list(...)$col 
    if(length(col) == 1) {
      col <- rep(col, nrow(x))
    }
  } else {
    col <- rep(x = 1, times = nrow(x))
  }
  
  if("cex" %in% names(list(...))) {
    cex <- list(...)$cex
    if(length(cex) == 1) {
      cex <- rep(cex, nrow(x))
    }
  } else {
    cex <- rep(1, nrow(x))
  }

  ## optionally subsample data set for faster plotting
  if(dynamic.resolution > 0) {
    dynamic.resolution <- ifelse(dynamic.resolution > length(t.plot),
                                 length(t.plot), dynamic.resolution)
    n <- seq(from = 1, 
             to = length(t.plot), 
             by =  round(length(t.plot) / dynamic.resolution))
    t.plot <- t.plot[n]
    x <- x[,n]
  }
  
  ## re-adjust signal data structure
  if(is(x, "matrix") == FALSE) {
    x <- rbind(x)
  }

  ## plot data set
  par.setting <- par()$mfcol
  par(mfcol = c(nrow(x), 1))
  
  for(i in 1:nrow(x)) {
    if(plot == TRUE) {
      
      if("ylim" %in% names(list(...))) {
        ylim <- matrix(data = rep(x = list(...)$ylim, 
                                  times = nrow(x)),
                       nrow = nrow(x), 
                       byrow = TRUE)
      } else {
        if(missing(shared.limits) == FALSE) {
          ylim <- matrix(data = rep(range(x, na.rm = TRUE), 
                                    times = nrow(x)),
                         nrow = nrow(x), 
                         byrow = TRUE)
        } else {
          ylim <- matrix(data = t(apply(x, 1, range, na.rm = TRUE)), 
                         nrow = nrow(x))
        }
      }
      
      ## adjust x-label
      if("xlab" %in% names(list(...))) {
        xlab <- list(...)$xlab
        if(length(xlab) == 1) {
          xlab <- rep(xlab, nrow(x))
        }
      } else {
        xlab <- rep(x.lab, nrow(x))
      }
      
      ## prepare plot area
      plot(NA, xlim = xlim, ylim = ylim[i,], xlab = xlab[i], ylab = ylab[i], 
           cex = cex[i], main = main[i],)
      
      ## plot signal traces
      lines(t.plot, x[i,], lty = lty, lwd = lwd, col = col[i])
      
      ## add mtext
      mtext(text = mtext[i], cex = cex * 0.8)
    }
  }
  
  par(mfcol = par.setting)
  
  if(output == TRUE) {
      M <- rbind(t.plot, x)
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

  ## create time and signal vector
  t <- signal.1$time$signal
  s <- signal.1$signal$station1$x
  
  ## plot of the data set with correct time
  plot.signal(time = t, data = s)
  
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
