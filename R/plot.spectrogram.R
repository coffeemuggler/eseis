plot.spectrogram <-
structure(function(# Function to plot spectrograms
  ### This function plots a spectrogram of a signal trace.

  data,
  ### Numeric \code{matrix} with power spectral density estimate. Time and 
  ### frequency vectors must be contained as column and row names, 
  ### respectively.

  legend,
  ### \code{Character} scalar, optional legend label.
  
  dynamic.resolution = 5000,
  ### Numeric \code{scalar}, option to decrease graph resolution for faster
  ### plottig, default is 5000. The value specifies the number of subsamples
  ### that are drawn from the data set. If set to \code{0} the entire data set
  ### is plotted.
  
  events,
  ### Numeric \code{vector}, optionally indicate events TO DO YETTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
  
  col,
  ### Character \code{vector}, colour palette to use for spectrogram plotting,
  ### e.g. \code{rainbow(n = 10, start = 0.3, end = 0.8)}
  
  plot = TRUE,
  ### \code{Logical} scalar, option to prevent plot output. Only useful in
  ### combination with \code{output = TRUE}.

  ...
  ### Further arguments passed.
){
  
  ## check data format
  if(is(data, "matrix") == FALSE) {
    stop("Data must be of type matrix!")
  }
  
  ## extract plot data
  t <- as.numeric(colnames(data))
  f <- as.numeric(rownames(data))
  S <- data
  
  ## check/set variables
  if(sum(is.na(t)) > 0 | length(t) == 0) {
    t <- seq(from = 1, to = ncol(data))
  }
  
  if(sum(is.na(f)) > 0 | length(f) == 0) {
    f <- seq(from = 1, to = nrow(data))
  }  
  
  ## optionally subsample data set for faster plotting
  if(dynamic.resolution > 0) {
    dynamic.resolution <- ifelse(dynamic.resolution > length(t),
                                 length(t), dynamic.resolution)
    n <- seq(1, length(t), by = round(length(t) / dynamic.resolution))
    t <- t[n]
    S <- S[,n]
  }

  ## check/set plot parameters
  if("xlim" %in% names(list(...))) {
    xlim <- list(...)$xlim
  } else {
    xlim <- range(t, na.rm = TRUE)
  }
  
  if("ylim" %in% names(list(...))) {
    ylim <- list(...)$ylim
  } else {
    ylim <- range(f, na.rm = TRUE)
  }
  
  S.numeric <- as.numeric(S)
  if("zlim" %in% names(list(...))) {
    zlim <- list(...)$zlim
  } else {
    zlim <- range(S.numeric[S.numeric != -Inf], na.rm = TRUE)
  }
  
  if(zlim[1] > min(S.numeric, na.rm = TRUE) | 
       zlim[1] < max(S.numeric, na.rm = TRUE)) {
    S <- S[f >= ylim[1] & f <= ylim[2],
           t >= xlim[1] & t <= xlim[2]]
    f <- f[f >= ylim[1] & f <= ylim[2]]
    t <- t[t >= xlim[1] & t <= xlim[2]]
    S <- ifelse(S >= zlim[1], S, zlim[1])
    S <- ifelse(S <= zlim[2], S, zlim[2])
  } 

  if("log" %in% names(list(...))) {
    log.plot <- list(...)$log
  } else {
    log.plot <- ""
  }
  
  if(log.plot == "y") {
    ylim = c(f[2], ylim[2])
  }

  if("xlab" %in% names(list(...))) {
    xlab <- list(...)$xlab
  } else {
    xlab <- "Time"
  }
  
  if("ylab" %in% names(list(...))) {
    ylab <- list(...)$ylab
  } else {
    ylab <- "Frequency"
  }
  
  if("main" %in% names(list(...))) {
    main <- list(...)$main
  } else {
    main <- ""
  }
  
  if("lty" %in% names(list(...))) {
    lty <- list(...)$lty
  } else {
    lty <- 1
  }
  
  if("lwd" %in% names(list(...))) {
    lwd <- list(...)$lwd
  } else {
    lwd <- 1
  }
  
  if("cex" %in% names(list(...))) {
    cex <- list(...)$cex
  } else {
    cex <- 1
  }

  if("colour" %in% names(list(...))) {
    colour <- list(...)$colour
  } else {
    colour <- rev(rainbow(n = 200, start = 0, end = 0.8))
  }
 
#   ## optionally adjust data set to modified x- and y-limits
#   S.2 <- S[t >= xlim[1] & t <= xlim[2],
#            f >= ylim[1] & f <= ylim[2]]
#   t.2 <- t[t >= xlim[1] & t <= xlim[2]]
#   f.2 <- f[f >= ylim[1] & f <= ylim[2]]
  
  ## plot data sets, with or without legend
  if(plot == TRUE) {
    ## adjust plot margins
    plot.mar.old <- par()$mar
    par(mar = c(5.1, 4.1, 4.1, 4.1))
    
    if(missing(legend) == FALSE) {
      fields::image.plot(x = t, y = f, z = t(S), log = log.plot, 
                         xlim = xlim, ylim = ylim, zlim = zlim,
                         xlab = xlab, ylab = ylab, main = main,
                         cex = cex, legend.lab = legend, legend.mar= 6.5,
                         legend.line = 3.5, col = colour)
    } else {
      image(x = t, y = f, z = t(S), log = log.plot,
            xlim = xlim, ylim = ylim, zlim = zlim,
            xlab = xlab, ylab = ylab, main = main,
            cex = cex, col = colour)
    } 
    
    ## restore plot margins
    par(mar = plot.mar.old)
  }  
  ### A power spectral density estimate image.
  
  ##keyword<<
  ## environmental seismology

}, ex = function() {
  ## load example data set
  data(signal.1, envir = environment())
  
  ## extract time and z-component of station 1
  t <- signal.1$time$signal
  x <- signal.1$signal$station1$z
  
  ## time series with generic, data array with Welch
  S <- spectrogram(time = t,
                   data = x,
                   dt = 1/200,
                   unit = "hour",
                   window = 6, 
                   window.sub = 2, 
                   component = "z", 
                   method = "Welch")
  
  ## plot spectrogram straightforward
  plot.spectrogram(S)
  
  ## plot spectrogram with logarithmic y-axis
  plot.spectrogram(S, log = "y")
  
  ## plot spectrogram with user-defined modifications
  plot.spectrogram(data = S,
                   main = "PSD station 1 z-component",
                   xlab = "Time [h]",
                   ylab = "f [Hz]",
                   legend = "Energy [db]",
                   zlim = c(-120, -80),
                   colour = rainbow(n = 200, start = 0.5, end = 0.7))
  
  ## plot spectrograms with even more decreased resolution
  plot.spectrogram(data = S, dynamic.resolution = 100)
  
})
