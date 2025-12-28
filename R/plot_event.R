#' Create a comprehensive multi panel plot of a seismic waveform
#' 
#' The function creates from an input waveform a multi panel plot, 
#' including a seismogram, spectrum and spectrogram, and additional 
#' frequency statistics information. 
#' 
#' Note that plot generation time can get long when other than short 
#' events are passed to the function. The axes limits can only be changed 
#' for the spectrum and spectrogram plots, \code{ylim} affects the frequency
#' range, \code{zlim} affects the spectral power range.
#' 
#' The function uses the native plot function \code{plot_signal()},
#' \code{plot_spectrum()} and \code{plot_spectrogram()} along with 
#' \code{signal_stats()} to build a four panel plot.
#' 
#' @param data \code{eseis} object, or \code{numeric} vector, data 
#' set to be plotted.
#' 
#' @param ratio \code{Numeric} vector of length two, ratios of the plot 
#' panels, i.e. in horizonal and vertical direction. Default is 
#' \code{c(0.3, 0.3)}.
#' 
#' @param \dots Additional arguments passed to the plot function. See 
#' details for further information
#' 
#' @return Graphic output of an event waveform.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## load and deconvolve example event
#' data(rockfall)
#' rockfall_eseis <- signal_deconvolve(rockfall_eseis)
#' 
#' ## plot event straight away
#' plot_event(data = rockfall_eseis)
#' 
#' ## plot event with adjusted parameters
#' plot_event(data = rockfall_eseis, 
#'            ratio = c(0.4, 0.3),
#'            method = "periodogram",
#'            n = 100, 
#'            window = 6,
#'            overlap = 0.8, 
#'            window_sub = 4, 
#'            overlap_sub = 0.8,
#'            format ="%M:%S", 
#'            col = "jet",
#'            ylim = c(5, 80),
#'            zlim = c(-170, -100))
#' }
#' 
#' @export plot_event

plot_event <- function(
    
  data, 
  ratio = c(0.3, 0.3), 
  ...
  
) {
  
  ## collect initial plot parameters
  prs <- par()
  
  ## collect additional arguments
  ags <- list(...)
  
  ## check/set number of samples
  if(class(data)[1] != "eseis") {
    n_smp <- length(data)
  }
  
  ## check/set dt and number of samples
  if("dt" %in% names(ags) == FALSE && class(data)[1] != "eseis") {
    
    ags$dt <- 1 / 100
    warning("No dt provided. Set to 1 / 100 s by default!")
    
  } else if ("dt" %in% names(ags) == FALSE && class(data)[1] == "eseis") {
    
    n_smp <- data$meta$n
    ags$dt <- data$meta$dt
    
  }
  
  ## set default arguments
  if("Welch" %in% names(ags) == FALSE) {ags$Welch <- TRUE}
  if("window" %in% names(ags) == FALSE) {ags$window <- 0.01 * ags$dt * n_smp}
  if("window_sub" %in% names(ags) == FALSE) {
    ags$window_sub <- 0.1 * ags$window}
  if("overlap" %in% names(ags) == FALSE) {ags$overlap <- 0.5}
  if("overlap_sub" %in% names(ags) == FALSE) {ags$overlap_sub <- 0.5}
  if("method" %in% names(ags) == FALSE) {ags$method <- "periodogram"}
  if("n" %in% names(ags) == FALSE) {ags$n <- NULL}
  if("log" %in% names(ags) == FALSE) {ags$log <- ""}
  if("ylim" %in% names(ags) == FALSE) {ags$ylim <- NULL}
  if("zlim" %in% names(ags) == FALSE) {ags$zlim <- NULL}
  if("format" %in% names(ags) == FALSE) {ags$format <- "%H:%M:%S"}
  
  if("xlab" %in% names(ags) == FALSE) {
    
    ags$xlab <- c(expression(paste("10 ", log[10], " (", 
                                    m^2, "/", s^2, ")/", "Hz", sep = "")),
                  "Time")
  }
  
  if("ylab" %in% names(ags) == FALSE) {
    
    ags$ylab <- c("Velocity (m/s)", "Frequency (Hz)")
  }
  print(ags)
  
  if("col" %in% names(ags) == FALSE) {
    ags$col <- colorspace::sequential_hcl(200, palette = "Inferno")
  } else {
    
    ## handle keyword case "jet"
    if(ags$col[1] == "jet") {
      
      ags$col <- colorRampPalette(colors = c("darkblue",
                                             "blue",
                                             "cyan",
                                             "lightgreen",
                                             "yellow",
                                             "red",
                                             "brown",
                                             "grey30"))
      
      ags$col <- ags$col(200)
    }
  }
  
  ## define colours of frequency information
  cls <- c("orange", "brown", "blue", "slategrey","slategrey")
  
  ## calculate spectrum
  p <- signal_spectrum(data = data, 
                       n = ags$n, 
                       method = ags$method, 
                       dt = ags$dt)
  
  ## remove possible zero value from spectrum
  p$spectrum <- p$spectrum[p$spectrum$frequency > 0,]
  
  ## calculate spectrogram
  psd <- signal_spectrogram(data = data, 
                            Welch = ags$Welch, 
                            dt = ags$dt, 
                            window = ags$window, 
                            overlap = ags$overlap, 
                            window_sub = ags$window_sub, 
                            overlap_sub = ags$overlap_sub, 
                            method = ags$method)
  
  ## calculate frequency statistics
  sts <- signal_stats(data = data, 
                      dt = ags$dt,
                      stats = c("f_modal", 
                                "f_mean", 
                                "f_median", 
                                "f_q25", 
                                "f_q75"))
  
  ## calculate frequency range for plots
  if(is.null(ags$ylim)) {
    ags$ylim <- range(p$spectrum$frequency[p$spectrum$frequency > 0])
  }

  ## adjust power range for plots
  pwr_lab <- pretty(10 * log10(p$spectrum$power))
  if(is.null(ags$zlim)) {ags$zlim <- 
    range(c(range(10 * log10(p$spectrum$power), pwr_lab)))
  }
  
  ## define layout matrix for plot panels
  r_1 <- matrix(data = rep(c(rep(4, ratio[1] * 100), 
                             rep(1, 100 - ratio[1] * 100)), 
                           times = ratio[2] * 100),
                nrow = ratio[2] * 100, byrow = TRUE)
  r_2 <- matrix(data = rep(c(rep(2, ratio[1] * 100), 
                             rep(3, 100 - ratio[1] * 100)), 
                           times = 100 - ratio[2] * 100),
                nrow = 100 - ratio[2] * 100, byrow = TRUE)
  r_12 <- rbind(r_1, r_2)
  
  ## assign layout matrix
  graphics::layout(mat = r_12)
  
  ## create time series plot
  par(mar = c(0, 0, 1, 3.5), xaxs = "i")
  plot(data, ann = FALSE, axes = FALSE)
  axis(side = 4)
  mtext(side = 4, text = ags$ylab[1], line = 2.5, cex = par()$cex)
  box()
  
  ## create spectrum plot
  par(mar = c(3.5, 3.5, 0, 0), yaxs = "i")
  plot(x = 10 * log10(p$spectrum$power), 
       y = p$spectrum$frequency, 
       type = "l", 
       ann = FALSE, 
       axes = FALSE, 
       log = ags$log, 
       ylim = ags$ylim,
       xlim = ags$zlim)
  
  ## add frequency statistics
  for(i in 1:length(sts)) {
    suppressWarnings(graphics::rug(x = sts[[i]], 
                                   side = 4, 
                                   lwd = 1.5, 
                                   col = cls[i]))
  }
  
  ## add axes and annotation
  axis(side = 1, at = pwr_lab, labels = pwr_lab)
  mtext(side = 1, text = ags$xlab[1], line = 2.5, cex = par()$cex)
  axis(side = 2)
  mtext(side = 2, text = ags$ylab[2], line = 2.5, cex = par()$cex)
  box()
  
  ## create spectrogram plot
  par(mar = c(3.5, 0, 0, 3.5), xaxs = "i")
  plot_spectrogram(data = psd, 
                   axes = FALSE,
                   ann = FALSE,
                   log = ags$log, 
                   ylim = ags$ylim,
                   zlim = ags$zlim,
                   col = ags$col,
                   legend = FALSE)
  axis.POSIXct(side = 1, at = pretty(psd$PSD$t)[-1], format = ags$format)
  mtext(side = 1, text = ags$xlab[2], line = 2.5, cex = par()$cex)

  ## create statistics text box
  par(mar = c(0, 3.5, 1, 0), xaxs = "i")
  plot(NA, xlim = c(0, 1), ylim = c(0, 1), ann = FALSE, axes = FALSE)
  box()
  for(i in 1:length(sts)) {
    graphics::text(x = 0.1, y = 1 - i/6, 
                   labels = paste("-", names(sts)[i], "=", 
                                  round(sts[[i]], 2), "Hz"), 
                   col = cls[i], adj = 0)
  }
  
  ## add colour legend bar for spectrum
  points(x = seq(from = 0,
                 to = 1,
                 length.out = length(ags$col)),
         y = rep(0, length(ags$col)), 
         pch = 20, col = ags$col)
  
  ## restore initial plot parameters
  graphics::par(mar = prs$mar, xaxs = prs$xaxs, yaxs = prs$yaxs)
  graphics::layout(mat = 1)
}


#f <- 1 / (2 * pi) * sqrt(stiffness / mass)
