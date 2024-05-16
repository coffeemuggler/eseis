#' Calculate signal statistics
#'
#' This function calculates a set of statistics for the seismic signal 
#' submitted.
#' 
#' Available statistics keywords are:
#' 1. `"t_duration"` (Duration of the signal)
#' 1. `"t_rise"` (Signal rise time, time from start to maximum amplitude)
#' 1. `"t_fall"` (Signal fall time, tme from maximum amplitude to end)
#' 1. `"t_risefall"` (Ratio of rise to fall time)
#' 1. `"a_skewness"` (Skewness of the signal amplitude, see \code{seewave::specprop})
#' 1. `"a_kurtosis"` (Kurtosis of the signal amplitude, see \code{seewave::specprop})
#' 1. `"a1_kurtosis"` (Kurtosis of the filtered (0.1-1 Hz) signal amplitude, see \code{seewave::specprop})
#' 1. `"a2_kurtosis"` (Kurtosis of the filtered (1-3 Hz) signal amplitude, see \code{seewave::specprop})
#' 1. `"a3_kurtosis"` (Kurtosis of the filtered (3-10 Hz) signal amplitude, see \code{seewave::specprop})
#' 1. `"a4_kurtosis"` (Kurtosis of the filtered (10-20 Hz) signal amplitude, see \code{seewave::specprop})
#' 1. `"a5_kurtosis"` (Kurtosis of the filtered (20-50 Hz) signal amplitude, see \code{seewave::specprop})
#' 1. `"e_maxmean"` (Ratio of maximum and mean envelope value, see Hibert et al. (2017))
#' 1. `"e_maxmedian"` (Ratio of maximum and median envelope value, see Hibert et al. (2017))
#' 1. `"e_skewness"` (Skewness of the signal envelope, see \code{seewave::specprop})
#' 1. `"e_kurtosis"` (Kurtosis of the signal envelope, see \code{seewave::specprop})
#' 1. `"e1_logsum"` (Logarithm of the filtered (0.1-1 Hz) envelope sum, see Hibert et al. (2017))
#' 1. `"e2_logsum"` (Logarithm of the filtered (1-3 Hz) envelope sum, see Hibert et al. (2017))
#' 1. `"e3_logsum"` (Logarithm of the filtered (3-10 Hz) envelope sum, see Hibert et al. (2017))
#' 1. `"e4_logsum"` (Logarithm of the filtered (10-20 Hz) envelope sum, see Hibert et al. (2017))
#' 1. `"e5_logsum"` (Logarithm of the filtered (20-50 Hz) envelope sum, see Hibert et al. (2017))
#' 1. `"e_rmsdecphaseline"` (RMS of envelope from linear decrease, see Hibert et al. (2017))
#' 1. `"c_peaks"` (Number of peaks (excursions above 75 % of the maximum) in signal cross correlation function, see Hibert et al. (2017))
#' 1. `"c_energy1"` (Sum of the first third of the signal cross correlation function, see Hibert et al. (2017))
#' 1. `"c_energy2"` (Sum of the last two thirds of the signal cross correlation function, see Hibert et al. (2017))
#' 1. `"c_energy3"` (Ratio of c_energy1 and c_energy2, see Hibert et al. (2017))
#' 1. `"s_peaks"` (Number of peaks (excursions above 75 % of the maximum) in spectral power, see Hibert et al. (2017))
#' 1. `"s_peakpower"` (Mean power of spectral peaks, see Hibert et al. (2017))
#' 1. `"s_mean"` (Mean spectral power, see Hibert et al. (2017))
#' 1. `"s_median"` (Median spectral power, see Hibert et al. (2017))
#' 1. `"s_max"` (Maximum spectral power, see Hibert et al. (2017))
#' 1. `"s_var"` (Variance of the spectral power, see Hibert et al. (2017))
#' 1. `"s_sd"` (Standard deviation of the spectral power, see \code{seewave::specprop})
#' 1. `"s_sem"` (Standard error of the mean of the spectral power, see \code{seewave::specprop})
#' 1. `"s_flatness"` (Spectral flatness, see \code{seewave::specprop})
#' 1. `"s_entropy"` (Spectral entropy, see \code{seewave::specprop})
#' 1. `"s_precision"` (Spectral precision, see \code{seewave::specprop})
#' 1. `"s1_energy"` (Energy of the filtered (0.1-1 Hz) spectrum, see Hibert et al. (2017))
#' 1. `"s2_energy"` (Energy of the filtered (1-3 Hz) spectrum, see Hibert et al. (2017))
#' 1. `"s3_energy"` (Energy of the filtered (3-10 Hz) spectrum, see Hibert et al. (2017))
#' 1. `"s4_energy"` (Energy of the filtered (10-20 Hz) spectrum, see Hibert et al. (2017))
#' 1. `"s5_energy"` (Energy of the filtered (20-30 Hz) spectrum, see Hibert et al. (2017))
#' 1. `"s_gamma1"` (Gamma 1, spectral centroid, see Hibert et al. (2017))
#' 1. `"s_gamma2"` (Gamma 2, spectral gyration radius, see Hibert et al. (2017))
#' 1. `"s_gamma3"` (Gamma 3, spectral centroid width, see Hibert et al. (2017))
#' 1. `"f_modal"` (Modal frequency, see \code{seewave::specprop})
#' 1. `"f_mean"` (Mean frequency (aka central frequency), see \code{seewave::specprop})
#' 1. `"f_median"` (Median frequency, see \code{seewave::specprop})
#' 1. `"f_q05"` (Quantile 0.05 of the spectrum, see \code{seewave::specprop})
#' 1. `"f_q25"` (Quantile 0.25 of the spectrum, see \code{seewave::specprop})
#' 1. `"f_q75"` (Quantile 0.75 of the spectrum, see \code{seewave::specprop})
#' 1. `"f_q95"` (Quantile 0.95 of the spectrum, see \code{seewave::specprop})
#' 1. `"f_iqr"` (Inter quartile range of the spectrum, see \code{seewave::specprop})
#' 1. `"f_centroid"` (Spectral centroid, see \code{seewave::specprop})
#' 1. `"p_kurtosismax"` (Kurtosis of the maximum spectral power over time, see Hibert et al. (2017))
#' 1. `"p_kurtosismedian"` (Kurtosis of the median spectral power over time, see Hibert et al. (2017))
#' 1. `"p_maxmean"` (Mean of the ratio of max to mean spectral power over time, see Hibert et al. (2017))
#' 1. `"p_maxmedian"` (Mean of the ratio of max to median spectral power over time, see Hibert et al. (2017))
#' 1. `"p_peaksmean"` (Number of peaks in normalised mean spectral power over time, see Hibert et al. (2017))
#' 1. `"p_peaksmedian"` (Number of peaks in normalised median spectral power over time, see Hibert et al. (2017))
#' 1. `"p_peaksmax"` (Number of peaks in normalised max spectral power over time, see Hibert et al. (2017))
#' 1. `"p_peaksmaxmean"` (Ratio of number of peaks in normalised max and mean spectral power over time, see Hibert et al. (2017))
#' 1. `"p_peaksmaxmedian"` (Ratio of number of peaks in normalised max and median spectral power over time, see Hibert et al. (2017))
#' 1. `"p_peaksfcentral"` (Number of peaks in spectral power at central frequency over time, see Hibert et al. (2017))
#' 1. `"p_diffmaxmean"` (Mean difference between max and mean power, see Hibert et al. (2017))
#' 1. `"p_diffmaxmedian"` (Mean difference between max and median power, see Hibert et al. (2017))
#' 1. `"p_diffquantile21"` (Mean difference between power quantiles 2 and 1, see Hibert et al. (2017))
#' 1. `"p_diffquantile32"` (Mean difference between power quantiles 3 and 2, see Hibert et al. (2017))
#' 1. `"p_diffquantile31"` (Mean difference between power quantiles 3 and 1, see Hibert et al. (2017))
#' 
#' References:
#' - Hibert C, Provost F, Malet J-P, Maggi A, Stumpf A, Ferrazzini V. 2017. 
#' Automatic identification of rockfalls and volcano-tectonic earthquakes
#' at the Piton de la Fournaise volcano using a Random Forest algorithm. 
#' Journal of Volcanology and Geothermal Research 340, 130-142.
#'
#' @param data \code{eseis} object, data set to be processed.
#' 
#' @param stats \code{Character} vector, keywords of statistics to be 
#' calculated. If omitted, all statistics will be calculated. Wrongly 
#' spelled keywords will be omitted without warning.
#' 
#' @param range_f \code{Numerical} vector of length two, range of the 
#' frequency spectra used to calculate spectral properties. This is 
#' recommended to account for spurious or unwanted frequency pars, for 
#' example caused by ocean micro seism or high frequency effects.
#' 
#' @param res_psd \code{Numerical} value, resolution of the spectrogram
#' used to calculate statistics, in seconds. Default is \code{1} sec. The 
#' spectrogram will be calculated with 90 % overlap and smoothened by a 
#' running window of 5 sec.
#' 
#' @param dt \code{Numeric} value, sampling period. If omitted, \code{dt} 
#' is set to 1/200.
#' 
#' @param cut \code{Logical} value, option to cut output vector to the 
#' required statistics, instead of returning the full length of statistics, 
#' filled with NA values where no statistic was calculated. Default is 
#' \code{TRUE}.
#' 
#' 
#' @return \code{data frame} with calculated statsitics
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## load example data
#' data(rockfall)
#' 
#' ## clip data to event of interest
#' eq <- signal_clip(data = rockfall_eseis, 
#'                   limits = as.POSIXct(c("2015-04-06 13:18:50",
#'                                         "2015-04-06 13:20:10"), 
#'                                       tz = "UTC"))
#' 
#' ## calculate full statistics
#' eq_stats <- signal_stats(data = eq)
#' 
#' ## show names of statistics
#' names(eq_stats)
#' 
#' ## calculate and show selected statistics, with truncated frequency range
#' eq_stats_sub <- signal_stats(data = eq, 
#'                              stats = c("t_rise", 
#'                                        "c_peaks",
#'                                        "f_centroid"),
#'                              range_f = c(1, 90))
#' print(eq_stats_sub)
#' 
#' @export signal_stats
#' 
signal_stats <- function(
  data,
  stats,
  range_f,
  res_psd = 1,
  dt,
  cut = TRUE
) {
  
  ## check/set dt
  if(missing(dt) == TRUE && class(data)[1] != "eseis") {
    
    if(class(data[[1]])[1] != "eseis") {
      
      warning("Sampling frequency missing! Set to 1/200")
    }
    
    dt <- 1 / 200
    
  } else if(missing(dt) == TRUE){
    
    dt <- data$meta$dt
  }
  
  ## get start time
  eseis_t_0 <- Sys.time()
  
  ## collect function arguments
  eseis_arguments <- list(data = "")
  
  ## check if input object is of class eseis
  if(class(data)[1] == "eseis") {
    
    ## set eseis flag
    eseis_class <- TRUE
    
    ## store initial object
    eseis_data <- data
    
    ## extract signal vector
    data <- eseis_data$signal
  } else {
    
    ## set eseis flag
    eseis_class <- FALSE
  }
  
  ## define correct stats keywords -------------------------------------------
  stats_available <- c(
    "t_duration", "t_rise", "t_fall", "t_risefall", "a_skewness", 
    "a_kurtosis", "a1_kurtosis", "a2_kurtosis", "a3_kurtosis", "a4_kurtosis", 
    "a5_kurtosis", "e_maxmean", "e_maxmedian", "e_skewness", "e_kurtosis", 
    "e1_logsum", "e2_logsum", "e3_logsum", "e4_logsum", "e5_logsum", 
    "e_rmsdecphaseline", "e_devlinearfall", "c_peaks", "c_energy1", 
    "c_energy2", "c_energy3", "s_peaks", "s_peakpower", "s_mean", 
    "s_median", "s_max", "s_var", "s_flatness", "s_entropy", "s_precision",
    "s_sd", "s_sem", "s1_energy", "s2_energy", "s3_energy", "s4_energy", 
    "s5_energy", "s_gamma1", "s_gamma2", "s_gamma3", "f_modal", "f_mean", 
    "f_median", "f_q05", "f_q25", "f_q75", "f_q95", "f_iqr", "f_centroid",
    "p_kurtosismax", "p_kurtosismedian", "p_maxmean", "p_maxmedian",
    "p_peaksmean", "p_peaksmedian", "p_peaksmax", "p_peaksmaxmean",
    "p_peaksmaxmedian", "p_peaksfcentral", "p_diffmaxmean", 
    "p_diffquantile21", "p_diffquantile32", "p_diffquantile31")
  
  ## check/set stats argument
  if(missing(stats) == TRUE) {
    
    stats <- stats_available
  }
  
  ## check stats keywords for correctness
  for(i in 1:length(stats)) {
    
    if(sum(grepl(pattern = stats[i], 
                 x = stats_available, 
                 fixed = TRUE)) < 1) {
      
      stats[i] <- "KICKMEOUT"
    }
  }
  
  stats <- stats[!grepl(x = stats, pattern = "KICKMEOUT")]
  
  ## check if parameters are in valid range
  if(res_psd > length(data) * dt) {
    
    res_psd <- (length(data) - 1) * dt
  }
  
  ## calculate data derivatives -----------------------------------------------
  
  ## create time vector
  t <- seq(from = 0, by = dt, length.out = length(data))
  
  ## demean and detrend data
  data <- eseis::signal_demean(data = data)
  a <- eseis::signal_detrend(data = data)

  ## calculate envelope
  e <- eseis::signal_envelope(data = a)
  
  ## calculate spectrum
  s <- eseis::signal_spectrum(data = a, dt = dt)
  
  ## optionally truncate spectrum
  if(missing(range_f) == FALSE) {
    
    f_ok <- which(s$frequency >= range_f[1] & s$frequency <= range_f[2])
  } else {
    
    f_ok <- 1:length(s$frequency)
  }
  
  s <- s[f_ok,]
  
  ## calculate weighted spectral amplitude
  s_scl <- s$power/sum(s$power)
  s_sclsum <- cumsum(s_scl)
  s_mean <- sum(s_scl * s$frequency)
  
  ## calculate and smoothen spectrogram
  p <- eseis::signal_spectrogram(data = a, 
                                 time = t, 
                                 window = res_psd, 
                                 overlap = 0.9, 
                                 dt = dt)
  p_f <- p$f
  p_t <- p$t
  p <- t(apply(X = p$S, 
               MARGIN = 1, 
               FUN = caTools::runmean, 
               k = 5))
  
  ## calculate max, mean and median power
  p_max <- matrixStats::colMaxs(p)
  p_mean <- colMeans(p)
  p_med <- matrixStats::colMedians(p)
  
  ## calculate first quartile frequency
  p_q1 <- apply(X = p, MARGIN = 2, FUN = function(x, p_f) {
    
    p_cumsum <- cumsum(10^(x/10)/sum(10^(x/10)))
    
    p_f[length(p_cumsum[p_cumsum <= 0.25]) + 1]
  }, p_f)
  
  ## calculate second quartile frequency
  p_q2 <- apply(X = p, MARGIN = 2, FUN = function(x, p_f) {
    
    p_cumsum <- cumsum(10^(x/10)/sum(10^(x/10)))
    
    p_f[length(p_cumsum[p_cumsum <= 0.50]) + 1]
  }, p_f)
  
  ## calculate third quartile frequency
  p_q3 <- apply(X = p, MARGIN = 2, FUN = function(x, p_f) {
    
    p_cumsum <- cumsum(10^(x/10)/sum(10^(x/10)))
    
    p_f[length(p_cumsum[p_cumsum <= 0.75]) + 1]
  }, p_f)
  
  ## optionally, calculate auto correlation function
  if("c_peaks" %in% stats | 
     "c_energy1" %in% stats | 
     "c_energy2" %in% stats |
     "c_energy3" %in% stats) {
    
    # c <- acf(x = a, plot = FALSE, lag.max = floor(length(a)/2))
    # c_abs <- abs(c$acf)
    
    a_fft <- fftw::FFT(x = signal_pad(a))
    a_corr <- Conj(a_fft) * a_fft
    a_corr <- Re(fftw::IFFT(x = a_corr)) / length(a_corr)
    a_corr <- (a_corr / max(a_corr))[1:floor(length(a)/2)]

    c_abs <- abs(a_corr)
  }
  
  ## optionally, calculate filtered data sets
  if("e_logsum1" %in% stats | 
     "a1_kurtosis" %in% stats | 
     "s1_energy" %in% stats) {
    
    a_1 <- try(eseis::signal_filter(data = a, 
                                    f = c(0.1, 1), 
                                    dt = dt), 
               silent = TRUE)
    a_1 <- try(eseis::signal_taper(data = a_1, 
                                   p = 0.01), 
               silent = TRUE)
    e_1 <- try(eseis::signal_envelope(data = a_1), 
               silent = TRUE)
    s_1 <- try(eseis::signal_spectrum(data = a_1, dt = dt)[f_ok,],
               silent = TRUE)
    
    if(class(a_1)[1] == "try-error") {a_1 <- NA}
    if(class(e_1)[1] == "try-error") {e_1 <- NA}
    if(class(s_1)[1] == "try-error") {s_1 <- NA}
  }
  
  if("e_logsum2" %in% stats |
     "a2_kurtosis" %in% stats | 
     "s2_energy" %in% stats) {
    
    a_2 <- try(eseis::signal_filter(data = a, 
                                    f = c(1, 3), 
                                    dt = dt), 
               silent = TRUE)
    a_2 <- try(eseis::signal_taper(data = a_2, 
                                   p = 0.01), 
               silent = TRUE)
    e_2 <- try(eseis::signal_envelope(data = a_2), 
               silent = TRUE)
    s_2 <- try(eseis::signal_spectrum(data = a_2, dt = dt)[f_ok,],
               silent = TRUE)
    
    if(class(a_2)[1] == "try-error") {a_2 <- NA}
    if(class(e_2)[1] == "try-error") {e_2 <- NA}
    if(class(s_2)[1] == "try-error") {s_2 <- NA}
  }
  
  if("e_logsum3" %in% stats | 
     "a3_kurtosis" %in% stats | 
     "s3_energy" %in% stats) {
    
    a_3 <- try(eseis::signal_filter(data = a, 
                                    f = c(3, 10), 
                                    dt = dt), 
               silent = TRUE)
    a_3 <- try(eseis::signal_taper(data = a_3, 
                                   p = 0.01), 
               silent = TRUE)
    e_3 <- try(eseis::signal_envelope(data = a_3), 
               silent = TRUE)
    s_3 <- try(eseis::signal_spectrum(data = a_3, dt = dt)[f_ok,],
               silent = TRUE)
    
    if(class(a_3)[1] == "try-error") {a_3 <- NA}
    if(class(e_3)[1] == "try-error") {e_3 <- NA}
    if(class(s_3)[1] == "try-error") {s_3 <- NA}
  }
  
  if("e_logsum4" %in% stats | 
     "a4_kurtosis" %in% stats | 
     "s4_energy" %in% stats) {
    
    a_4 <- try(eseis::signal_filter(data = a, 
                                    f = c(10, 20), 
                                    dt = dt), 
               silent = TRUE)
    a_4 <- try(eseis::signal_taper(data = a_4, 
                                   p = 0.01), 
               silent = TRUE)
    e_4 <- try(eseis::signal_envelope(data = a_4), 
               silent = TRUE)
    s_4 <- try(eseis::signal_spectrum(data = a_4, dt = dt)[f_ok,],
               silent = TRUE)
    
    if(class(a_4)[1] == "try-error") {a_4 <- NA}
    if(class(e_4)[1] == "try-error") {e_4 <- NA}
    if(class(s_4)[1] == "try-error") {s_4 <- NA}
  }
  
  if("e_logsum5" %in% stats | 
     "a5_kurtosis" %in% stats | 
     "s5_energy" %in% stats) {
    
    ## check if 
    if(1/dt/2 >= 50) {
      f_lim <- 49.999
    } else {
      f_lim <- 50
    }
    
    a_5 <- try(eseis::signal_filter(data = a, 
                                    f = c(20, f_lim), 
                                    dt = dt), 
               silent = TRUE)
    a_5 <- try(eseis::signal_taper(data = a_5, 
                                   p = 0.01), 
               silent = TRUE)
    e_5 <- try(eseis::signal_envelope(data = a_5), 
               silent = TRUE)
    s_5 <- try(eseis::signal_spectrum(data = a_5, dt = dt)[f_ok,],
               silent = TRUE)
    
    if(class(a_5)[1] == "try-error") {a_5 <- NA}
    if(class(e_5)[1] == "try-error") {e_5 <- NA}
    if(class(s_5)[1] == "try-error") {s_5 <- NA}
  }
  
  ## create output data set
  data_out <- as.data.frame(t(rep(NA, length(stats_available))))
  names(data_out) <- stats_available
  
  ## calculate statistics -----------------------------------------------------
  ## duration
  if("t_duration" %in% stats) {
    
    data_out$t_duration <- length(a)  * dt
  }
  
  ## rise time
  if("t_rise" %in% stats) {
    
    data_out$t_rise <- t[e == max(e)]
  }
  
  ## fall time
  if("t_fall" %in% stats) {
    
    data_out$t_fall <- max(t) - t[e == max(e)]
  }
  
  ## ratio of rise to fall time
  if("t_risefall" %in% stats) {
    
    i_max <- which(e == max(e))
    
    data_out$t_risefall <- (i_max * dt) / ((length(e) - i_max) * dt)
  }
  
  ## amplitude skewness
  if("a_skewness" %in% stats) {
    
    data_out$a_skewness <- 1 / length(a) * sum(((a - mean(a)) / sd(a))^3)
  }
  
  ## amplitude kurtosis
  if("a_kurtosis" %in% stats) {
    
    data_out$a_kurtosis <- 1 / length(a) * sum(((a - mean(a)) / sd(a))^4)
  }
  
  ## amplitude kurtosis, data set filtered between 0.1-1 Hz
  if("a1_kurtosis" %in% stats) {
    
    data_out$a1_kurtosis <- 1 / length(a_1) * 
      sum(((a_1 - mean(a_1)) / sd(a_1))^4)
  }
  
  ## amplitude kurtosis, data set filtered between 1-3 Hz
  if("a2_kurtosis" %in% stats) {
    
    data_out$a2_kurtosis <- 1 / length(a_2) * 
      sum(((a_2 - mean(a_2)) / sd(a_2))^4)
  }
  
  ## amplitude kurtosis, data set filtered between 3-10 Hz
  if("a3_kurtosis" %in% stats) {
    
    data_out$a3_kurtosis <- 1 / length(a_3) * 
      sum(((a_3 - mean(a_3)) / sd(a_3))^4)
  }
  
  ## amplitude kurtosis, data set filtered between 10-20 Hz
  if("a4_kurtosis" %in% stats) {
    
    data_out$a4_kurtosis <- 1 / length(a_4) * 
      sum(((a_4 - mean(a_4)) / sd(a_4))^4)
  }
  
  ## amplitude kurtosis, data set filtered between 20-50 Hz
  if("a5_kurtosis" %in% stats) {
    
    data_out$a5_kurtosis <- 1 / length(a_5) * 
      sum(((a_5 - mean(a_5)) / sd(a_5))^4)
  }
  
  ## envelope amplitude ratio max to mean
  if("e_maxmean" %in% stats) {
    
    data_out$e_maxmean <- max(e) / mean(e)
  }
  
  ## envelope amplitude ratio max to median
  if("e_maxmedian" %in% stats) {
    
    data_out$e_maxmedian <- max(e) / median(e)
  }
  
  ## envelope skewness
  if("e_skewness" %in% stats) {
    
    data_out$e_skewness <- 1 / length(e) * sum(((e - mean(e)) / sd(e))^3)
  }
  
  ## envelope kurtosis
  if("e_kurtosis" %in% stats) {
    
    data_out$e_kurtosis <- 1 / length(e) * sum(((e - mean(e)) / sd(e))^4)
  }
  
  ## logged envelope sum, filtered between 0.1–1 Hz
  if("e1_logsum" %in% stats) {
    
    data_out$e1_logsum <- log10(sum(e_1))
  }
  
  ## logged envelope sum, filtered between 1-3 Hz
  if("e2_logsum" %in% stats) {
    
    data_out$e2_logsum <- log10(sum(e_2))
  }
  
  ## logged envelope sum, filtered between 3-10 Hz
  if("e3_logsum" %in% stats) {
    
    data_out$e3_logsum <- log10(sum(e_3))
  }
  
  ## logged envelope sum, filtered between 10-20 Hz
  if("e4_logsum" %in% stats) {
    
    data_out$e4_logsum <- log10(sum(e_4))
  }
  
  ## logged envelope sum, filtered between 20-50 Hz
  if("e5_logsum" %in% stats) {
    
    data_out$e5_logsum <- log10(sum(e_5))
  }
  
  ## RMS of declining phase line
  if("e_rmsdecphaseline" %in% stats) {
    
    e_max <- max(e)
    t_max <- t[e == max(e)]
    t_fall <- max(t) - t_max
    
    l <- e_max - (max(e) / (t_fall - t_max) * t)
    
    data_out$e_rmsdecphaseline <- sqrt(mean(e - l)^2)
  }
  
  if("e_devlinearfall" %in% stats) {
    
    ii <- which(e == max(e)):length(t)
    t_mod <- t[ii]
    e_mod <- e[ii]
    mod_lin <- seq(from = e_mod[1], 
                   to = e_mod[length(e_mod)], 
                   length.out = length(t_mod))
    
    data_out$e_devlinearfall <- sqrt(mean((e_mod - mod_lin)^2))
  }
  
  ## number of peaks above mean + sd in auto correlation function
  if("c_peaks" %in% stats) {
    
    c_norm <- c_abs / max(c_abs)
    
    data_out$c_peaks <- sum(diff(c(FALSE, c_norm > 0.75)) == 1)
    
    # data_out$c_peaks <- sum(diff(c(FALSE, c_abs > mean(c_abs) +
    #                                  sd(c_abs))) == 1)
  }
  
  ## energy in first thrid of auto correlation function
  if("c_energy1" %in% stats) {
    
    n_1 <- floor(length(c_abs) / 3)
    data_out$c_energy1 <- sum(c_abs[1:n_1]) / n_1 * dt
  }
  
  ## energy in last two thrids of auto correlation function
  if("c_energy2" %in% stats) {
    
    n_3 <- length(c_abs)
    n_2 <- floor(n_3 / 3)
    data_out$c_energy2 <- sum(c_abs[n_2:n_3]) / (n_3 - n_2) * dt
  }
  
  ## ratio of energy in first and last two thrids of auto correlation function
  if("c_energy3" %in% stats) {
    
    n_3 <- length(c_abs)
    n_2 <- floor(n_3 / 3)
    n_1 <- floor(length(c_abs) / 3)
    data_out$c_energy3 <- (sum(c_abs[1:n_1]) / n_1 * dt) / 
      (sum(c_abs[n_2:n_3]) / (n_3 - n_2) * dt)
  }
  
  ## calculate number of peaks in spectrum
  if("s_peaks" %in% stats) {
    
    s_db <- 10 * log10(s$power)
    s_norm <- (s_db - min(s_db)) / (max(s_db) - min(s_db))
    s_norm_smooth <- caTools::runmean(x = s_norm, k = 0.02 * length(s_db))
    
    data_out$s_peaks <- sum(diff(c(FALSE, s_norm_smooth > 0.75)) == 1)
  }
  
  ## calculate mean specturm peak energy
  if("s_peakpower" %in% stats) {
    
    s_db <- 10 * log10(s$power)
    s_norm <- (s_db - min(s_db)) / (max(s_db) - min(s_db))
    
    data_out$s_peakpower <- mean(s_db[s_norm > 0.75])
  }
  
  ## calculate mean spectral power
  if("s_mean" %in% stats) {
    
    data_out$s_mean <- mean(10 * log10(s$power))
  }
  
  ## calculate median spectral power
  if("s_median" %in% stats) {
    
    data_out$s_median <- median(10 * log10(s$power))
  }
  
  ## calculate maximum spectral power
  if("s_max" %in% stats) {
    
    data_out$s_max <- max(10 * log10(s$power))
  }
  
  ## calculate spectral power variance
  if("s_var" %in% stats) {
    
    data_out$s_var <- stats::var(10 * log10(s$power))
  }
  
  ## calculate spectral standard deviation
  if("s_sd" %in% stats) {
    
    data_out$s_sd <- sqrt(sum(s_scl * ((s$frequency - s_mean)^2)))
  }
  
  ## calculate spectral standard deviation of the mean
  if("s_sem" %in% stats) {
    
    s_sd <- sqrt(sum(s_scl * ((s$frequency - s_mean)^2)))
    data_out$s_sem <- s_sd/sqrt(length(s$frequency))
  }

  if("s_flatness" %in% stats) {
    
    data_out$s_flatness <- (prod(s$power^(1/length(s$power)))) / 
      (mean(s$power))
  }
  
  if("s_entropy" %in% stats) {
    
    data_out$s_entropy <- -sum(s_scl * log(s_scl))/log(length(s_scl))
  }
  
  if("s_precision" %in% stats) {
    
    data_out$s_precision <- (1 / dt) / (2 * length(s$frequency))
  }
  
  if("s1_energy" %in% stats) {
    
    data_out$s1_energy <- 10 * log10(sum(s_1$power * 
                                           c(0, diff(s_1$frequency))))
  }

  if("s2_energy" %in% stats) {
    
    data_out$s2_energy <- 10 * log10(sum(s_2$power * 
                                           c(0, diff(s_2$frequency))))
  }
  
  if("s3_energy" %in% stats) {
    
    data_out$s3_energy <- 10 * log10(sum(s_3$power * 
                                           c(0, diff(s_3$frequency))))
  }
  
  if("s4_energy" %in% stats) {
    
    data_out$s4_energy <- 10 * log10(sum(s_4$power * 
                                           c(0, diff(s_4$frequency))))
  }
  
  if("s5_energy" %in% stats) {
    
    data_out$s5_energy <- 10 * log10(sum(s_5$power * 
                                           c(0, diff(s_5$frequency))))
  }
  
  if("s_gamma1" %in% stats) {
    
    data_out$s_gamma1 <- sum(s$frequency * 10 * log10(s$power)^2) / 
      sum(10 * log10(s$power)^2)
  }
  
  if("s_gamma2" %in% stats) {
    
    data_out$s_gamma2 <- sqrt(sum(s$frequency^2 * s$power^2) / 
                                sum(s$power^2))
  }

  if("s_gamma3" %in% stats) {
    
    g_1 <- sum(s$frequency * 10 * log10(s$power)^2) / 
      sum(10 * log10(s$power)^2)
    g_2 <- sqrt(sum(s$frequency^2 * s$power^2) / 
                  sum(s$power^2))
    
    data_out$s_gamma3 <- sqrt(g_1^2 - g_2^2)
  }
  
  if("p_kurtosismax" %in% stats) {
    
    data_out$p_kurtosismax <- 
      1 / length(p_max) * sum(((p_max - mean(p_max)) / sd(p_max))^4)
  }
  
  if("p_kurtosismedian" %in% stats) {
    
    data_out$p_kurtosismedian <- 
      1 / length(p_med) * sum(((p_med - mean(p_med)) / sd(p_med))^4)
  }
  
  if("p_maxmean" %in% stats) {
    
    data_out$p_maxmean <- mean(p_max / p_mean)
  }
  
  if("p_maxmedian" %in% stats) {
    
    data_out$p_maxmedian <- mean(p_max / p_mean)
  }
  
  if("p_peaksmean" %in% stats) {
    
    p_mean_norm <- (p_mean - min(p_mean)) / (max(p_mean) - min(p_mean))
    
    data_out$p_peaksmean <- sum(diff(c(FALSE, p_mean_norm > 0.75)) == 1)
  }
  
  if("p_peaksmedian" %in% stats) {
    
    p_med_norm <- (p_med - min(p_med)) / (max(p_med) - min(p_med))
    
    data_out$p_peaksmedian <- sum(diff(c(FALSE, p_med_norm > 0.75)) == 1)
  }
  
  if("p_peaksmax" %in% stats) {
    
    p_max_norm <- (p_max - min(p_max)) / (max(p_max) - min(p_max))
    
    data_out$p_peaksmax <- sum(diff(c(FALSE, p_max_norm > 0.75)) == 1)
  }
  
  if("p_peaksmaxmean" %in% stats) {
    
    p_max_norm <- (p_max - min(p_max)) / (max(p_max) - min(p_max))
    n_p_max_norm <- sum(diff(c(FALSE, p_max_norm > 0.75)) == 1)
    
    p_mean_norm <- (p_mean - min(p_mean)) / (max(p_mean) - min(p_mean))
    n_p_mean_norm <- sum(diff(c(FALSE, p_mean_norm > 0.75)) == 1)
    
    data_out$p_peaksmaxmean <- n_p_max_norm / n_p_mean_norm
  }
  
  if("p_peaksmaxmedian" %in% stats) {
    
    p_max_norm <- (p_max - min(p_max)) / (max(p_max) - min(p_max))
    n_p_max_norm <- sum(diff(c(FALSE, p_max_norm > 0.75)) == 1)
    
    p_med_norm <- (p_med - min(p_med)) / (max(p_med) - min(p_med))
    n_p_med_norm <- sum(diff(c(FALSE, p_med_norm > 0.75)) == 1)
    
    data_out$p_peaksmaxmedian <- n_p_max_norm / n_p_med_norm
  }
  
  if("p_peaksfcentral" %in% stats) {
    
    p_f_cent <- apply(X = p, MARGIN = 2, FUN = function(x, p_f) {
      
      f_central <- sum(10^(x/10)/sum(10^(x/10)) * p_f)
      return(x[abs(p_f - f_central) == min(abs(p_f - f_central))])
    }, p_f)
    
    p_f_cent_norm <- (p_f_cent - min(p_f_cent)) / 
      (max(p_f_cent) - min(p_f_cent))

    data_out$p_peaksfcentral <- sum(diff(c(FALSE, p_f_cent_norm > 0.75)) == 1)
  }
  
  if("p_diffmaxmean" %in% stats) {

    data_out$p_diffmaxmean <- mean(p_max - p_mean)
  }
  
  if("p_diffquantile21" %in% stats) {
    
    data_out$p_diffquantile21 <- mean(p_q2 - p_q1)
  }
  
  if("p_diffquantile31" %in% stats) {
    
    data_out$p_diffquantile31 <- mean(p_q3 - p_q1)
  }
  
  if("p_diffquantile32" %in% stats) {
    
    data_out$p_diffquantile32 <- mean(p_q3 - p_q2)
  }
  
  ## calculate frequency of maximum power, i.e. modal frequency
  if("f_modal" %in% stats) {
    
    data_out$f_modal <- s$frequency[s$power == max(s$power)]
  }
  
  if("f_mean" %in% stats) {
    
    data_out$f_mean <- sum(s_scl * s$frequency)
  }
  
  if("f_median" %in% stats) {
    
    data_out$f_median <- s$frequency[length(s_sclsum[s_sclsum <= 0.50]) + 1]
  }

  if("f_q05" %in% stats) {
    
    data_out$f_q05 <- s$frequency[length(s_sclsum[s_sclsum <= 0.05]) + 1]
  }

  if("f_q25" %in% stats) {
    
    data_out$f_q25 <- s$frequency[length(s_sclsum[s_sclsum <= 0.25]) + 1]
  }

  if("f_q75" %in% stats) {
    
    data_out$f_q75 <- s$frequency[length(s_sclsum[s_sclsum <= 0.75]) + 1]
  }

  if("f_q95" %in% stats) {
    
    data_out$f_q95 <- s$frequency[length(s_sclsum[s_sclsum <= 0.95]) + 1]
  }
  
  if("f_iqr" %in% stats) {
    
    data_out$f_iqr <- s$frequency[length(s_sclsum[s_sclsum <= 0.75]) + 1] - 
      s$frequency[length(s_sclsum[s_sclsum <= 0.25]) + 1]
  }
  
  if("f_centroid" %in% stats) {
    
    data_out$f_centroid <- sum(s$frequency * s_scl)
  }
  
  ## optionally cut output data set
  if(cut == TRUE) {
    data_out <- data_out[stats_available %in% stats]
  }
  
  ## return output data set
  return(data_out)
}


