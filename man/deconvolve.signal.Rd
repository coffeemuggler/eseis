\name{deconvolve.signal}
\alias{deconvolve.signal}
\title{Function to deconvolve a signal.}
\description{This function deconvolves the signal from the instrument response.}
\usage{deconvolve.signal(data, output = "velocity", setup, detrend = TRUE, 
    subtract.mean = TRUE, waterlevel = 10^-6, dt, k, s, gain, 
    AD, bilinear = "butter", unit = "f")}
\arguments{
  \item{data}{\code{list} object, data set to be converted. The signal part must be in
counts. ATTENTION, CURRENTLY ONLY MATRICES ARE SUPPORTED!!!!!}
  \item{output}{\code{Character} scalar, output data type. One out of \code{"velocity"},
\code{"displacement"} or \code{"acceleration"}, default is 
\code{"velocity"}.}
  \item{setup}{\code{List} object with station setup information (cf. 
\code{station.setup}), needed to convert counts into m/s. If
missing, the necessary paramters need to be specified manually.}
  \item{detrend}{\code{Logical} scalar, option to detrend data set prior to 
deconvolution, default is \code{FALSE}.}
  \item{subtract.mean}{\code{Logical} scalar, optionally remove the global mean before 
processing, default is \code{TRUE}.}
  \item{waterlevel}{\code{Numeric} scalar, providing the waterlevel value for frequency
division, default is \code{10^-6}.}
  \item{dt}{\code{Numeric} scalar, sampling rate.}
  \item{k}{\code{Numeric} scalar, optional k-factor. Needed if setup object
is missing.}
  \item{s}{\code{Numeric} scalar, optional s-factor. Needed if setup object
is missing.}
  \item{gain}{\code{Numeric} scalar, optional gain-factor. Needed if setup object
is missing.}
  \item{AD}{\code{Numeric} scalar, optional AD-factor. Needed if setup object
is missing.}
  \item{bilinear}{\code{Numeric} list object with external, user-defined bilinear 
transformed polynomial coefficients, cf. \code{bilinear}.
This can be used to override the bilinear transformation by R. 
Alternatively, a simplified second order Butterworth filter can 
be used if \code{bilinear = "butter"} is specified.}
  \item{unit}{\code{Character} scalar, unit of the instrument parameters. Either
\code{"f"} (rad/s) or \code{"w"} (pulsation frequencies), default 
is \code{"f"}.}
}

\value{A deconvolved data set of the same structure as the input data set.}

\author{Michael Dietze}



\seealso{\code{read.sac}, \code{zoo}}
\examples{
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
#plot.signal(time = t, 
#            data = rbind(x, x.decon), 
#            main = c("Raw signal", "Deconvolved signal"),
#            xlab = "Time (min)")

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

#plot.spectrogram(data = S.1, legend = "dB")
#plot.spectrogram(data = S.2, legend = "dB")
}
