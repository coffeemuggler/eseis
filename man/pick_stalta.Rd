% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pick_stalta.R
\name{pick_stalta}
\alias{pick_stalta}
\title{Calculate stal-lta-ratio.}
\usage{
pick_stalta(data, time, dt, sta, lta, freeze = FALSE, on, off)
}
\arguments{
\item{data}{\code{eseis} object, \code{numeric} vector or list of 
objects, data set to be processed.}

\item{time}{\code{POSIXct} vector, time vector of the signal(s). If not 
provided, a synthetic time vector will be created.}

\item{dt}{\code{Numeric} value, sampling period. If omitted, either 
estimated from \code{time} or set to 0.01 s (i.e., f = 100 Hz).}

\item{sta}{\code{Numeric} value, number of samples for short-term window.}

\item{lta}{\code{Numeric} value, number of samples for long-term window.}

\item{freeze}{\code{Logical} value, option to freeze lta value at start of 
an event. Useful to avoid self-adjustment of lta for long-duration events.}

\item{on}{\code{Numeric} value, threshold value for event onset.}

\item{off}{\code{Numeric} value, threshold value for event end.}
}
\value{
\code{data frame}, detected events (ID, start time, duration in 
seconds, STA-LTA vaue).
}
\description{
The function calculates the ratio of the short-term-average and 
long-term-average of the input signal.
}
\examples{

## load example data
data(rockfall)

## filter signal
rockfall_f <- signal_filter(data = rockfall_eseis,
                            f = c(1, 90), 
                            p = 0.05)
                    
## calculate signal envelope
rockfall_e <- signal_envelope(data = rockfall_f)

## pick earthquake and rockfall event
p <- pick_stalta(data = rockfall_e,
                 sta = 100, 
                 lta = 18000, 
                 freeze = TRUE, 
                 on = 5, 
                 off = 3)
                 
p$picks
                     
}
\author{
Michael Dietze
}
\keyword{eseis}
