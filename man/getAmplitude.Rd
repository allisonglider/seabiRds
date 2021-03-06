% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Process_ACC.R
\name{getAmplitude}
\alias{getAmplitude}
\title{Calculate amplitude in wing beats}
\usage{
getAmplitude(dat, window, time, frequency = NULL)
}
\arguments{
\item{dat}{Vector of accelerometer X value}

\item{window}{Size of moving window for estimating peaks, in seconds}

\item{time}{Vector of POSIXct times, should use \%H:\%M:\%OS if sampling frequency >1 Hz}

\item{frequency}{Sampling frequency of data, in Hz, if missing the function will estimate frequency from time}
}
\description{
Calculate amplitude in wing beats
}
\details{
Calculates inter-quartile range using a moving window of time*frequency
}
