% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/General_Functions.R
\name{filterSpeed}
\alias{filterSpeed}
\title{Calculates the ground speed for each location in a track and removes locations requiring movements above a speed threshold.}
\usage{
filterSpeed(data, lon = "lon", lat = "lat", time = "time", threshold)
}
\arguments{
\item{data}{Data frame with tracking data for 1 individual, must contain fields with longitude, latitude, and time.}

\item{lon}{Character string giving the name of longitude variable.}

\item{lat}{Character string giving the name of latitude variable.}

\item{time}{Character string giving the name of the time variable, time must be in POSIXct format.}

\item{threshold}{Maximum ground speed for species in km/hr.}
}
\value{
Data frame with speed, distance between points, time between points added. Locations with unrealistic speeds are removed.
The alogrithm is iterative, so consecutive unrealistic locations will be removed.
}
\description{
Calculates the ground speed for each location in a track and removes locations requiring movements above a speed threshold.
}
\section{Warning}{

This function assumes that time values are in chronological order and all values are from a single individual.
Make sure that your data are ordered, and use a for loop or some other method to apply this to multiple tracks
}

