# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


# ---------------------------------------------------------------------------------------------------------------
#' Find the modal value in a vector
#'
#' @param x A vector of values.
#' @return The modal value in \code{x}.
#' @examples
#' x <- c(1,2,1,2,4,5,7,8,4,2,3,4,4,4)
#' getMode(x)

getMode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

# ---------------------------------------------------------------------------------------------------------------
#' Calculates the distance between the colony (or any fixed location) and each point in a track.
#'
#' @param lon A vector of longitude values.
#' @param lon A vector of latitude values.
#' @param colonyLon Longitude of the colony (or fixed location).
#' @param colonyLat Latitude of the colony (or fixed location).
#' @return A vector of values (in km) giving the distance between each location in the track and the colony (or fixed location).

getColDist <- function(lon, lat, colonyLon, colonyLat) {
  raster::pointDistance(data.frame(lon = lon, lat = lat), c(colonyLon, colonyLat), lonlat = T)/1000 #distance from colony in km
}

# --------------------------------------------------------------------------------------------------------------
#' Calculates the distance between the current location and the previous location in a track.
#'
#' @param lon A vector of longitude values.
#' @param lat A vector of latitude values.
#' @return A vector of values (in km) giving the distance between each location in the track and the previous location.
#'
#' @section Warning:
#' This function assumes that longitude and latitude values are in chronological order and all values are from a single individual.
#' Make sure that your data are ordered, and use a for loop or some other method to apply this to multiple tracks

getDist <- function(lon, lat) {

  if (min(lon) < -180 | max(lon) > 180) {
    stop("Longitude values are not between -180 and 180")
  }

  if (min(lat) < -180 | max(lat) > 90) {
    stop("Longitude values are not between -180 and 180")
  }

  dd <- data.frame(lon = lon, lat = lat)


  c(NA,
    raster::pointDistance(dd[2:nrow(dd),c("lon","lat")],
                  dd[1:(nrow(dd)-1),c("lon","lat")],
                  lonlat = T)/1000)
}

# --------------------------------------------------------------------------------------------------------------
#' Calculates the time between between the current location and the previous location in a track.
#'
#' @param time A vector of time values in POSIXct format.
#' @param units The units of time for the output (eg. "days","hours","min","sec")
#' @return A numeric vector of values in the requested time unit, giving the time between each location in the track and the previous location.
#'
#' @section Warning:
#' This function assumes that time values are in chronological order and all values are from a single individual.
#' Make sure that your data are ordered, and use a for loop or some other method to apply this to multiple tracks

getDT <- function(time, units = "hours") {

  if (is.na.POSIXlt(time) == F) {
    stop("time values must be in POSIXct format")
    }

  tt <- c(NA,
    as.numeric(difftime(time[2:length(time)],
                        time[1:(length(time) - 1)],
                        units = units)))

  if (length(tt < 0) > 0) {
    stop("Negative values for time difference, data need to be in chronological order before running getDT")
  }

  tt
}

# ---------------------------------------------------------------------------------------
#' Calculates the ground speed for each location in a track and removes locations requiring movements above a speed threshold.
#'
#' @param data Data frame with tracking data for 1 individual, must contain fields with longitude, latitude, and time.
#' @param lon Character string giving the name of longitude variable.
#' @param lat Character string giving the name of latitude variable.
#' @param time Character string giving the name of the time variable, time must be in POSIXct format.
#' @param threshold Maximum ground speed for species in km/hr.
#'
#' @return Data frame with speed, distance between points, time between points added. Locations with unrealistic speeds are removed.
#' The alogrithm is iterative, so consecutive unrealistic locations will be removed.
#'
#' @section Warning:
#' This function assumes that time values are in chronological order and all values are from a single individual.
#' Make sure that your data are ordered, and use a for loop or some other method to apply this to multiple tracks

filterSpeed <- function(data, lon, lat, time, threshold) {

  if (is.na.POSIXlt(data[,time]) == F) {
    stop("time values must be in POSIXct format")
  }

  if (min(lon) < -180 | max(lon) > 180) {
    stop("Longitude values are not between -180 and 180")
  }

  if (min(lat) < -180 | max(lat) > 90) {
    stop("Longitude values are not between -180 and 180")
  }

  maxSpeed <- threshold + 1
  while (maxSpeed > threshold) {
    data$dist <- getDist(data[,lon], data[,lat])
    data$dt <- getDT(data[,time], units = "hours")
    data$speed <- data$dist/data$dt
    data <- subset(data, data$speed <= threshold)
    maxSpeed <- max(data$speed, na.rm = T)
  }

  data
}
