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
#' @param lon A vector of latitude values.
#' @return A vector of values (in km) giving the distance between each location in the track and the previous location.
#'
#' @section Warning:
#' This function assumes that longitude and latitude values are in chronological order and all values are from a single individual.
#' Make sure that your data are ordered, and use a for loop or some other method to apply this to multiple tracks

getDist <- function(lon, lat) {
  dd <- data.frame(lon = lon, lat = lat)
  c(NA,
    raster::pointDistance(dd[2:nrow(dd),c("lon","lat")],
                  dd[1:(nrow(dd)-1),c("lon","lat")],
                  lonlat = T)/1000)
}
