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
  #' @export getMode
}

# ---------------------------------------------------------------------------------------------------------------
#' Calculates the distance between the colony (or any fixed location) and each point in a track.
#'
#' @param lon A vector of longitude values.
#' @param lat A vector of latitude values.
#' @param colonyLon Longitude of the colony (or fixed location).
#' @param colonyLat Latitude of the colony (or fixed location).
#' @return A vector of values (in km) giving the distance between each location in the track and the colony (or fixed location).

getColDist <- function(lon, lat, colonyLon, colonyLat) {
  raster::pointDistance(data.frame(lon = lon, lat = lat), c(colonyLon, colonyLat), lonlat = T)/1000 #distance from colony in km
  #' @export getColDist
}

# ---------------------------------------------------------------------------------------------------------------
#' Creates a unique ID for consecutive matching values in a series.
#'
#' @param value A vector containing the values you want to create IDs for, can be any data type.
#' @param ignore Use TRUE if there are values that should not get IDs.
#' @param ignoreValue a singe value or list of values to ignore when assigning IDs.
#' @param maxSession A numeric value indicating the maximum number of repeating values that should be assigned the same ID. Defaults to Inf for no limit.
#'
#' @description This function is useful for createing a unique id for all dives or trips.
#'
#' @return A numeric vector of IDs
#'
#' @examples
#'
#' # example with numeric input, that ignores 0
#' tt <- c(0, 0, 1, 1, 1, 1, 3, 3, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 2, 2)
#' getSessions(value = tt, maxSession = Inf, ignore = TRUE, ignoreValue = 0)
#'
#' # example with text input
#' tt <- c('fly', 'fly', 'fly', 'dive', 'dive', 'dive', 'fly', 'fly', 'fly',
#' 'colony', 'colony', 'colony','fly','fly')
#' getSessions(value = tt, ignore = FALSE)
#'
#' # example with text input that does not assign ID to colony
#' getSessions(value = tt, ignore = TRUE, ignoreValue = 'colony')

getSessions <- function(value, ignore = FALSE, ignoreValue = NULL, maxSession = Inf) {

  if (sum(is.na(value)) > 0) stop("getSessions() cannot accept NA in input", call. = F)
  if (ignore == T & is.null(ignoreValue)) stop("ignoreValue cannot be NULL when ignore == T", call. = F)

  output <- 1
  j <- 1
  k <- 0
  for (i in 2:length(value)) {
    j <- ifelse(value[i] == value[i - 1], j, j + 1)
    k <- ifelse(value[i] == value[i - 1], k + 1, 0)
    if (k >= maxSession ) {
      j <- j + 1
      k <- 0
    }
    output[i] <- j
  }

  if (ignore == T) {
    output[value %in% ignoreValue] <- NA
    output <- as.numeric(as.factor(output))
  }

  output

  #' @export getSessions
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

  if (min(lon, na.rm = T) < -180 | max(lon, na.rm = T) > 180) {
    stop("Longitude values are not between -180 and 180")
  }

  if (min(lat, na.rm = T) < -180 | max(lat, na.rm = T) > 90) {
    stop("Longitude values are not between -180 and 180")
  }

  dd <- data.frame(lon = lon, lat = lat)


  c(NA,
    raster::pointDistance(dd[2:nrow(dd),c("lon","lat")],
                  dd[1:(nrow(dd)-1),c("lon","lat")],
                  lonlat = T)/1000)
  #' @export getDist
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

  if (class(time)[1] != "POSIXct") {
    stop("time values must be in POSIXct format")
    }

  tt <- c(NA,
    as.numeric(difftime(time[2:length(time)],
                        time[1:(length(time) - 1)],
                        units = units)))

  if (min(tt < 0, na.rm = T)) {
    stop("Negative values for time difference, data need to be in chronological order before running getDT")
  }

  tt

  #' @export getDT
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

filterSpeed <- function(data, lon = "lon", lat = "lat", time = "time", threshold) {

  if (class(data[,"time"])[1] != "POSIXct") {
    stop("time values must be in POSIXct format")
  }

  if (min(data[,"lon"], na.rm = T) < -180 | max(data[,"lon"], na.rm = T) > 180) {
    stop("Longitude values are not between -180 and 180")
  }

  if (min(data[,"lat"], na.rm = T) < -90 | max(data[,"lat"], na.rm = T) > 90) {
    stop("Longitude values are not between -180 and 180")
  }

  maxSpeed <- threshold + 1
  while (maxSpeed > threshold) {
    data$dist <- getDist(data[,lon], data[,lat])
    data$dt <- getDT(data[,time], units = "hours")
    data$speed <- data$dist/data$dt
    data <- subset(data, data$speed <= threshold | is.na(data$speed))
    maxSpeed <- max(data$speed, na.rm = T)
  }

  data
  #' @export filterSpeed
}

# ---------------------------------------------------------------------------------------
#' Combines files from a folder into a single dataframe
#'
#' @param files a list  of file paths to the files to be combined.
#' @param pattern Character string of pattern in file name to select files.
#' @param type File type suffix, supported options are "txt" and "csv".
#' @param sep File delimiter, if required.
#' @param stringsAsFactors True or False if strings should be read as factors, defaults to F
#' @param header Should first row be read as file header.
#' @param skip Rows at the start of the file to skip.
#' @param combineColumns Change to TRUE if you want to merge files by columns, default is FALSE which binds files by rows.
#'
#' @return Dataframe with all files combined.


combineFiles <- function(files,
                         pattern = NULL,
                         type = "csv",
                         sep = NULL,
                         stringsAsFactors = F,
                         header = T,
                         skip = 0,
                         combineColumns = F) {

  temp <- data.frame()

  if (type %in% c("csv","txt")) {
    for (ff in 1:length(files)) {
      if (file.size(files[ff]) > 0) {
        tt <- utils::read.csv(files[ff], sep = sep, header = header, stringsAsFactors = stringsAsFactors, skip = skip)
        if (ncol(tt) == 1) tt <- utils::read.csv(files[ff], header = header, stringsAsFactors = stringsAsFactors, skip = skip)
        if (combineColumns == F) temp <- rbind(temp, tt)
        if (combineColumns == T & ff == 1) temp <- tt
        if (combineColumns == T & ff > 1) temp <- merge(temp, tt, all = T)
      }
    }
  }

  # if (type %in% c("xlsx")) {
  #   for (ff in 1:length(files)) {
  #     if (file.size(files[ff]) > 0) {
  #       tt <- read.xlsx(files[ff], sep = sep, sheetIndex = sheetIndex, stringsAsFactors = stringsAsFactors)
  #       if (combineColumns == T & ff == 1) temp <- tt
  #       if (combineColumns == T & ff > 1) temp <- merge(temp, tt, all = T)      }
  #   }
  # }

  temp

  #' @export combineFiles

}

# ---------------------------------------------------------------------------------------
#' Calculates dominant sampling frequency from a list of times
#'
#' @param time vector of POSIXct time values
#'
#' @details This function calculates the time difference between consecutive values and returns the mode of those differences in Hz. If your sampling frequency was >1 Hz, make sure that the POSIXct vector was formatted to include miliseconds. This can be done using the \%OS conversion specification instead of \%S, see ?strptime. When working with data at > 1Hz, it is also useful to set options(digits.secs = 6), so that your times will print with miliseconds in R.
#'
#' @examples
#' options(digits.secs = 6)
#' myTimes <- c("2019-07-16 17:43:52.04","2019-07-16 17:43:52.08",
#' "2019-07-16 17:43:52.12", "2019-07-16 17:43:52.16", "2019-07-16 17:43:52.20",
#' "2019-07-16 17:43:52.24","2019-07-16 17:43:52.28")
#' myTimes <- as.POSIXct(strptime(myTimes, '%Y-%m-%d %H:%M:%OS', tz = 'UTC'))
#' print(myTimes)
#' getFrequency(myTimes)
#'

getFrequency <- function(time) {

  mt <- signif(getMode(getDT(time, units = "sec")),2)
  if (mt == 0) stop("Dominant sampling frequency was 0, check that your POSIXct values use milliseconds: %H:%M:%OS", call. = F)
   1/signif(getMode(getDT(time, units = "sec")),2)

  #' @export getFrequency
}
