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
#' NEW with terra package: Calculates the distance between the colony (or any fixed location) and each point in a track.
#'
#' @param lon A vector of longitude values.
#' @param lat A vector of latitude values.
#' @param colonyLon Longitude of the colony (or fixed location).
#' @param colonyLat Latitude of the colony (or fixed location).
#' @return A vector of values (in km) giving the distance between each location in the track and the colony (or fixed location).

getColDist2 <- function(lon, lat, colonyLon, colonyLat) {
  d <- terra::distance(matrix(c(lon, lat), ncol = 2, nrow = nrow(dplyr::cur_data())),
                       matrix(c(colonyLon, colonyLat),
                              ncol = 2, nrow =1),
                       lonlat = T)/1000 #distance from colony in km

  d<-c(d)
}
#' @export getColDist2

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

  if (length(value) == 1) {
    output <- 1
  } else{
    for (i in 2:length(value)) {
      j <- ifelse(value[i] == value[i - 1], j, j + 1)
      k <- ifelse(value[i] == value[i - 1], k + 1, 0)
      if (k >= maxSession ) {
        j <- j + 1
        k <- 0
      }
      output[i] <- j
    }
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


# ------

#' Check for overlapping deployments for metal band or logger id fields
#'
#' @param deployments data.frame with deployment data formatted using seabiRds::formatDeployments()
#' @param variables list of variable names to check for overlapping deployments, options include:
#' "metal_band", "gps_id", "gps_id","tdr_id","acc_id","gls_id","mag_id"
#' @param verbose logical. Print a success message if TRUE
#'
#' @return
#'
#' Warnings and an error message are returned if overlapping deployments are found for the same band or logger_id
#'
#' @export
#'


check_overlaps <- function(deployments, variables = c("metal_band", "gps_id", "gps_id","tdr_id","acc_id","gls_id","mag_id"), verbose = FALSE) {

  flag <- 0

  var_names <- c("metal_band", "gps_id", "gps_id","tdr_id","acc_id","gls_id","mag_id")

  for (v in variables) {

    if (!(v %in% var_names)) {
      stop(paste(v, "must be one of:", paste(var_names, collapse = ', ')))
    }

    if (!('time_released' %in% names(deployments))) stop('time_released column not found in deployments, deployments should be output from seabiRds::formatDeployments()')
    if (!('time_recaptured' %in% names(deployments))) stop('time_recaptured column not found in deployments, deployments should be output from seabiRds::formatDeployments()')


    mb <- na.omit(unique(deployments[,v]))
    for (m in mb) {

      temp <- subset(deployments, deployments[,v]== m)
      if (nrow(temp) > 1) {
        temp <- temp[order(temp$time_released),]
        for (i in 2:nrow(temp)) {
          idx <- which(temp$time_released[i:nrow(temp)] < temp$time_recaptured[i - 1])
          if (length(idx > 0)) {
            warning(paste0('dep_id ', temp$dep_id[i - 1],' overlaps with ', temp$dep_id[idx[1]], ' for ', v, ' ', m), call. = F)
            flag <- flag + 1
          }
        }
      }
    }
  }

  if (flag > 0) stop(paste('Overlapping deployments found. Fix deployment data before continuing. Use warnings() to see them.'), call. = FALSE)
  if (verbose & flag == 0) print(paste0("No deployment overlaps found."))
}

# -----

#' Defines a bounding box at a specified zoom level around a set of coordinates, useful
#' for making maps with a consistent aspect ratio
#'
#' @description This function returns a bounding box with a constant aspect ratio.
#' By default the zoom leve is fit to the input spatial file or the zoom level
#' can be defined by the user and centered on the center value of the input.
#'
#' @param locs An sf or sp object. The bounding box will be centered on the mid point value of
#' bound box of this object.
#' @param zoom_level Numeric. Specifies how zoomed in the bounding box should be,
#' eg. 1 = whole world and 4 = 1/4 of world. Defaults to NULL, which will calculate the zoom level required to contain locs
#' @references Code adapted from: https://www.r-bloggers.com/2019/04/zooming-in-on-maps-with-sf-and-ggplot2/
#'
#' @returns an object with class "bbox" containing four values: xmin, ymin, xmax, and ymax.
#' Values will be in units of locs (either decimal degrees or meters).
#' @export


bbox_at_zoom <- function(locs, zoom_level = NULL) {

  if (!(substr(class(locs)[1], 1, 7) %in% c('sf','Spatial'))) stop('locs must be an sf or sp object')

  if (class(locs)[1] != 'sf') {
    locs <- as(locs, 'sf')
  }
  C <- 40075016.686   # ~ circumference of Earth in meters

  bb <- sf::st_bbox(locs)

  zoom_to <- data.frame(
    X = ((bb$xmax - bb$xmin)/2) + bb$xmin,
    Y = ((bb$ymax - bb$ymin)/2) + bb$ymin
  ) |>  sf::st_as_sf(coords = c('X','Y'), crs = sf::st_crs(locs))

  if (is.null(zoom_level)) {
    if (sf::st_is_longlat(zoom_to) == T) {
      lon_zoom <- log2(360/(bb$xmax - bb$xmin))
      lat_zoom <- log2(180/(bb$ymax - bb$ymin))
      zoom_level <- min(c(lon_zoom, lat_zoom))
    } else {
      lon_zoom <- log2(C/(bb$xmax - bb$xmin))
      lat_zoom <- log2(C/(bb$ymax - bb$ymin))
      zoom_level <- min(c(lon_zoom, lat_zoom))
    }
  }

  if (sf::st_is_longlat(zoom_to) == T) {
    lon_span <- 360/2^zoom_level
    lat_span <- 180/2^zoom_level
  } else {
    lon_span <- C / 2^zoom_level
    lat_span <- C / 2^(zoom_level)
  }

  cc <- as.vector(sf::st_coordinates(zoom_to))

  lon_bounds <- c(cc[1] - lon_span / 2, cc[1] + lon_span / 2)
  lat_bounds <- c(cc[2] - lat_span / 2, cc[2] + lat_span / 2)

  bb <- sf::st_bbox(c(xmin = min(lon_bounds), xmax = max(lon_bounds),
                      ymax = min(lat_bounds), ymin = max(lat_bounds)), crs = sf::st_crs(locs))

  return(bb)
}

# -----
