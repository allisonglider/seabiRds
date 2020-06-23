# ---------------------------------------------------------------------------------------------------------------
#' Prepares deployment data for use in clipping tracks
#'
#' @param deployments A data frame with deployment data. Required.
#' @param species Character string with name of the field containing the 4-letter AOU species code. Required.
#' @param metal_band Character string with name of the field containing the metal band number, input should be an integer with 9-10 digits.
#' @param colour_band Character string with name of the field containing the colour band code.
#' @param dep_id Character string with name of the field containing deployment ID (see details).
#' @param site Character string with name of the field containing the site (e.g. Coats).
#' @param subsite Character string with name of the field containing the subsite (e.g. Coats West). This is used if your study area has distinct units within the main site.
#' @param nest Character string with name of the field containing the nest id.
#' @param time_released Character string with name of the field containing deployment start time (when birds was released with tag), must be in a POSIXct compatible format (e.g. YYYY-mm-dd HH:MM).
#' @param time_recaptured Character string with name of the field containing deployment end time (when birds was recaptured with tag), must be in a POSIXct compatible format (e.g. YYYY-mm-dd HH:MM)
#' @param dateFormat Character string specifying the POSIX standard format for times.
#' @param dep_tz Timezone of deployment.
#' @param dep_lon Character string with name of the field containing the longitude of colony (or nest), default is NULL.
#' @param dep_lat Character string with name of the field containing the latitude of colony (or nest), default is NULL.
#' @param status_on Breeding status at start of deployment (E: eggs, C: chicks, F: failed-breeder, N: non-breeder, P: pre-breeder, J: juvenile).
#' @param status_off Breeding status at end of deployment (E: eggs, C: chicks, F: failed-breeder, N: non-breeder, P: pre-breeder, J: juvenile).
#' @param mass_on Character string with name of the field containing the bird mass (g) at start of deployment.
#' @param mass_off Character string with name of the field containing the bird mass (g) at end of deployment.
#' @param exclude Character string with name of the field containing flags for deployments with a significant treatment, which could make the data unsuitable for other analysis (e.g. Fed, Handicapped, Wing-clipped).
#' @param gps_id Character string with name of the field containing the name of the GPS tag deployed.
#' @param tdr_id Character string with name of the field containing the name of the TDR tag deployed.
#' @param acc_id Character string with name of the field containing the name of the ACC tag deployed.
#' @param gls_id Character string with name of the field containing the name of the GLS tag deployed.
#' @param mag_id Character string with name of the field containing the name of the magnetometer tag deployed.
#' @param cam_id Character string with name of the field containing the name of the camera tag deployed.
#' @param hrl_id Character string with name of the field containing the name of the hear rate logger tag deployed.
#' @param keep List of variable names for other dpeloyment data to keep with output.
#'
#' @details
#'
#' dep_id is the key field for matching deployment information to the GPS data. If your GPS data has a single data file for
#' each deployment (Technosmart units), then the dep_id should be consistent with the name of this file. If you have remotely
#' downloaded data, where all the locations from all units are mixed together (Ecotone units), then leave this field as NA. The
#' function willcreate a 'dep_id' based on the tag and metal_band from each deployment.
#'
#' dep_tz and tagTZ are used to make sure a consistent time zone is used clip the GPS data. The output times from this function
#' will be in the tagTZ. Most devices record time in 'UTC'. You can look up timezone codes here: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones.
#' Eastern time is 'US/Eastern' and Pacific time is 'US/Pacific'.
#'
#' @return A new dataframe with deployment times in the same timzone as the GPS data and field names that are compatible with other functions in this package.

formatDeployments <- function(deployments, species, metal_band, colour_band, dep_id,
                              site, subsite = NA, nest = NA, dep_lon = NA, dep_lat = NA,
                              time_released, time_recaptured, dateFormat = "%Y-%m-%d %H:%M", dep_tz,
                              status_on = NA, status_off = NA, mass_on = NA, mass_off = NA, exclude = NA,
                              gps_id = NA, tdr_id = NA, acc_id = NA, gls_id = NA, mag_id = NA, cam_id = NA, hrl_id = NA,
                              keep = NULL)
{

  if (is.na(dep_lon) | is.na(dep_lat)) {
    deployments$dep_lon <- as.numeric(NA)
    deployments$dep_lat <- as.numeric(NA)
    dep_lat <- "dep_lat"
    dep_lon <- "dep_lon"
  }

  if (is.na(subsite)) {
    deployments$subsite <- NA
    subsite <- 'subsite'
  }

  if (is.na(nest)) {
    deployments$nest <- NA
    nest <- 'nest'
  }

  if (is.na(status_on)) {
    deployments$status_on <- NA
    status_on <- 'status_on'
  }

  if (is.na(status_off)) {
    deployments$status_off <- NA
    status_off <- 'status_off'
  }

  if (is.na(mass_on)) {
    deployments$mass_on <- as.numeric(NA)
    mass_on <- 'mass_on'
  }

  if (is.na(mass_off)) {
    deployments$mass_off <- as.numeric(NA)
    mass_off <- 'mass_off'
  }

  if (is.na(exclude)) {
    deployments$exclude <- NA
    exclude <- 'exclude'
  }

  if (is.na(gps_id)) {
    deployments$gps_id <- NA
    gps_id <- 'gps_id'
  }

  if (is.na(tdr_id)) {
    deployments$tdr_id <- NA
    tdr_id <- 'tdr_id'
  }

  if (is.na(acc_id)) {
    deployments$acc_id <- NA
    acc_id <- 'acc_id'
  }

  if (is.na(gls_id)) {
    deployments$gls_id <- NA
    gls_id <- 'gls_id'
  }

  if (is.na(gls_id)) {
    deployments$gls_id <- NA
    gps_id <- 'gps_id'
  }

  if (is.na(mag_id)) {
    deployments$mag_id <- NA
    mag_id <- 'mag_id'
  }

  if (is.na(cam_id)) {
    deployments$cam_id <- NA
    cam_id <- 'cam_id'
  }

  if (is.na(hrl_id)) {
    deployments$hrl_id <- NA
    hrl_id <- 'hrl_id'
  }

  # Extract key variables from deployment data and standardize names
  if (is.null(keep) == F) {
    dep <- deployments[,c(species, metal_band, colour_band, dep_id,
                          site, subsite, nest, dep_lon, dep_lat,
                          time_released, time_recaptured, status_on, status_off, mass_on, mass_off,
                          gps_id, tdr_id, acc_id, gls_id, mag_id, cam_id, hrl_id, exclude, keep)]
    names(dep) <- c('species', 'metal_band', 'colour_band', 'dep_id',
                    'site', 'subsite', 'nest','dep_lon', 'dep_lat',
                    'time_released', 'time_recaptured', 'status_on', 'status_off', 'mass_on', 'mass_off',
                    'gps_id', 'tdr_id', 'acc_id', 'gls_id', 'mag_id', 'cam_id', 'hrl_id', 'exclude', keep)
  }

  if (is.null(keep) == T) {
    dep <- deployments[,c(species, metal_band, colour_band, dep_id,
                          site, subsite, nest, dep_lon, dep_lat,
                          time_released, time_recaptured, status_on, status_off, mass_on, mass_off,
                          gps_id, tdr_id, acc_id, gls_id, mag_id, cam_id, hrl_id, exclude)]
    names(dep) <- c('species', 'metal_band', 'colour_band', 'dep_id',
                    'site', 'subsite', 'nest','dep_lon', 'dep_lat',
                    'time_released', 'time_recaptured', 'status_on', 'status_off', 'mass_on', 'mass_off',
                    'gps_id', 'tdr_id', 'acc_id', 'gls_id', 'mag_id', 'cam_id', 'hrl_id', 'exclude')
  }

  # Convert deployment times to GPS times
  dep$time_released <- lubridate::force_tz(as.POSIXct(strptime(dep$time_released, dateFormat)), tz = dep_tz)
  dep$time_released <- lubridate::with_tz(dep$time_released, tz = 'UTC')
  dep$time_recaptured <- lubridate::force_tz(as.POSIXct(strptime(dep$time_recaptured, dateFormat)), tz = dep_tz)
  dep$time_recaptured <- lubridate::with_tz(dep$time_recaptured, tz = 'UTC')

  if (length(unique(dep$dep_id)) == 1 & is.na(unique(dep$dep_id)[1])) {
    dep$dep_id <- paste0(dep$tag, '_', dep$metal_band)
  }

  # Convert empty characters to NA
  dep$colour_band[dep$colour_band == ""] <- NA
  dep$status_on[dep$status_on == ""] <- NA
  dep$status_off[dep$status_off == ""] <- NA
  dep$dep_id[dep$dep_id == ""] <- NA
  dep$site[dep$site == ""] <- NA
  dep$subsite[dep$subsite == ""] <- NA
  dep$nest[dep$nest == ""] <- NA
  dep$gps_id[dep$gps_id == ""] <- NA
  dep$tdr_id[dep$tdr_id == ""] <- NA
  dep$gls_id[dep$gls_id == ""] <- NA
  dep$mag_id[dep$mag_id == ""] <- NA
  dep$cam_id[dep$cam_id == ""] <- NA
  dep$hrl_id[dep$hrl_id == ""] <- NA
  dep$exclude[dep$exclude == ""] <- NA


  # check for duplicate dep_id
  if (max(table(dep$dep_id)) > 1) stop("All dep_id values must be unique", call. = F)

  # Make sure status_on and status_off are upper case
  dep$status_on <- toupper(as.character(dep$status_on))
  dep$status_off <- toupper(as.character(dep$status_off))

  # check all entries for status_on and status_off are valid
  status_values <- c('E','C','F','N','P','J',NA)
  if (sum(dep$status_on %in% status_values) != length(dep$status_on)) stop("Values in status_on can only be: E, C, F, N, P, J, or NA", call. = F)
  if (sum(dep$status_off %in% status_values) != length(dep$status_off)) stop("Values in status_off can only be: E, C, F, N, P, J, or NA", call. = F)

  # check all mass values are
  if (!is.numeric(dep$mass_on)) stop('Values in mass_on must be numeric', call. = F)
  if (!is.numeric(dep$mass_off)) stop('Values in mass_off must be numeric', call. = F)
  if (min(dep$mass_on, na.rm = T) <= 0) stop('Values in mass_on must be >0 or NA', call. = F)
  if (min(dep$mass_off, na.rm = T) <= 0) stop('Values in mass_off must be >0 or NA', call. = F)

  # check metal_band values
  if (!is.integer(dep$metal_band) | min(dep$metal_band, na.rm = T) < 10000000 | max(dep$metal_band, na.rm = T) > 999999999) stop('Values in metal_band must be integers with 8 or 9 digits', call. = F)

  # check dep_lon values
  if (sum(is.na(dep$dep_lon)) < length(dep$dep_lon)) {
    if (min(dep$dep_lon, na.rm = T) < -180 | max(dep$dep_lon, na.rm = T) > 180) stop('Values in dep_lon must be between -180 and 180', call. = F)
  }
  # check dep_lat values
  if (sum(is.na(dep$dep_lat)) < length(dep$dep_lon)) {
    if (min(dep$dep_lat, na.rm = T) < -90 | max(dep$dep_lat, na.rm = T) > 90) stop('Values in dep_lat must be between -90 and 90', call. = F)
  }

  # Return formatted deployment data
  dep

  #' @export formatDeployments
}

# ---------------------------------------------------------------------------------------------------------------
#' Reads in raw GPS data and sets up standard field names.
#'
#' @param inputFolder Folder containing all the raw GPS files to be processed.
#' @param deployments Name of object with deployment data.
#' @param tagTZ Timezone of GPS data.
#' @param tagType Type of GPS biologger used, options are "Technosmart" and "Ecotone".
#' @param date_format POSIXct string indicating how dates are formatted in the GPS files.
#'
#' @details
#'
#' Technosmart can export GPS data with the following date formats: %d-%m-%Y, %m-%d-%Y, "%Y-%m-%d, %Y-%d-%m. All the files in the inputFolder need to have the same formatting.
#'
#' The function will print lists of: GPS files that contain no data, GPS files that have no matching deployments in the deployment data, and deployments that have no matching GPS files. This is intended to help you identify errors in data entry or file naming. For technosmart units, I recommend you re-export data any time you change the file name because X-Manager includes the file name as a column in the data.
#'
#' @return A new dataframe containing all GPS data.

readGPSData <- function(inputFolder,
                        deployments,
                        tagTZ = "UTC",
                        tagType = "Technosmart",
                        date_format = "%d-%m-%Y") {

  if (!(tagType %in% c("Technosmart","Ecotone"))){
    warning("Supported tagTypes are: Technosmart and Ecotone. If you have a different biologger please contact me.")
  }


  if (tagType == "Technosmart") {
    output <- readTechnosmartGPS(inputFolder = inputFolder,
                                 deployments = deployments,
                                 tagTZ = tagTZ,
                                 date_format = date_format)
  }

  if (tagType == "Ecotone") {
    output <- readEcotoneGPS(inputFolder = inputFolder,
                             deployments = deployments,
                             tagTZ = tagTZ)
  }


  output

  #' @export readGPSData
}

# ---------------------------------------------------------------------------------------------------------------

readTechnosmartGPS <- function(inputFolder,
                               deployments,
                               tagTZ = "UTC",
                               date_format = date_format) {

  dd <- list.files(inputFolder, pattern = '.txt', full.names = T)

  emptyfiles <- dd[file.size(dd) == 0]
  if (length(emptyfiles) > 0) {
    print("-- Files containing no data --")
    print(emptyfiles)
  }

  .matchFiles(files = dd, deployment_ids = deployments$dep_id)

  dd <- dd[file.size(dd) > 0]

  output <- data.frame()

  for (i in 1:nrow(deployments)) {

    if (is.na(deployments$dep_id[i]) == F) {

      # get lists of file names
      theFiles <- dd[grep(deployments$dep_id[i], dd)]

      if (length(theFiles > 0)) {

        temp <- combineFiles(files = theFiles,
                             pattern = "txt",
                             type = "txt",
                             sep = "\t",
                             stringsAsFactors = F,
                             header = F)

        if (nrow(temp) > 5) {

          # set names and format date
          names(temp) <- c("time","lat","lon","altitude","gpsspeed","satellites","hdop","maxsignal")
          df <- paste0(date_format, ",%T")

          # check that date format is correct
          if (is.na(as.POSIXct(strptime(temp$time[1], df), tz = tagTZ))) stop(paste("Check date format is correct for", deployments$dep_id[i]), call. = F)

          # format dates
          temp$time <- as.POSIXct(strptime(temp$time, df), tz = tagTZ)

          # order data and remove duplicate records
          temp <- temp[order(temp$time),]
          temp <- temp[duplicated(temp) == F,]

          # add fields for metal_band and tag and deployment
          temp$dep_id <- deployments$dep_id[i]
          temp <- temp[,c("dep_id","time","lon","lat",names(temp)[!(names(temp) %in% c("dep_id","time","lon","lat"))])]
          output <- rbind(output, temp)

        } else (print(paste("-- Less than 5 locations:", deployments$dep_id[i], "- not processed")))

      }

    }

  }

  output
}

# ---------------------------------------------------------------------------------------------------------------

readEcotoneGPS <- function(inputFolder,
                           deployments,
                           tagTZ = "UTC") {


  theFiles <- list.files(inputFolder, full.names = T)

  output <- combineFiles(files = theFiles,
                         pattern = "csv",
                         type = "csv",
                         sep = ";",
                         stringsAsFactors = F,
                         header = T)
  output <- unique(output)

  output$time <- paste(output$Year, output$Month, output$Day, output$Hour, output$Minute, output$Second, sep = "-")
  output$time <- as.POSIXct(strptime(output$time, "%Y-%m-%d-%H-%M-%S"), tz = tagTZ)
  output <- output[order(output$Logger.ID, output$time),]

  names(output) <- gsub("[.]","",names(output))
  output$tag <- output$LoggerID
  output$lon <- output$Longitude
  output$lat <- output$Latitude
  output$gpsspeed <- output$Speed
  output <- output[,names(output)[!(names(output) %in% c("LoggerID","Longitude","Latitude",
                                                         "Year","Month","Day","Hour","Minute","Second",
                                                         "Rawlatitude","RawLongitude","Speed"))]]
  names(output) <- tolower(names(output))


  output <- merge(output, deployments[,c("tag","metal_band","dep_id")])
  output$dep_id <- paste0(output$tag, '_', output$metal_band)

  output <- output[,c("metal_band","tag","dep_id","time","lon","lat",
                      names(output)[!(names(output) %in% c("metal_band","tag","dep_id","time","lon","lat"))])]

  output <- subset(output, !is.na(output$lon))

  output
}

# ---------------------------------------------------------------------------------------------------------------
#' Cleans up GPS data, by clipping to deployment times and filtering unrealistic locations.
#'
#' @param data Name of object with formatted GPS data.
#' @param deployments Name of object with formatted deployment data.
#' @param tagTZ Timezone of GPS data.
#' @param speedThresold Fastest possible movement in km/hr.
#' @param plot Should data be plotted (TRUE or FALSE).
#'
#' @details This function clips the GPS data to the start and end times of each GPS deployment. The link between deployment
#' times and the GPS data is made using the 'dep_id' field. Because
#' Ecotone GPS units are remotely downloaded and data from multiple devices are stored in the same output file, this connection
#' is missing. In this case the function creates a 'dep_id' field in each data frame by concatenating the tag
#'
#' @return A new dataframe containing cleaned GPS data.

cleanGPSData <- function(data,
                         deployments,
                         tagTZ = "UTC",
                         speedThreshold = NA,
                         plot = T)
{

  output <- data.frame()
  theDeps <- unique(data$dep_id)

  for (dd in 1:length(theDeps)) {

    temp <- subset(data, data$dep_id == theDeps[dd])

    tt <- subset(deployments, deployments$dep_id == theDeps[dd])
    if (nrow(tt) > 0) {

      if (is.na(tt$time_recaptured)) tt$time_recaptured <- max(temp$time, na.rm = T)

      if ("inrange" %in% names(temp)) {
        temp$lon[temp$inrange == 1] <- tt$dep_lon
        temp$lat[temp$inrange == 1] <- tt$dep_lat
        temp <- subset(temp, is.na(temp$lon) == F)
      }

      if (is.na(tt$dep_lon)) {
        temp$colDist <- getColDist(lon = temp$lon, lat = temp$lat, colonyLon = temp$lon[temp$time >= tt$time_released][1], colonyLat = temp$lat[temp$time >= tt$time_released][1])
        yy <- "Distance from first location (km)"
      } else {
        temp$colDist <- getColDist(lon = temp$lon, lat = temp$lat, colonyLon = tt$dep_lon, colonyLat = tt$dep_lat)
        yy <- "Distance from colony (km)"
      }

      newData <- subset(temp, temp$time >= tt$time_released & temp$time <= tt$time_recaptured)

      if (nrow(newData) > 5) {

        newData$dist <- getDist(newData$lon, newData$lat)
        newData$dt <- getDT(newData$time, units = "hours")
        newData$speed <- newData$dist/newData$dt

        if (!is.na(speedThreshold)) {
          newData <- filterSpeed(newData, threshold = speedThreshold)
        }

        if (plot) {

          pp <- .cleanGPSDataPlot(temp, newData, tt, yy)
          print(pp)
          Sys.sleep(3)

          readline("Press [enter] for next plot")

        }

        output <- rbind(output, newData)
      } else (print(paste("Less than 5 deployed locations for", tt$dep_id, "- removed")))
    }
  }

  output

  #' @export cleanGPSData
}

# -----
# Sub-function for plotting, called inside of cleanGPSData

.cleanGPSDataPlot <- function(temp, newData, tt, yy) {

  if (max(temp$colDist) < 500) {
    world <- rnaturalearth::ne_countries(scale = 50, returnclass = 'sf')
  } else {
    world <- rnaturalearth::ne_countries(scale = 110, returnclass = 'sf')
  }

  ss <- min(c(temp$time[1],tt$time_released))
  ee <- max(c(temp$time[nrow(temp)],tt$time_recaptured))

  suppressMessages(
    myPlot <- ggplot2::ggplot(temp, ggplot2::aes(x = time, y = colDist)) +
      ggplot2::geom_line(col = "red") +
      ggplot2::geom_point(col = "red") +
      ggplot2::geom_point(data = newData, ggplot2::aes(x = time, y = colDist)) +
      ggplot2::geom_line(data = newData, ggplot2::aes(x = time, y = colDist)) +
      ggplot2::geom_vline(xintercept = c(tt$time_released, tt$time_recaptured), linetype = 2, col = "red") +
      ggplot2::xlim(ss,ee) +
      ggplot2::theme_light() +
      ggplot2::labs(title = paste(temp$dep_id[1]), y = yy, x = "Time")
  )
  xran <- range(temp$lon)
  yran <- range(temp$lat)

  suppressMessages(
    myMap<- ggplot2::ggplot(data = newData) +
      ggplot2::geom_sf(data = world) +
      ggplot2::geom_point(data = temp, ggplot2::aes(x = lon, y = lat), col = 'red') +
      ggplot2::geom_path(data = temp, ggplot2::aes(x = lon, y = lat), col = 'red') +
      ggplot2::geom_point(data = newData, ggplot2::aes(x = lon, y = lat)) +
      ggplot2::geom_path(data = newData, ggplot2::aes(x = lon, y = lat)) +
      #ggplot2::geom_point(data = tt, ggplot2::aes(x = tt$dep_lon, y = tt$dep_lon), fill = 'green', shape = 24, size = 3) +
      ggplot2::coord_sf(xlim = xran, ylim = yran) +
      ggplot2::theme_light() +
      ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90)) +
      ggplot2::labs(title = paste(temp$dep_id[1]), x = "", y = "")
  )

  pp <- cowplot::plot_grid(myPlot, myMap)
  pp

  #' @export .cleanGPSDataPlot
}

# -----
# Sub-function to check if files in the input folder have a matching record in the deployment data

.matchFiles <- function(files, deployment_ids) {
  mf <- data.frame(
    files = files,
    match = NA
  )

  for (theDeps in deployment_ids) {
    mf$match[grep(theDeps, mf$files)] <- theDeps
  }

  unmatchedFiles <- mf$files[is.na(mf$match)]

  if (length(unmatchedFiles) > 0) {
    print("-- Files that have no matching deployment --")
    print(as.character(unmatchedFiles))
  }

  unmatchedDeployments <- deployment_ids[!(deployment_ids %in% mf$match)]
  if (length(unmatchedDeployments) > 0) {
    print("-- Deployments that have no matching files --")
    unmatchedDeployments <- unmatchedDeployments[!is.na(unmatchedDeployments)]
    print(as.character(unmatchedDeployments))
  }
  #' @export .matchFiles
}

# ---------------------------------------------------------------------------------------------------------------
#' Reads in raw TDR data and sets up a standard format.
#'
#' @param inputFolder Folder containing all the raw GPS files to be processed.
#' @param deployments Name of object with deployment data.
#' @param tagTZ Timezone of TDR data.
#' @param tagType Type of TDR biologger used, options are "Technosmart".
#' @param date_format POSIXct string indicating how dates are formatted in the TDR files.
#'
#' @details
#'
#' Technosmart can export TDR data with the following date formats: %d-%m-%Y, %m-%d-%Y, "%Y-%m-%d, %Y-%d-%m. All the files in the inputFolder need to have the same formatting.
#'
#' The function will print lists of: TDR files that contain no data, TDR files that have no matching deployments in the deployment data, and deployments that have no matching TDR files. This is intended to help you identify errors in data entry or file naming. For technosmart units, I recommend you re-export data any time you change the file name because X-Manager includes the file name as a column in the data.
#'
#' @return A new dataframe containing all TDR data.

readTDRData <- function(inputFolder,
                        deployments,
                        tagTZ = "UTC",
                        tagType = "Technosmart",
                        date_format = "%d-%m-%Y") {

  if (!(tagType %in% c("Technosmart"))){
    warning("Supported tagTypes are: Technosmart. If you have a different biologger please contact me.")
  }


  if (tagType == "Technosmart") {
    output <- readTechnosmartTDR(inputFolder = inputFolder,
                                 deployments = deployments,
                                 tagTZ = tagTZ,
                                 date_format = date_format)
  }

  output

  #' @export readTDRData
}

# ---------------------------------------------------------------------------------------------------------------

readTechnosmartTDR <- function(inputFolder = 'E:/Biologgers/Coats/TBMU/2018',
                               deployments = depData,
                               tagTZ = "UTC",
                               tagType = "Technosmart",
                               date_format = "%Y-%m-%d") {

  dd <- list.files(inputFolder, pattern = '.csv', full.names = T)

  # Message for empty files
  emptyfiles <- dd[file.size(dd) == 0]
  if (length(emptyfiles) > 0) {
    print("-- Files containing no data --")
    print(emptyfiles)
  }
  dd <- dd[file.size(dd) > 0]

  # Message for unmatched files
  .matchFiles(files = dd, deployment_ids = deployments$dep_id)

  output <- data.frame()

  for (i in 1:nrow(deployments)) {

    if (is.na(deployments$dep_id[i]) == F) {

      # get lists of file names
      theFiles <- dd[grep(deployments$dep_id[i], dd)]

      if (length(theFiles > 0)) {

        temp <- combineFiles(files = theFiles,
                             pattern = "csv",
                             type = "csv",
                             sep = ",",
                             stringsAsFactors = F,
                             header = T)

        if (nrow(temp) > 5) {

          tempNames <- names(temp)

          # combine date and time into one column if they are separate
          if ('Date' %in% tempNames & 'Time' %in% tempNames)  temp$Timestamp <- paste(temp$Date, temp$Time)

          # create empty pressure or depth field if missing
          if (('Pressure' %in% tempNames) == F)  temp$Pressure <- NA
          if (('Depth' %in% tempNames) == F)  temp$Depth <- NA

          # add fields for dep_id
          temp$dep_id <- deployments$dep_id[i]
          tdrCols <- c("dep_id","Timestamp","Depth","Pressure","Temp....C.","Activity")
          temp <- temp[,tdrCols]

          # set names and format date
          names(temp) <- c("dep_id","time","depth","pressure","temperature","activity")
          df <- paste0(date_format, " %H:%M:%OS")

          # check that date format is correct
          if (is.na(as.POSIXct(strptime(temp$time[1], df), tz = tagTZ))) stop(paste("Check date format is correct for", deployments$dep_id[i]), call. = F)

          # format dates
          temp$time <- as.POSIXct(strptime(temp$time, df), tz = tagTZ)

          # remove missing data
          temp <- subset(temp, !is.na(temp$depth) | !is.na(temp$pressure))

          # order data and remove duplicate records
          temp <- temp[order(temp$time),]
          temp <- temp[duplicated(temp) == F,]

          # get sampling frequency of data
          tt <- as.numeric(difftime(temp$time, dplyr::lag(temp$time, 1), units = 'sec'))
          freq <- round(1/getMode(tt))

          # subsample to 1 Hz if the sampling frequency was higher than 1 Hz
          if (freq > 1) {temp <- temp[seq(1, nrow(temp), freq),]}

          output <- rbind(output, temp)

          (print(paste("Finished:", deployments$dep_id[i])))


        } else (print(paste("-- Less than 5 records:", deployments$dep_id[i], "- not processed")))

      }

    }

  }

  output
}

# ---------------------------------------------------------------------------------------------------------------
