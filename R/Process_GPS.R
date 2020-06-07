# ---------------------------------------------------------------------------------------------------------------
#' Prepares deployment data for use in clipping tracks
#'
#' @param deployments A data frame with deployment data.
#' @param band Character string with name of the field containing band number or individual identifier.
#' @param tag Character string with name of the field containing tag IDs.
#' @param depID Character string with name of the field containing deployment ID (see Details).
#' @param onTime Character string with name of the field containing deployment start time, must be in a POSIXct compatible format.
#' @param offTime Character string with name of the field containing deployment end time, must be in a POSIXct compatible format.
#' @param dateFormat Character string specifying the POSIX standard format for times.
#' @param colonyLon Longitude of colony (or nest) , default is NULL.
#' @param colonyLat Latitude of colony (or nest), default is NULL.
#' @param depTZ Timezone of deployment.
#' @param tagTZ Timezone of GPS data.
#' @param sinceDate Limit output to data collected after a certain date.
#' @param keep List of variable names for other dpeloyment data to keep with output.
#'
#' @details
#'
#' depID is the key field for matching deployment information to the GPS data. If your GPS data has a single data file for
#' each deployment (Technosmart units), then the depID should be consistent with the name of this file. If you have remotely
#' downloaded data, where all the locations from all units are mixed together (Ecotone units), then leave this field as NA. The
#' function willcreate a 'depID' based on the tag and band from each deployment.
#'
#' depTZ and tagTZ are used to make sure a consistent time zone is used clip the GPS data. The output times from this function
#' will be in the tagTZ. Most devices record time in 'UTC'. You can look up timezone codes here: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones.
#' Eastern time is 'US/Eastern' and Pacific time is 'US/Pacific'.
#'
#' @return A new dataframe with deployment times in the same timzone as the GPS data and field names that are compatible with other functions in this package.

formatDeployments <- function(deployments, band, tag, depID, onTime, offTime, dateFormat = "%Y-%m-%d %H:%M", colonyLon = NA,
                              colonyLat = NA, depTZ, tagTZ = "UTC", sinceDate = NULL, keep = NULL)
  {

  if (is.na(colonyLon) | is.na(colonyLat)) {
    deployments$colonyLon <- NA
    deployments$colonyLat <- NA
    colonyLat <- "colonyLat"
    colonyLon <- "colonyLon"
  }

  # Extract key variables from deployment data and standardize names
  if (is.null(keep) == F) {
    dep <- deployments[,c(band, tag, onTime, offTime, colonyLon, colonyLat, depID, keep)]
    names(dep) <- c("band","tag","onTime","offTime", "colonyLon", "colonyLat","depID", keep)
  }

  if (is.null(keep) == T) {
    dep <- deployments[,c(band, tag, onTime, offTime, colonyLon, colonyLat, depID)]
    names(dep) <- c("band","tag","onTime","offTime", "colonyLon", "colonyLat","depID")
  }

  # Convert deployment times to GPS times
  dep$onTime <- lubridate::force_tz(as.POSIXct(strptime(dep$onTime, dateFormat)), tz = depTZ)
  dep$onTime <- lubridate::with_tz(dep$onTime, tz = tagTZ)
  dep$offTime <- lubridate::force_tz(as.POSIXct(strptime(dep$offTime, dateFormat)), tz = depTZ)
  dep$offTime <- lubridate::with_tz(dep$offTime, tz = tagTZ)

  # Only keep records where the GPS was recovered
  #dep <- subset(dep, is.na(dep$offTime) == F & is.na(dep$depID) == F)

  # If using sinceDate, only keep deployments after this date
  if (is.null(sinceDate) == F) {dep <- subset(dep, dep$offTime >= as.POSIXct(sinceDate, tagTZ))}

  if (length(unique(dep$depID)) == 1 & is.na(unique(dep$depID)[1])) {
    dep$depID <- paste0(dep$tag, '_', dep$band)
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
#' @param tagType Type of GPS biologger used, options are "Technosmart".
#' @return A new dataframe containing all GPS data.

readGPSData <- function(inputFolder,
                        deployments,
                        tagTZ = "UTC",
                        tagType = "Technosmart") {

  if (!(tagType %in% c("Technosmart","Ecotone"))){
    warning("Supported tagTypes are: Technosmart and Ecotone. If you have a different biologger please contact me.")
  }


  if (tagType == "Technosmart") {
    output <- readTechnosmartGPS(inputFolder = inputFolder,
                                 deployments = deployments,
                                 tagTZ = tagTZ)
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
                               tagTZ = "UTC") {

  dd <- list.files(inputFolder, pattern = '.txt', full.names = T)
  output <- data.frame()

  for (i in 1:nrow(deployments)) {

    if (is.na(deployments$depID[i]) == F) {

      # get lists of file names
      theFiles <- dd[grep(deployments$depID[i], dd)]

      if (length(theFiles > 0)) {

        temp <- combineFiles(files = theFiles,
                             pattern = "txt",
                             type = "txt",
                             sep = "\t",
                             stringsAsFactors = F,
                             header = F)
        head(temp)

        if (nrow(temp) > 5) {

          # set names and format date
          names(temp) <- c("time","lat","lon","altitude","gpsspeed","satellites","hdop","maxsignal")
          temp$time <- as.POSIXct(strptime(temp$time, "%d-%m-%Y,%T"), tz = tagTZ)
          temp <- temp[order(temp$time),]

          # add fields for band and tag and deployment
          temp$band <- deployments$band[i]
          temp$tag <- deployments$tag[i]
          temp$depID <- deployments$depID[i]
          temp <- temp[,c("band","tag","depID","time","lon","lat",names(temp)[!(names(temp) %in% c("band","tag","depID","time","lon","lat"))])]
          output <- rbind(output, temp)

        } else (print(paste("Less than 5 locations:", deployments$depID[i], "- not processed")))

      } else (print(paste("No gps files found:", deployments$depID[i])))

    } else (print(paste("No gps file in deployment data:", deployments$tag[i], deployments$band[i])))

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


  output <- merge(output, deployments[,c("tag","band","depID")])
  output$depID <- paste0(output$tag, '_', output$band)

  output <- output[,c("band","tag","depID","time","lon","lat",
                      names(output)[!(names(output) %in% c("band","tag","depID","time","lon","lat"))])]

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
#' times and the GPS data is made using the 'depID' field. Because
#' Ecotone GPS units are remotely downloaded and data from multiple devices are stored in the same output file, this connection
#' is missing. In this case the function creates a 'depID' field in each data frame by concatenating the tag
#'
#' @return A new dataframe containing cleaned GPS data.

cleanGPSData <- function(data,
                         deployments,
                         tagTZ = "UTC",
                         speedThreshold = NA,
                         plot = T)
{

  output <- data.frame()
  theDeps <- unique(data$depID)

  for (dd in 1:length(theDeps)) {
    temp <- subset(data, data$depID == theDeps[dd])
    tt <- subset(deployments, deployments$depID == theDeps[dd])
    if (is.na(tt$offTime)) tt$offTime <- max(temp$time)

    if ("inrange" %in% names(temp)) {
      temp$lon[temp$inrange == 1] <- tt$colonyLon
      temp$lat[temp$inrange == 1] <- tt$colonyLat
      temp <- subset(temp, is.na(temp$lon) == F)
    }

    if (is.na(tt$colonyLon)) {
      temp$colDist <- getColDist(lon = temp$lon, lat = temp$lat, colonyLon = temp$lon[temp$time >= tt$onTime][1], colonyLat = temp$lat[temp$time >= tt$onTime][1])
      yy <- "Distance from first location (km)"

    } else {
      temp$colDist <- getColDist(lon = temp$lon, lat = temp$lat, colonyLon = tt$colonyLon, colonyLat = tt$colonyLat)
      yy <- "Distance from colony (km)"
    }

    newData <- subset(temp, temp$time >= tt$onTime & temp$time <= tt$offTime)

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
    } else (print(paste("Less than 5 deployed locations for", tt$depID, "- removed")))
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

  ss <- min(c(temp$time[1],tt$onTime))
  ee <- max(c(temp$time[nrow(temp)],tt$offTime))

  suppressMessages(
    myPlot <- ggplot2::ggplot(temp, ggplot2::aes(x = time, y = colDist)) +
      ggplot2::geom_line(col = "red") +
      ggplot2::geom_point(col = "red") +
      ggplot2::geom_point(data = newData, ggplot2::aes(x = time, y = colDist)) +
      ggplot2::geom_line(data = newData, ggplot2::aes(x = time, y = colDist)) +
      ggplot2::geom_vline(xintercept = c(tt$onTime, tt$offTime), linetype = 2, col = "red") +
      ggplot2::xlim(ss,ee) +
      ggplot2::theme_light() +
      ggplot2::labs(title = paste(temp$depID[1]), y = yy, x = "Time")
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
      ggplot2::geom_point(data = tt, ggplot2::aes(x = colonyLon, y = colonyLat), fill = 'green', shape = 24, size = 3) +
      ggplot2::coord_sf(xlim = xran, ylim = yran) +
      ggplot2::theme_light() +
      ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90)) +
      ggplot2::labs(title = paste(temp$depID[1]), x = "", y = "")
  )

  pp <- plot_grid(myPlot, myMap)
  pp

  #' @export .cleanGPSDataPlot
}
