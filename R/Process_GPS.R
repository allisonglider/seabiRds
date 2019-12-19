# ---------------------------------------------------------------------------------------------------------------
#' Prepares deployment data for use in clipping tracks
#'
#' @param deployments A data frame with deployment data.
#' @param band Character string with name of the field containing band number or individual identifier.
#' @param tag Character string with name of the field containing tag IDs.
#' @param onTime Character string with name of the field containing deployment start time, must be in a POSIXct compatible format.
#' @param offTime Character string with name of the field containing deployment end time, must be in a POSIXct compatible format.
#' @param dateFormat Character string specifying the POSIX standard format for times.
#' @param colonyLon Longitude of colony (or nest or deployment), default is NULL.
#' @param colonyLat Latitude of colony (or nest or deployment), default is NULL.
#' @param depTZ Timezone of deployment.
#' @param tagTZ Timezone of GPS data.
#' @param sinceDate Limit output to data collected after a certain date.
#' @param keep List of variable names for other dpeloyment data to keep with output.
#' @return A new dataframe with deployment times in the same timzone as the GPS data and field names that are compatible with other functions in this package.

formatDeployments <- function(deployments,
                              band,
                              tag,
                              onTime,
                              offTime,
                              dateFormat = "%Y-%m-%d %H:%M",
                              colonyLon = NA,
                              colonyLat = NA,
                              gpsFile,
                              depTZ,
                              tagTZ = "UTC",
                              sinceDate = NULL,
                              keep = NULL) {

  if (is.na(colonyLon) | is.na(colonyLat)) {
    deployments$colonyLon <- NA
    deployments$colonyLat <- NA
    colonyLat <- "colonyLat"
    colonyLon <- "colonyLon"
  }

  # Extract key variables from deployment data and standardize names
  if (is.null(keep) == F) {
    dep <- deployments[,c(band, tag, onTime, offTime, colonyLon, colonyLat, gpsFile, keep)]
    names(dep) <- c("band","tag","onTime","offTime", "colonyLon", "colonyLat","gpsFile", keep)
  }

  if (is.null(keep) == T) {
    dep <- deployments[,c(band, tag, onTime, offTime, colonyLon, colonyLat, gpsFile)]
    names(dep) <- c("band","tag","onTime","offTime", "colonyLon", "colonyLat","gpsFile")
  }

  # Convert deployment times to GPS times
  dep$onTime <- lubridate::force_tz(as.POSIXct(strptime(dep$onTime, dateFormat)), tz = depTZ)
  dep$onTime <- lubridate::with_tz(dep$onTime, tz = tagTZ)
  dep$offTime <- lubridate::force_tz(as.POSIXct(strptime(dep$offTime, dateFormat)), tz = depTZ)
  dep$offTime <- lubridate::with_tz(dep$offTime, tz = tagTZ)

  # Only keep records where the GPS was recovered
  dep <- subset(dep,is.na(dep$offTime) == F)

  # If using sinceDate, only keep deployments after this date
  if (is.null(sinceDate) == F) {dep <- subset(dep, dep$offTime >= as.POSIXct(sinceDate, tagTZ))}

  # Return formatted deployment data
  dep

  #' @export formatDeployments
}

# ---------------------------------------------------------------------------------------------------------------
#' Reads in raw GPS data, clips tracks to deployment times, and calculates distance from colony.
#'
#' @param inputFolder Folder containing all the raw GPS files to be processed.
#' @param deployments Name of object with deployment data.
#' @param tagTZ Timezone of GPS data.
#' @param tagType Type of GPS biologger used, options are "Technosmart".
#' @param plot Should a plot be created for each deployment (TRUE or FALSE).
#' @return A new dataframe containing all GPS data.

prepGPSData <- function(inputFolder,
                        deployments,
                        tagTZ = "UTC",
                        tagType = "Technosmart",
                        speedThreshold = NA,
                        plot = T) {

  if (tagType == "Technosmart") {
    output <- prepTechnosmartGPS(inputFolder = inputFolder,
                                 deployments = deployments,
                                 tagTZ = tagTZ,
                                 speedThreshold = speedThreshold,
                                 plot = plot)
  }

  output

  #' @export prepGPSData
}

# ---------------------------------------------------------------------------------------------------------------

prepTechnosmartGPS <- function(inputFolder,
                               deployments,
                               tagTZ = "UTC",
                               tagType = "Technosmart",
                               speedThreshold = NA,
                               plot = T) {

  gpsFiles <- list.files(inputFolder, pattern = '.txt', full.names = T)
  output <- data.frame()

  for (i in 1:nrow(deployments)) {

    # get lists of file names
    theFiles <- gpsFiles[grep(dep$gpsFile[i], gpsFiles)]

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
        names(temp) <- c("time","lat","lon","altitude","groundspeed","satellites","hdop","maxsignal")
        temp$time <- as.POSIXct(strptime(temp$time, "%d-%m-%Y,%T"), tz = tagTZ)
        temp <- temp[order(temp$time),]

        # add fields for band and tag and deployment
        temp$band <- deployments$band[i]
        temp$tag <- deployments$tag[i]
        temp$deployment <- deployments$gpsFile[i]

        if (is.na(dep$colonyLon[i])) {
          temp$colDist <- getColDist(lon = temp$lon, lat = temp$lat, colonyLon = subData$lon[1], colonyLat = subData$lat[1])
          yy <- "Distance from first location (km)"

        } else {
          temp$colDist <- getColDist(lon = temp$lon, lat = temp$lat, colonyLon = dep$colonyLon[i], colonyLat = dep$colonyLat[i])
          yy <- "Distance from colony (km)"
        }

        if (plot) {

          ss <- min(c(temp$time[1],deployments$onTime[i]))
          ee <- max(c(temp$time[nrow(temp)],deployments$offTime[i]))

          myPlot <- ggplot2::ggplot(temp, ggplot2::aes(x = time, y = colDist)) +
            ggplot2::geom_line() +
            ggplot2::geom_point() +
            ggplot2::geom_vline(xintercept = c(deployments$onTime[i], deployments$offTime[i]), col = "red") +
            ggplot2::xlim(ss,ee) +
            ggplot2::theme_light() +
            ggplot2::labs(title = paste(temp$deployment[1]), y = yy, x = "Time")
          print(myPlot) # plot distance from colony against time

          readline("Press [enter] for next plot")
        }

        # subset data collected during the deployment
        subData <- subset(temp, temp$time >= deployments$onTime[i] & temp$time <= deployments$offTime[i] & temp$lon != 0)
        subData <- subData[,c("band","tag","deployment",names(temp)[!(names(temp) %in% c("band","tag","deployment"))])]

        if (nrow(subData) > 5) {

          subData$dist <- getDist(subData$lon, subData$lat)
          subData$dt <- getDT(subData$time, units = "hours")
          subData$speed <- subData$dist/subData$dt

          if (!is.na(speedThreshold)) {
            subData <- filterSpeed(subData, threshold = speedThreshold)
          }

          output <- rbind(output, subData)
          #print(paste("Finished: " ,subData$deployment[1]))

        } else (print(paste("Less than 5 locations for", dep$gpsFile[i], "- not processed")))

      } else (print(paste("Less than 5 locations for", dep$gpsFile[i], "- not processed")))

    } else (print(paste("No gps files found for", dep$gpsFile[i])))

  }

  output

}
