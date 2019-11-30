# ---------------------------------------------------------------------------------------------------------------
#' Prepares deployment data for use in clipping tracks
#'
#' @param deployments A data frame with deployment data.
#' @param birdID Character string with name of the field containing bird IDs.
#' @param gpsID Character string with name of the field containing GPS IDs.
#' @param onTime Character string with name of the field containing deployment start time, must be in a POSIXct compatible format.
#' @param offTime Character string with name of the field containing deployment end time, must be in a POSIXct compatible format.
#' @param dateFormat Character string specifying the POSIX standard format for times (default "%Y-%m-%d %H:%M")
#' @param depTZ Timezone of deployment.
#' @param gpsTZ Timezone of GPS data (default is "UTC").
#' @param sinceDate Limit output to data collected after a certain date.
#' @param keep List of variable names for other dpeloyment data to keep with output.
#' @return A new dataframe with deployment times in the same timzone as the GPS data and field names that are compatible with other functions in this package.


formatDeployments <- function(deployments,
                              birdID,
                              gpsID,
                              onTime,
                              offTime,
                              dateFormat = "%Y-%m-%d %H:%M",
                              gpsFile,
                              depTZ,
                              gpsTZ = "UTC",
                              sinceDate = NULL,
                              keep = NULL) {

  # Extract key variables from deployment data and standardize names
  if (is.null(keep) == F) {
    dep <- deployments[,c(birdID, gpsID, onTime, offTime, gpsFile, keep)]
    names(dep) <- c("bird","gps","onTime","offTime","gpsFile", keep)
  }

  if (is.null(keep) == T) {
    dep <- deployments[,c(birdID, gpsID, onTime, offTime, gpsFile)]
    names(dep) <- c("bird","gps","onTime","offTime","gpsFile")
  }

  # Convert deployment times to GPS times
  dep$onTime <- lubridate::force_tz(as.POSIXct(strptime(dep$onTime, dateFormat)), tz = depTZ)
  dep$onTime <- lubridate::with_tz(dep$onTime, tz = gpsTZ)
  dep$offTime <- lubridate::force_tz(as.POSIXct(strptime(dep$offTime, dateFormat)), tz = depTZ)
  dep$offTime <- lubridate::with_tz(dep$offTime, tz = gpsTZ)

  # Only keep records where the GPS was recovered
  dep <- subset(dep,is.na(dep$offTime) == F)

  # If using sinceDate, only keep deployments after this date
  if (is.null(sinceDate) == F) {dep <- subset(dep, dep$offTime >= as.POSIXct(sinceDate, gpsTZ))}

  # Return formatted deployment data
  dep
}
