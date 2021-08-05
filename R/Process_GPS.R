# ---------------------------------------------------------------------------------------------------------------
#' Format deployment data
#'
#' @description
#' This function standardizes deployment data, which is the first step in processing any tracking data. The function will create standard field names and formats for key data about each deployment. All deployment times will be converted to UTC, to ensure tracking data are clipped to the correct time.
#'
#' @param deployments A data frame with deployment data. Required.
#' @param dateFormat Character string specifying the POSIX standard format for your deployment times, defaults is Y-m-d H:M:S. See ?strftime for other datetime formats
#' @param dep_tz Timezone of deployment.
#' @param species Character string with name of the field containing the 4-letter AOU species code. Required.
#' @param metal_band Character string with name of the field containing the metal band number. Values should be numeric with 9-10 digits, dashes are not allowed.
#' @param colour_band Character string with name of the field containing the colour band code.
#' @param dep_id Character string with name of the field containing deployment ID (see details). Each dep_id must be unique. Required.
#' @param fill_dep_id Should missing dep_id values be filled by combining metal_band and release date, default is TRUE.
#' @param site Character string with name of the field containing the site (e.g. Coats). Required.
#' @param subsite Character string with name of the field containing the subsite (e.g. Coats West). This is used if your study area has distinct units within the main site.
#' @param nest Character string with name of the field containing the nest id. Optional.
#' @param time_released Character string with name of the field containing deployment start time (when birds was released with tag), must use date format specified with dateFormat argument. Required.
#' @param time_recaptured Character string with name of the field containing deployment end time (when birds was recaptured with tag), must use date format specified with dateFormat argument.
#' @param dep_lon Character string with name of the field containing the deployment longitude. Required.
#' @param dep_lat Character string with name of the field containing the the deployment latitude. Required.
#' @param status_on Breeding status at start of deployment. Values accepted: E - eggs, C - chicks, FB - failed-breeder, NB - non-breeder, PB - pre-breeder, J - juvenile.
#' @param status_off Breeding status at end of deployment. Values accepted: E - eggs, C - chicks, FB - failed-breeder, NB - non-breeder, PB - pre-breeder, J - juvenile.
#' @param mass_on Character string with name of the field containing the bird mass (g) at start of deployment.
#' @param mass_off Character string with name of the field containing the bird mass (g) at end of deployment.
#' @param exclude Character string with name of the field containing flags for deployments with a significant treatment, which could make the data unsuitable for other analysis (e.g. Fed, Handicapped, Wing-clipped).
#' @param fed_unfed Character string with name of the field containing information about fed/unfed treatments. Used for BLKI from Middleton. Acceptable values are: fed, unfed, semi.
#' @param gps_id Character string with name of the field containing the name of the GPS tag deployed.
#' @param tdr_id Character string with name of the field containing the name of the TDR tag deployed.
#' @param acc_id Character string with name of the field containing the name of the ACC tag deployed.
#' @param gls_id Character string with name of the field containing the name of the GLS tag deployed.
#' @param mag_id Character string with name of the field containing the name of the magnetometer tag deployed.
#' @param cam_id Character string with name of the field containing the name of the camera tag deployed.
#' @param hrl_id Character string with name of the field containing the name of the hear rate logger tag deployed.
#' @param keep List of variable names for other dpeloyment data to keep with output.
#'
#' @details dep_id is a critical field for matching deployment information to the tracking data. Every dep_id in your data must be unique. If these data will be imported into the Arctic Ecology Lab Biologging database,
#' then each dep_id must be unique within the database. Using a dep_id based on the band number and deployment date will ensure that each dep_id is unique. If your tracking data has a single data file for
#' each deployment (Technosmart, Cattrack, and Lotek units), then the dep_id should be consistent with the name of this file. If the dep_id is not contained within the file name,
#' then the readGPSdata() or readTDRdata() function you use in the next step will not be able to find the data associated with this deployment.  If you have remotely
#' downloaded data, where all the locations from all units are mixed together (Ecotone units), then you can leave this field empty use the fill_dep_id = T argument
#' to automatically generate unique dep_ids.
#'
#' dep_tz and tagTZ are used to make sure a consistent time zone is used clip the GPS data. The output times from this function will be in the tagTZ. Most devices record time in 'UTC'. You can look up timezone codes here: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones. Eastern time is 'US/Eastern' and Pacific time is 'US/Pacific'.
#'
#' fed_unfed is specific to data from black-legged kittiwakes (BLKI) at Middleton. This field is used to indicate if the bird in the deployment was part of the fed, unfed, or semi-fed groups.
#' Input values must be either fed, unfed, or semi, captilization does not matter. Other users can ignore this field.
#'
#' gps_id, tdr_id, acc_id, gls_id, mag_id, cam_id, hrl_id are each used to store the identifier for the biologger units deployed. Because some deployments use multiple devices,
#' and some devices have multiple logger types, each is recorded separately. For example, Technosmart Axy-trek units record GPS, TDR, and ACC data, so the logger id should be entered in each of these fields
#' to show that all thre data types are available. If both an Ecotone GPS and a Lotek TDR were deployed at the same time on the same bird, then record the Ecotone logger ID
#' in the gps_id field and the Lotek TDR logger id in the TDR field.
#'
#' @return A new dataframe with deployment times in UTC and field names that are compatible with other functions in this package and the lab biologging database.

formatDeployments <- function(deployments, dateFormat = "%Y-%m-%d %H:%M", dep_tz,
                              species, metal_band, colour_band, dep_id, fill_dep_id = F,
                              site, subsite = NA, nest = NA, dep_lon, dep_lat,
                              time_released, time_recaptured = NA,
                              status_on = NA, status_off = NA, mass_on = NA, mass_off = NA, exclude = NA, fed_unfed = NA,
                              gps_id = NA, tdr_id = NA, acc_id = NA, gls_id = NA, mag_id = NA, cam_id = NA, hrl_id = NA,
                              keep = NULL)
{

  if (sum(is.na(deployments[,dep_lon])) > 0 | sum(is.na(deployments[,dep_lat])) > 0) stop("Cannot have missing values in deployment location. Check dep_lon and dep_lat.", call. = F)

  if (sum(is.na(deployments[,dep_id])) > 0 & fill_dep_id == F) stop("Cannot have missing values in dep_id. Fill in all dep_id records or use argument fill_dep_id = T.", call. = F)


  if (is.na(time_recaptured)) {
    deployments$time_recaptured <- NA
    time_recaptured <- 'time_recaptured'
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

  if (is.na(fed_unfed)) {
    deployments$fed_unfed <- NA
    fed_unfed <- 'fed_unfed'
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
                          gps_id, tdr_id, acc_id, gls_id, mag_id, cam_id, hrl_id, exclude, fed_unfed, keep)]
    names(dep) <- c('species', 'metal_band', 'colour_band', 'dep_id',
                    'site', 'subsite', 'nest','dep_lon', 'dep_lat',
                    'time_released', 'time_recaptured', 'status_on', 'status_off', 'mass_on', 'mass_off',
                    'gps_id', 'tdr_id', 'acc_id', 'gls_id', 'mag_id', 'cam_id', 'hrl_id', 'exclude', 'fed_unfed', keep)
  }

  if (is.null(keep) == T) {
    dep <- deployments[,c(species, metal_band, colour_band, dep_id,
                          site, subsite, nest, dep_lon, dep_lat,
                          time_released, time_recaptured, status_on, status_off, mass_on, mass_off,
                          gps_id, tdr_id, acc_id, gls_id, mag_id, cam_id, hrl_id, exclude, fed_unfed)]
    names(dep) <- c('species', 'metal_band', 'colour_band', 'dep_id',
                    'site', 'subsite', 'nest','dep_lon', 'dep_lat',
                    'time_released', 'time_recaptured', 'status_on', 'status_off', 'mass_on', 'mass_off',
                    'gps_id', 'tdr_id', 'acc_id', 'gls_id', 'mag_id', 'cam_id', 'hrl_id', 'exclude', 'fed_unfed')
  }

  # Convert deployment times to GPS times
  dep$time_released <- as.character(dep$time_released)
  dep$time_released[dep$time_released == ""] <- NA
  dep$time_recaptured <- as.character(dep$time_recaptured)
  dep$time_recaptured[dep$time_recaptured == ""] <- NA

  if (sum(is.na(dep$time_released)) > 0) stop("Cannot have missing values for time_released", call. = F)
  dep$time_released <- lubridate::force_tz(as.POSIXct(strptime(dep$time_released, dateFormat)), tz = dep_tz)
  dep$time_released <- lubridate::with_tz(dep$time_released, tz = 'UTC')
  if (sum(is.na(dep$time_released)) > 0) stop("Check date formats for time_released", call. = F)

  dep$time_recaptured <- as.character(dep$time_recaptured)
  valid_recap_dates <- sum(!is.na(dep$time_recaptured))
  dep$time_recaptured <- lubridate::force_tz(as.POSIXct(strptime(dep$time_recaptured, dateFormat)), tz = dep_tz)
  dep$time_recaptured <- lubridate::with_tz(dep$time_recaptured, tz = 'UTC')
  if (sum(!is.na(dep$time_recaptured)) != valid_recap_dates) stop("Check date formats for time_recaptured", call. = F)

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
  dep$acc_id[dep$acc_id == ""] <- NA
  dep$gls_id[dep$gls_id == ""] <- NA
  dep$mag_id[dep$mag_id == ""] <- NA
  dep$cam_id[dep$cam_id == ""] <- NA
  dep$hrl_id[dep$hrl_id == ""] <- NA
  dep$exclude[dep$exclude == ""] <- NA
  dep$fed_unfed[dep$fed_unfed == ""] <- NA

  # make sure variables are character class
  dep$dep_id <- as.character(dep$dep_id)
  dep$colour_band <- as.character(dep$colour_band)
  dep$gps_id <- as.character(dep$gps_id)
  dep$tdr_id <- as.character(dep$tdr_id)
  dep$acc_id <- as.character(dep$acc_id)
  dep$gls_id <- as.character(dep$gls_id)
  dep$mag_id <- as.character(dep$mag_id)
  dep$cam_id <- as.character(dep$cam_id)
  dep$hrl_id <- as.character(dep$hrl_id)
  dep$exclude <- as.character(dep$exclude)
  dep$fed_unfed <- as.character(dep$fed_unfed)

  # check metal_band values
  valid_band <- sum(!is.na(dep$metal_band))
  dep$metal_band <- as.integer(dep$metal_band)
  if (sum(!is.na(dep$metal_band)) != valid_band) stop('Values in metal_band must be numeric without a dash', call. = F)
  if (min(dep$metal_band, na.rm = T) < 10000000 | max(dep$metal_band, na.rm = T) > 999999999) stop('Values in metal_band must be numeric with 8 or 9 digits', call. = F)

  # Create a dep_id if fill_dep_id == T
  if (fill_dep_id == T) {

    if (sum(is.na(dep$metal_band[is.na(dep$dep_id)])) > 0) stop("Cannot use fill_dep_id = TRUE with missing metal_band values", call. = F)
    if (sum(is.na(dep$time_released[is.na(dep$dep_id)])) > 0) stop("Cannot use fill_dep_id = TRUE with missing time_released", call. = F)

    dep$dep_id[is.na(dep$dep_id)] <- paste0(dep$metal_band[is.na(dep$dep_id)],
                                            "_",
                                            gsub("-","",as.Date(dep$time_released[is.na(dep$dep_id)])))
  }

  # check for duplicate dep_id
  if (max(table(dep$dep_id)) > 1) stop("All dep_id values must be unique", call. = F)

  # Make sure status_on and status_off are upper case
  dep$status_on <- toupper(as.character(dep$status_on))
  dep$status_off <- toupper(as.character(dep$status_off))

  # check all entries for status_on and status_off are valid
  status_values <- c('E','C','FB','NB','PB','J',NA)
  if (sum(dep$status_on %in% status_values) != length(dep$status_on)) stop("Values in status_on can only be: E, C, FB, NB, PB, J, or NA", call. = F)
  if (sum(dep$status_off %in% status_values) != length(dep$status_off)) stop("Values in status_off can only be: E, C, FB, NB, PB, J, or NA", call. = F)

  # Make sure fed_unfed are lower case
  dep$fed_unfed <- tolower(as.character(dep$fed_unfed))
  fed_values <- c('fed','unfed','semi',NA)
  if (sum(dep$fed_unfed %in% fed_values) != length(dep$fed_unfed)) stop("Values in fed_unfed can only be: fed, unfed, semi, or NA", call. = F)

  # # check all mass values are
  # if (!is.numeric(dep$mass_on))  stop('Values in mass_on must be numeric', call. = F)
  # if (!is.numeric(dep$mass_off)) stop('Values in mass_off must be numeric', call. = F)

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
#' @param tagType Type of GPS biologger used, options are "Technosmart", "Ecotone", and "Cattrack.
#' @param dateFormat POSIXct string indicating how dates are formatted in the GPS files.
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
                        dateFormat = "%d-%m-%Y") {

  if (!(tagType %in% c("Technosmart","Ecotone", "Cattrack", "igotU"))){
    warning("Supported tagTypes are: Technosmart, Ecotone, Cattrack, igotU. If you have a different biologger please contact me.")
  }


  if (tagType == "Technosmart") {
    output <- readTechnosmartGPS(inputFolder = inputFolder,
                                 deployments = deployments,
                                 tagTZ = tagTZ,
                                 dateFormat = dateFormat)
  }

  if (tagType == "Ecotone") {
    output <- readEcotoneGPS(inputFolder = inputFolder,
                             deployments = deployments,
                             tagTZ = tagTZ,
                             dateFormat = dateFormat)
  }

  if (tagType %in% c("Cattrack", "igotU")) {
    output <- readCattrackGPS(inputFolder = inputFolder,
                              deployments = deployments,
                              tagTZ = tagTZ,
                              dateFormat = dateFormat)
  }


  output

  #' @export readGPSData
}

# ---------------------------------------------------------------------------------------------------------------

readTechnosmartGPS <- function(inputFolder,
                               deployments,
                               tagTZ = "UTC",
                               dateFormat = dateFormat) {

  dateFormat <- gsub('/', '-', dateFormat)
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
        temp <- temp[!is.na(temp$V2),]

        if (nrow(temp) > 5) {

          # set names and format date
          names(temp) <- c("time","lat","lon","altitude","gpsspeed","satellites","hdop","maxsignal")
          temp$time <- gsub('/','-',temp$time)
          df <- paste0(dateFormat, ",%T")

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

          # add ecotone fields
          temp$diving <- NA
          temp$inrange <- NA

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
                           tagTZ = "UTC",
                           dateFormat = dateFormat) {

  theFiles <- list.files(inputFolder, full.names = T, pattern = 'csv')

  output <- combineFiles(files = theFiles,
                         pattern = "csv",
                         type = "csv",
                         sep = ";",
                         stringsAsFactors = F,
                         header = T)
  output <- unique(output)

  output$time <- paste(output$Year, output$Month, output$Day, output$Hour, output$Minute, output$Second, sep = "-")
  output$time <- as.POSIXct(strptime(output$time, "%Y-%m-%d-%H-%M-%S"), tz = tagTZ)
  output <- subset(output, output$time > as.POSIXct("1900-01-01", tz = tagTZ))
  output <- unique(output)
  output <- output[order(output$Logger.ID, output$time),]

  names(output) <- gsub("[.]","",names(output))
  output$gps_id <- output$LoggerID
  output$lon <- output$Longitude
  output$lat <- output$Latitude
  output$gpsspeed <- output$Speed

  if (c("Divdown") %in% names(output)) {
    output$diving <- ifelse(output$Divdown == 1 | output$Divup == 1, 1, NA)
    output$diving[is.na(output$diving)] <- 0
  }

  output <- output[,names(output)[!(names(output) %in% c("LoggerID","Longitude","Latitude",
                                                         "Year","Month","Day","Hour","Minute","Second",
                                                         "Rawlatitude","RawLongitude","Speed"))]]
  names(output) <- tolower(names(output))
  output <- subset(output, !is.na(output$lon) | output$diving == 1 | output$inrange == 1)

  output <- merge(output, deployments[,c("gps_id","metal_band","dep_id","dep_lon","dep_lat")])
  output <- output[order(output$dep_id, output$time),]

  if ("inrange" %in% names(output)) {
    output$inrange[is.na(output$inrange)] <- 0
    output$lon[output$inrange == 1] <- output$dep_lon[output$inrange == 1]
    output$lat[output$inrange == 1] <- output$dep_lat[output$inrange == 1]
  }

  output$satellites <- NA
  output$hdop <- NA
  output$maxsignal <- NA
  output <- unique(output)
  output <- output[!duplicated(output[c('dep_id', 'time')]),]

  output <- output[,c("dep_id","time","lon","lat","altitude","gpsspeed","satellites","hdop","maxsignal","diving","inrange")]

  output
}

# ---------------------------------------------------------------------------------------------------------------

readCattrackGPS <- function(inputFolder,
                            deployments,
                            tagTZ = "UTC",
                            dateFormat = dateFormat) {

  dateFormat <- gsub("/", '-', dateFormat)
  dateFormat <- paste(dateFormat, '%T')

  dd <- list.files(inputFolder, pattern = '.csv', full.names = T)

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
                             pattern = "csv",
                             type = "csv",
                             sep = ",",
                             stringsAsFactors = F,
                             header = T)

        if (nrow(temp) > 5) {

          temp$Date <- gsub("/", '-', temp$Date)

          # check that date format is correct
          if (is.na(as.POSIXct(as.POSIXct(strptime(paste(temp$Date[1], temp$Time[1]), dateFormat)), tz = tagTZ))) stop(paste("Check date format is correct for", deployments$dep_id[i]), call. = F)

          # format dates
          temp$time <- lubridate::force_tz(as.POSIXct(strptime(paste(temp$Date, temp$Time), dateFormat)), tz = tagTZ)
          temp$time <- lubridate::with_tz(temp$time, tz = 'UTC')

          # order data and remove duplicate records
          temp <- temp[order(temp$time),]
          temp <- temp[duplicated(temp) == F,]

          # add fields for metal_band and tag and deployment
          temp$dep_id <- deployments$dep_id[i]
          if (!('Altitude' %in% names(temp))) temp$Altitude <- NA
          if (!('Speed' %in% names(temp))) temp$Speed <- NA

          temp <- temp[,c('dep_id','time','Longitude','Latitude','Altitude',"Speed")]
          names(temp) <- c('dep_id','time','lon','lat','altitude','gpsspeed')
          temp$gpsspeed <- temp$gpsspeed/1000
          temp$satellites <- NA
          temp$hdop <- NA
          temp$maxsignal <- NA
          temp$diving <- NA
          temp$inrange <- NA

          temp <- temp[duplicated(temp[,c('dep_id','time')]) == F,]

          output <- rbind(output, temp)

          output

        } else (print(paste("-- Less than 5 locations:", deployments$dep_id[i], "- not processed")))

      }

    }

  }

  output
}


# ---------------------------------------------------------------------------------------------------------------
#' Cleans up GPS data, by clipping to deployment times and filtering unrealistic locations.
#'
#' @param data Name of object with formatted GPS data.
#' @param deployments Name of object with formatted deployment data.
#' @param tagTZ Timezone of GPS data.
#' @param speedThreshold Fastest possible movement in km/hr.
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

      if (is.na(tt$dep_lon)) {
        temp$coldist <- getColDist(lon = temp$lon, lat = temp$lat, colonyLon = temp$lon[temp$time >= tt$time_released][1], colonyLat = temp$lat[temp$time >= tt$time_released][1])
        yy <- "Distance from first location (km)"
      } else {
        temp$coldist <- getColDist(lon = temp$lon, lat = temp$lat, colonyLon = tt$dep_lon, colonyLat = tt$dep_lat)
        yy <- "Distance from colony (km)"
      }

      newData <- subset(temp, temp$time >= tt$time_released & temp$time <= tt$time_recaptured)
      newData <- newData[order(newData$dep_id, newData$time),]

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

  if (max(temp$coldist, na.rm = T) < 500) {
    world <- rnaturalearth::ne_countries(scale = 50, returnclass = 'sf')
  } else {
    world <- rnaturalearth::ne_countries(scale = 110, returnclass = 'sf')
  }

  ss <- min(c(temp$time[1],tt$time_released), na.rm = T)
  ee <- max(c(temp$time[nrow(temp)],tt$time_recaptured), na.rm = T)

  suppressMessages(
    myPlot <- ggplot2::ggplot(temp[!is.na(temp$lon),], ggplot2::aes(x = time, y = coldist)) +
      ggplot2::geom_line(col = "red") +
      ggplot2::geom_point(col = "red") +
      ggplot2::geom_point(data = newData[!is.na(newData$lon),], ggplot2::aes(x = time, y = coldist)) +
      ggplot2::geom_line(data = newData[!is.na(newData$lon),], ggplot2::aes(x = time, y = coldist)) +
      ggplot2::geom_vline(xintercept = c(tt$time_released, tt$time_recaptured), linetype = 2, col = "red") +
      ggplot2::xlim(ss,ee) +
      ggplot2::theme_light() +
      ggplot2::labs(title = paste(temp$dep_id[1]), y = yy, x = "Time")
  )
  xran <- range(temp$lon, na.rm = T)
  yran <- range(temp$lat, na.rm = T)

  coord_range <- max(c(xran[2] - xran[1], yran[2] - yran[1]))
  coord_breaks <- 0.1
  if (coord_range > 0.5) coord_breaks <- 0.2
  if (coord_range >= 1) coord_breaks <- 0.5
  if (coord_range >= 2) coord_breaks <- 1
  if (coord_range >= 5) coord_breaks <- 2
  if (coord_range >= 10) coord_breaks <- 5

  suppressMessages(
    myMap<- ggplot2::ggplot(data = newData[!is.na(newData$lon),]) +
      ggplot2::geom_sf(data = world) +
      ggplot2::geom_point(data = temp[!is.na(temp$lon),], ggplot2::aes(x = lon, y = lat), col = 'red') +
      ggplot2::geom_path(data = temp[!is.na(temp$lon),], ggplot2::aes(x = lon, y = lat), col = 'red') +
      ggplot2::geom_point(data = newData[!is.na(newData$lon),], ggplot2::aes(x = lon, y = lat)) +
      ggplot2::geom_path(data = newData[!is.na(newData$lon),], ggplot2::aes(x = lon, y = lat)) +
      ggplot2::scale_x_continuous(breaks = seq(-180, 180, coord_breaks)) +
      ggplot2::scale_y_continuous(breaks = seq(-90, 90, coord_breaks)) +
      #ggplot2::geom_point(data = tt, ggplot2::aes(x = tt$dep_lon, y = tt$dep_lon), fill = 'green', shape = 24, size = 3) +
      ggplot2::coord_sf(xlim = xran, ylim = yran) +
      ggplot2::theme_light() +
      #ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90)) +
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

  unmatchedDeployments <- c()
  for (dd in deployment_ids) {
    idx <- grep(dd, files)
    if (length(idx) <1)  unmatchedDeployments <- c(unmatchedDeployments, dd)
  }

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
#' @param tagType Type of TDR biologger used, options are "Technosmart", "LAT150.
#' @param dateFormat POSIXct string indicating how dates are formatted in the TDR files.
#'
#' @details
#'
#' Technosmart can export TDR data with the following date formats: %d-%m-%Y, %m-%d-%Y, "%Y-%m-%d, %Y-%d-%m. All the files in the inputFolder need to have the same formatting.
#' The lowest sampling frequency for TDR data on a Technosmart unit is 1 Hz, however these data are exported along with the acceleration data, which is usually at a higher frequency.
#' This function will resample the TDR data to 1Hz if they appear at a higher frequency in the data (i.e. exported using the 'fill missing fields setting')
#'
#' The function will print lists of: TDR files that contain no data, TDR files that have no matching deployments in the deployment data, and deployments that have no matching TDR files. This is intended to help you identify errors in data entry or file naming. For technosmart units, I recommend you re-export data any time you change the file name because X-Manager includes the file name as a column in the data.
#'
#' @return A new dataframe containing all TDR data.

readTDRData <- function(inputFolder,
                        deployments,
                        tagTZ = "UTC",
                        tagType,
                        dateFormat = "%d-%m-%Y") {

  if (!(tagType %in% c("Technosmart","LAT150"))){
    warning("Supported tagTypes are: Technosmart. If you have a different biologger please contact me.")
  }


  if (tagType == "Technosmart") {
    output <- readTechnosmartTDR(inputFolder = inputFolder,
                                 deployments = deployments,
                                 tagTZ = tagTZ,
                                 dateFormat = dateFormat)
  }

  if (tagType == "LAT150") {
    output = readLAT150(inputFolder = inputFolder,
                        deployments = deployments,
                        tagTZ = tagTZ)
  }

  output

  #' @export readTDRData
}

# ---------------------------------------------------------------------------------------------------------------

readTechnosmartTDR <- function(inputFolder,
                               deployments = depData,
                               tagTZ = "UTC",
                               tagType = "Technosmart",
                               dateFormat = "%Y-%m-%d")
  {

  dateFormat <- gsub('/', '-', dateFormat)
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
          if (("Depth" %in% names(temp)) | "Pressure" %in% names(temp)){

          tempNames <- names(temp)

          # combine date and time into one column if they are separate
          if ('Date' %in% tempNames & 'Time' %in% tempNames)  temp$Timestamp <- paste(temp$Date, temp$Time)

          # create empty pressure or depth field if missing
          if (('Pressure' %in% tempNames) == F)  temp$Pressure <- NA
          if (('Depth' %in% tempNames) == F)  temp$Depth <- NA

          # add fields for dep_id
          temp$dep_id <- deployments$dep_id[i]
          temp$wetdry <- NA

          tdrCols <- c("dep_id","Timestamp","Depth","Pressure","Temp....C.","wetdry")
          temp <- temp[,tdrCols]

          # set names and format date
          names(temp) <- c("dep_id","time","depth","pressure","temperature","wetdry")

          temp$time <- gsub('/', '-', temp$time)

          df <- paste0(dateFormat, " %H:%M:%OS")

          # check that date format is correct
          if (is.na(as.POSIXct(strptime(temp$time[1], df), tz = tagTZ))) stop(paste("Check date format is correct for", deployments$dep_id[i]), call. = F)

          # format dates
          temp$time <- as.POSIXct(strptime(temp$time, df), tz = tagTZ)

          # remove missing data
          temp <- subset(temp, !is.na(temp$depth) | !is.na(temp$pressure))
          # get sampling frequency of data
          tt <- c(NA, as.numeric(difftime(temp$time[2:nrow(temp)], temp$time[1:(nrow(temp) - 1)], units = 'sec')))
          freq <- round(1/getMode(tt))

          # subsample to 1 Hz if the sampling frequency was higher than 1 Hz
          if (freq > 1) {temp <- temp[seq(1, nrow(temp), freq),]}

          # convert milibars to dbars - need to check this
          temp$pressure <- temp$pressure/100

          # order data and remove duplicate records
          temp <- temp[order(temp$time),]
          temp <- temp[duplicated(temp) == F,]

          output <- rbind(output, temp)

          (print(paste("Finished:", deployments$dep_id[i])))


          } else (print(paste("-- Missing depth or pressure:", deployments$dep_id[i], "- not processed")))
        } else (print(paste("-- Less than 5 records:", deployments$dep_id[i], "- not processed")))

      }

    }

  }

  output
}

# ---------------------------------------------------------------------------------------------------------------

readLAT150 <- function(inputFolder,
                       deployments,
                       tagTZ = "UTC") {

  dd <- list.files(inputFolder, pattern = '.csv', full.names = T, recursive = T)

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

        # temp <- combineFiles(files = theFiles,
        #                      pattern = "csv",
        #                      type = "csv",
        #                      sep = ",",
        #                      stringsAsFactors = F,
        #                      header = T,
        #                      skip = 2,
        #                      combineColumns = T)
        temp <- utils::read.csv(theFiles[1], sep = ",", stringsAsFactors = F, header = T, skip = 2)
        if (length(theFiles) > 1) {
          temp <- temp[,2:ncol(temp)]
          for (k in 2:length(theFiles)) {
            tt <- utils::read.csv(theFiles[k], sep = ",", stringsAsFactors = F, header = T, skip = 2)
            tt <- tt[,2:ncol(tt)]
            temp <- merge(temp, tt, all = T)
          }
        }

        if (nrow(temp) > 5) {

          # Combine date and time into one variable
          temp$time <- paste(temp$Date, temp$Time)

          # check that date format is correct
          if (is.na(as.POSIXct(temp$time[1], tz = tagTZ))) stop(paste("Check date format is correct for", deployments$dep_id[i]), call. = F)

          # format dates
          temp$time <- as.POSIXct(temp$time, tz = tagTZ)

          # order data and remove duplicate records
          temp <- temp[order(temp$time),]
          temp <- temp[duplicated(temp) == F,]

          # add fields for metal_band and tag and deployment
          temp$dep_id <- deployments$dep_id[i]
          names(temp)[grep("Temp", names(temp))] <- "temperature"
          names(temp)[grep("Pressure", names(temp))] <- "pressure"
          if (length(grep("Wet", names(temp))) >0) {
            names(temp)[grep("Wet", names(temp))] <- "wetdry"
          } else temp$wetdry <- NA
          temp$depth <- NA

          temp <- temp[,c("dep_id","time","depth",'pressure','temperature','wetdry')]
          output <- rbind(output, temp)

        } else (print(paste("-- Less than 5 locations:", deployments$dep_id[i], "- not processed")))

      }

    }

  }

  output
}


# ---------------------------------------------------------------------------------------------------------------
#' Cleans up TDR data, by clipping to deployment times
#'
#' @param data Name of object with formatted TDR data.
#' @param deployments Name of object with formatted deployment data.
#' @param tagTZ Timezone of TDR data.
#' @param plot Should data be plotted (TRUE or FALSE).
#'
#' @details This function clips the TDR data to the start and end times of each TDR deployment. The link between deployment
#' times and the TDR data is made using the 'dep_id' field. Because
#'
#' @return A new dataframe containing cleaned TDR data.

cleanTDRData <- function(data,
                         deployments,
                         plot = T)
{

  output <- data.frame()
  theDeps <- unique(data$dep_id)

  for (dd in 1:length(theDeps)) {

    temp <- subset(data, data$dep_id == theDeps[dd])

    tt <- subset(deployments, deployments$dep_id == theDeps[dd])

    if (nrow(tt) > 0) {

      if (is.na(tt$time_recaptured)) tt$time_recaptured <- max(temp$time, na.rm = T)

      newData <- subset(temp, temp$time >= tt$time_released & temp$time <= tt$time_recaptured)

      if (nrow(newData) > 5) {

        if (plot) {

          if (nrow(temp) > 100000) idx <- seq(1,nrow(temp), 5) else idx <- 1:nrow(temp)

          plotPressure <- ifelse(sum(is.na(temp$depth) == F) == 0, T, F)

          ss <- min(c(temp$time[1],tt$time_released))
          ee <- max(c(temp$time[nrow(temp)],tt$time_recaptured))

          if (plotPressure == F) {

            dd <- subset(temp, temp$time %in% temp$time[idx] & !is.na(temp$depth))
            nn <- subset(newData, newData$time %in% temp$time[idx] & !is.na(newData$depth))

            suppressMessages(
              myPlot <- ggplot2::ggplot(dd, ggplot2::aes(x = time, y = depth * -1)) +
                ggplot2::geom_line(col = "red") +
                ggplot2::geom_line(data = nn, ggplot2::aes(x = time, y = depth * -1)) +
                ggplot2::geom_vline(xintercept = c(tt$time_released, tt$time_recaptured), linetype = 2, col = "red") +
                ggplot2::xlim(ss,ee) +
                ggplot2::theme_light() +
                ggplot2::labs(title = paste(temp$dep_id[1]), y = 'Depth (m)', x = "Time")
            )
            print(myPlot)
          }

          if (plotPressure == T) {

            pp <- subset(temp, temp$time %in% temp$time[idx] & !is.na(temp$pressure))
            nn <- subset(newData, newData$time %in% temp$time[idx] & !is.na(newData$pressure))
            suppressMessages(
              myPlot <- ggplot2::ggplot(pp, ggplot2::aes(x = time, y = pressure)) +
                ggplot2::geom_line(col = "red") +
                ggplot2::geom_line(data = nn, ggplot2::aes(x = time, y = pressure)) +
                ggplot2::geom_vline(xintercept = c(tt$time_released, tt$time_recaptured), linetype = 2, col = "red") +
                ggplot2::xlim(ss,ee) +
                ggplot2::theme_light() +
                ggplot2::labs(title = paste(temp$dep_id[1]), y = 'Pressure (dBar)', x = "Time")
            )

            print(myPlot)
          }

          Sys.sleep(1)

          readline("Press [enter] for next plot")


        }

        output <- rbind(output, newData)
      } else (print(paste("Less than 5 deployed locations for", tt$dep_id, "- removed")))
    }
  }

  output

  #' @export cleanTDRData
}

# -----

