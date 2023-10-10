# -----

#' @title Convert Ecotone TDR data to Arrow dataset

#' @description Reads Ecotone TDR data and adds it to an arrow dataset
#'
#' @param data Data passed from seabiRds::ecotone_to_dataset()
#' @param deployments Deployment data formatted using seabiRds::formatDeployments()
#' @param output_dataset Path where arrow dataset should be saved
#' @param plot Should plot of data be generated, TRUE or FALSE?
#'
#' @return Code checks that data meet formatting requirements. Produces a plot
#' showing a time series of depth data from the TDR Deployed data are shown in black,
#' undeployed data are shown in red, and deployment start/end times are shown with
#' dashed vertical red lines.
#'
#' Output is saved to an arrow dataset at the location indicated by output_dataset.
#'
ecotone_tdr_to_dataset <- function(data,
                                   deployments,
                                   output_dataset,
                                   plot = T) {

  #if (!('Depth') %in% names(data)) {data$Depth <- NA}

  idx <- which(data$Div.down == 1)
  data$time[idx] <- data$time[idx] - 1

  data <- data %>%
    rename(temperature_c = Temp_sens, depth_m = Depth) %>%
    mutate(
      Div.down = ifelse(is.na(Div.down), 0, Div.down),
      Div.up = ifelse(is.na(Div.up), 0, Div.up),
      depth_m = ifelse(Div.up == 1 | Div.down == 1, 0, depth_m),
      depth_m = ifelse(is.na(depth_m), 0, depth_m),
      depth_m = depth_m/100,
      wet = NA,
    ) %>% dplyr::filter(!is.na(temperature_c) | !is.na(depth_m)) %>%
    dplyr::inner_join(deployments[, c('tdr_id','metal_band', 'species', 'site', 'subsite')], by = 'tdr_id') %>%
    dplyr::select(site, subsite, species, year,
                  metal_band, dep_id, time, temperature_c, depth_m, wet, deployed) %>%
    dplyr::group_by(site, subsite, species, year, metal_band, dep_id, deployed) %>%
    dplyr::arrange(time) %>%
    dplyr::filter(duplicated(time) == F)

  seabiRds::cleanTDRData(data = data %>% dplyr::rename(depth = depth_m) %>% dplyr::filter(!is.na(depth)),
                         deployments = deployments, plot = plot)

  data %>%
    arrow::write_dataset(paste0(output_dataset,'/tdr'), format = "parquet",
                         existing_data_behavior = 'delete_matching')

}

# -----

#' @title Convert Ecotone dive summary data to Arrow dataset

#' @description Reads Ecotone dive summary data and adds it to an arrow dataset
#'
#' @param data Data passed from seabiRds::ecotone_to_dataset()
#' @param deployments Deployment data formatted using seabiRds::formatDeployments()
#' @param output_dataset Path where arrow dataset should be saved

#'
#' @return Code checks that data meet formatting requirements. Dive summary data
#' (start, end, duration) is extracted from tracking data.
#'
#' Output is saved to an arrow dataset at the location indicated by output_dataset.
#'
ecotone_dive_to_dataset <- function(data,
                                   deployments,
                                   output_dataset) {

  #if (!('Depth') %in% names(data)) {data$Depth <- NA}

  data <- data |>
    dplyr::filter(Div.up == 1) |>
    dplyr::select(dep_id, time, Diving.duration, deployed) |>
    dplyr::mutate(dive_start = time - Diving.duration,
           dive_end = time,
           dive_duration_s = Diving.duration,
           max_depth_m = NA) |>
    dplyr::select(dep_id, dive_start, dive_end, dive_duration_s, max_depth_m, deployed) |>
    dplyr::filter(!is.na(dive_duration_s)) |>
    dplyr::inner_join(deployments[, c('dep_id','metal_band', 'species', 'site', 'subsite')], by = c('dep_id')) %>%
    dplyr::mutate(year = strftime(dive_start, '%Y')) |>
    dplyr::select(site, subsite, species, year,
                  metal_band, dep_id, dive_start, dive_end, dive_duration_s, max_depth_m, deployed) %>%
    dplyr::group_by(site, subsite, species, year, metal_band, dep_id, deployed) %>%
    dplyr::arrange(dive_start) %>%
    dplyr::filter(duplicated(dive_start) == F)

  if (nrow(data) > 0) {
    data %>%
      arrow::write_dataset(paste0(output_dataset,'/dive'), format = "parquet",
                           existing_data_behavior = 'delete_matching')
  }

}

# -----
#' @title Convert Ecotone GPS data to Arrow dataset

#' @description Reads Ecotone GPS data and adds it to an arrow dataset
#'
#' @param data Data passed from seabiRds::ecotone_to_dataset()
#' @param deployments Deployment data formatted using seabiRds::formatDeployments()
#' @param output_dataset Path where arrow dataset should be saved
#' @param plot Should plot of data be generated, TRUE or FALSE?
#'
#' @return Code checks that data meet formatting requirements. Produces a plot
#' showing a time series of distance from the colony and a map of locations from the GPS.
#' Deployed data are shown in black, undeployed data are shown in red, and
#' deployment start/end times are shown with dashed vertical red lines.
#'
#' Output is saved to an arrow dataset at the location indicated by output_dataset.
#'
#'
ecotone_gps_to_dataset <- function(data,
                                   deployments,
                                   output_dataset,
                                   plot = T) {

  data <- data %>%
    dplyr::mutate(
      inrange = ifelse(is.na(In.range), 0, 1),
      Longitude = ifelse(inrange == 1, deployments$dep_lon, Longitude),
      Latitude = ifelse(inrange == 1, deployments$dep_lat, Latitude),
    ) %>%
    dplyr::rename(lat = Latitude , lon = Longitude , altitude_m = Altitude, hdop = HDOP, satellites = Sat..Count) %>%
    dplyr::filter(!is.na(lon)) %>%
    dplyr::inner_join(deployments[, c('gps_id','metal_band', 'species', 'site', 'subsite')], by = 'gps_id') %>%
    dplyr::select(site, subsite, species, year,
                  metal_band, dep_id, time, lon, lat, altitude_m, satellites, hdop, inrange, deployed) %>%
    dplyr::group_by(site, subsite, species, year, metal_band, dep_id, deployed) %>%
    dplyr::arrange(time) %>%
    dplyr::filter(duplicated(time) == F)

  cleanGPSData(data = data,
               deployments = deployments,
               speedThreshold = NA,
               plot = plot)

  data %>%
    arrow::write_dataset(paste0(output_dataset,'/gps'), format = "parquet",
                         existing_data_behavior = 'delete_matching')

}


# -----
#' @title Convert Ecotone biologger data to Arrow dataset
#' @description Reads ecotone biologger data and parses it into two arrow datasets for GPS and TDR
#' @param files List of ecotone .csv files, data must include logger IDs that match the gps_id in deployment data
#' @param deployments Deployment data formatted using seabiRds::formatDeployments()
#' @param output_dataset Path where arrow dataset should be saved
#' @param timezone Timezone of raw data, default is UTC
#' @param plot Should plot of data be generated, TRUE or FALSE?
#'
#' @export

ecotone_to_dataset <- function(files,
                               deployments,
                               output_dataset,
                               timezone = 'UTC',
                               plot = T) {

  check_filetype <- grep('.csv', files)
  if (length(check_filetype) != length(files)) stop('All files must be .csv format')

  output <- combineFiles(files = files,
                         pattern = "csv",
                         type = "csv",
                         sep = ";",
                         stringsAsFactors = F,
                         header = T)

  output <- output %>%
    dplyr::mutate(
      time = paste(Year, Month, Day, Hour, Minute, Second, sep = '-'),
      time = as.POSIXct(time, format = "%Y-%m-%d-%H-%M-%S", tz = timezone),
      tdr_id = Logger.ID,
    ) %>%
    dplyr::filter(time > as.POSIXct("1900-01-01", tz = timezone)) %>%
    dplyr::rename(gps_id = Logger.ID)

  if (!('HDOP' %in% names(output))) output$HDOP <- NA
  if (!('Sat..Count' %in% names(output))) output$`Sat..Count` <- NA

  for (i in 1:nrow(deployments)) {

    dat <- output %>%
      filter(gps_id == deployments$gps_id[i])

    if (nrow(dat) > 0 & is.na(deployments$time_recaptured[i])) {
      deployments$time_recaptured[i] <- max(dat$time)
      warning(
        paste(deployments$dep_id[i], 'is missing time_recaptured'), call. = FALSE, immediate. = TRUE)
    }

    if (nrow(dat) > 0) {

      dd <- deployments %>%
        filter(gps_id == deployments$gps_id[i]) %>%
        arrange(time_released)
      idx <- which(dd$dep_id == deployments$dep_id[i])
      if (idx > 1) start_time <- mean(c(dd$time_recaptured[idx - 1], deployments$time_released[i])) else start_time <- min(dat$time)
      if (idx < nrow(dd)) end_time <- mean(c(dd$time_released[idx + 1], deployments$time_recaptured[i])) else end_time <- max(dat$time)

      dat <- dat %>%
        dplyr::filter(time >= start_time, time <= end_time)

      dat <- dat %>%
        dplyr::mutate(
          dep_id = deployments$dep_id[i],
          year = as.integer(strftime(time, '%Y')),
          deployed = ifelse(time >= deployments$time_released[i] &
                              time <= deployments$time_recaptured[i], 1, 0),
        )

      ecotone_gps_to_dataset(data = dat,
                             deployments = deployments[i,],
                             output_dataset = output_dataset,
                             plot = plot)

      if ('Depth' %in% names(dat)) {
        ecotone_tdr_to_dataset(data = dat,
                               deployments = deployments[i,],
                               output_dataset = output_dataset,
                               plot = plot)
      }

      if ('Div.down' %in% names(dat) & !'Depth' %in% names(dat)) {
        ecotone_dive_to_dataset(data = dat,
                               deployments = deployments[i,],
                               output_dataset = output_dataset)
      }

      print(paste0('Finished [',i,']: ', deployments$dep_id[i]))

    } else {print(paste0('No matching data [',i,']: ', deployments$dep_id[i]))}
  }
}
