
axytrek_acc_to_dataset <- function(data,
                                   deployments,
                                   output_dataset,
                                   date_format = '%Y-%m-%d %H:%M:%OS',
                                   timezone = 'UTC',
                                   plot = T) {

  data <- data %>%
    dplyr::rename(time = Timestamp, x = X, y = Y, z = Z) %>%
    dplyr::inner_join(deployments[, c('dep_id', 'metal_band', 'species', 'site', 'subsite')], by = 'dep_id') %>%
    dplyr::select(site, subsite, species, year,
                  metal_band, dep_id, time, x, y, z, deployed) %>%
    dplyr::group_by(site, subsite, species, year, metal_band, dep_id, deployed)%>%
    dplyr::arrange(time) %>%
    dplyr::filter(duplicated(time) == F)

  dd <- na.omit(c(deployments$time_released, deployments$time_recaptured))
  temp <- data[seq(1, nrow(data), 30 * getFrequency(data$time)),]

  if (plot == T) {
    suppressMessages(p <- ggplot2::ggplot(temp, ggplot2::aes(x = time, y = x)) +
                       ggplot2::geom_line(linewidth = 0.1, col = 'red') +
                       ggplot2::geom_line(data = temp[temp$deployed == 1,], ggplot2::aes(x = time, y = x), size = 0.1, col = 'black') +
                       ggplot2::geom_vline(xintercept = dd, linetype = 2, col = 'blue', size = 0.5) +
                       ggplot2::labs(title = data$dep_id[1], x = 'Time', y = 'x-axis (g)') +
                       ggplot2::theme_light()
    )
    print(p)
  }

  data %>%
    arrow::write_dataset(paste0(output_dataset,'/acc'), format = "parquet",
                         existing_data_behavior = 'delete_matching')

}

# -----

axytrek_tdr_to_dataset <- function(data,
                                   deployments,
                                   output_dataset,
                                   date_format = '%Y-%m-%d %H:%M:%OS',
                                   timezone = 'UTC',
                                   plot = T) {

  if (!('Depth') %in% names(data)) {data$Depth <- NA}

  data <- data %>%
    rename(temperature_c = dplyr::starts_with('Temp')) %>%
    dplyr::filter(!is.na(temperature_c)) %>%
    tidyr::separate(Activity, into = c('active', 'wet'), sep = '/') %>%
    mutate(
      wet = ifelse(wet == 'Wet', 1, 0)
      ) %>%
    dplyr::rename(time = Timestamp, depth_m = Depth) %>%
    dplyr::inner_join(deployments[, c('dep_id', 'metal_band', 'species', 'site', 'subsite')], by = 'dep_id') %>%
    dplyr::select(site, subsite, species, year,
                  metal_band, dep_id, time, temperature_c, depth_m, wet, deployed) %>%
    dplyr::group_by(site, subsite, species, year, metal_band, dep_id, deployed) %>%
    dplyr::arrange(time) %>%
    dplyr::filter(duplicated(time) == F)

  dd <- na.omit(c(deployments$time_released, deployments$time_recaptured))
  temp <- data[seq(1, nrow(data), 30 * getFrequency(data$time)),]

  if (plot == T) {
    p <- ggplot2::ggplot(temp, ggplot2::aes(x = time, y = depth_m)) +
      ggplot2::geom_line(col = 'red', linewidth = 0.1) +
      ggplot2::geom_line(data = temp[temp$deployed == 1,], ggplot2::aes(x = time, y = depth_m), size = 0.1, col = 'black') +
      ggplot2::geom_vline(xintercept = dd, linetype = 2, col = 'blue', size = 0.5) +
      ggplot2::labs(title = data$dep_id[1], x = 'Time', y = 'Depth (m)') +
      ggplot2::scale_y_reverse() +
      ggplot2::theme_light()

    suppressWarnings(print(p))
  }

  data %>%
    arrow::write_dataset(paste0(output_dataset,'/tdr'), format = "parquet",
                         existing_data_behavior = 'delete_matching')

}

# -----

axytrek_gps_to_dataset <- function(data,
                                   deployments,
                                   output_dataset,
                                   date_format = '%Y-%m-%d %H:%M:%OS',
                                   timezone = 'UTC',
                                   plot = T) {

  data <- data %>%
    dplyr::mutate(
      inrange = NA
    ) %>%
    dplyr::rename(time = Timestamp, lat = location.lat, lon = location.lon, altitude_m = height.msl) %>%
    dplyr::filter(!is.na(lon)) %>%
    dplyr::inner_join(deployments[, c('dep_id', 'metal_band', 'species', 'site', 'subsite')], by = 'dep_id') %>%
    dplyr::select(site, subsite, species, year,
                  metal_band, dep_id, time, lon, lat, altitude_m, satellites, hdop, inrange, deployed) %>%
    dplyr::group_by(site, subsite, species, year, metal_band, dep_id, deployed) %>%
    dplyr::arrange(time) %>%
    dplyr::filter(duplicated(time) == F)

  cleanGPSData(data = data,
               deployments = deployments,
               speedThreshold = NA,
               plot = F)

  ss <- min(c(data$time[1],deployments$time_released), na.rm = T)
  ee <- max(c(data$time[nrow(data)],deployments$time_recaptured), na.rm = T)
  temp <- data %>%
    mutate(coldist = getColDist(lon, lat, colonyLon = deployments$dep_lon, colonyLat = deployments$dep_lat))

  if (plot == T) {
    suppressMessages(
      myPlot <- ggplot2::ggplot(temp, ggplot2::aes(x = time, y = coldist)) +
        ggplot2::geom_line(col = "red") +
        ggplot2::geom_point(col = "red") +
        ggplot2::geom_point(data = temp[temp$deployed == 1,], ggplot2::aes(x = time, y = coldist)) +
        ggplot2::geom_line(data = temp[temp$deployed == 1,], ggplot2::aes(x = time, y = coldist)) +
        ggplot2::geom_vline(xintercept = c(deployments$time_released, deployments$time_recaptured), linetype = 2, col = "red") +
        ggplot2::xlim(ss,ee) +
        ggplot2::theme_light() +
        ggplot2::labs(title = paste(deployments$dep_id),
                      y = 'Distance from deployment location (km)',
                      x = "Time")
    )
    print(myPlot)
  }

  data %>%
    arrow::write_dataset(paste0(output_dataset,'/gps'), format = "parquet",
                         existing_data_behavior = 'delete_matching')

}

# -----

axytrek_acc_check <- function(file,
                              date_format = '%Y-%m-%d %H:%M:%OS',
                              timezone = 'UTC'){

  tt <- read.csv(file = file, stringsAsFactors = F, nrows = 1)

  check_names <- c('Timestamp', 'X', 'Y', 'Z') %in% names(tt)

  if (sum(check_names) != 4) stop(paste0(file, 'does not contain columns named Timestamp, X, Y, and Z'))

  date_error <- paste0(' --- Date format test failed for: ', file, ' \nCheck that the date format',
                       tt$Timestamp, ' matches the date_format string: ', date_format, '. \n\n See ?strptime for help with POSIX string formats')
  check_dateformat <- as.POSIXct(tt$Timestamp, format = date_format, tz = 'UTC')
  if (is.na(check_dateformat)) stop(date_error)
  if (check_dateformat < as.POSIXct('2000-01-01', tz = timezone)) stop(date_error)
  if (check_dateformat > Sys.time()) stop(date_error)
  if (length(grep('%OS', date_format)) != 1) stop('date_format must include miliseconds for accelerometer data. Replace %S with %OS')

}

# -----

axytrek_tdr_check <- function(file,
                              date_format = '%Y-%m-%d %H:%M:%OS',
                              timezone = 'UTC'){

  tt <- read.csv(file = file, stringsAsFactors = F, nrows = 1)
  names(tt)[grep('Temp', names(tt))] <- 'Temperature'

  check_depth <- c('Depth') %in% names(tt)

  if (sum(check_depth) != 1) warning(paste0(file, ' is missing Depth data'), call. = F)

  check_names <- c('Timestamp', 'Temperature', 'Activity') %in% names(tt)

  if (sum(check_names) != 3) stop(paste0(file, ' does not contain columns named Timestamp, Activity, and Temp...C'))

  date_error <- paste0(' --- Date format test failed for: ', file, ' \nCheck that the date format',
                       tt$Timestamp, ' matches the date_format string: ', date_format, '. \n\n See ?strptime for help with POSIX string formats')
  check_dateformat <- as.POSIXct(tt$Timestamp, format = date_format, tz = 'UTC')
  if (is.na(check_dateformat)) stop(date_error)
  if (check_dateformat < as.POSIXct('2000-01-01', tz = timezone)) stop(date_error)
  if (check_dateformat > Sys.time()) stop(date_error)
  if (length(grep('%OS', date_format)) != 1) stop('date_format must include miliseconds for accelerometer data. Replace %S with %OS')

}

# -----

axytrek_gps_check <- function(file,
                              date_format = '%Y-%m-%d %H:%M:%OS',
                              timezone = 'UTC'){

  tt <- read.csv(file = file, stringsAsFactors = F, nrows = 1)

  check_names <- c('Timestamp', 'location.lat', 'location.lon') %in% names(tt)

  if (sum(check_names) != 3) stop(paste0(file, 'does not contain columns named Timestamp, location.lat, and/or location.lon'))

  check_names <- c('height.msl','ground.speed', 'satellites','hdop') %in% names(tt)

  if (sum(check_names) != 4) stop(paste0(file, 'does not contain columns named height.msl, ground.speed, satellites, and/or hdop'))

  date_error <- paste0(' --- Date format test failed for: ', file, ' \nCheck that the date format',
                       tt$Timestamp, ' matches the date_format string: ', date_format, '. \n\n See ?strptime for help with POSIX string formats')
  check_dateformat <- as.POSIXct(tt$Timestamp, format = date_format, tz = 'UTC')
  if (is.na(check_dateformat)) stop(date_error)
  if (check_dateformat < as.POSIXct('2000-01-01', tz = timezone)) stop(date_error)
  if (check_dateformat > Sys.time()) stop(date_error)

}

# -----
#' @title Convert Axytrek biologger data to Arrow dataset
#' @description Reads Axytrek biologger data and parses it into three arrow datasets for ACC, GPS, and TDR data
#' @param files List of Axytrek .csv files, data must have file names taht are a partial match to the dep_id in deployment data
#' @param deployments Deployment data formatted using seabiRds::formatDeployments()
#' @param output_dataset Path where arrow dataset should be saved
#' @param date_format The POSIXct date format used in .csv files
#' @param timezone Timezone of raw data, default is UTC
#' @param plot Should plot of data be generated, TRUE or FALSE?
#' @export

axytrek_to_dataset <- function(files,
                               deployments,
                               output_dataset,
                               date_format = '%Y-%m-%d %H:%M:%OS',
                               timezone = 'UTC',
                               acc = T,
                               tdr = T,
                               gps = T,
                               plot = T) {

  check_filetype <- grep('.csv', files)
  if (length(check_filetype) != length(files)) stop('All files must be .csv format')

  for (i in 1:nrow(deployments)) {

    idx <- grep(deployments$dep_id[i], files)

    if (length(idx) > 0) {

      if (is.na(deployments$time_recaptured[i])) warning(
        paste(deployments$dep_id[i], 'is missing time_recaptured'), call. = FALSE, immediate. = TRUE)

      if (acc == TRUE) axytrek_acc_check(file = files[idx[1]],
                        date_format = date_format,
                        timezone = timezone)

      if (tdr == TRUE) axytrek_tdr_check(file = files[idx[1]],
                        date_format = date_format,
                        timezone = timezone)

      if (gps == TRUE) axytrek_gps_check(file = files[idx[1]],
                        date_format = date_format,
                        timezone = timezone)

      dat <- files[idx] %>%
        purrr::map_df(~read.csv(file = ., stringsAsFactors = F))%>%
        dplyr::mutate(
          Timestamp = as.POSIXct(Timestamp, format = date_format, tz = timezone),
          Timestamp = lubridate::with_tz(Timestamp, tzone = 'UTC'),
          dep_id = deployments$dep_id[i],
          year = as.integer(strftime(Timestamp, '%Y')),
          deployed = ifelse(Timestamp >= deployments$time_released[i] &
                              Timestamp <= min(c(deployments$time_recaptured[i],max(Timestamp)), na.rm = T), 1, 0),
        )

      if (acc == TRUE) axytrek_acc_to_dataset(data = dat,
                             deployments = deployments[i,],
                             output_dataset = output_dataset,
                             date_format = date_format,
                             timezone = timezone,
                             plot = plot)

      if (tdr == TRUE) axytrek_tdr_to_dataset(data = dat,
                             deployments = deployments[i,],
                             output_dataset = output_dataset,
                             date_format = date_format,
                             timezone = timezone,
                             plot = plot)

      if (gps == TRUE & length(!is.na(dat$location.lon)) > 0) axytrek_gps_to_dataset(data = dat,
                             deployments = deployments[i,],
                             output_dataset = output_dataset,
                             date_format = date_format,
                             timezone = timezone,
                             plot = plot)

      print(paste0('Finished [',i,']: ', deployments$dep_id[i]))

    } else {print(paste0('No matching data [',i,']: ', deployments$dep_id[i]))}
  }
}
