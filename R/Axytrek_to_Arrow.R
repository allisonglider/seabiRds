
axytrek_acc_to_dataset <- function(data,
                                   deployments,
                                   output_dataset,
                                   date_format = '%Y-%m-%d %H:%M:%OS',
                                   timezone = 'UTC',
                                   plot = T,
                                   ask = F,
                                   force = F) {

  data <- data |>
    dplyr::rename(time = Timestamp, x = X, y = Y, z = Z) |>
    dplyr::inner_join(deployments[, c('dep_id', 'metal_band', 'species', 'site', 'subsite')], by = 'dep_id') |>
    dplyr::select(site, subsite, species, year,
                  metal_band, dep_id, time, x, y, z, deployed) |>
    dplyr::group_by(site, subsite, species, year, metal_band, dep_id, deployed)|>
    dplyr::arrange(time) |>
    dplyr::filter(duplicated(time) == F)

  data <- checkAxes(data, ask = ask, force = force)

  dd <- stats::na.omit(c(deployments$time_released, deployments$time_recaptured))
  temp <- data[seq(1, nrow(data), 30 * getFrequency(data$time)),]

  if (plot == T) {
    suppressMessages(p <- ggplot2::ggplot(temp, ggplot2::aes(x = time, y = x)) +
                       ggplot2::geom_line(linewidth = 0.1, col = 'red') +
                       ggplot2::geom_line(data = temp[temp$deployed == 1,], ggplot2::aes(x = time, y = x), linewidth = 0.1, col = 'black') +
                       ggplot2::geom_vline(xintercept = dd, linetype = 2, col = 'blue', linewidth = 0.5) +
                       ggplot2::labs(title = data$dep_id[1], x = 'Time', y = 'x-axis (g)') +
                       ggplot2::theme_light()
    )
    print(p)
  }

  data |>
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

  data <- data |>
    dplyr::rename(temperature_c = dplyr::starts_with('Temp')) |>
    dplyr::filter(!is.na(temperature_c)) |>
    dplyr::mutate(
      # Activity is stored as 'active/wet' text, keep the part after the '/'
      wet = vapply(strsplit(as.character(Activity), '/', fixed = TRUE),
                   function(x) x[2], character(1)),
      wet = ifelse(wet == 'Wet', 1, 0)
      ) |>
    dplyr::select(-Activity) |>
    dplyr::rename(time = Timestamp, depth_m = Depth) |>
    dplyr::inner_join(deployments[, c('dep_id', 'metal_band', 'species', 'site', 'subsite')], by = 'dep_id') |>
    dplyr::select(site, subsite, species, year,
                  metal_band, dep_id, time, temperature_c, depth_m, wet, deployed) |>
    dplyr::group_by(site, subsite, species, year, metal_band, dep_id, deployed) |>
    dplyr::arrange(time) |>
    dplyr::filter(duplicated(time) == F)

  dd <- stats::na.omit(c(deployments$time_released, deployments$time_recaptured))
  temp <- data[seq(1, nrow(data), 30 * getFrequency(data$time)),]

  if (plot == T) {
    p <- ggplot2::ggplot(temp, ggplot2::aes(x = time, y = depth_m)) +
      ggplot2::geom_line(col = 'red', linewidth = 0.1) +
      ggplot2::geom_line(data = temp[temp$deployed == 1,], ggplot2::aes(x = time, y = depth_m), linewidth = 0.1, col = 'black') +
      ggplot2::geom_vline(xintercept = dd, linetype = 2, col = 'blue', linewidth = 0.5) +
      ggplot2::labs(title = data$dep_id[1], x = 'Time', y = 'Depth (m)') +
      ggplot2::scale_y_reverse() +
      ggplot2::theme_light()

    suppressWarnings(print(p))
  }

  data |>
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

  data <- data |>
    dplyr::mutate(
      inrange = NA
    ) |>
    dplyr::rename(time = Timestamp, lat = location.lat, lon = location.lon, altitude_m = height.msl) |>
    dplyr::filter(!is.na(lon)) |>
    dplyr::inner_join(deployments[, c('dep_id', 'metal_band', 'species', 'site', 'subsite')], by = 'dep_id') |>
    dplyr::select(site, subsite, species, year,
                  metal_band, dep_id, time, lon, lat, altitude_m, satellites, hdop, inrange, deployed) |>
    dplyr::group_by(site, subsite, species, year, metal_band, dep_id, deployed) |>
    dplyr::arrange(time) |>
    dplyr::filter(duplicated(time) == F)

  # All data are kept and the deployed column flags the deployment period,
  # so the plot only needs to show which locations are flagged.
  if (plot == T) .plot_gps_check(data = data, deployments = deployments)

  data |>
    arrow::write_dataset(paste0(output_dataset,'/gps'), format = "parquet",
                         existing_data_behavior = 'delete_matching')

}
# -----

axytrek_acc_check <- function(file,
                              date_format = '%Y-%m-%d %H:%M:%OS',
                              timezone = 'UTC'){

  tt <- utils::read.csv(file = file, stringsAsFactors = F, nrows = 1)

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

  tt <- utils::read.csv(file = file, stringsAsFactors = F, nrows = 1)
  names(tt)[grep('Temp', names(tt))] <- 'Temperature'

  # A logger with the TDR turned off has no TDR columns. Warn and return FALSE so the
  # TDR step can be skipped for this deployment.
  check_names <- c('Timestamp', 'Temperature', 'Activity') %in% names(tt)

  if (sum(check_names) != 3) {
    warning(paste0(file, ' has no TDR data (it does not contain columns named Timestamp, Activity, and Temp...C). ',
                   'TDR data skipped for this deployment.'), call. = FALSE, immediate. = TRUE)
    return(invisible(FALSE))
  }

  check_depth <- c('Depth') %in% names(tt)

  if (sum(check_depth) != 1) warning(paste0(file, ' is missing Depth data'), call. = F)

  date_error <- paste0(' --- Date format test failed for: ', file, ' \nCheck that the date format',
                       tt$Timestamp, ' matches the date_format string: ', date_format, '. \n\n See ?strptime for help with POSIX string formats')
  check_dateformat <- as.POSIXct(tt$Timestamp, format = date_format, tz = 'UTC')
  if (is.na(check_dateformat)) stop(date_error)
  if (check_dateformat < as.POSIXct('2000-01-01', tz = timezone)) stop(date_error)
  if (check_dateformat > Sys.time()) stop(date_error)
  if (length(grep('%OS', date_format)) != 1) stop('date_format must include miliseconds for accelerometer data. Replace %S with %OS')

  invisible(TRUE)
}

# -----

axytrek_gps_check <- function(file,
                              date_format = '%Y-%m-%d %H:%M:%OS',
                              timezone = 'UTC'){

  tt <- utils::read.csv(file = file, stringsAsFactors = F, nrows = 1)

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
#' @param files List of Axytrek .csv files, data must have file names that contain the dep_id in the deployment data
#' @param deployments Deployment data formatted using seabiRds::formatDeployments()
#' @param output_dataset Path where arrow dataset should be saved
#' @param date_format The POSIXct date format used in .csv files
#' @param timezone Timezone of raw data, default is UTC
#' @param acc Should accelerometer data be processed and saved, TRUE or FALSE?
#' @param tdr Should TDR (temperature and depth) data be processed and saved, TRUE or FALSE? If a file has no TDR
#' columns (e.g. the TDR was turned off), a warning names the file and the TDR data are skipped for that deployment.
#' The other data types are still processed. A summary of the skipped deployments is printed at the end.
#' @param gps Should GPS data be processed and saved, TRUE or FALSE?
#' @param plot Should plot of data be generated, TRUE or FALSE?
#' @param fix_axes If TRUE, pitch and roll are plotted for each deployment and you are asked whether the
#' accelerometer x and y axes should be flipped (see checkAxes()). Requires an interactive R session.
#' @param force_axes If TRUE, the accelerometer x and y axes are flipped for all deployments without asking
#' (see checkAxes()). Only use this if you know that all of your data have this issue.
#' @export

axytrek_to_dataset <- function(files,
                               deployments,
                               output_dataset,
                               date_format = '%Y-%m-%d %H:%M:%OS',
                               timezone = 'UTC',
                               acc = T,
                               tdr = T,
                               gps = T,
                               plot = T,
                               fix_axes = F,
                               force_axes = F) {

  .check_arrow()
  .check_csv_files(files)
  .warn_ambiguous_files(files, deployments$dep_id)

  if (nrow(deployments) == 0) stop('deployments has no rows', call. = FALSE)

  n_with_data <- 0
  tdr_skipped <- character(0)

  for (i in seq_len(nrow(deployments))) {

    idx <- grep(deployments$dep_id[i], files, fixed = TRUE)

    if (length(idx) > 0) {

      n_with_data <- n_with_data + 1

      if (is.na(deployments$time_recaptured[i])) warning(
        paste(deployments$dep_id[i], 'is missing time_recaptured'), call. = FALSE, immediate. = TRUE)

      if (acc == TRUE) axytrek_acc_check(file = files[idx[1]],
                        date_format = date_format,
                        timezone = timezone)

      run_tdr <- tdr
      if (tdr == TRUE) {
        run_tdr <- axytrek_tdr_check(file = files[idx[1]],
                          date_format = date_format,
                          timezone = timezone)
        if (!run_tdr) tdr_skipped <- c(tdr_skipped, deployments$dep_id[i])
      }

      if (gps == TRUE) axytrek_gps_check(file = files[idx[1]],
                        date_format = date_format,
                        timezone = timezone)

      dat <- files[idx] |>
        lapply(utils::read.csv, stringsAsFactors = F) |>
        dplyr::bind_rows() |>
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
                             plot = plot,
                             ask = fix_axes,
                             force = force_axes)

      if (run_tdr == TRUE) axytrek_tdr_to_dataset(data = dat,
                             deployments = deployments[i,],
                             output_dataset = output_dataset,
                             date_format = date_format,
                             timezone = timezone,
                             plot = plot)

      if (gps == TRUE & sum(!is.na(dat$location.lon)) > 0)  axytrek_gps_to_dataset(data = dat,
                             deployments = deployments[i,],
                             output_dataset = output_dataset,
                             date_format = date_format,
                             timezone = timezone,
                             plot = plot)

      print(paste0('Finished [',i,']: ', deployments$dep_id[i]))

    } else {print(paste0('No matching data [',i,']: ', deployments$dep_id[i]))}
  }

  if (length(tdr_skipped) > 0) {
    message('TDR data skipped for ', length(tdr_skipped), ' of ', n_with_data,
            ' deployments with data (no TDR columns in the file): ',
            paste(tdr_skipped, collapse = ', '))
  }

  invisible(NULL)
}
