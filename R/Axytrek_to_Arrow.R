

#' Reads axytrek accelerometer data and adds it to an arrow dataset
#'
#' @param files List of axytrek .csv files, file names must include the dep_id in deployments
#' @param deployments Deployment data formatted using seabiRds::formatDeployments()
#' @param output_dataset Path where arrow dataset should be saved
#' @param date_format Date format used in .csv files, default is '%Y-%m-%d %H:%M:%OS'
#' @param timezone Timezone of raw data, default is UTC
#'
#' @return Code checks that data meet formatting requirements. Produces a plot
#' showing a time series of x-axis data from the accelerometer. Deployed data are shown in black,
#' undeployed data are shown in red, and deployment start/end times are shown with
#' dashed vertical bluse lines.
#'
#' Output is saved to an arrow dataset at the location indicated by output_dataset.
#'
#' @export
#'
#'
axytrek_acc_to_dataset <- function(files,
                           deployments,
                           output_dataset,
                           date_format = '%Y-%m-%d %H:%M:%OS',
                           timezone = 'UTC') {

  check_filetype <- grep('.csv', files)
  if (length(check_filetype) != length(files)) stop('All files must be .csv format')

  for (i in 1:nrow(deployments)) {

    idx <- grep(deployments$dep_id[i], files)

    if (length(idx) > 0) {

      if (is.na(deployments$time_recaptured[i])) warning(
        paste(deployments$dep_id[i], 'is missing time_recaptured'), call. = FALSE, immediate. = TRUE)

      axytrek_acc_check(file = files[idx[1]],
                                        date_format = date_format,
                                        timezone = timezone)

      dat <- files[idx] %>%
        purrr::map_df(~read.csv(file = ., stringsAsFactors = F)) %>%
        select(Timestamp, X, Y, Z) %>%
        dplyr::mutate(
          Timestamp = as.POSIXct(Timestamp, format = date_format, tz = timezone),
          Timestamp = lubridate::with_tz(Timestamp, tzone = 'UTC'),
          dep_id = deployments$dep_id[i],
          year = as.integer(strftime(Timestamp, '%Y')),
          deployed = ifelse(Timestamp > deployments$time_released[i] &
                              Timestamp <= min(c(deployments$time_recaptured[i],max(Timestamp)), na.rm = T), 1, 0),
          logger_type = 'acc'
        ) %>%
        dplyr::rename(time = Timestamp, x = X, y = Y, z = Z) %>%
        dplyr::inner_join(deployments[i, c('dep_id', 'metal_band', 'species', 'site', 'subsite')]) %>%
        dplyr::select(logger_type, site, subsite, species, year,
                      metal_band, dep_id, time, x, y, z, deployed) %>%
        dplyr::group_by(logger_type, site, subsite, species, year, metal_band, dep_id, deployed)%>%
        dplyr::arrange(time) %>%
        dplyr::filter(duplicated(time) == F)

      dd <- na.omit(c(deployments$time_released[i], deployments$time_recaptured[i]))
      temp <- dat[seq(1, nrow(dat), 30 * getFrequency(dat$time)),]

      p <- ggplot2::ggplot(temp, ggplot2::aes(x = time, y = x)) +
        ggplot2::geom_line(size = 0.1, col = 'red') +
        ggplot2::geom_line(data = temp[temp$deployed == 1,], ggplot2::aes(x = time, y = x), size = 0.1, col = 'black') +
        ggplot2::geom_vline(xintercept = dd, linetype = 2, col = 'blue', size = 0.5) +
        ggplot2::labs(title = dat$dep_id[1], x = 'Time', y = 'x-axis (g)') +
        ggplot2::theme_light()
      print(p)

      dat %>%
        arrow::write_dataset(output_dataset, format = "parquet",
                      existing_data_behavior = 'delete_matching')

      print(paste0('Finished [',i,']: ', deployments$dep_id[i]))

    } else {print(paste0('No matching acc data [',i,']: ', deployments$dep_id[i]))}

  }
}

# -----

#' Reads axytrek accelerometer data and adds it to an arrow dataset
#'
#' @param files List of axytrek .csv files, file names must include the dep_id in deployments
#' @param deployments Deployment data formatted using seabiRds::formatDeployments()
#' @param output_dataset Path where arrow dataset should be saved
#' @param date_format Date format used in .csv files, default is '%Y-%m-%d %H:%M:%OS'
#' @param timezone Timezone of raw data, default is UTC
#'
#' @return Code checks that data meet formatting requirements. Produces a plot
#' showing a time series of depth data from the tdr. Deployed data are shown in black,
#' undeployed data are shown in red, and deployment start/end times are shown with
#' dashed vertical bluse lines.
#'
#' Output is saved to an arrow dataset at the location indicated by output_dataset.
#'
#' @export
#'
#'
axytrek_tdr_to_dataset <- function(files,
                                       deployments,
                                       output_dataset,
                                       date_format = '%Y-%m-%d %H:%M:%OS',
                                       timezone = 'UTC') {

  check_filetype <- grep('.csv', files)
  if (length(check_filetype) != length(files)) stop('All files must be .csv format')

  for (i in 1:nrow(deployments)) {

    idx <- grep(deployments$dep_id[i], files)

    if (length(idx) > 0) {

      if (is.na(deployments$time_recaptured[i])) warning(
        paste(deployments$dep_id[i], 'is missing time_recaptured'), call. = FALSE, immediate. = TRUE)

      axytrek_tdr_check(file = files[idx[1]],
                            date_format = date_format,
                            timezone = timezone)

      dat <- files[idx] %>%
        purrr::map_df(~read.csv(file = ., stringsAsFactors = F)) %>%
        rename(temperature_c = dplyr::starts_with('Temp')) %>%
        dplyr::select(Timestamp, Depth, temperature_c) %>%
        dplyr::mutate(
          Timestamp = as.POSIXct(Timestamp, format = date_format, tz = timezone),
          Timestamp = lubridate::with_tz(Timestamp, tzone = 'UTC'),
          dep_id = deployments$dep_id[i],
          year = as.integer(strftime(Timestamp, '%Y')),
          deployed = ifelse(Timestamp > deployments$time_released[i] &
                              Timestamp <= min(c(deployments$time_recaptured[i],max(Timestamp)), na.rm = T), 1, 0),
          logger_type = 'tdr'
        ) %>%
        dplyr::rename(time = Timestamp, depth_m = Depth) %>%
        dplyr::filter(!is.na(depth_m)) %>%
        dplyr::inner_join(deployments[i, c('dep_id', 'metal_band', 'species', 'site', 'subsite')]) %>%
        dplyr::select(logger_type, site, subsite, species, year,
                      metal_band, dep_id, time, depth_m, temperature_c,deployed) %>%
        dplyr::group_by(logger_type, site, subsite, species, year, metal_band, dep_id, deployed) %>%
        dplyr::arrange(time) %>%
        dplyr::filter(duplicated(time) == F)

      dd <- na.omit(c(deployments$time_released[i], deployments$time_recaptured[i]))
      temp <- dat[seq(1, nrow(dat), 30 * getFrequency(dat$time)),]

      p <- ggplot2::ggplot(temp, ggplot2::aes(x = time, y = depth_m)) +
        ggplot2::geom_line(size = 0.1, col = 'red', size = 0.1) +
        ggplot2::geom_line(data = temp[temp$deployed == 1,], ggplot2::aes(x = time, y = depth_m), size = 0.1, col = 'black') +
        ggplot2::geom_vline(xintercept = dd, linetype = 2, col = 'blue', size = 0.5) +
        ggplot2::labs(title = dat$dep_id[1], x = 'Time', y = 'Depth (m)') +
        ggplot2::scale_y_reverse() +
        ggplot2::theme_light()
      print(p)

      dat %>%
        arrow::write_dataset("E:/test-arrow/biologging", format = "parquet",
                      existing_data_behavior = 'delete_matching')

      print(paste0('Finished [',i,']: ', deployments$dep_id[i]))

    } else {print(paste0('No matching tdr data [',i,']: ', deployments$dep_id[i]))}

  }
}

# -----

#' Runs checks on axytrek accelerometer data
#'
#' @param files List of axytrek .csv files, file names must include the dep_id in deployments
#' @param date_format Date format used in .csv files, default is '%Y-%m-%d %H:%M:%OS'
#' @param timezone Timezone of raw data, default is UTC
#'
#' @return Ends dataset importing if file names or date formats do not meet checks
#'
#' @export
#'
#' @examples
#'
#'

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

#' Runs checks on axytrek tdr data
#'
#' @param files List of axytrek .csv files, file names must include the dep_id in deployments
#' @param date_format Date format used in .csv files, default is '%Y-%m-%d %H:%M:%OS'
#' @param timezone Timezone of raw data, default is UTC
#'
#' @return Ends dataset importing if file names or date formats do not meet checks
#'
#' @export
#'
#' @examples
#'

axytrek_tdr_check <- function(file,
                                  date_format = '%Y-%m-%d %H:%M:%OS',
                                  timezone = 'UTC'){

  tt <- read.csv(file = file, stringsAsFactors = F, nrows = 1)
  names(tt)[grep('Temp', names(tt))] <- 'Temperature'

  check_names <- c('Timestamp', 'Depth', 'Temperature') %in% names(tt)

  if (sum(check_names) != 3) stop(paste0(file, 'does not contain columns named Timestamp, Depth, and Temp...C'))

  date_error <- paste0(' --- Date format test failed for: ', file, ' \nCheck that the date format',
                       tt$Timestamp, ' matches the date_format string: ', date_format, '. \n\n See ?strptime for help with POSIX string formats')
  check_dateformat <- as.POSIXct(tt$Timestamp, format = date_format, tz = 'UTC')
  if (is.na(check_dateformat)) stop(date_error)
  if (check_dateformat < as.POSIXct('2000-01-01', tz = timezone)) stop(date_error)
  if (check_dateformat > Sys.time()) stop(date_error)
  if (length(grep('%OS', date_format)) != 1) stop('date_format must include miliseconds for accelerometer data. Replace %S with %OS')

}

