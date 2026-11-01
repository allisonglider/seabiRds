# -----
#' @title Convert Lat2800 biologger data to Arrow dataset
#' @description Reads Lat2800 biologger data and parses it into an arrow dataset for TDR data
#' @param files List of Lat2800 .csv files, data must have file names that contain the dep_id in the deployment data
#' @param deployments Deployment data formatted using seabiRds::formatDeployments()
#' @param output_dataset Path where arrow dataset should be saved
#' @param date_format The POSIXct date format used in .csv files
#' @param plot Should plot of data be generated, TRUE or FALSE?
#' @param max_rows Maximum rows to read in from each file, LAT2800 units sometimes fill memory with rows of invalid data that are too long to read in. There is no default, so a value must be supplied.
#' @param timezone Timezone of raw data, default is UTC
#' @export

lat2800_to_dataset <- function(files,
                               deployments,
                               output_dataset,
                               date_format = '%Y-%m-%d %H:%M:%OS',
                               plot = T,
                               max_rows,
                               timezone = 'UTC') {

  .check_arrow()
  .check_csv_files(files)
  .warn_ambiguous_files(files, deployments$dep_id)

  if (nrow(deployments) == 0) stop('deployments has no rows', call. = FALSE)

  for (i in seq_len(nrow(deployments))) {

    idx <- grep(deployments$dep_id[i], files, fixed = TRUE)

    if (length(idx) > 0) {

      if (is.na(deployments$time_recaptured[i])) warning(
        paste(deployments$dep_id[i], 'is missing time_recaptured'), call. = FALSE, immediate. = TRUE)

      dat <- files[idx] |>
        lapply(utils::read.csv, stringsAsFactors = F, nrows = max_rows, skip = 2) |>
        dplyr::bind_rows()

      if (nrow(dat) == 0) {
        print(paste0('No data in file [',i,']: ', deployments$dep_id[i]))
        next
      }

      if (!all(c('Date', 'Time') %in% names(dat))) {
        stop('The LAT2800 files for ', deployments$dep_id[i], ' must have columns named Date and Time', call. = FALSE)
      }

      # check that the date format matches the files, and say so clearly if it does not
      raw_time <- paste(dat$Date, dat$Time)
      parsed_time <- as.POSIXct(raw_time, format = date_format, tz = timezone)

      if (all(is.na(parsed_time))) {
        warning(deployments$dep_id[i], ': the dates and times in the file could not be read, so this deployment was skipped. ',
                'The first value in the file is "', raw_time[1], '", which does not match date_format = "', date_format,
                '". Check date_format (see ?strptime for formats).',
                call. = FALSE, immediate. = TRUE)
        next
      }

      if (any(is.na(parsed_time))) {
        message(sum(is.na(parsed_time)), ' of ', length(parsed_time), ' rows for ', deployments$dep_id[i],
                ' do not match date_format and were removed')
        dat <- dat[!is.na(parsed_time), ]
        parsed_time <- parsed_time[!is.na(parsed_time)]
      }

      dat <- dat |>
        dplyr::mutate(
          Timestamp = lubridate::with_tz(parsed_time, tzone = 'UTC'),
          dep_id = deployments$dep_id[i],
          year = as.integer(strftime(Timestamp, '%Y')),
          deployed = ifelse(Timestamp >= deployments$time_released[i] &
                              Timestamp <= min(c(deployments$time_recaptured[i],max(Timestamp)), na.rm = T), 1, 0),
        )

      if (!('WetDryState' %in% names(dat))) dat$WetDryState <- NA


      lat2800_tdr_to_dataset(data = dat,
                             deployments = deployments[i,],
                             output_dataset = output_dataset,
                             plot = plot)


      print(paste0('Finished [',i,']: ', deployments$dep_id[i]))

    } else {print(paste0('No matching data [',i,']: ', deployments$dep_id[i]))}
  }
}

# -----

lat2800_tdr_to_dataset <- function(data,
                                   deployments,
                                   output_dataset,
                                   plot) {

  data <- data |>
    dplyr::rename(temperature_c = dplyr::starts_with('IntTemp'),
                  depth_m = dplyr::starts_with('Pressure'),
                  wet = WetDryState,
                  time = Timestamp) |>
    dplyr::mutate(
      wet = ifelse(wet == 0, 1, 0)
    ) |>
    dplyr::inner_join(deployments[, c('dep_id', 'metal_band', 'species', 'site', 'subsite')], by = 'dep_id') |>
    dplyr::select(site, subsite, species, year,
                  metal_band, dep_id, time, temperature_c, depth_m, wet, deployed) |>
    dplyr::group_by(site, subsite, species, year, metal_band, dep_id, deployed) |>
    dplyr::arrange(time) |>
    dplyr::filter(duplicated(time) == F)

  # All data are kept and the deployed column flags the deployment period,
  # so the plot only needs to show which records are flagged.
  if (plot == T) .plot_tdr_check(data = data, deployments = deployments)

  data |>
    arrow::write_dataset(paste0(output_dataset,'/tdr'), format = "parquet",
                         existing_data_behavior = 'delete_matching')

}
