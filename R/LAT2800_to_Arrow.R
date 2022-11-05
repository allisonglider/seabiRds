# -----
#' @title Convert Lat2800 biologger data to Arrow dataset
#' @description Reads Lat2800 biologger data and parses it into three arrow datasets for  TDR data
#' @param files List of Lat2800 .csv files, data must have file names that are a partial match to the dep_id in deployment data
#' @param deployments Deployment data formatted using seabiRds::formatDeployments()
#' @param output_dataset Path where arrow dataset should be saved
#' @param date_format The POSIXct date format used in .csv files
#' @param timezone Timezone of raw data, default is UTC
#' @param plot Should plot of data be generated, TRUE or FALSE?
#' @param max_rows Maximum rows to read in, LAT2800 units sometimes fill memory with rows of invalid data that are too long to read in
#' @export

lat2800_to_dataset <- function(files,
                               deployments,
                               output_dataset,
                               date_format = '%Y-%m-%d %H:%M:%OS',
                               plot = T,
                               max_rows,
                               timezone = 'UTC') {

  check_filetype <- grep('.csv', files)
  if (length(check_filetype) != length(files)) stop('All files must be .csv format')

  for (i in 1:nrow(deployments)) {

    idx <- grep(deployments$dep_id[i], files)

    if (length(idx) > 0) {

      if (is.na(deployments$time_recaptured[i])) warning(
        paste(deployments$dep_id[i], 'is missing time_recaptured'), call. = FALSE, immediate. = TRUE)

      dat <- files[idx] %>%
        purrr::map_df(~read.csv(file = ., stringsAsFactors = F, nrows = max_rows, skip = 2)) %>%
        dplyr::mutate(
          Timestamp = as.POSIXct(paste(Date, Time), format = date_format, tz = timezone),
          Timestamp = lubridate::with_tz(Timestamp, tzone = 'UTC'),
          dep_id = deployments$dep_id[i],
          year = as.integer(strftime(Timestamp, '%Y')),
          deployed = ifelse(Timestamp >= deployments$time_released[i] &
                              Timestamp <= min(c(deployments$time_recaptured[i],max(Timestamp)), na.rm = T), 1, 0),
        )

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

  data <- data %>%
    rename(temperature_c = dplyr::starts_with('IntTemp'),
           depth_m = dplyr::starts_with('Pressure'),
           wet = WetDryState,
           time = Timestamp) %>%
    mutate(
      wet = ifelse(wet == 0, 1, 0)
    ) %>%
    dplyr::inner_join(deployments[, c('dep_id', 'metal_band', 'species', 'site', 'subsite')], by = 'dep_id') %>%
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
