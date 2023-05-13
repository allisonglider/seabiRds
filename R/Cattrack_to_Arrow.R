# -----
#' @title Convert Cattrack GPS data to Arrow dataset

#' @description Reads Cattrack GPS data and adds it to an arrow dataset
#'
#' @param data Data passed from seabiRds::cattrack_to_dataset()
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
cattrack_gps_to_dataset <- function(data,
                                    deployments,
                                    output_dataset,
                                    plot = T) {

  data <- data %>%
    dplyr::mutate(
      hdop = NA,
      satellites = NA,
      inrange = NA
    ) %>%
    dplyr::rename(lat = Latitude , lon = Longitude , altitude_m = Altitude) %>%
    dplyr::filter(!is.na(lon)) %>%
    dplyr::inner_join(deployments[, c('dep_id','metal_band', 'species', 'site', 'subsite')], by = 'dep_id') %>%
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
#' @title Convert Cattrack biologger data to Arrow dataset
#' @description Reads Cattrack biologger data and parses it into two arrow datasets for GPS and TDR
#' @param files List of Cattrack .csv files, data must include logger IDs that match the gps_id in deployment data
#' @param deployments Deployment data formatted using seabiRds::formatDeployments()
#' @param output_dataset Path where arrow dataset should be saved
#' @param timezone Timezone of raw data, default is UTC
#' @param plot Should plot of data be generated, TRUE or FALSE?
#'
#' @export

cattrack_to_dataset <- function(files,
                                deployments,
                                output_dataset,
                                timezone = 'UTC',
                                dateFormat,
                                plot = T) {

  check_filetype <- grep('.csv', files)
  if (length(check_filetype) != length(files)) stop('All files must be .csv format')

  for (i in 1:nrow(deployments)) {

    idx <- files[grep(deployments$dep_id[i], files)]

    if (length(idx) > 0) {

      output <- combineFiles(files = idx,
                             pattern = "csv",
                             type = "csv",
                             sep = ";",
                             stringsAsFactors = F,
                             header = T)

      output <- output %>%
        dplyr::mutate(
          time = paste(Date, Time, sep = ' '),
          time = as.POSIXct(time, format = paste(dateFormat, "%H:%M:%S"), tz = timezone),
          dep_id = deployments$dep_id[i],
        ) %>%
        dplyr::filter(time > as.POSIXct("1900-01-01", tz = timezone))

      dat <- output

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
          dplyr::mutate(
            dep_id = deployments$dep_id[i],
            year = as.integer(strftime(time, '%Y')),
            deployed = ifelse(time >= deployments$time_released[i] &
                                time <= deployments$time_recaptured[i], 1, 0),
          )

        cattrack_gps_to_dataset(data = dat,
                                deployments = deployments[i,],
                                output_dataset = output_dataset,
                                plot = plot)

        print(paste0('Finished [',i,']: ', deployments$dep_id[i]))

      }  else print(paste0('No matching data [',i,']: ', deployments$dep_id[i]))
    }  else print(paste0('No matching data [',i,']: ', deployments$dep_id[i]))
  }
}

