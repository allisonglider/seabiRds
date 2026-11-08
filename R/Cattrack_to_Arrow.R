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

  data <- data |>
    dplyr::mutate(
      hdop = NA,
      satellites = NA,
      inrange = NA
    ) |>
    dplyr::rename(lat = Latitude , lon = Longitude , altitude_m = Altitude) |>
    dplyr::filter(!is.na(lon)) |>
    dplyr::inner_join(deployments[, c('dep_id','metal_band', 'species', 'site', 'subsite')], by = 'dep_id') |>
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
#' @title Convert Cattrack biologger data to Arrow dataset
#' @description Reads Cattrack biologger data and parses it into an arrow dataset for GPS data
#' @param files List of Cattrack .csv files, data must have file names that contain the dep_id in the deployment data
#' @param deployments Deployment data formatted using seabiRds::formatDeployments()
#' @param output_dataset Path where arrow dataset should be saved
#' @param timezone Timezone of raw data, default is UTC
#' @param dateFormat POSIXct format of the Date column in the files (e.g. \%d/\%m/\%Y). The Time column is
#' read as \%H:\%M:\%S and added to this format. There is no default, so a value must be supplied.
#' @param plot Should plot of data be generated, TRUE or FALSE?
#'
#' @export

cattrack_to_dataset <- function(files,
                                deployments,
                                output_dataset,
                                timezone = 'UTC',
                                dateFormat,
                                plot = T) {

  .check_arrow()
  .check_csv_files(files)
  .warn_ambiguous_files(files, deployments$dep_id)

  if (missing(dateFormat)) stop("dateFormat must be supplied. It is the format of the Date column in the files, e.g. '%Y-%m-%d' or '%Y/%m/%d'.", call. = FALSE)

  if (nrow(deployments) == 0) stop('deployments has no rows', call. = FALSE)

  for (i in seq_len(nrow(deployments))) {

    idx <- files[grep(deployments$dep_id[i], files, fixed = TRUE)]

    if (length(idx) > 0) {

      output <- combineFiles(files = idx,
                             pattern = "csv",
                             type = "csv",
                             sep = ";",
                             stringsAsFactors = F,
                             header = T)

      if (nrow(output) == 0) {
        print(paste0('No data in file [',i,']: ', deployments$dep_id[i]))
        next
      }

      if (!all(c('Date', 'Time') %in% names(output))) {
        stop('The Cattrack files for ', deployments$dep_id[i], ' must have columns named Date and Time', call. = FALSE)
      }

      # check that the date format matches the files, and say so clearly if it does not
      raw_time <- paste(output$Date, output$Time, sep = ' ')
      parsed_time <- as.POSIXct(raw_time, format = paste(dateFormat, "%H:%M:%S"), tz = timezone)

      if (all(is.na(parsed_time))) {
        warning(deployments$dep_id[i], ': the dates and times in the file could not be read, so this deployment was skipped. ',
                'The first value in the file is "', raw_time[1], '", but dateFormat = "', dateFormat,
                '" expects dates written like "', format(Sys.Date(), dateFormat), '". Check dateFormat (see ?strptime for formats).',
                call. = FALSE, immediate. = TRUE)
        next
      }

      if (any(is.na(parsed_time))) {
        message(sum(is.na(parsed_time)), ' of ', length(parsed_time), ' rows for ', deployments$dep_id[i],
                ' do not match dateFormat and were removed')
      }

      output <- output |>
        dplyr::mutate(
          time = parsed_time,
          dep_id = deployments$dep_id[i],
        ) |>
        dplyr::filter(!is.na(time), time > as.POSIXct("1900-01-01", tz = timezone))

      dat <- output

      if (nrow(dat) > 0 & is.na(deployments$time_recaptured[i])) {
        deployments$time_recaptured[i] <- max(dat$time)
        warning(
          paste(deployments$dep_id[i], 'is missing time_recaptured'), call. = FALSE, immediate. = TRUE)
      }

      if (nrow(dat) > 0) {

        # Cattrack files are matched to deployments by file name, so gps_id is not needed here
        dat <- dat |>
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

      }  else print(paste0('No valid data [',i,']: ', deployments$dep_id[i]))
    }  else print(paste0('No matching data [',i,']: ', deployments$dep_id[i]))
  }
}

