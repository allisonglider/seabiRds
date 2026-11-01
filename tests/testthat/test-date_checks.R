# Tests for the date checks in the Ecotone, LAT2800 and Axy-trek importers

make_check_deployment <- function(dep_id, gps_id, release = '03:00', recapture = '09:00', date = '2010-07-23') {
  data.frame(
    dep_id = dep_id,
    gps_id = gps_id,
    metal_band = 123456789L,
    species = 'TBMU',
    site = 'Coats',
    subsite = 'Coats West',
    dep_lon = -82.01,
    dep_lat = 62.95,
    time_released = as.POSIXct(paste(date, release), tz = 'UTC'),
    time_recaptured = as.POSIXct(paste(date, recapture), tz = 'UTC'),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------------------------------------------
# Ecotone

write_ecotone_file <- function(year = 2016, month = 7) {

  folder <- tempfile('ecotone_files_')
  dir.create(folder)

  x <- data.frame(
    Year = year, Month = month, Day = 27, Hour = 1:12, Minute = 13, Second = 23,
    Logger.ID = 'E07',
    Latitude = 62.95 + c(0, 0, 0.05, 0.12, 0.2, 0.25, 0.2, 0.12, 0.05, 0, 0, 0),
    Longitude = -82.01 - c(0, 0, 0.1, 0.3, 0.5, 0.6, 0.5, 0.3, 0.1, 0, 0, 0),
    Altitude = 5,
    In.range = NA
  )
  utils::write.csv(x, file.path(folder, 'ecotone_test.csv'), row.names = FALSE)

  folder
}

testthat::test_that("ecotone_to_dataset stops clearly when the dates cannot be read", {
  testthat::skip_if_not_installed('arrow')

  folder <- write_ecotone_file(year = 'abc')

  testthat::expect_error(
    ecotone_to_dataset(files = list.files(folder, full.names = TRUE),
                       deployments = make_check_deployment('E07_123456789_20160727', 'E07', date = '2016-07-27'),
                       output_dataset = tempfile(), plot = FALSE),
    'could not be read'
  )

  unlink(folder, recursive = TRUE)
})

testthat::test_that("ecotone_to_dataset reports rows with unreadable dates and imports the rest", {
  testthat::skip_if_not_installed('arrow')

  folder <- write_ecotone_file(month = c(13, rep(7, 11)))
  out <- tempfile('ecotone_dataset_')

  utils::capture.output(
    testthat::expect_message(
      ecotone_to_dataset(files = list.files(folder, full.names = TRUE),
                         deployments = make_check_deployment('E07_123456789_20160727', 'E07', date = '2016-07-27'),
                         output_dataset = out, plot = FALSE),
      '1 of 12 rows'
    )
  )

  res <- dplyr::collect(arrow::open_dataset(file.path(out, 'gps')))
  testthat::expect_equal(nrow(res), 11)
  testthat::expect_false(any(is.na(res$time)))

  unlink(c(folder, out), recursive = TRUE)
})

# ---------------------------------------------------------------------------------------------------------------
# LAT2800

write_lat2800_file <- function(bad_row = FALSE) {

  folder <- tempfile('lat2800_files_')
  dir.create(folder)

  dates <- rep('2010-07-23', 12)
  if (bad_row) dates[5] <- 'not a date'

  lines <- c(
    'LAT2800 export',
    'second header line',
    'Date,Time,IntTemp,Pressure,WetDryState',
    paste(dates, sprintf('%02d:13:23', 1:12), 5, c(0, 0, 5, 20, 35, 10, 40, 15, 2, 0, 0, 0), 1, sep = ',')
  )
  writeLines(lines, file.path(folder, 'L01_123456789_20100723.csv'))

  folder
}

testthat::test_that("lat2800_to_dataset says clearly when date_format does not match the files", {
  testthat::skip_if_not_installed('arrow')

  folder <- write_lat2800_file()
  out <- tempfile('lat2800_dataset_')

  utils::capture.output(
    testthat::expect_warning(
      lat2800_to_dataset(files = list.files(folder, full.names = TRUE),
                         deployments = make_check_deployment('L01_123456789_20100723', 'L01'),
                         output_dataset = out, date_format = '%d/%m/%Y %H:%M:%OS',
                         plot = FALSE, max_rows = 1000),
      'could not be read'
    )
  )

  testthat::expect_false(dir.exists(file.path(out, 'tdr')))

  unlink(c(folder, out), recursive = TRUE)
})

testthat::test_that("lat2800_to_dataset imports files when date_format matches, and removes unreadable rows", {
  testthat::skip_if_not_installed('arrow')

  out <- tempfile('lat2800_dataset_')

  folder <- write_lat2800_file()
  utils::capture.output(
    lat2800_to_dataset(files = list.files(folder, full.names = TRUE),
                       deployments = make_check_deployment('L01_123456789_20100723', 'L01'),
                       output_dataset = out, plot = FALSE, max_rows = 1000)
  )
  res <- dplyr::collect(arrow::open_dataset(file.path(out, 'tdr')))
  testthat::expect_equal(nrow(res), 12)
  testthat::expect_equal(sum(res$deployed == 1), 6)

  out_bad <- tempfile('lat2800_dataset_')
  folder_bad <- write_lat2800_file(bad_row = TRUE)
  utils::capture.output(
    testthat::expect_message(
      lat2800_to_dataset(files = list.files(folder_bad, full.names = TRUE),
                         deployments = make_check_deployment('L01_123456789_20100723', 'L01'),
                         output_dataset = out_bad, plot = FALSE, max_rows = 1000),
      '1 of 12 rows'
    )
  )
  res_bad <- dplyr::collect(arrow::open_dataset(file.path(out_bad, 'tdr')))
  testthat::expect_equal(nrow(res_bad), 11)
  testthat::expect_false(any(is.na(res_bad$time)))

  unlink(c(folder, folder_bad, out, out_bad), recursive = TRUE)
})

# ---------------------------------------------------------------------------------------------------------------
# Axy-trek

testthat::test_that("axytrek_to_dataset reports a date_format that does not match, and rows that do not parse", {
  testthat::skip_if_not_installed('arrow')

  example_dir <- system.file('extdata', 'axytrek', package = 'seabiRds')
  example_file <- list.files(example_dir, pattern = '_S[0-9]+\\.csv$', full.names = TRUE)
  testthat::skip_if(length(example_file) == 0, 'example data not found')

  dep <- formatDeployments(utils::read.csv(file.path(example_dir, 'axytrek_ex.csv')), dep_tz = 'Canada/Eastern')

  # a date_format that does not match the file stops with a message that shows the first value
  testthat::expect_error(
    axytrek_to_dataset(files = example_file, deployments = dep, output_dataset = tempfile(),
                       date_format = '%d/%m/%Y %H:%M:%OS', plot = FALSE),
    'does not match the date_format'
  )

  # one row with a bad time is removed, with a message
  x <- utils::read.csv(example_file, stringsAsFactors = FALSE)
  x$Timestamp[100] <- 'not a date'

  folder <- tempfile('axytrek_files_')
  dir.create(folder)
  utils::write.csv(x, file.path(folder, basename(example_file)), row.names = FALSE)

  out <- tempfile('axytrek_dataset_')
  utils::capture.output(
    testthat::expect_message(
      axytrek_to_dataset(files = list.files(folder, full.names = TRUE), deployments = dep, output_dataset = out,
                         plot = FALSE, force_axes = TRUE),
      '1 of .* rows'
    )
  )

  gps <- dplyr::collect(arrow::open_dataset(file.path(out, 'gps')))
  testthat::expect_false(any(is.na(gps$time)))

  unlink(c(folder, out), recursive = TRUE)
})
