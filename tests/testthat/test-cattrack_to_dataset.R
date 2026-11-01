# Tests for the date checks in cattrack_to_dataset() (R/Cattrack_to_Arrow.R)

make_cattrack_folder <- function() {

  folder <- tempfile('cattrack_files_')
  dir.create(folder)

  # dates are written with slashes and the hour has no leading zero, as in some older Cattrack files
  files <- data.frame(
    Date = '2010/07/23',
    Time = paste0(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), ':13:23'),
    Latitude = 62.95 + c(0, 0, 0.05, 0.12, 0.2, 0.25, 0.2, 0.12, 0.05, 0, 0, 0),
    Longitude = -82.01 - c(0, 0, 0.1, 0.3, 0.5, 0.6, 0.5, 0.3, 0.1, 0, 0, 0),
    Altitude = NA, Speed = NA, Course = NA, Type = NA, Distance = NA, Essential = NA
  )
  utils::write.csv(files, file.path(folder, '99678610_20100723.csv'), row.names = FALSE)

  folder
}

make_cattrack_deployments <- function() {
  data.frame(
    dep_id = '99678610_20100723',
    gps_id = '99678610',
    metal_band = 123456789L,
    species = 'TBMU',
    site = 'Coats',
    subsite = 'Coats West',
    dep_lon = -82.01,
    dep_lat = 62.95,
    time_released = as.POSIXct('2010-07-23 03:00', tz = 'UTC'),
    time_recaptured = as.POSIXct('2010-07-23 09:00', tz = 'UTC'),
    stringsAsFactors = FALSE
  )
}

testthat::test_that("cattrack_to_dataset says clearly when dateFormat does not match the files", {
  testthat::skip_if_not_installed('arrow')

  folder <- make_cattrack_folder()
  out <- tempfile('cattrack_dataset_')

  utils::capture.output(
    testthat::expect_warning(
      cattrack_to_dataset(files = list.files(folder, pattern = '\\.csv$', full.names = TRUE),
                          deployments = make_cattrack_deployments(),
                          output_dataset = out,
                          timezone = 'UTC',
                          dateFormat = '%Y-%m-%d',
                          plot = FALSE),
      'could not be read'
    )
  )

  # nothing is written for the skipped deployment
  testthat::expect_false(dir.exists(file.path(out, 'gps')))

  unlink(c(folder, out), recursive = TRUE)
})

testthat::test_that("cattrack_to_dataset imports files when dateFormat matches", {
  testthat::skip_if_not_installed('arrow')

  folder <- make_cattrack_folder()
  out <- tempfile('cattrack_dataset_')

  utils::capture.output(
    cattrack_to_dataset(files = list.files(folder, pattern = '\\.csv$', full.names = TRUE),
                        deployments = make_cattrack_deployments(),
                        output_dataset = out,
                        timezone = 'UTC',
                        dateFormat = '%Y/%m/%d',
                        plot = FALSE)
  )

  res <- dplyr::collect(arrow::open_dataset(file.path(out, 'gps')))

  # all twelve locations are kept, flagged by the deployment period
  testthat::expect_equal(nrow(res), 12)
  testthat::expect_equal(sum(res$deployed == 1), 6)
  unlink(c(folder, out), recursive = TRUE)
})

testthat::test_that("cattrack_to_dataset needs dateFormat", {
  testthat::skip_if_not_installed('arrow')

  testthat::expect_error(
    cattrack_to_dataset(files = 'x.csv', deployments = make_cattrack_deployments(), output_dataset = tempfile()),
    'dateFormat must be supplied'
  )
})
