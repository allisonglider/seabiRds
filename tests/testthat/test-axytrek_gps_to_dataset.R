# Tests for axytrek_gps_to_dataset() in R/Axytrek_to_Arrow.R
# This is an internal function, so it is called directly (testthat can see internal functions).

make_axy_deployment <- function() {
  data.frame(
    dep_id = 'A12_123456789_20261014',
    metal_band = 123456789L,
    species = 'TBMU',
    site = 'Coats',
    subsite = 'Coats West',
    dep_lon = -82.01,
    dep_lat = 62.95,
    time_released = as.POSIXct('2026-10-14 09:30', tz = 'UTC'),
    time_recaptured = as.POSIXct('2026-10-14 12:00', tz = 'UTC'),
    stringsAsFactors = FALSE
  )
}

# six rows: two before/after the deployment, one duplicated time, one missing longitude
make_axy_gps <- function() {
  t0 <- as.POSIXct('2026-10-14 09:00', tz = 'UTC')
  data.frame(
    Timestamp = t0 + c(0, 1800, 3600, 3600, 5400, 14400),
    location.lat = c(62.95, 62.96, 62.97, 62.97, 62.98, 62.99),
    location.lon = c(-82.01, -82.02, -82.03, -82.03, NA, -82.05),
    height.msl = 5,
    satellites = 8,
    hdop = 1.2,
    dep_id = 'A12_123456789_20261014',
    year = 2026L,
    deployed = c(0, 1, 1, 1, 1, 0),
    stringsAsFactors = FALSE
  )
}

testthat::test_that("axytrek_gps_to_dataset keeps all data and flags the deployed period", {

  testthat::skip_if_not_installed('arrow')

  out <- tempfile('axy_dataset_')
  axytrek_gps_to_dataset(make_axy_gps(), make_axy_deployment(), out, plot = FALSE)

  res <- dplyr::collect(arrow::open_dataset(file.path(out, 'gps')))

  # the duplicated time and the missing longitude are dropped, nothing is clipped
  testthat::expect_equal(nrow(res), 4)
  testthat::expect_equal(sort(as.numeric(res$deployed)), c(0, 0, 1, 1))
  testthat::expect_true(all(c('lon', 'lat', 'time', 'dep_id') %in% names(res)))

  unlink(out, recursive = TRUE)
})

testthat::test_that("axytrek_gps_to_dataset writes the same data with and without a plot", {

  testthat::skip_if_not_installed('arrow')
  testthat::skip_if_not_installed('ggplot2')

  out_no_plot <- tempfile('axy_dataset_')
  out_plot <- tempfile('axy_dataset_')

  axytrek_gps_to_dataset(make_axy_gps(), make_axy_deployment(), out_no_plot, plot = FALSE)
  grDevices::pdf(NULL)
  axytrek_gps_to_dataset(make_axy_gps(), make_axy_deployment(), out_plot, plot = TRUE)
  grDevices::dev.off()

  a <- dplyr::collect(arrow::open_dataset(file.path(out_no_plot, 'gps')))
  b <- dplyr::collect(arrow::open_dataset(file.path(out_plot, 'gps')))

  testthat::expect_equal(a[order(a$time), ], b[order(b$time), ], ignore_attr = TRUE)

  unlink(c(out_no_plot, out_plot), recursive = TRUE)
})
