# Tests for .plot_tdr_check() in R/Process_GPS.R and its use in the Ecotone and LAT2800 importers.
# These are internal functions, so they are called directly (testthat can see internal functions).

skip_if_no_tdr_plot_packages <- function() {
  testthat::skip_if_not_installed('arrow')
  testthat::skip_if_not_installed('ggplot2')
}

make_tdr_deployment <- function() {
  data.frame(
    dep_id = 'T01_123456789_20261014',
    tdr_id = 'T01',
    metal_band = 123456789L,
    species = 'TBMU',
    site = 'Coats',
    subsite = 'Coats West',
    time_released = as.POSIXct('2026-10-14 10:00', tz = 'UTC'),
    time_recaptured = as.POSIXct('2026-10-14 16:00', tz = 'UTC'),
    stringsAsFactors = FALSE
  )
}

# twelve hourly records: two before release, seven during the deployment, three after recapture
make_tdr_data <- function() {
  t0 <- as.POSIXct('2026-10-14 08:00', tz = 'UTC')
  data.frame(
    time = t0 + 3600 * (0:11),
    depth_m = c(0, 0, 5, 20, 35, 10, 40, 15, 2, 0, 0, 0),
    dep_id = 'T01_123456789_20261014',
    deployed = c(0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
    stringsAsFactors = FALSE
  )
}

testthat::test_that(".plot_tdr_check draws a plot without changing the data", {
  skip_if_no_tdr_plot_packages()

  dat <- make_tdr_data()
  before <- dat

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  testthat::expect_no_error(.plot_tdr_check(dat, make_tdr_deployment()))
  testthat::expect_identical(dat, before)
})

testthat::test_that(".plot_tdr_check works with no recapture time and with all depths missing", {
  skip_if_no_tdr_plot_packages()

  dep <- make_tdr_deployment()
  dep$time_recaptured <- as.POSIXct(NA, tz = 'UTC')

  all_na <- make_tdr_data()
  all_na$depth_m <- NA_real_

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  testthat::expect_no_error(.plot_tdr_check(make_tdr_data(), dep))
  testthat::expect_no_error(.plot_tdr_check(all_na, make_tdr_deployment()))
})

testthat::test_that(".plot_tdr_check reports few deployed records and does not fail", {
  skip_if_no_tdr_plot_packages()

  dat <- make_tdr_data()
  dat$deployed <- c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)

  testthat::expect_message(.plot_tdr_check(dat, make_tdr_deployment()), 'Less than 5 deployed depth records')
})

testthat::test_that("lat2800_tdr_to_dataset keeps all data and writes the same data with and without a plot", {
  skip_if_no_tdr_plot_packages()

  make_lat2800 <- function() {
    x <- make_tdr_data()
    data.frame(
      Timestamp = x$time,
      IntTemp.C = 5,
      Pressure.dBar = x$depth_m,
      WetDryState = c(1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1),
      dep_id = x$dep_id,
      year = 2026L,
      deployed = x$deployed,
      stringsAsFactors = FALSE
    )
  }

  out_no_plot <- tempfile('lat2800_dataset_')
  out_plot <- tempfile('lat2800_dataset_')

  lat2800_tdr_to_dataset(make_lat2800(), make_tdr_deployment(), out_no_plot, plot = FALSE)
  grDevices::pdf(NULL)
  lat2800_tdr_to_dataset(make_lat2800(), make_tdr_deployment(), out_plot, plot = TRUE)
  grDevices::dev.off()

  a <- dplyr::collect(arrow::open_dataset(file.path(out_no_plot, 'tdr')))
  b <- dplyr::collect(arrow::open_dataset(file.path(out_plot, 'tdr')))

  # all twelve records are kept, including those before release and after recapture
  testthat::expect_equal(nrow(a), 12)
  testthat::expect_equal(sort(as.numeric(a$deployed)), c(rep(0, 5), rep(1, 7)))
  testthat::expect_equal(a[order(a$time), ], b[order(b$time), ], ignore_attr = TRUE)

  unlink(c(out_no_plot, out_plot), recursive = TRUE)
})

testthat::test_that("ecotone_tdr_to_dataset keeps all data and writes the same data with and without a plot", {
  skip_if_no_tdr_plot_packages()

  make_ecotone_tdr <- function() {
    x <- make_tdr_data()
    data.frame(
      time = x$time,
      Temp_sens = 5,
      Depth = x$depth_m * 100,
      Div.down = NA_real_,
      Div.up = NA_real_,
      tdr_id = 'T01',
      dep_id = x$dep_id,
      year = 2026L,
      deployed = x$deployed,
      stringsAsFactors = FALSE
    )
  }

  out_no_plot <- tempfile('ecotone_tdr_dataset_')
  out_plot <- tempfile('ecotone_tdr_dataset_')

  ecotone_tdr_to_dataset(make_ecotone_tdr(), make_tdr_deployment(), out_no_plot, plot = FALSE)
  grDevices::pdf(NULL)
  ecotone_tdr_to_dataset(make_ecotone_tdr(), make_tdr_deployment(), out_plot, plot = TRUE)
  grDevices::dev.off()

  a <- dplyr::collect(arrow::open_dataset(file.path(out_no_plot, 'tdr')))
  b <- dplyr::collect(arrow::open_dataset(file.path(out_plot, 'tdr')))

  testthat::expect_equal(nrow(a), 12)
  testthat::expect_equal(sort(as.numeric(a$deployed)), c(rep(0, 5), rep(1, 7)))
  testthat::expect_equal(a[order(a$time), ], b[order(b$time), ], ignore_attr = TRUE)

  unlink(c(out_no_plot, out_plot), recursive = TRUE)
})
