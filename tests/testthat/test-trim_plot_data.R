# Tests for .trim_plot_data() in R/Process_GPS.R, and its use in the plot helpers

released <- as.POSIXct('2026-10-14 10:00', tz = 'UTC')

testthat::test_that(".trim_plot_data drops records more than 30 days before release", {

  dat <- data.frame(time = released + 3600 * c(-24 * 60, -2, -1, 0, 1, 2), value = 1:6)

  testthat::expect_no_message(res <- .trim_plot_data(dat, released, 'A12'))
  testthat::expect_equal(nrow(res), 5)
  testthat::expect_equal(res$value, 2:6)

  # the original data are not changed
  testthat::expect_equal(nrow(dat), 6)
})

testthat::test_that(".trim_plot_data keeps records less than 30 days before release, without a message", {

  dat <- data.frame(time = released + 3600 * c(-24 * 29, -2, 0, 1), value = 1:4)

  testthat::expect_no_message(res <- .trim_plot_data(dat, released, 'A12'))
  testthat::expect_equal(nrow(res), 4)
})

testthat::test_that(".trim_plot_data does not remove records after the deployment", {

  dat <- data.frame(time = released + 3600 * c(0, 1, 24 * 90), value = 1:3)

  testthat::expect_no_message(res <- .trim_plot_data(dat, released))
  testthat::expect_equal(nrow(res), 3)
})

testthat::test_that("the GPS and TDR plot helpers leave out an early record from the plot, but not from the data", {
  testthat::skip_if_not_installed('ggplot2')
  testthat::skip_if_not_installed('cowplot')
  testthat::skip_if_not_installed('sf')
  testthat::skip_if_not_installed('rnaturalearth')
  testthat::skip_if_not_installed('rnaturalearthdata')

  dep <- data.frame(
    dep_id = 'A12_123456789_20261014',
    dep_lon = -82.01,
    dep_lat = 62.95,
    time_released = released,
    time_recaptured = released + 6 * 3600,
    stringsAsFactors = FALSE
  )

  # an early record 60 days before release, then twelve hourly records starting two hours before release
  t0 <- released - 2 * 3600
  gps <- data.frame(
    time = c(released - 60 * 24 * 3600, t0 + 3600 * (0:11)),
    lat = 62.95 + c(0, 0, 0, 0.05, 0.12, 0.2, 0.25, 0.2, 0.12, 0.05, 0, 0, 0),
    lon = -82.01 - c(0, 0, 0, 0.1, 0.3, 0.5, 0.6, 0.5, 0.3, 0.1, 0, 0, 0),
    dep_id = 'A12_123456789_20261014',
    deployed = c(0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
    stringsAsFactors = FALSE
  )
  tdr <- data.frame(
    time = gps$time,
    depth_m = c(0, 0, 0, 5, 20, 35, 10, 40, 15, 2, 0, 0, 0),
    dep_id = gps$dep_id,
    deployed = gps$deployed,
    stringsAsFactors = FALSE
  )
  gps_before <- gps
  tdr_before <- tdr

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  testthat::expect_no_error(.plot_gps_check(gps, dep))
  testthat::expect_no_error(.plot_tdr_check(tdr, dep))

  testthat::expect_identical(gps, gps_before)
  testthat::expect_identical(tdr, tdr_before)
})
