# Tests for the GPS plot in axytrek_gps_to_dataset() (R/Axytrek_to_Arrow.R), which uses .plot_gps_check().
# This is an internal function, so it is called directly (testthat can see internal functions).

make_axy_plot_deployment <- function() {
  data.frame(
    dep_id = 'A12_123456789_20261014',
    metal_band = 123456789L,
    species = 'TBMU',
    site = 'Coats',
    subsite = 'Coats West',
    dep_lon = -82.01,
    dep_lat = 62.95,
    time_released = as.POSIXct('2026-10-14 10:00', tz = 'UTC'),
    time_recaptured = as.POSIXct('2026-10-14 16:00', tz = 'UTC'),
    stringsAsFactors = FALSE
  )
}

# twelve hourly fixes: two before release, seven during the deployment, three after recapture
make_axy_plot_gps <- function() {
  t0 <- as.POSIXct('2026-10-14 08:00', tz = 'UTC')
  data.frame(
    Timestamp = t0 + 3600 * (0:11),
    location.lat = 62.95 + c(0, 0, 0.05, 0.12, 0.2, 0.25, 0.2, 0.12, 0.05, 0, 0, 0),
    location.lon = -82.01 - c(0, 0, 0.1, 0.3, 0.5, 0.6, 0.5, 0.3, 0.1, 0, 0, 0),
    height.msl = 5,
    satellites = 8,
    hdop = 1.2,
    dep_id = 'A12_123456789_20261014',
    year = 2026L,
    deployed = c(0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
    stringsAsFactors = FALSE
  )
}

testthat::test_that("axytrek_gps_to_dataset draws the distance plot and map, and writes the same data as without a plot", {

  testthat::skip_if_not_installed('arrow')
  testthat::skip_if_not_installed('ggplot2')
  testthat::skip_if_not_installed('cowplot')
  testthat::skip_if_not_installed('sf')
  testthat::skip_if_not_installed('rnaturalearth')
  testthat::skip_if_not_installed('rnaturalearthdata')

  out_no_plot <- tempfile('axy_dataset_')
  out_plot <- tempfile('axy_dataset_')

  axytrek_gps_to_dataset(make_axy_plot_gps(), make_axy_plot_deployment(), out_no_plot, plot = FALSE)
  grDevices::pdf(NULL)
  testthat::expect_no_error(
    axytrek_gps_to_dataset(make_axy_plot_gps(), make_axy_plot_deployment(), out_plot, plot = TRUE)
  )
  grDevices::dev.off()

  a <- dplyr::collect(arrow::open_dataset(file.path(out_no_plot, 'gps')))
  b <- dplyr::collect(arrow::open_dataset(file.path(out_plot, 'gps')))

  # all twelve fixes are kept, including those before release and after recapture
  testthat::expect_equal(nrow(a), 12)
  testthat::expect_equal(sort(as.numeric(a$deployed)), c(rep(0, 5), rep(1, 7)))
  testthat::expect_equal(a[order(a$time), ], b[order(b$time), ], ignore_attr = TRUE)

  unlink(c(out_no_plot, out_plot), recursive = TRUE)
})
