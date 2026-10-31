# Tests for .plot_gps_check() in R/Process_GPS.R and its use in the Cattrack and Ecotone importers.
# These are internal functions, so they are called directly (testthat can see internal functions).

skip_if_no_plot_packages <- function() {
  testthat::skip_if_not_installed('arrow')
  testthat::skip_if_not_installed('ggplot2')
  testthat::skip_if_not_installed('cowplot')
  testthat::skip_if_not_installed('sf')
  testthat::skip_if_not_installed('rnaturalearth')
  testthat::skip_if_not_installed('rnaturalearthdata')
}

make_plot_deployment <- function(dep_lon = -82.01, dep_lat = 62.95) {
  data.frame(
    dep_id = 'C01_123456789_20261014',
    gps_id = 'C01',
    metal_band = 123456789L,
    species = 'TBMU',
    site = 'Coats',
    subsite = 'Coats West',
    dep_lon = dep_lon,
    dep_lat = dep_lat,
    time_released = as.POSIXct('2026-10-14 10:00', tz = 'UTC'),
    time_recaptured = as.POSIXct('2026-10-14 16:00', tz = 'UTC'),
    stringsAsFactors = FALSE
  )
}

# twelve hourly locations: two before release, seven during the deployment, three after recapture
make_plot_gps <- function() {
  t0 <- as.POSIXct('2026-10-14 08:00', tz = 'UTC')
  data.frame(
    time = t0 + 3600 * (0:11),
    lat = 62.95 + c(0, 0, 0.05, 0.12, 0.2, 0.25, 0.2, 0.12, 0.05, 0, 0, 0),
    lon = -82.01 - c(0, 0, 0.1, 0.3, 0.5, 0.6, 0.5, 0.3, 0.1, 0, 0, 0),
    dep_id = 'C01_123456789_20261014',
    year = 2026L,
    deployed = c(0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
    stringsAsFactors = FALSE
  )
}

make_cattrack_gps <- function() {
  x <- make_plot_gps()
  names(x)[names(x) == 'lat'] <- 'Latitude'
  names(x)[names(x) == 'lon'] <- 'Longitude'
  x$Altitude <- 5
  x
}

testthat::test_that(".plot_gps_check draws a plot without changing the data", {
  skip_if_no_plot_packages()

  dat <- make_plot_gps()
  before <- dat

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  testthat::expect_no_error(.plot_gps_check(dat, make_plot_deployment()))
  testthat::expect_identical(dat, before)
})

testthat::test_that(".plot_gps_check works without a deployment location or recapture time", {
  skip_if_no_plot_packages()

  dep <- make_plot_deployment(dep_lon = NA, dep_lat = NA)
  dep$time_recaptured <- as.POSIXct(NA, tz = 'UTC')

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  testthat::expect_no_error(.plot_gps_check(make_plot_gps(), dep))
})

testthat::test_that(".plot_gps_check reports few deployed locations and does not fail", {
  skip_if_no_plot_packages()

  dat <- make_plot_gps()
  dat$deployed <- c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)

  testthat::expect_message(.plot_gps_check(dat, make_plot_deployment()), 'Less than 5 deployed locations')
})

testthat::test_that("cattrack_gps_to_dataset keeps all data and writes the same data with and without a plot", {
  skip_if_no_plot_packages()

  out_no_plot <- tempfile('cattrack_dataset_')
  out_plot <- tempfile('cattrack_dataset_')

  cattrack_gps_to_dataset(make_cattrack_gps(), make_plot_deployment(), out_no_plot, plot = FALSE)
  grDevices::pdf(NULL)
  cattrack_gps_to_dataset(make_cattrack_gps(), make_plot_deployment(), out_plot, plot = TRUE)
  grDevices::dev.off()

  a <- dplyr::collect(arrow::open_dataset(file.path(out_no_plot, 'gps')))
  b <- dplyr::collect(arrow::open_dataset(file.path(out_plot, 'gps')))

  # all twelve locations are kept, including those before release and after recapture
  testthat::expect_equal(nrow(a), 12)
  testthat::expect_equal(sort(as.numeric(a$deployed)), c(rep(0, 5), rep(1, 7)))
  testthat::expect_equal(a[order(a$time), ], b[order(b$time), ], ignore_attr = TRUE)

  unlink(c(out_no_plot, out_plot), recursive = TRUE)
})

testthat::test_that("ecotone_gps_to_dataset keeps all data and writes the same data with and without a plot", {
  skip_if_no_plot_packages()

  make_ecotone_gps <- function() {
    x <- make_plot_gps()
    names(x)[names(x) == 'lat'] <- 'Latitude'
    names(x)[names(x) == 'lon'] <- 'Longitude'
    x$Altitude <- 5
    x$HDOP <- 1.2
    x$Sat..Count <- 8
    x$In.range <- NA
    x$gps_id <- 'C01'
    x
  }

  out_no_plot <- tempfile('ecotone_dataset_')
  out_plot <- tempfile('ecotone_dataset_')

  ecotone_gps_to_dataset(make_ecotone_gps(), make_plot_deployment(), out_no_plot, plot = FALSE)
  grDevices::pdf(NULL)
  ecotone_gps_to_dataset(make_ecotone_gps(), make_plot_deployment(), out_plot, plot = TRUE)
  grDevices::dev.off()

  a <- dplyr::collect(arrow::open_dataset(file.path(out_no_plot, 'gps')))
  b <- dplyr::collect(arrow::open_dataset(file.path(out_plot, 'gps')))

  testthat::expect_equal(nrow(a), 12)
  testthat::expect_equal(sort(as.numeric(a$deployed)), c(rep(0, 5), rep(1, 7)))
  testthat::expect_equal(a[order(a$time), ], b[order(b$time), ], ignore_attr = TRUE)

  unlink(c(out_no_plot, out_plot), recursive = TRUE)
})
