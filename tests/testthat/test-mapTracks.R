# Tests for mapTracks() in R/Visualize_tracks.R

make_map_data <- function() {
  data.frame(
    dep_id = rep(c('A12_123456789_20261014', 'A13_987654321_20261015'), each = 6),
    time = rep(as.POSIXct('2026-10-14 10:00', tz = 'UTC') + 3600 * (0:5), 2),
    lon = -82.01 - rep(c(0, 0.1, 0.3, 0.5, 0.3, 0.1), 2),
    lat = 62.95 + rep(c(0, 0.05, 0.12, 0.2, 0.12, 0.05), 2),
    coldist = rep(c(0, 5, 14, 22, 14, 5), 2),
    deployed = rep(c(0, 1, 1, 1, 1, 0), 2),
    stringsAsFactors = FALSE
  )
}

skip_if_no_map_packages <- function() {
  testthat::skip_if_not_installed('ggplot2')
  testthat::skip_if_not_installed('sf')
  testthat::skip_if_not_installed('rnaturalearth')
  testthat::skip_if_not_installed('rnaturalearthdata')
}

testthat::test_that("mapTracks colours the tracks by the grouping variable without deprecation warnings", {
  skip_if_no_map_packages()

  testthat::expect_no_warning(p <- mapTracks(make_map_data()))
  testthat::expect_s3_class(p, 'ggplot')

  # the point and path layers (layers 2 and 3, after the map) are coloured by dep_id
  built <- ggplot2::ggplot_build(p)
  testthat::expect_equal(length(unique(built$data[[2]]$colour)), 2)
  testthat::expect_equal(length(unique(built$data[[3]]$colour)), 2)
})

testthat::test_that("mapTracks can colour by another column and can draw without a grouping variable", {
  skip_if_no_map_packages()

  dat <- make_map_data()
  dat$bird <- rep(c('one', 'two'), each = 6)

  testthat::expect_no_error(ggplot2::ggplot_build(mapTracks(dat, groups = 'bird')))
  testthat::expect_no_error(ggplot2::ggplot_build(mapTracks(dat, groups = NULL)))
})

testthat::test_that("mapTracks works without a coldist column, as in the standardized datasets", {
  skip_if_no_map_packages()

  dat <- make_map_data()
  dat$coldist <- NULL

  testthat::expect_no_warning(p <- mapTracks(dat))
  testthat::expect_s3_class(p, 'ggplot')
})

testthat::test_that("mapTracks puts the locations in time order for each deployment", {
  skip_if_no_map_packages()

  dat <- make_map_data()
  shuffled <- dat[c(12:1), ]

  p <- mapTracks(shuffled)
  plotted <- p$layers[[3]]$data

  testthat::expect_equal(plotted$dep_id, dat$dep_id)
  testthat::expect_equal(plotted$time, dat$time)
})

testthat::test_that("mapTracks can map only the deployed locations", {
  skip_if_no_map_packages()

  dat <- make_map_data()

  # all twelve locations are mapped by default, only the eight deployed ones with deployed_only
  testthat::expect_equal(nrow(mapTracks(dat)$layers[[3]]$data), 12)
  testthat::expect_equal(nrow(mapTracks(dat, deployed_only = TRUE)$layers[[3]]$data), 8)

  # without a deployed column all locations are mapped, with a warning
  dat$deployed <- NULL
  testthat::expect_warning(p <- mapTracks(dat, deployed_only = TRUE), 'no deployed column')
  testthat::expect_equal(nrow(p$layers[[3]]$data), 12)
})
