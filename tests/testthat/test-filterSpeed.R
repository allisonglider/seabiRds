testthat::test_that("filterSpeed handles tracks with fewer than two locations", {
  t0 <- as.POSIXct('2020-01-01 00:00:00', tz = 'UTC')
  one <- data.frame(lon = 0, lat = 0, time = t0)

  out <- seabiRds::filterSpeed(one, threshold = 50)
  testthat::expect_equal(nrow(out), 1)
  testthat::expect_true(all(c('dist', 'dt', 'speed') %in% names(out)))

  testthat::expect_equal(nrow(seabiRds::filterSpeed(one[0, ], threshold = 50)), 0)
})

testthat::test_that("filterSpeed does not warn when only the first location is left", {
  t0 <- as.POSIXct('2020-01-01 00:00:00', tz = 'UTC')

  # every move after the first location is about 1,100 km/hr
  track <- data.frame(lon = c(0, 10, 20, 30), lat = 0, time = t0 + 3600 * (0:3))

  testthat::expect_no_warning(out <- seabiRds::filterSpeed(track, threshold = 50))
  testthat::expect_equal(nrow(out), 1)
})
