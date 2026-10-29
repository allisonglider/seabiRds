testthat::test_that("getDT returns time differences", {
  t0 <- as.POSIXct('2020-01-01 00:00:00', tz = 'UTC')
  tt <- t0 + c(0, 60, 120)

  testthat::expect_equal(seabiRds::getDT(tt, units = 'sec'), c(NA, 60, 60))
  testthat::expect_equal(seabiRds::getDT(tt, units = 'min'), c(NA, 1, 1))
  testthat::expect_equal(seabiRds::getDT(tt), c(NA, 60, 60) / 3600) # default is hours
  testthat::expect_equal(seabiRds::getDT(t0), NA_real_)             # single time
})

testthat::test_that("getDT stops when times are out of order or not POSIXct", {
  t0 <- as.POSIXct('2020-01-01 00:00:00', tz = 'UTC')

  testthat::expect_error(seabiRds::getDT(t0 + c(0, 120, 60)), 'chronological')
  testthat::expect_error(seabiRds::getDT(c(0, 60, 120)), 'POSIXct')
})
