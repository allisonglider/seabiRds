testthat::test_that("getPeakFrequency uses pracma when method is not given", {
  ts <- 25
  t0 <- as.POSIXct('2020-01-01 00:00:00', tz = 'UTC')
  tim <- t0 + (0:749) / ts
  dat <- sin(2 * pi * 2 * (0:749) / ts) # a 2 Hz wave sampled at 25 Hz

  res <- seabiRds::getPeakFrequency(data = dat, time = tim, window = 5,
                                    frequency = ts, threshold = 0.01)
  testthat::expect_equal(stats::median(res, na.rm = TRUE), 2, tolerance = 0.05)

  res_pracma <- seabiRds::getPeakFrequency(data = dat, time = tim, method = 'pracma', window = 5,
                                           frequency = ts, threshold = 0.01)

  testthat::expect_length(res, length(dat))
  testthat::expect_equal(res, res_pracma)

  testthat::expect_error(
    seabiRds::getPeakFrequency(data = dat, time = tim, method = 'other', window = 5, frequency = ts),
    'one of'
  )
})

# only if you make change 2
testthat::test_that("getPeakFrequency handles data with no peaks", {
  ts <- 25
  t0 <- as.POSIXct('2020-01-01 00:00:00', tz = 'UTC')
  tim <- t0 + (0:249) / ts

  res <- seabiRds::getPeakFrequency(data = rep(0, 250), time = tim, window = 5,
                                    frequency = ts, threshold = 0.1)

  testthat::expect_length(res, 250)
  testthat::expect_true(all(res == 0, na.rm = TRUE))
})
