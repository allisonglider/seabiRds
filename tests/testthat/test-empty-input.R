# Tests that the loops in the older functions cope with empty input

testthat::test_that("combineFiles returns an empty data frame when there are no files", {

  res <- combineFiles(files = character(0))

  testthat::expect_s3_class(res, 'data.frame')
  testthat::expect_equal(nrow(res), 0)
})

testthat::test_that("cleanGPSData and cleanTDRData return empty data frames for empty data", {

  dep <- data.frame(
    dep_id = 'A12_123456789_20261014',
    time_released = as.POSIXct('2026-10-14 10:00', tz = 'UTC'),
    time_recaptured = as.POSIXct('2026-10-14 16:00', tz = 'UTC'),
    dep_lon = -82.01,
    dep_lat = 62.95,
    stringsAsFactors = FALSE
  )

  empty_gps <- data.frame(
    dep_id = character(0),
    time = as.POSIXct(character(0), tz = 'UTC'),
    lon = numeric(0),
    lat = numeric(0)
  )

  empty_tdr <- data.frame(
    dep_id = character(0),
    time = as.POSIXct(character(0), tz = 'UTC'),
    depth = numeric(0)
  )

  testthat::expect_equal(nrow(cleanGPSData(empty_gps, dep, plot = FALSE)), 0)
  testthat::expect_equal(nrow(cleanTDRData(empty_tdr, dep, plot = FALSE)), 0)
})
