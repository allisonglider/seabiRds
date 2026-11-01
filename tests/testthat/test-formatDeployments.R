make_deployments <- function(...) {
  dd <- data.frame(
    bird = 'BLKI',
    band = c(123456789, 987654321),
    colour = c('A', 'B'),
    id = c('d1', 'd2'),
    site = 'Coats',
    lon = -80,
    lat = 62,
    start = c('2020-07-01 10:00', '2020-07-02 10:00'),
    stringsAsFactors = FALSE
  )
  args <- list(...)
  for (nm in names(args)) dd[[nm]] <- args[[nm]]
  dd
}

format_it <- function(dd, ...) {
  seabiRds::formatDeployments(
    dd, dep_tz = 'UTC', species = 'bird', metal_band = 'band', colour_band = 'colour',
    dep_id = 'id', site = 'site', dep_lon = 'lon', dep_lat = 'lat', time_released = 'start', ...
  )
}

testthat::test_that("formatDeployments standardises names and times", {
  out <- format_it(make_deployments())

  testthat::expect_true(all(c('species', 'metal_band', 'dep_id', 'time_released', 'gps_id') %in% names(out)))
  testthat::expect_equal(out$dep_id, c('d1', 'd2'))
  testthat::expect_equal(out$time_released[1], as.POSIXct('2020-07-01 10:00:00', tz = 'UTC'))
})

testthat::test_that("formatDeployments checks metal_band digits", {
  testthat::expect_error(format_it(make_deployments(band = c(1234567, 987654321))), 'digits')    # 7 digits
  testthat::expect_error(format_it(make_deployments(band = c(1234567890, 987654321))), 'digits') # 10 digits
})

testthat::test_that("formatDeployments handles dep_id", {
  # missing dep_id is an error unless fill_dep_id = TRUE
  testthat::expect_error(format_it(make_deployments(id = c(NA, 'd2'))), 'dep_id')
  out <- format_it(make_deployments(id = c(NA, 'd2')), fill_dep_id = TRUE)
  testthat::expect_equal(out$dep_id, c('123456789_20200701', 'd2'))

  testthat::expect_error(format_it(make_deployments(id = c('d1', 'd1'))), 'unique')
})
