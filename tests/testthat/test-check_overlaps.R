# Tests for check_overlaps() (R/General_Functions.R)

make_overlap_deployments <- function() {
  t0 <- as.POSIXct('2020-07-01 00:00', tz = 'UTC')
  data.frame(
    dep_id = c('A', 'B', 'C'),
    metal_band = 123456789L,
    gps_id = 'G1',
    cam_id = c('CAM1', 'CAM2', 'CAM2'),
    hrl_id = NA,
    # A and B do not overlap, B and C do
    time_released = t0 + c(1, 3, 5) * 86400,
    time_recaptured = t0 + c(2, 6, 8) * 86400,
    stringsAsFactors = FALSE
  )
}

# run check_overlaps() and keep its warnings and error message
run_check_overlaps <- function(...) {
  warns <- character(0)
  err <- tryCatch(
    withCallingHandlers(
      seabiRds::check_overlaps(...),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart('muffleWarning')
      }
    ),
    error = function(e) conditionMessage(e)
  )
  list(warnings = warns, error = err)
}

testthat::test_that("check_overlaps names the deployments that overlap", {
  res <- run_check_overlaps(make_overlap_deployments(), variables = 'metal_band')

  testthat::expect_equal(res$warnings, 'dep_id B overlaps with C for metal_band 123456789')
  testthat::expect_match(res$error, 'Overlapping deployments found')
})

testthat::test_that("check_overlaps warns once for each variable", {
  res <- run_check_overlaps(make_overlap_deployments())

  testthat::expect_equal(res$warnings,
                         c('dep_id B overlaps with C for metal_band 123456789',
                           'dep_id B overlaps with C for gps_id G1',
                           'dep_id B overlaps with C for cam_id CAM2'))
})

testthat::test_that("check_overlaps accepts cam_id and hrl_id", {
  dep <- make_overlap_deployments()
  dep$hrl_id <- 'H1'

  res <- run_check_overlaps(dep, variables = c('cam_id', 'hrl_id'))

  testthat::expect_equal(res$warnings,
                         c('dep_id B overlaps with C for cam_id CAM2',
                           'dep_id B overlaps with C for hrl_id H1'))
})

testthat::test_that("check_overlaps passes deployments that do not overlap", {
  dep <- make_overlap_deployments()
  dep <- dep[dep$dep_id != 'C', ]

  testthat::expect_no_error(seabiRds::check_overlaps(dep, variables = c('metal_band', 'gps_id')))
  testthat::expect_output(seabiRds::check_overlaps(dep, variables = 'metal_band', verbose = TRUE),
                          'No deployment overlaps found')
})

testthat::test_that("check_overlaps works with a tibble", {
  dep <- dplyr::as_tibble(make_overlap_deployments())
  res <- run_check_overlaps(dep, variables = 'metal_band')

  testthat::expect_equal(res$warnings, 'dep_id B overlaps with C for metal_band 123456789')
})
