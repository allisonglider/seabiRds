# Tests for standard column names in formatDeployments()

tz <- 'Canada/Eastern'

# a sheet with every template column, using the standard names
make_deployments <- function() {
  data.frame(
    site = 'Coats',
    subsite = c('Coats West', 'Coats East'),
    dep_lon = c(-82.01, -81.98),
    dep_lat = 62.95,
    species = 'TBMU',
    metal_band = c(123456789, 987654321),
    colour_band = c('X/M-X/R', ''),
    nest = c('N042', ''),
    time_released = c('2026-10-14 09:30', '2026-10-15 11:15'),
    status_on = c('E', 'C'),
    mass_on = c(410, 395),
    fed_unfed = '',
    gps_id = c('A12', 'A13'),
    tdr_id = c('A12', ''),
    acc_id = c('A12', ''),
    gls_id = '',
    mag_id = '',
    cam_id = '',
    hrl_id = '',
    dep_id = c('A12_123456789_20261014', 'A13_987654321_20261015'),
    time_recaptured = c('2026-10-20 14:05', ''),
    status_off = c('E', ''),
    mass_off = c(402, NA),
    exclude = '',
    stringsAsFactors = FALSE
  )
}

testthat::test_that("formatDeployments finds the standard column names without being told", {

  d <- make_deployments()

  auto <- seabiRds::formatDeployments(d, dep_tz = tz)

  explicit <- seabiRds::formatDeployments(
    d, dep_tz = tz,
    species = 'species', metal_band = 'metal_band', colour_band = 'colour_band', dep_id = 'dep_id',
    site = 'site', subsite = 'subsite', nest = 'nest',
    dep_lon = 'dep_lon', dep_lat = 'dep_lat',
    time_released = 'time_released', time_recaptured = 'time_recaptured',
    status_on = 'status_on', status_off = 'status_off',
    mass_on = 'mass_on', mass_off = 'mass_off',
    exclude = 'exclude', fed_unfed = 'fed_unfed',
    gps_id = 'gps_id', tdr_id = 'tdr_id', acc_id = 'acc_id', gls_id = 'gls_id',
    mag_id = 'mag_id', cam_id = 'cam_id', hrl_id = 'hrl_id'
  )

  testthat::expect_equal(auto, explicit)
  testthat::expect_equal(auto$mass_on, c(410, 395))
  testthat::expect_equal(auto$gps_id, c('A12', 'A13'))
})

testthat::test_that("formatDeployments fills optional columns with NA when they are not in the data", {

  d <- make_deployments()
  d$mass_on <- NULL
  d$mass_off <- NULL
  d$nest <- NULL

  res <- seabiRds::formatDeployments(d, dep_tz = tz)

  testthat::expect_true(all(is.na(res$mass_on)))
  testthat::expect_true(all(is.na(res$mass_off)))
  testthat::expect_true(all(is.na(res$nest)))
  testthat::expect_equal(res$dep_id, d$dep_id)
})

testthat::test_that("formatDeployments uses a column name that is given, even if a standard column exists", {

  d <- make_deployments()
  d$my_mass <- c(1, 2)

  res <- seabiRds::formatDeployments(d, dep_tz = tz, mass_on = 'my_mass')

  testthat::expect_equal(res$mass_on, c(1, 2))
  # the other standard columns are still found
  testthat::expect_equal(res$mass_off[1], 402)
})

testthat::test_that("formatDeployments still leaves a column out when it is set to NA", {

  d <- make_deployments()

  res <- seabiRds::formatDeployments(d, dep_tz = tz, mass_on = NA)

  testthat::expect_true(all(is.na(res$mass_on)))
  testthat::expect_equal(res$mass_off[1], 402)
})

testthat::test_that("formatDeployments accepts non-standard names for required columns", {

  d <- make_deployments()
  names(d)[names(d) == 'metal_band'] <- 'band'

  res <- seabiRds::formatDeployments(d, dep_tz = tz, metal_band = 'band')

  testthat::expect_equal(res$metal_band, c(123456789L, 987654321L))
})

testthat::test_that("formatDeployments names the required columns it cannot find", {

  d <- make_deployments()
  d$species <- NULL
  d$site <- NULL

  testthat::expect_error(seabiRds::formatDeployments(d, dep_tz = tz), 'species')
  testthat::expect_error(seabiRds::formatDeployments(d, dep_tz = tz), 'site')

  # giving the column names does not rescue columns that do not exist under the standard name
  d$sp <- 'TBMU'
  d$st <- 'Coats'
  res <- seabiRds::formatDeployments(d, dep_tz = tz, species = 'sp', site = 'st')
  testthat::expect_equal(res$species, c('TBMU', 'TBMU'))
})

testthat::test_that("formatDeployments keeps extra columns listed in keep", {

  d <- make_deployments()
  d$project <- c('murre diving', 'murre diving')

  res <- seabiRds::formatDeployments(d, dep_tz = tz, keep = 'project')

  testthat::expect_true('project' %in% names(res))
  testthat::expect_equal(res$project, d$project)
})
