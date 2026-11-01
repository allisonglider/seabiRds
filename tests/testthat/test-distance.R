testthat::test_that("getDist and getColDist return distances in km", {
  lon <- c(0, 0, 0)
  lat <- c(0, 1, 2)

  # one degree of latitude is about 111 km
  d <- seabiRds::getDist(lon, lat)
  testthat::expect_length(d, 3)
  testthat::expect_equal(d[2:3], c(111, 111), tolerance = 0.02)
  testthat::expect_equal(d[1], 0)

  cd <- seabiRds::getColDist(lon, lat, colonyLon = 0, colonyLat = 0)
  testthat::expect_length(cd, 3)
  testthat::expect_equal(cd, c(0, 111, 222), tolerance = 0.02)

})

testthat::test_that("getDist checks coordinate ranges and names the right problem", {
  testthat::expect_error(seabiRds::getDist(c(0, 200), c(0, 0)), 'Longitude')
  testthat::expect_error(seabiRds::getDist(c(0, 0), c(0, 100)), 'Latitude')
  testthat::expect_error(seabiRds::getDist(c(0, 0), c(0, -120)), 'Latitude') # used to pass
})

testthat::test_that("getColDist checks coordinate ranges and names the right problem", {
  testthat::expect_error(seabiRds::getColDist(c(0, 200), c(0, 0), 0, 0), 'Longitude')
  testthat::expect_error(seabiRds::getColDist(c(0, 0), c(0, 100), 0, 0), 'Latitude')
  testthat::expect_error(seabiRds::getColDist(c(0, 0), c(0, -120), 0, 0), 'Latitude') # used to pass
  testthat::expect_error(seabiRds::getColDist(c(0, 0), c(0, 0), 200, 0), 'colonyLon')
  testthat::expect_error(seabiRds::getColDist(c(0, 0), c(0, 0), 0, 95), 'colonyLat')
  testthat::expect_error(seabiRds::getColDist(c(0, 0), c(0, 0), c(0, 1), 0), 'length of 1')
})
