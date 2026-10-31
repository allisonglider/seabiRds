testthat::test_that("importers stop clearly when there are no deployments", {
  testthat::skip_if_not_installed('arrow')

  empty <- data.frame(dep_id = character(0))

  testthat::expect_error(
    seabiRds::lat2800_to_dataset(files = character(0), deployments = empty,
                                 output_dataset = tempfile(), date_format = '%d/%m/%Y',
                                 max_rows = 10),
    'no rows'
  )
})
