# End-to-end test of axytrek_to_dataset() using the example data in inst/extdata/axytrek.
# The example is a trimmed 1 hour deployment, built by data-raw/axytrek_sample.R.

example_dir <- function() system.file('extdata', 'axytrek', package = 'seabiRds')

testthat::test_that("axytrek_to_dataset imports GPS, TDR and ACC data from the example deployment", {

  testthat::skip_if_not_installed('arrow')

  data_files <- list.files(example_dir(), pattern = '_S[0-9]+\\.csv$', full.names = TRUE)
  testthat::skip_if(length(data_files) == 0, 'example Axy-trek data not found')

  deployments <- utils::read.csv(file.path(example_dir(), 'axytrek_ex.csv'))
  dep <- seabiRds::formatDeployments(deployments, dep_tz = 'Canada/Eastern')

  testthat::expect_equal(dep$dep_id, 'AA51_99699520_20240807')

  out <- tempfile('axy_example_')
  invisible(utils::capture.output(
    seabiRds::axytrek_to_dataset(data_files, dep, out, plot = FALSE)
  ))

  for (type in c('acc', 'tdr', 'gps')) {

    testthat::expect_true(dir.exists(file.path(out, type)), info = type)

    res <- dplyr::collect(arrow::open_dataset(file.path(out, type)))

    testthat::expect_gt(nrow(res), 0)
    testthat::expect_true(all(res$dep_id == 'AA51_99699520_20240807'), info = type)

    # all of the data are kept, and the deployment period is flagged
    testthat::expect_equal(sort(unique(as.numeric(res$deployed))), c(0, 1), info = type)

    deployed_times <- as.numeric(res$time[as.numeric(res$deployed) == 1])
    testthat::expect_gte(min(deployed_times), as.numeric(dep$time_released), label = paste(type, 'first deployed time'))
    testthat::expect_lte(max(deployed_times), as.numeric(dep$time_recaptured), label = paste(type, 'last deployed time'))
  }

  unlink(out, recursive = TRUE)
})
