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

testthat::test_that("axytrek_to_dataset skips TDR with a warning for files that have no TDR data", {

  testthat::skip_if_not_installed('arrow')

  example_files <- list.files(example_dir(), pattern = '_S[0-9]+\\.csv$', full.names = TRUE)
  testthat::skip_if(length(example_files) == 0, 'example Axy-trek data not found')

  # a copy of the example file without the TDR columns, as if the TDR had been turned off
  raw <- utils::read.csv(example_files[1], stringsAsFactors = FALSE)
  raw <- raw[, !grepl('^(Temp|Depth|Activity)', names(raw))]

  data_dir <- tempfile('axy_no_tdr_')
  dir.create(data_dir)
  no_tdr_file <- file.path(data_dir, basename(example_files[1]))
  utils::write.csv(raw, no_tdr_file, row.names = FALSE, na = '')

  deployments <- utils::read.csv(file.path(example_dir(), 'axytrek_ex.csv'))
  dep <- seabiRds::formatDeployments(deployments, dep_tz = 'Canada/Eastern')

  warns <- character(0)
  msgs <- character(0)
  out <- tempfile('axy_no_tdr_out_')

  withCallingHandlers(
    utils::capture.output(seabiRds::axytrek_to_dataset(no_tdr_file, dep, out, plot = FALSE)),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart('muffleWarning') },
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart('muffleMessage') }
  )

  # the warning names the file, and the summary names the deployment
  testthat::expect_true(any(grepl('no TDR data', warns)))
  testthat::expect_true(any(grepl(basename(no_tdr_file), warns, fixed = TRUE)))
  testthat::expect_true(any(grepl('TDR data skipped for 1 of 1', msgs)))
  testthat::expect_true(any(grepl('AA51_99699520_20240807', msgs, fixed = TRUE)))

  # ACC and GPS are still imported, TDR is not
  testthat::expect_true(dir.exists(file.path(out, 'acc')))
  testthat::expect_true(dir.exists(file.path(out, 'gps')))
  testthat::expect_false(dir.exists(file.path(out, 'tdr')))

  # turning TDR off in the call gives no warning and no summary
  warns <- character(0)
  msgs <- character(0)
  out2 <- tempfile('axy_no_tdr_out_')

  withCallingHandlers(
    utils::capture.output(seabiRds::axytrek_to_dataset(no_tdr_file, dep, out2, tdr = FALSE, plot = FALSE)),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart('muffleWarning') },
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart('muffleMessage') }
  )

  testthat::expect_equal(length(warns), 0)
  testthat::expect_false(any(grepl('TDR data skipped', msgs)))

  unlink(c(data_dir, out, out2), recursive = TRUE)
})
