# Tests for check_deployment_files() in R/Check_Deployments.R

# makes a folder of empty files with the given names and returns its path
make_folder <- function(file_names) {
  folder <- tempfile('raw_files_')
  dir.create(folder)
  invisible(file.create(file.path(folder, file_names)))
  folder
}

deps <- data.frame(dep_id = c('A12_123456789_20261014', 'A13_987654321_20261015'))

testthat::test_that("check_deployment_files passes when every file and deployment match", {

  folder <- make_folder(c('A12_123456789_20261014_S1.csv', 'A13_987654321_20261015_S1.csv'))
  res <- seabiRds::check_deployment_files(deps, folder, quiet = TRUE)

  testthat::expect_true(res$ok)
  testthat::expect_equal(nrow(res$matched), 2)
  testthat::expect_equal(length(res$deployments_without_files), 0)
  testthat::expect_equal(length(res$files_without_deployments), 0)
})

testthat::test_that("check_deployment_files accepts a vector of file paths", {

  folder <- make_folder(c('A12_123456789_20261014_S1.csv', 'A13_987654321_20261015_S1.csv'))
  files <- list.files(folder, full.names = TRUE)
  res <- seabiRds::check_deployment_files(deps, files, quiet = TRUE)

  testthat::expect_true(res$ok)
})

testthat::test_that("check_deployment_files allows several files per deployment", {

  folder <- make_folder(c('A12_123456789_20261014_S1.csv', 'A12_123456789_20261014_S2.csv',
                          'A13_987654321_20261015_S1.csv'))
  res <- seabiRds::check_deployment_files(deps, folder, quiet = TRUE)

  testthat::expect_true(res$ok)
  testthat::expect_equal(nrow(res$matched), 3)
  testthat::expect_equal(sum(res$matched$dep_id == 'A12_123456789_20261014'), 2)
})

testthat::test_that("check_deployment_files counts the files for each deployment", {

  dd <- data.frame(dep_id = c('A12_123456789_20261014', 'A13_987654321_20261015', 'A14_111222333_20261016'))
  folder <- make_folder(c('A12_123456789_20261014_S1.csv', 'A12_123456789_20261014_S2.csv',
                          'A12_123456789_20261014_S3.csv', 'A13_987654321_20261015_S1.csv'))
  res <- seabiRds::check_deployment_files(dd, folder, quiet = TRUE)

  counts <- res$files_per_deployment
  testthat::expect_equal(names(counts), c('dep_id', 'n_files'))
  testthat::expect_equal(counts$dep_id, dd$dep_id)
  testthat::expect_equal(counts$n_files, c(3, 1, 0))

  # a deployment with no file has a count of zero and is also reported as missing
  testthat::expect_equal(res$deployments_without_files, 'A14_111222333_20261016')
  testthat::expect_false(res$ok)
})

testthat::test_that("check_deployment_files counts each dep_id once when it is repeated in the table", {

  dd <- data.frame(dep_id = rep('A12_123456789_20261014', 2))
  folder <- make_folder(c('A12_123456789_20261014_S1.csv', 'A12_123456789_20261014_S2.csv'))
  res <- seabiRds::check_deployment_files(dd, folder, quiet = TRUE)

  testthat::expect_equal(nrow(res$files_per_deployment), 1)
  testthat::expect_equal(res$files_per_deployment$n_files, 2)
  testthat::expect_equal(res$duplicated_dep_ids, 'A12_123456789_20261014')
})

testthat::test_that("check_deployment_files finds deployments without files and files without deployments", {

  folder <- make_folder(c('A12_123456789_20261014_S1.csv', 'X99_555666777_20261017_S1.csv'))
  res <- seabiRds::check_deployment_files(deps, folder, quiet = TRUE)

  testthat::expect_false(res$ok)
  testthat::expect_equal(res$deployments_without_files, 'A13_987654321_20261015')
  testthat::expect_equal(res$files_without_deployments, 'X99_555666777_20261017_S1.csv')
})

testthat::test_that("check_deployment_files finds files that match more than one dep_id", {

  # A21_10_S1.csv contains both A21_1 and A21_10
  dd <- data.frame(dep_id = c('A21_1', 'A21_10'))
  folder <- make_folder(c('A21_1_S1.csv', 'A21_10_S1.csv'))
  res <- seabiRds::check_deployment_files(dd, folder, quiet = TRUE)

  testthat::expect_false(res$ok)
  testthat::expect_equal(unique(res$ambiguous_files$file), 'A21_10_S1.csv')
  testthat::expect_setequal(res$ambiguous_files$dep_id, c('A21_1', 'A21_10'))
})

testthat::test_that("check_deployment_files reports files that are not csv and ignores system files", {

  folder <- make_folder(c('A12_123456789_20261014_S1.csv', 'A13_987654321_20261015_S1.CSV',
                          'notes.txt', 'Thumbs.db'))
  res <- seabiRds::check_deployment_files(deps, folder, quiet = TRUE)

  # upper case .CSV is accepted, notes.txt is reported, Thumbs.db is ignored
  testthat::expect_equal(res$other_files, 'notes.txt')
  testthat::expect_equal(length(res$deployments_without_files), 0)
  testthat::expect_false(res$ok)
})

testthat::test_that("check_deployment_files reports repeated and blank dep_id values", {

  dd <- data.frame(dep_id = c('A12_123456789_20261014', 'A12_123456789_20261014', NA, ''))
  folder <- make_folder('A12_123456789_20261014_S1.csv')
  res <- seabiRds::check_deployment_files(dd, folder, quiet = TRUE)

  testthat::expect_false(res$ok)
  testthat::expect_equal(res$duplicated_dep_ids, 'A12_123456789_20261014')
  testthat::expect_equal(res$missing_dep_ids, 2)
})

testthat::test_that("check_deployment_files only searches sub-folders when asked to", {

  folder <- make_folder('A13_987654321_20261015_S1.csv')
  dir.create(file.path(folder, 'sub'))
  invisible(file.create(file.path(folder, 'sub', 'A12_123456789_20261014_S1.csv')))

  res <- seabiRds::check_deployment_files(deps, folder, quiet = TRUE)
  testthat::expect_equal(res$deployments_without_files, 'A12_123456789_20261014')

  res <- seabiRds::check_deployment_files(deps, folder, recursive = TRUE, quiet = TRUE)
  testthat::expect_true(res$ok)
})

testthat::test_that("check_deployment_files prints a report unless quiet", {

  folder <- make_folder('A12_123456789_20261014_S1.csv')

  testthat::expect_message(
    seabiRds::check_deployment_files(deps, folder),
    'Deployments with no matching file'
  )
  testthat::expect_message(
    seabiRds::check_deployment_files(deps, folder),
    'Files per deployment: 1 to 1'
  )
  testthat::expect_silent(seabiRds::check_deployment_files(deps, folder, quiet = TRUE))
})

testthat::test_that("check_deployment_files reminds you to confirm deployments with no data", {

  folder <- make_folder('A12_123456789_20261014_S1.csv')

  testthat::expect_message(
    seabiRds::check_deployment_files(deps, folder),
    'produced no data'
  )

  # no reminder when every deployment has a file, and none when quiet
  folder <- make_folder(c('A12_123456789_20261014_S1.csv', 'A13_987654321_20261015_S1.csv'))
  testthat::expect_message(
    seabiRds::check_deployment_files(deps, folder),
    'No problems found'
  )
  testthat::expect_silent(seabiRds::check_deployment_files(deps, folder, quiet = TRUE))
})

testthat::test_that("check_deployment_files only reports sections that have problems", {

  folder <- make_folder(c('A12_123456789_20261014_S1.csv', 'A13_987654321_20261015_S1.csv'))
  msgs <- testthat::capture_messages(seabiRds::check_deployment_files(deps, folder))

  testthat::expect_false(any(grepl('more than one', msgs)))
  testthat::expect_false(any(grepl('no matching', msgs)))
  testthat::expect_true(any(grepl('No problems found', msgs)))
})

testthat::test_that("check_deployment_files needs a dep_id column", {

  testthat::expect_error(
    seabiRds::check_deployment_files(data.frame(id = 'A12'), tempdir(), quiet = TRUE),
    'dep_id'
  )
  testthat::expect_error(seabiRds::check_deployment_files('A12', tempdir(), quiet = TRUE), 'data frame')
})
