# ---------------------------------------------------------------------------------------------------------------
#' Check that deployment IDs and raw data files match
#'
#' Compares the dep_id values in a deployment table with the names of the raw data files. It reports
#' deployments that have no file, files that belong to no deployment, and other problems that would stop
#' the importers from finding the right data. It also counts the files found for each deployment.
#'
#' @param deployments Data frame with one row per deployment and a dep_id column. This can be the deployment
#' sheet read with read.csv() or the output of formatDeployments().
#' @param files Either a vector of file paths, as you would give to an importer, or the path to a single folder
#' that contains the raw files.
#' @param pattern Regular expression for the file type the importers expect, default is .csv files.
#' Files that do not match are reported.
#' @param recursive If files is a folder, should sub-folders be searched too?
#' @param quiet If TRUE no report is printed and only the result is returned.
#'
#' @details
#' A file is matched to a deployment when the dep_id appears anywhere in the file path. This is the rule used by
#' axytrek_to_dataset(), cattrack_to_dataset() and lat2800_to_dataset() to find the files for each deployment.
#' Deployments that produced no data have no file, so they are listed as deployments without files and ok stays FALSE.
#' The report reminds you to confirm that these deployments really did not produce data.
#' A deployment can have more than one file (e.g. Axy-trek files that end in _S1, _S2 and so on). There is no
#' expected number of files, so the count in files_per_deployment is for you to review.
#'
#' Use this check for loggers where each deployment generates its own file (e.g. Axy-trek or Cattrack), and pass only the
#' rows of the deployments for those loggers. It does not apply to Ecotone files, which are matched by the logger ID
#' inside the data and not by file name. Deployments with Ecotone loggers would be reported as having no file.
#'
#' Hidden files and the system files Thumbs.db and desktop.ini are ignored when a folder is given.
#'
#' @return Invisibly, a list with
#' \itemize{
#'   \item ok: TRUE if no problems were found.
#'   \item matched: data frame with the file name and dep_id of every match.
#'   \item files_per_deployment: data frame with every dep_id and the number of files (n_files) that match it.
#'   \item deployments_without_files: dep_id values that no file matches.
#'   \item files_without_deployments: files that match no dep_id.
#'   \item ambiguous_files: data frame of files that match more than one dep_id (for example A21_1 and A21_10).
#'   \item other_files: files that do not match pattern.
#'   \item duplicated_dep_ids: dep_id values that are used by more than one row.
#'   \item missing_dep_ids: number of rows with a blank dep_id.
#' }
#'
#' @examples
#' # Three deployments. The folder has three files for two of them (one deployment has two
#' # files, _S1 and _S2), plus one file that belongs to no deployment
#' deployments <- data.frame(dep_id = c('A12_123456789_20261014',
#'                                      'A13_987654321_20261015',
#'                                      'A14_111222333_20261016'))
#'
#' folder <- file.path(tempdir(), 'axytrek_example')
#' dir.create(folder, showWarnings = FALSE)
#' invisible(file.create(file.path(folder, c('A12_123456789_20261014_S1.csv',
#'                                          'A12_123456789_20261014_S2.csv',
#'                                          'A13_987654321_20261015_S1.csv',
#'                                          'A99_555666777_20261017_S1.csv'))))
#'
#' res <- check_deployment_files(deployments, folder)
#'
#' # The result can be used in code
#' res$ok
#' res$files_per_deployment
#' res$deployments_without_files
#' res$files_without_deployments
#'
#' unlink(folder, recursive = TRUE)
#'
#' @export

check_deployment_files <- function(deployments, files, pattern = '\\.csv$', recursive = FALSE, quiet = FALSE) {

  if (!is.data.frame(deployments) || !('dep_id' %in% names(deployments))) {
    stop('deployments must be a data frame with a dep_id column', call. = FALSE)
  }

  # a single folder can be given instead of a list of files
  if (length(files) == 1 && dir.exists(files)) {
    files <- list.files(files, full.names = TRUE, recursive = recursive)
  }

  files <- as.character(files)
  files <- files[!dir.exists(files)]
  files <- files[!grepl('^(thumbs\\.db|desktop\\.ini)$', basename(files), ignore.case = TRUE)]

  # deployment ids
  dep_ids <- as.character(deployments$dep_id)
  blank <- is.na(dep_ids) | trimws(dep_ids) == ''
  missing_dep_ids <- sum(blank)
  duplicated_dep_ids <- unique(dep_ids[!blank & duplicated(dep_ids)])
  dep_ids <- unique(dep_ids[!blank])

  # files that are not the type the importers expect
  expected_type <- grepl(pattern, basename(files), ignore.case = TRUE)
  other_files <- basename(files[!expected_type])
  files <- files[expected_type]

  # which dep_ids appear in each file path (same rule as the importers)
  hits <- lapply(files, function(f) dep_ids[vapply(dep_ids, grepl, logical(1), x = f, fixed = TRUE)])
  n_hits <- lengths(hits)

  matched <- data.frame(
    file = rep(basename(files), times = n_hits),
    dep_id = as.character(unlist(hits)),
    stringsAsFactors = FALSE
  )

  files_per_deployment <- data.frame(
    dep_id = dep_ids,
    n_files = vapply(dep_ids, function(d) sum(matched$dep_id == d), numeric(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )

  files_without_deployments <- basename(files[n_hits == 0])
  ambiguous_files <- matched[matched$file %in% basename(files[n_hits > 1]), , drop = FALSE]
  deployments_without_files <- files_per_deployment$dep_id[files_per_deployment$n_files == 0]

  ok <- length(deployments_without_files) == 0 &
    length(files_without_deployments) == 0 &
    nrow(ambiguous_files) == 0 &
    length(other_files) == 0 &
    length(duplicated_dep_ids) == 0 &
    missing_dep_ids == 0

  if (!quiet) {
    .report_section <- function(title, x) {
      if (length(x) > 0) message(title, '\n', paste0('  ', x, collapse = '\n'))
    }

    message('Checked ', length(dep_ids), ' deployments and ', length(files) + length(other_files), ' files')
    .report_section('-- Deployments with no matching file --', deployments_without_files)
    if (length(deployments_without_files) > 0) {
      message('  Note: a deployment that produced no data will not have a file. Please confirm, using the field notes,\n',
              '  that each of these deployments produced no data.')
    }
    .report_section('-- Files with no matching deployment --', files_without_deployments)
    if (nrow(ambiguous_files) > 0) {
      .report_section('-- Files that match more than one dep_id --',
                      paste0(ambiguous_files$file, ' matches ', ambiguous_files$dep_id))
    }
    .report_section('-- Files that are not the expected file type --', other_files)
    .report_section('-- dep_id values used by more than one row --', duplicated_dep_ids)
    if (missing_dep_ids > 0) message('-- ', missing_dep_ids, ' row(s) have a blank dep_id --')

    with_files <- files_per_deployment$n_files[files_per_deployment$n_files > 0]
    if (length(with_files) > 0) {
      message('Files per deployment: ', min(with_files), ' to ', max(with_files),
              ' (see files_per_deployment in the result)')
    }
    if (ok) message('No problems found: every deployment has a file and every file has a deployment.')
  }

  invisible(list(
    ok = ok,
    matched = matched,
    files_per_deployment = files_per_deployment,
    deployments_without_files = deployments_without_files,
    files_without_deployments = files_without_deployments,
    ambiguous_files = ambiguous_files,
    other_files = other_files,
    duplicated_dep_ids = duplicated_dep_ids,
    missing_dep_ids = missing_dep_ids
  ))
}
