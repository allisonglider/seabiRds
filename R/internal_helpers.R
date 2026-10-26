# -----
# Internal helpers (not exported, no help pages)

# Stop with a clear message if arrow is not installed (arrow is only needed to write datasets)
.check_arrow <- function() {

  if (!requireNamespace('arrow', quietly = TRUE)) {
    stop("The 'arrow' package is required to write datasets. Install it with install.packages('arrow').",
         call. = FALSE)
  }

  invisible(TRUE)
}

# -----
# Stop unless every path in `files` is a .csv file
.check_csv_files <- function(files) {

  if (!all(grepl('\\.csv$', files, ignore.case = TRUE))) {
    stop('All files must be .csv format', call. = FALSE)
  }

  invisible(TRUE)
}

# -----
# Deployment ids are matched to file names by looking for the dep_id anywhere in the file path.
# If one dep_id is contained within another (e.g. 'A21_1' and 'A21_10'), a file can match both
# deployments. Warn so the user can fix the ids; matching behaviour is not changed.

.warn_ambiguous_files <- function(files, deployment_ids) {

  deployment_ids <- unique(as.character(deployment_ids[!is.na(deployment_ids)]))
  files <- as.character(files)

  if (length(files) == 0 | length(deployment_ids) == 0) return(invisible(NULL))

  matches <- lapply(files, function(f) {
    deployment_ids[vapply(deployment_ids, grepl, logical(1), x = f, fixed = TRUE)]
  })

  multi <- which(lengths(matches) > 1)

  if (length(multi) > 0) {
    msg <- vapply(multi, function(k) {
      paste0('  ', basename(files[k]), ' matches: ', paste(matches[[k]], collapse = ', '))
    }, character(1))

    warning('Some files match more than one dep_id and will be used for each of them. ',
            'Check that no dep_id is contained within another dep_id:\n',
            paste(msg, collapse = '\n'), call. = FALSE)
  }

  invisible(NULL)
}
