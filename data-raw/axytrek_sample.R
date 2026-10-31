# data-raw/axytrek_sample.R
#
# Builds a small example file from one full raw Axy-trek export (about 0.5 GB), for use in
# inst/extdata, the vignette and the tests. Base R only. The big file is read in chunks, so it
# is never loaded into memory all at once.
#
# The sample keeps a short window of time around the deployment, and within that window keeps every
# `thin`-th row. The original lines are copied as they are, so the format of the file is unchanged.
#
# Note: the kept rows are no longer at the original sampling rate, so the sample is for testing the
# import and for showing the workflow. Do not use it for analyses that depend on the accelerometer
# sampling frequency (e.g. getPeakFrequency(), dynamic body acceleration, pitch).

input_file  <- 'E:/Biologgers/CGM/TBMU/2024/technosmart/AA51_99699520_20240807_S1.csv'
output_file <- 'inst/extdata/axytrek/AA51_99699520_20240807_S1.csv'

# Window of data to keep, written in the same clock as the Timestamp column in the file.
# The importers assume the Timestamp column is UTC. If it is, a release at 15:41 local time in
# eastern daylight time (UTC-4) is 19:41 here. The window starts about 20 minutes before the 2 hour
# deployment and ends about 15 minutes after it.
window_start <- '2024-08-07 19:21:00'
window_end   <- '2024-08-07 20:56:00'
timestamp_format <- '%Y-%m-%d %H:%M:%OS'
timestamp_tz     <- 'UTC'

thin       <- 10       # keep every 10th row within the window
keep_gps   <- TRUE     # also keep every row that has a GPS fix, so GPS locations are not lost to thinning
chunk_size <- 200000   # rows read at a time

# ---------------------------------------------------------------------------------------------------------------

start_time <- as.POSIXct(window_start, format = '%Y-%m-%d %H:%M:%S', tz = timestamp_tz)
end_time   <- as.POSIXct(window_end,   format = '%Y-%m-%d %H:%M:%S', tz = timestamp_tz)
if (is.na(start_time) | is.na(end_time)) stop('window_start and window_end must look like 2024-08-07 19:21:00')

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

con_in  <- file(input_file, 'r')
con_out <- file(output_file, 'w')

header <- readLines(con_in, n = 1)
writeLines(header, con_out)

rows_in_window <- 0   # rows seen inside the window, used to pick every `thin`-th row
n_kept  <- 0
n_gps   <- 0
n_depth <- 0
first_time <- NA
last_time  <- NA
first_gps_time <- NA
last_gps_time  <- NA

repeat {

  lines <- readLines(con_in, n = chunk_size)
  if (length(lines) == 0) break

  chunk <- utils::read.csv(text = c(header, lines), stringsAsFactors = FALSE)
  time  <- as.POSIXct(chunk$Timestamp, format = timestamp_format, tz = timestamp_tz)

  if (all(is.na(time))) {
    stop('The Timestamp column could not be read with timestamp_format = ', timestamp_format,
         '. Example value: ', chunk$Timestamp[1])
  }

  # the file is in time order, so stop once the window has been passed
  past_window <- all(time > end_time, na.rm = TRUE)
  if (past_window) break

  in_window <- !is.na(time) & time >= start_time & time <= end_time
  if (!any(in_window)) next

  lines   <- lines[in_window]
  chunk   <- chunk[in_window, , drop = FALSE]
  time    <- time[in_window]
  has_gps <- !is.na(chunk$location.lat)

  position <- rows_in_window + seq_along(lines) - 1
  rows_in_window <- rows_in_window + length(lines)

  keep <- position %% thin == 0
  if (keep_gps) keep <- keep | has_gps

  writeLines(lines[keep], con_out)

  # numbers for the summary
  if (any(keep)) {
    if (is.na(first_time)) first_time <- format(time[keep][1])
    last_time <- format(time[keep][sum(keep)])
  }
  if (any(keep & has_gps)) {
    if (is.na(first_gps_time)) first_gps_time <- format(time[keep & has_gps][1])
    last_gps_time <- format(time[keep & has_gps][sum(keep & has_gps)])
  }
  n_kept <- n_kept + sum(keep)
  n_gps  <- n_gps + sum(keep & has_gps)
  if ('Depth' %in% names(chunk)) n_depth <- n_depth + sum(!is.na(chunk$Depth[keep]))
}

close(con_in)
close(con_out)

if (n_kept == 0) warning('No rows were found between window_start and window_end. Check the times and the clock used in the file.')

cat('Rows in the window:', rows_in_window, '\n')
cat('Rows kept:', n_kept, '\n')
cat('  with a GPS fix:', n_gps, '\n')
cat('  with a depth value:', n_depth, '\n')
cat('First and last kept time:', first_time, 'to', last_time, '\n')
cat('First and last GPS fix:', first_gps_time, 'to', last_gps_time, '\n')
cat('Output file:', output_file, '\n')
cat('Output size (MB):', round(file.size(output_file) / 1e6, 2), '\n')

