# Column names used inside dplyr pipelines and plotting code (non-standard evaluation).
# Listing them here stops R CMD check from reporting them as 'no visible binding for global variable'.
# Only add names of data columns here, not functions or objects.
utils::globalVariables(c(
  'Activity', 'Altitude', 'Date', 'Day', 'Depth', 'Div.down', 'Div.up', 'Diving.duration',
  'HDOP', 'Hour', 'In.range', 'Latitude', 'Logger.ID', 'Longitude', 'Minute', 'Month',
  'Sat..Count', 'Second', 'Temp_sens', 'Time', 'Timestamp', 'WetDryState', 'X', 'Y', 'Year', 'Z',
  'altitude_m', 'coldist', 'dep_id', 'deployed', 'depth', 'depth_m', 'dive_duration_s',
  'dive_end', 'dive_start', 'gps_id', 'hdop', 'height.msl', 'inrange', 'lat', 'location.lat',
  'location.lon', 'lon', 'max_depth_m', 'metal_band', 'pressure', 'satellites', 'site',
  'species', 'subsite', 'temperature_c', 'time', 'time_released', 'wet', 'x', 'y', 'year', 'z'
))
