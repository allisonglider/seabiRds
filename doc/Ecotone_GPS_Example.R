## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------

library(seabiRds)

# Get path to the example deployment data
fn <- system.file("extdata/ecotone", "ecotone_ex.csv", package = "seabiRds")

# Read in data from this file
raw_dep <- read.csv(fn, stringsAsFactors = F)

# Look at head of the data
head(raw_dep)


## ------------------------------------------------------------------------

# Create a field called dep_id, where all values are NA
raw_dep$dep_id <- NA


## ------------------------------------------------------------------------

# Run formatDeployments
dep_data <- formatDeployments(
  deployments = raw_dep,
  dateFormat = "%Y-%m-%d %H:%M",
  dep_tz = "US/Eastern",
  species = 'Species',
  metal_band = 'Band_num',
  colour_band = 'Colour_band',
  dep_id = 'dep_id',
  fill_dep_id = T,
  site = 'Colony',
  dep_lon = 'Dep_lon',
  dep_lat = 'Dep_lat',
  time_released = 'Time_on',
  time_recaptured = 'Time_off',
  status_on = 'Stage_on',
  status_off = 'Stage_off',
  mass_on = 'Mass_on',
  mass_off = 'Mass_off',
  gps_id = 'GPS',
  tdr_id = 'TDR'
)


## ------------------------------------------------------------------------

# Look at the results
head(dep_data)


## ------------------------------------------------------------------------

# Get the path to the example data stored within this package
folder <- paste0(find.package('seabiRds'), "/extdata/ecotone/gps_data")

# Run the readGPSData function for Ecotone data
ecotone_data <- readGPSData(
  inputFolder = folder, # path to the colder storing the raw GPS files
  deployments = dep_data, # formatted deployment data
  tagType = "Ecotone"
)

# Look at the output
head(ecotone_data)


## ------------------------------------------------------------------------

ecotone_data <- cleanGPSData(data = ecotone_data,
                            deployments = dep_data,
                            speedThreshold = 150,
                            plot =T)



