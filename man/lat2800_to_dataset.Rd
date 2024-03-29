% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LAT2800_to_Arrow.R
\name{lat2800_to_dataset}
\alias{lat2800_to_dataset}
\title{Convert Lat2800 biologger data to Arrow dataset}
\usage{
lat2800_to_dataset(
  files,
  deployments,
  output_dataset,
  date_format = "\%Y-\%m-\%d \%H:\%M:\%OS",
  plot = T,
  max_rows,
  timezone = "UTC"
)
}
\arguments{
\item{files}{List of Lat2800 .csv files, data must have file names that are a partial match to the dep_id in deployment data}

\item{deployments}{Deployment data formatted using seabiRds::formatDeployments()}

\item{output_dataset}{Path where arrow dataset should be saved}

\item{date_format}{The POSIXct date format used in .csv files}

\item{plot}{Should plot of data be generated, TRUE or FALSE?}

\item{max_rows}{Maximum rows to read in, LAT2800 units sometimes fill memory with rows of invalid data that are too long to read in}

\item{timezone}{Timezone of raw data, default is UTC}
}
\description{
Reads Lat2800 biologger data and parses it into three arrow datasets for  TDR data
}
