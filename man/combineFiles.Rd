% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/General_Functions.R
\name{combineFiles}
\alias{combineFiles}
\title{Combines files from a folder into a single dataframe}
\usage{
combineFiles(
  files,
  pattern = NULL,
  type = "csv",
  sep = NULL,
  stringsAsFactors = F,
  header = T,
  skip = 0,
  combineColumns = F
)
}
\arguments{
\item{files}{a list  of file paths to the files to be combined.}

\item{pattern}{Character string of pattern in file name to select files.}

\item{type}{File type suffix, supported options are "txt" and "csv".}

\item{sep}{File delimiter, if required.}

\item{stringsAsFactors}{True or False if strings should be read as factors, defaults to F}

\item{header}{Should first row be read as file header.}

\item{skip}{Rows at the start of the file to skip.}

\item{combineColumns}{Change to TRUE if you want to merge files by columns, default is FALSE which binds files by rows.}
}
\value{
Dataframe with all files combined.
}
\description{
Combines files from a folder into a single dataframe
}
