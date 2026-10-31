# seabiRds 0.2.0

This version adds a documented workflow for importing biologger data into the lab's parquet datasets. Existing
exported functions keep their arguments, and the old reader and cleaning functions are still available.

## New

* New vignette, `vignette('Managing-biologging-data')`, which covers recording deployment metadata, organizing biologger
  files, checking the records in R, and importing the data with the `*_to_dataset()` functions. It replaces the
  Ecotone and Technosmart GPS example vignettes.
* New deployment template, `deployments_template.xlsx`, installed in `extdata`. Use
  `system.file('extdata', 'deployments_template.xlsx', package = 'seabiRds')` to find it.
* New `check_deployment_files()` compares the `dep_id` values in a deployment sheet with the names of the biologger
  files, and reports deployments without files, files without deployments, files that match more than one `dep_id`,
  and the number of files for each deployment.
* New example Axy-trek deployment and data in `extdata/axytrek`, used by the vignette and the tests.
* New tests (testthat) for the importers, the plotting helpers, `formatDeployments()`, `check_deployment_files()`
  and `mapTracks()`.

## Changes to existing functions

* `formatDeployments()` finds the standard column names on its own, so the time zone (`dep_tz`) is the only argument
  needed when the sheet follows the template. Missing required columns give an error that names them. The help
  page now describes how it is used by the `*_to_dataset()` functions and has an example.
* The `*_to_dataset()` importers stop with a clear message if arrow is not installed, if the files are not csv files,
  or if `deployments` has no rows, and they warn when a file matches more than one `dep_id` (for example when one
  `dep_id` is contained within another).
* The importers keep all of the data and flag the deployment period with the `deployed` column. The review plots
  (`plot = TRUE`) show deployed data in black and undeployed data in red, and they no longer pause for input.
  The Axy-trek GPS plot now includes a map, like the Cattrack and Ecotone plots.
* `axytrek_to_dataset()` gives a warning and skips the TDR data for a deployment when the file has no TDR columns,
  and lists those deployments at the end. The other data types are still imported.
* `checkAxes()` (used by `fix_axes = TRUE` in `axytrek_to_dataset()`) now warns, and does not flip the axes, when
  it is asked to prompt in a session that is not interactive. Use `force_axes = TRUE` to flip the axes without
  asking.
* `mapTracks()` works with data from the standardized gps dataset, which has no `coldist` column. It has a new
  `deployed_only` argument (default `FALSE`) to map only deployed locations, puts locations in time order for
  each deployment, uses ggplot2's default axis breaks, and no longer uses the deprecated `aes_string()`.

## Legacy functions

* `readGPSData()`, `cleanGPSData()`, `readTDRData()` and `cleanTDRData()` are now marked as legacy in their help pages.
  They have been superseded by the `*_to_dataset()` functions and are kept for backward compatibility.
* `.cleanGPSDataPlot()` and `.matchFiles()` are no longer exported. They were internal helpers.
