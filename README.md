
<!-- README.md is generated from README.Rmd. Please edit that file -->

# seabiRds

<!-- badges: start -->
<!-- badges: end -->

Tools to analyse biologger data collected in the Arctic Ecology Lab.
Primarily for GPS and accelerometer data, but maybe also some
geolocators and heart rate loggers. Please add an issue to the GitHub
issues page for the package if you encounter any problems, would like me
to add something, or want to make a contribution to the package.

## Installation

You can install this package from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("allisonglider/seabiRds")
```

To install the package with the vignette, which contains helpful
examples, use:

``` r
# install.packages("devtools")
devtools::install_github("allisonglider/seabiRds", build_vignettes = TRUE)
```

Importing biologger data into a dataset also needs the arrow package,
which is not installed automatically:

``` r
install.packages("arrow")
```

## Vignette

The vignette *Managing biologging data* is the best place to start. It
shows how to record deployment metadata in the standard template,
organize the files from your biologgers, check the records in R, and
import the data into the lab’s biologging database.

``` r
library(seabiRds)

vignette(topic = 'Managing-biologging-data', package = 'seabiRds')
```
