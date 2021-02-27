
<!-- README.md is generated from README.Rmd. Please edit that file -->
seabiRds
========

<!-- badges: start -->
<!-- badges: end -->
Tools to analyse biologger data collected in the Arctic Ecology Lab. Primarily for GPS and accelerometer data, but maybe also some geolocators and heart rate loggers. PLease add an issue to the GitHub issues page for the package if you encounter any problems, would like me to add something, or want to make a contribution to the package.

Installation
------------

You can install this package from github with [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("allisonglider/seabiRds")
```

To install the package with vignettes containing helpful examples use:

``` r
# install.packages("devtools")
devtools::install_github("allisonglider/seabiRds", build_vignettes = T)
```

Vignettes
---------

This will take you to an example of how to process data from Ecotone GPS devices:

``` r
library(seabiRds)

vignette(topic = 'Ecotone-GPS-Example', package = 'seabiRds')
```

This will take you to an example of how to process data from Technosmart GPS devices:

``` r
library(seabiRds)

vignette(topic = 'Technosmart-GPS-Example', package = 'seabiRds')
```
