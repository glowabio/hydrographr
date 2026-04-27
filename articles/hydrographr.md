# Getting started with hydrographr

One of the major advantages of the package is that it can handle large
geodata efficiently using a suite of Open Source geoprocessing tools
like `GRASS GIS`, `GDAL`, and `pktools`. With the`hydrographr` package
large geodata is not loaded into R. Instead the files get processed in
the background using `GRASS GIS`, `GDAL`, and `pktools` and only the
output gets read into R.

Since the `hydrographr` package is optimized to run in a Linux operating
system, in a Windows environment some additional software needs to be
installed (see [Windows system
setup](https://glowabio.github.io/hydrographr/articles/windows_system_setup.html)).

## System requirements

To work smoothly with the `hydrographr` package, `GRASS GIS`, `GDAL`,
and `pktools` need to be installed.

Here you can find the installation guideline for your operating systems:

[Linux](https://glowabio.github.io/hydrographr/articles/linux_system_setup.html)

[Windows](https://glowabio.github.io/hydrographr/articles/windows_system_setup.html)

[macOS](https://glowabio.github.io/hydrographr/articles/macos_system_setup.html)

## Loading `hydrographr`

You can install `hydrographr` from its GitHub repository. If you did not
install the `R` package yet, install it with
[`remotes::install_github()`](https://remotes.r-lib.org/reference/install_github.html).

``` r

# If the package remotes is not installed run first:
install.packages("remotes")

remotes::install_github("glowabio/hydrographr")
```

Before we start exploring the package load `hydrographr`.

``` r

library(hydrographr)
#> Warning: replacing previous import 'dplyr::as_data_frame' by
#> 'igraph::as_data_frame' when loading 'hydrographr'
```
