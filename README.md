# hydrographr <img src="man/figures/hydrographr.svg" align="right" />

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

`hydrographr` provides a collection of `R` function wrappers for GDAL and GRASS-GIS functions to efficiently work with Hydrography90m spatial data. The package is currently under development. More and more useful functions will be made available over time here, and we invite users to test the functions and report issues as well as feature requests.

Installation
-----------
Please see the installation guide of the required tools at https://glowabio.github.io/hydrographr/articles/hydrographr.html. Afterwards, use the following lines to install the package in R:

```{r}
install.packages("remotes")
remotes::install_github("glowabio/hydrographr")
library(hydrographr)
```

