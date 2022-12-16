# hydrographr <img src="man/figures/hydrographr.svg" align="right" />

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

`hydrographr` provides a collection of `R` function wrappers for GDAL and GRASS-GIS functions to efficiently work with [Hydrography90m](https://essd.copernicus.org/articles/14/4525/2022/essd-14-4525-2022.html) and spatial biodiversity data. The easy-to-use functions process large raster and vector data directly on disk in parallel, such that the memory of R does not get overloaded. This allows creating scalable data processing and analysis workflows in R, even though the data is not processed directly in R.

We will add more functions and a vignette over time, and we invite users to test the package and report issues as well as feature requests.

Installation
-----------
Please see the installation guide of the required tools at https://glowabio.github.io/hydrographr/articles/hydrographr.html. Afterwards, use the following lines to install the package in R:

```{r}
install.packages("remotes")
remotes::install_github("glowabio/hydrographr")
library(hydrographr)
```


We thank [NFDI4Biodiversity](https://www.nfdi4biodiversity.org/en/) and [NFDI4Earth](https://www.nfdi4earth.de/) for providing the funding that helped us getting the hydrographr package together!

<img src="man/figures/nfdi_logos.png" align="left" />
