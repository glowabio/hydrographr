# Get Hydrography90m regional unit IDs

Given the coordinates of input points (in WGS84), the function
identifies the IDs of the regional units of the Hydrography90m in which
the points are located. Input is a point data frame. The regional units
refer to non-interrupted basins (as opposed to the 20°x20° tiles). These
IDs can then be used to download the Hydrography90m regional unit raster
mask(s) using [`download_tiles()`](download_tiles.md).

## Usage

``` r
get_regional_unit_id(data, lon, lat, quiet = TRUE, tempdir = NULL)
```

## Arguments

- data:

  a data.frame or data.table that contains the columns regarding the
  longitude / latitude coordinates in WGS84.

- lon:

  character. The name of the column with the longitude coordinates.

- lat:

  character. The name of the column with the latitude coordinates.

- quiet:

  logical. If FALSE, the standard output will be printed. Default is
  TRUE.

- tempdir:

  character. The directory to be used as temp folder. Optional.

## References

<https://gdal.org/programs/gdallocationinfo.html>

## Author

Afroditi Grigoropoulou

## Examples

``` r
# Download test data into the temporary R folder
# or define a different directory
my_directory <- tempdir()
download_test_data(my_directory)

# Read the species data
species_occurrence <- read.table(paste0(my_directory,
                                       "/hydrography90m_test_data",
                                       "/spdata_1264942.txt"),
                              header = TRUE)

# Get the regional unit ID
get_regional_unit_id(species_occurrence, lon = "longitude",
                    lat = "latitude")
```
