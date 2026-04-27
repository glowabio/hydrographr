# Get the Hydrography90m 20°x20° tile ID

Identifies the 20°x20° tile IDs of the Hydrography90m data in which the
input points are located. The IDs can then be used to download the data
using [`download_tiles()`](download_tiles.md). The input is a data frame
with point coordinates. For orientation, please also see the tiles at
the <https://hydrography.org/hydrography90m/hydrography90m_layers>

## Usage

``` r
get_tile_id(data, lon, lat, tempdir = NULL)
```

## Arguments

- data:

  a data.frame or data.table that contains the columns regarding the
  longitude / latitude coordinates in WGS84.

- lon:

  character. The name of the column with the longitude coordinates.

- lat:

  character. The name of the column with the latitude coordinates.

- tempdir:

  character. The directory to be used as temp folder. Optional.

## Author

Afroditi Grigoropoulou

## Examples

``` r
# Download test data into the temporary R folder
# or define a different directory
my_directory <- tempdir()
download_test_data(my_directory)

# Load species occurrence data
species_occurrence <- read.table(paste0(my_directory,
                                       "/hydrography90m_test_data",
                                       "/spdata_1264942.txt"),
                                 header = TRUE)

# Get the tile ID
get_tile_id(data = species_occurrence,
            lon = "longitude", lat = "latitude")
```
