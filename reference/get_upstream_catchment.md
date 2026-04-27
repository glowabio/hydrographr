# Delineate the upstream catchment

Delineates the upstream catchment of a given point, where the point is
considered as the outlet of the catchment.

## Usage

``` r
get_upstream_catchment(
  data,
  id,
  lon,
  lat,
  direction_layer = NULL,
  out_dir = NULL,
  n_cores = NULL,
  compression = "low",
  bigtiff = TRUE,
  quiet = TRUE
)
```

## Arguments

- data:

  a data.frame or data.table that contains the columns regarding the
  longitude / latitude coordinates in WGS84. Note that the points need
  to be snapped to the stream network with
  [`snap_to_network()`](snap_to_network.md) or
  [`snap_to_subc_segment()`](snap_to_subc_segment.md).

- id:

  character. The name of a column containing unique IDs for each row of
  "data" (e.g., occurrence or site IDs). The unique IDs need to be
  numeric and less than 10 characters long.

- lon:

  character. The name of the column with the longitude coordinates.

- lat:

  character. The name of the column with the latitude coordinates.

- direction_layer:

  character. Full path to the flow direction raster file.

- out_dir:

  Full path to the directory where the output(s) will be stored. The
  output file name includes the "id" which helps identifying the
  upstream corresponding catchment.

- n_cores:

  numeric. Number of cores used for parallelisation. If NULL, available
  cores - 1 will be used. Default is NULL.

- compression:

  character. Compression of the written output file. Compression levels
  can be defined as "none", "low", or "high". Default is "low",
  referring to compression type "DEFLATE" and compression level 2.
  "high" refers to compression level 9.

- bigtiff:

  logical. Define whether the output file is expected to be a BIGTIFF
  (file size larger than 4 GB). If FALSE and size \> 4GB no file will be
  written. Default is TRUE.

- quiet:

  logical. If FALSE, the standard output will be printed. Default is
  TRUE.

## References

- <https://grass.osgeo.org/grass82/manuals/r.water.outlet.html>

- <https://grass.osgeo.org/grass82/manuals/r.region.html>

## See also

- [`snap_to_network`](snap_to_network.md) to snap the data points to the
  next stream segment within a given radius and/or a given flow
  accumulation threshold value.

- [`snap_to_subc_segment`](snap_to_subc_segment.md) to snap the data
  points to the next stream segment within the sub-catchment the point
  is located.

- [`extract_ids`](extract_ids.md) to extract basin and sub-catchment
  IDs.

## Author

Jaime Garcia Marquez, Afroditi Grigoropoulou, Marlene Schürz

## Examples

``` r
# Download test data into temporary R folder
# or define a different directory
my_directory <- tempdir()
download_test_data(my_directory)

# Before running the function get_upstream_catchment(), snap the points to
# to the stream segment. There are multiple ways to snap the points. Here is
# one example:

# Load occurrence data
species_occurrence <- read.table(paste0(my_directory,
                                       "/hydrography90m_test_data",
                                       "/spdata_1264942.txt"),
                              header = TRUE)

# Define full path to the basin and sub-catchments raster layer
basin_raster <- paste0(my_directory,
                       "/hydrography90m_test_data/basin_1264942.tif")
subc_raster <- paste0(my_directory,
                      "/hydrography90m_test_data/subcatchment_1264942.tif")

# Define full path to the vector file of the stream network
stream_vector <- paste0(my_directory,
                        "/hydrography90m_test_data/order_vect_59.gpkg")

# Automatically extract the basin and sub-catchment IDs and
# snap the data points to the stream segment
snapped_coordinates <- snap_to_subc_segment(data = species_occurrence,
                                            lon = "longitude",
                                            lat = "latitude",
                                            id = "occurrence_id",
                                            basin_layer = basin_raster,
                                            subc_layer = subc_raster,
                                            stream_layer = stream_vector,
                                            n_cores = 2)

# Define full path to the direction .tif
direction_raster <- paste0(my_directory,
                           "/hydrography90m_test_data/direction_1264942.tif")
# Define the path for the output file(s)
output_folder <-  paste0(my_directory, "/upstream_catchments")
if(!dir.exists(output_folder)) dir.create(output_folder)
# Get the upstream catchment for each point location
get_upstream_catchment(snapped_coordinates,
                       lon = "lon_snap",
                       lat = "lat_snap",
                       id = "occurrence_id",
                       direction_layer = direction_raster,
                       out_dir = output_folder,
                       n_cores = 2)
```
