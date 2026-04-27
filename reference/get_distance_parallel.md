# Calculate euclidean or along the network distance between points

Calculate euclidean or along-the-network distance (in meters) between
points. To calculate the distance along the network, point coordinates
need to be snapped to the stream network using the function
[`snap_to_network()`](snap_to_network.md) or
[`snap_to_subc_segment()`](snap_to_subc_segment.md).

## Usage

``` r
get_distance_parallel(
  data,
  lon,
  lat,
  id,
  basin_id = NULL,
  basin_layer = NULL,
  stream_layer = NULL,
  distance = "both",
  n_cores = 1,
  quiet = TRUE
)
```

## Arguments

- data:

  a data.frame or data.table that contains the columns regarding the
  longitude / latitude coordinates in WGS84.

- lon:

  character. The name of the column with the longitude coordinates.

- lat:

  character. The name of the column with the latitude coordinates.

- id:

  character. The name of a column containing unique IDs for each row of
  "data" (e.g., occurrence or site IDs). The unique IDs need to be
  numeric and less than 10 characters long.

- basin_id:

  character. The name of the column with the basin IDs. If NULL and
  distance is set to 'network' or 'both', the basin IDs will be
  extracted automatically. Default is NULL.

- basin_layer:

  character. Full path to the basin ID .tif layer. Needs to be defined
  to calculate the distance along the network.

- stream_layer:

  character. Full path of the stream network .gpkg file. Needs to be
  defined to calculate the distance along the network.

- distance:

  character. One of "euclidean", "network", or "both". If "euclidean",
  the euclidean distances between all pairs of points are calculated. If
  "network", the shortest path along the network between all pairs of
  points is calculated. (see "Details" for more information). If method
  is set to "both", both distance measures are calculated. Distances are
  given in meters. Default is "both".

- n_cores:

  numeric. Number of cores used for parallelisation. Default is 1.

- quiet:

  logical. If FALSE, the standard output will be printed. Default is
  TRUE.

## Value

If distance='euclidean', a distance matrix, in meters, of the euclidean
distances between all the pairs of points (object of class data.frame)
is returned. If distance='network', a data.frame with three columns:
from_id, to_id, dist is returned. The 'dist' column includes the
distance, in meters, of the shortest path along the network from the
point "from_id" to the point "to_id". If distance='both', a list
containing both objects is returned.

## Details

To calculate the euclidian distance between all pairs of points the
function uses the v.distance command of GRASS GIS, which has been set up
to produce a square matrix of distances. The calculation of distances
along the stream network has been implemented with the command
v.net.allpairs of GRASS GIS. The along-the-network distance calculation
is done for all pairs of points located within the same basin. If the
points are located in different basins, the function can be run in
parallel (i.e., each core for the distance calculations of all points
within one basin). The distance between points located in different
basins is zero because they are not connected through the network.

## References

<https://grass.osgeo.org/grass82/manuals/v.net.allpairs.html>
<https://grass.osgeo.org/grass82/manuals/v.distance.html>

## See also

- [`get_distance()`](get_distance.md) to calculate the distance along
  the network in points located in only one basin.

- [`snap_to_network()`](snap_to_network.md) to snap the data points to
  the next stream segment within a given radius and/or a given flow
  accumulation threshold value.

- [`snap_to_subc_segment()`](snap_to_subc_segment.md) to snap the data
  points to the next stream segment of the sub-catchment the data point
  is located.

- [`extract_ids()`](extract_ids.md) to extract basin and sub-catchment
  IDs.

## Author

Afroditi Grigoropoulou, Marlene Schürz, Jaime Garcia Marquez

## Examples

``` r
# Download test data into the temporary R folder
# or define a different directory
my_directory <- tempdir()
download_test_data(my_directory)

# Load occurrence data
species_occurrence <- read.table(paste0(my_directory,
                            "/hydrography90m_test_data/spdata_1264942.txt"),
                              header = TRUE)

basin_rast <- paste0(my_directory,
                     "/hydrography90m_test_data/basin_1264942.tif")

# Define full path to the sub-catchment raster layer
subc_rast <- paste0(my_directory,
                    "/hydrography90m_test_data/subcatchment_1264942.tif")

# Define full path to the vector file of the stream network
stream_vect <- paste0(my_directory,
                      "/hydrography90m_test_data/order_vect_59.gpkg")

# Automatically extract the basin and sub-catchment IDs and
# snap the data points to the stream segment
snapped_coordinates <- snap_to_subc_segment(data = species_occurrence,
                                            lon = "longitude",
                                            lat = "latitude",
                                            id = "occurrence_id",
                                            basin_layer = basin_rast,
                                            subc_layer = subc_rast,
                                            stream_layer = stream_vect,
                                            n_cores = 2)
# Show head of output table
head(snapped_coordinates)

# Get the euclidean distance and the distance along the network between all
# pairs of points
distance_table <- get_distance_parallel(data = snapped_coordinates,
                               lon = "lon_snap",
                               lat = "lat_snap",
                               id = "occurrence_id",
                               basin_id = "basin_id",
                               basin_layer = basin_rast,
                               stream_layer = stream_vect,
                               distance = "network")
# Show table
distance_table
```
