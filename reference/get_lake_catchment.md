# Get lake catchments based on intersection points between stream network and lakes

Delineate lake catchments in GeoTIFF format based on intersection points
between stream network (i.e. Hydrography90m) and lake geospatial files
(e.g. HydroLAKES). The function uses the lake intersection tables from
get_lake_intersection.R as input data.

## Usage

``` r
get_lake_catchment(
  data,
  flow = "flow_accu_mean",
  lake_id = "lake_ID",
  n = "all",
  direction,
  lake_basin,
  n_cores = 1,
  quiet = TRUE
)
```

## Arguments

- data:

  a data.frame or data.table that contains the columns regarding the
  stream segment ids of the intersection points between lake and stream
  network; (i.e., output of get_lake_intersection)

- flow:

  character. The name of the flow accumulation column used for sorting
  the intersection table; i.e. either flow_accu (flow accumulation value
  at intersection point pixel), flow_accu_max (maximum flow accumulation
  value of 3 x 3 neighboring pixels),

- lake_id:

  character. The name of the column containing lake ids; (i.e., output
  "lake_ID" of get_lake_intersection) flow_accu_mean (mean flow
  accumulation value of 3 x 3 neighboring pixels). Default is
  flow_accu_mean

- n:

  integer. Number of intersection points used for lake catchment
  delineation; e.g. n=1 equals first row of intersection table and lake
  outlet; n=20 equals first 20 rows of intersection table; Default is
  all.

- direction:

  character. Full path to Hydrography90m flow direction tif file

- lake_basin:

  character. Full path to output catchment tif files

- n_cores:

  integer. Number of cores used in parallelization; Default is one.

- quiet:

  logical. If FALSE, the standard output will be printed.; Default is
  TRUE.

## Note

For the function to work we need the output of the function
get_lake_intersection that are the lake_referenceID.txt and intersection
table (i.e. coord_lakeID.txt)

## References

add reference manual html here https://grass.osgeo.org/grass82/manuals/

## Author

Jaime Garcia Marquez, Thomas Tomiczek

## Examples

``` r
# Download test data into the temporary R folder
# or define a different directory
my_directory <- tempdir()
download_test_data(my_directory)

data <- fread(paste0(my_directory,
                       "/hydrography90m_test_data",
                       "/coord_lake_1.txt"),
                      header = TRUE)

direction <- (paste0(my_directory,
                    "/hydrography90m_test_data",
                  "/direction_1264942.tif"))

catch <- (paste0(my_directory,
                   "/hydrography90m_test_data/"))

get_lake_catchment(data, direction = direction, lake_basin = catch)
```
