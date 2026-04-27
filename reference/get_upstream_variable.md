# Calculate upstream variables for each sub-catchment

For each input sub-catchment, the function calculates the upstream mean,
median, min, max, sd or sum for one or more variables. Note that the
stream segment and sub-catchment IDs are identical. The input graph can
be created with [`read_geopackage()`](read_geopackage.md) and
[`get_catchment_graph()`](get_catchment_graph.md).

This function can be used to obtain all upstream stream segments /
sub-catchments for species distribution modelling, or for spatial
prioritization analyses (e.g. Marxan/Gurobi) to specify the connectivity
(and in this case the "length" attribute can be used).

The function accepts as an input a graph with one ore more outlets (e.g.
an entire tile with many drainage basins). The variable that should be
aggregated upstream should be prepared as a data.table, e.g. with
[`extract_zonal_stat()`](extract_zonal_stat.md).

## Usage

``` r
get_upstream_variable(
  g,
  variable_table = NULL,
  subc_id = NULL,
  var_layer = NULL,
  upstream_stat = NULL,
  include_focal = FALSE,
  save_up_conn = NULL,
  load_up_conn = NULL,
  n_cores = 1,
  max_size = 3000
)
```

## Arguments

- g:

  igraph object. A directed graph.

- variable_table:

  a data.table that includes the `stream` column (corresponding to the
  subc_id) as well as the attributes that should be aggregated across
  the the upstream network subc_id. Default is NULL.

- subc_id:

  A vector of subc_id for which the upstream variables should be
  calculated. Optional; default is to use all subc_id of the input
  graph.

- var_layer:

  character vector. One or more attributes (variable layers) of the
  variable_table should be reported for each output subc_id. Default is
  NULL.

- upstream_stat:

  one of the functions mean, median, min, max, sd, sum (without quotes).
  The function will be used to aggregate (or summarize) the upstream
  variables for each subc_id (e.g., the average land cover across the
  entire upstream area). Default is NULL.

- include_focal:

  Whether the focal subc_id should be included in the aggregation with
  `include_focal = TRUE` which is the default. Set to FALSE if the focal
  subc_id should not be included in the upstream aggregation of the
  given sub-catchment.

- save_up_conn:

  character. Provide a name of the .RData file that will be written to
  disk (to [`getwd()`](https://rdrr.io/r/base/getwd.html)), and which
  includes the intermediate result consisting of a data.table that
  includes all upstream connections for each subc_id. Useful for large
  study areas as it avoids re-running the possibly time-consuming
  pre-processing to obtain other metrics (e.g. mean, sum) or other
  variables. The data.table is called `upstream_dt` and has the columns
  `stream` and `base`. Default is FALSE.

- load_up_conn:

  Optional, and if used, it should be `upstream_dt`. In case the file
  with the upstream connections was previously written to disk (using
  e.g. `save_up_conn = "my_file"`), then this data.table can be first
  loaded with `load(paste0(getwd(), "/my_file.RData"))`, which loads the
  `upstream_dt` data.table with the columns `c("stream", "base")`. Use
  `load_up_conn = upstream_dt` to then skip the pre-processing.
  Optional, default is NULL.

- n_cores:

  numeric. Number of cores used for parallelisation. In case the graph
  is very large, and many segments are used as an input, setting n_cores
  to a higher value can speed up the computation. Note however that the
  parallelisation process requires copying the input graph to each core.
  This may result in possible RAM limitations and even slower
  processing. Hence consider testing first with a lower number of cores.
  Default is n_cores = 1. Optional.

- max_size:

  numeric. Specifies the maximum size of the data passed to the parallel
  back-end in MB. Default is 1500 (1.5 GB). Consider a higher value for
  large study areas (more than one 20°x20° tile). Optional.

## Value

A data.table indicating the "stream" (=subc_id) and the upstream
variables which have the same column names as the `var_layer` argument.
The intermediate output can be saved to disk (requires setting
`save_up_conn` = "my_file").

## References

Csardi G, Nepusz T: The igraph software package for complex network
research, InterJournal, Complex Systems 1695. 2006. <https://igraph.org>

## See also

[`read_geopackage()`](read_geopackage.md) and
[`get_catchment_graph()`](get_catchment_graph.md) to create the input
graph. Alternatively, see
[`get_segment_neighbours()`](get_segment_neighbours.md) to obtain the
upstream variables for a specified neighbourhood.

## Author

Sami Domisch

## Examples

``` r
# Download test data into the temporary R folder
# or define a different directory
my_directory <- tempdir()
setwd(my_directory)
download_test_data(my_directory)

# Load the stream network as graph
my_graph <- read_geopackage(gpkg = paste0(my_directory,
                                         "/hydrography90m_test_data",
                                         "/order_vect_59.gpkg"),
                                          import_as = "graph")

# Subset the graph and get a smaller catchment
my_graph <- get_catchment_graph(g = my_graph,
                                subc_id = 513867228,
                                mode = "in",
                                use_outlet = FALSE,
                                as_graph = TRUE,
                                n_cores = 1)


## Prepare the variables that should be accumulated.
## Load the table
variable_table <- read_geopackage(gpkg = paste0(my_directory,
                                               "/hydrography90m_test_data",
                                              "/order_vect_59.gpkg"),
                                              import_as = "data.table")

## Specify the layers for the upstream aggregation
var_layer= c("length", "flow_accum")

## Subset the table
keep_these <- c("stream", var_layer)
variable_table <- variable_table[, ..keep_these]


## Get the upstream sum of the variables "length" and "flow_accum" for
## single subc_id
result <- get_upstream_variable(my_graph,
                                variable_table = variable_table,
                                var_layer = var_layer,
                                upstream_stat=sum,
                                subc_id = c(513861908, 513864129),
                                include_focal = TRUE)


## Get the upstream sum of the variables "length" and "flow_accum" across the
## entire network
result <- get_upstream_variable(my_graph,
                                variable_table = variable_table,
                                var_layer = var_layer,
                                upstream_stat=sum,
                                include_focal = TRUE,
                                n_cores = 4,
                                save_up_conn = "my_file")


## Alternatively, load the previously generated upstream connections
## and use it directly, skipping the pre-processing
load(paste0(getwd(), "/my_file.RData"))

result <- get_upstream_variable(my_graph,
                                variable_table = variable_table,
                                var_layer = var_layer,
                                upstream_stat=max,
                                include_focal = TRUE,
                                load_up_conn = upstream_dt)



## Map the new variable across the network

## Specify tif-layer for reclassification
subc_raster <- paste0(my_directory, "/hydrography90m_test_data/subcatchment_1264942.tif")
recl_raster <- paste0(my_directory, "/upstream_sum.tif")

## Set columns as integer
result <- result[, names(result) := lapply(.SD, as.integer)]

### Create raster - select the "to" column which represents the unique subc_id
r <- reclass_raster(data = result,
                   rast_val = "stream",
                   new_val = "flow_accum",
                   raster_layer = subc_raster,
                   recl_layer = recl_raster,
                   read = TRUE)

## Plot the map
terra::plot(r, background = "grey")
```
