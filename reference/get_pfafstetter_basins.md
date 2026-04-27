# Get Pfafstetter sub-basins

Subset a basin or catchment into up to nine smaller sub-basins following
the Pfafstetter basin delineation scheme. The functions takes a network
graph as the input and splits it into smaller sub-basins following a
hierarchical topological coding scheme (see Verdin & Verdin (1999) for
details), using the flow accumulation as the basis. The user has to
define the sub-catchment (stream segment) ID that serves as the outlet
of the basin. Note that this can be any stream segment that has an
upstream catchment. The input graph can be created with
[`read_geopackage()`](read_geopackage.md) and
[`get_catchment_graph()`](get_catchment_graph.md).

## Usage

``` r
get_pfafstetter_basins(
  g,
  subc_raster,
  out_dir,
  file_name,
  data_table = FALSE,
  n_cores = NULL
)
```

## Arguments

- g:

  igraph object. A directed graph of a basin with one outlet. The outlet
  can be any stream / sub-catchment for which the upstream basin should
  be split into smaller sub-basins. The input graph can be created with
  [`read_geopackage()`](read_geopackage.md) and
  [`get_catchment_graph()`](get_catchment_graph.md).

- subc_raster:

  character. Full path to the sub-catchment raster file of the basin.
  Does not need to be cropped / masked to the basin, but the IDs of the
  sub-catchments need to match with those in the input graph.

- out_dir:

  character. The path of the output directory where the Pfafstetter
  raster layer will be written. Only needed when data.table=FALSE.

- file_name:

  character. The filename and extension of the Pfafstetter raster layer
  (e.g. 'pfafstetter_raster.tif"). Only needed when data.table=FALSE.

- data_table:

  Logical. If TRUE, then the result will be loaded into R as a 2-column
  data.table (sub-catchment ID and Pfafstetter code). If FALSE, the
  result is loaded as a raster (terra object) in R and written to disk.
  Default is FALSE.

- n_cores:

  numeric. Number of cores used for parallelisation. Default is NULL (=
  detectCores(logical=FALSE)-1). Optional.

## Value

Either a data.table, or a raster (terra object) loaded into R. In case
the result is a raster, then a .tif file is written to disk.

## Note

You can use the online map at https://geo.igb-berlin.de/maps/351/view to
identify an ID of a stream segment (use the "Stream segment ID" layer to
the left)

## References

Verdin, K.L. & Verdin, J.P. (1999). A topological system for delineation
and codification of the Earth’s river basins. Journal of Hydrology,
218(1-2), 1-12. doi:10.1016/s0022-1694(99)00011-6

## See also

[`read_geopackage()`](read_geopackage.md) and `get_catchment_graph.()`
to create the input graph.

## Author

Sami Domisch

## Examples

``` r
# Download test data into the temporary R folder
# or define a different directory
my_directory <- tempdir()
download_test_data(my_directory)

# Import the stream network as a graph
# Load stream network as a graph
my_graph <- read_geopackage(gpkg = paste0(my_directory,
                                         "/hydrography90m_test_data",
                                         "/order_vect_59.gpkg"),
                           import_as = "graph")

# Subset the graph such that it contains only one basin. You can use
# a random ID, i.e. it does not need to be the real outlet of the basin.
g_subset <- get_catchment_graph(g = my_graph,
                         subc_id = "513867227",
                         use_outlet = FALSE,
                         mode = "in",
                         as_graph = TRUE)

# Specify the sub-catchment raster file
subc_raster <- paste0(my_directory,"/hydrography90m_test_data",
                     "/subcatchment_1264942.tif")

# Specify the output directory
out_dir <- my_directory

# Calculate the Pfafstetter sub-basins and write the raster layer to disk (
# and import into R)
pfafstetter <- get_pfafstetter_basins(g = g_subset ,
                                      subc_raster = subc_raster,
                                      out_dir = out_dir,
                                      file_name = "pfafstetter_raster.tif",
                                      data_table = FALSE,
                                      n_cores = 4)
```
