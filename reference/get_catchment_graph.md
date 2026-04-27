# Get catchment from stream network graph

Subset the stream network graph by extracting the upstream, downstream
or entire catchment, for one or multiple stream segments. The function
will return either one or more data.tables or graph objects for each
input stream segment. Note that the stream segment and sub-catchment IDs
are identical, and for consistency, we use the term "subc_id".

By switching the mode to either "in", "out" or "all", only the upstream,
downstream or all connected segments will be returned, respectively. The
function [`read_geopackage()`](read_geopackage.md) can be used to create
the input network graph.

## Usage

``` r
get_catchment_graph(
  g,
  subc_id = NULL,
  use_outlet = FALSE,
  mode = NULL,
  as_graph = FALSE,
  n_cores = 1,
  max_size = 1500
)
```

## Arguments

- g:

  igraph object. A directed graph.

- subc_id:

  numeric vector of a single or multiple IDs, e.g (c(ID1, ID2, ID3,
  ...). The sub-catchment (equivalent to stream segment) IDs for which
  to delineate the upstream drainage area. If empty, then outlets will
  be used as sub-catchment IDs (with use_outlet = TRUE). Note that you
  can browse the entire network online at
  <https://geo.igb-berlin.de/maps/351/view> and to left hand side,
  select the "Stream segment ID" layer and click on the map to get the
  ID. Optional.

- use_outlet:

  logical. If TRUE, the outlets of the given network graph will be used
  as additional input subc_ids. Outlets will be identified internally as
  those stream segments that do not have any downstream connected
  segment. Default is FALSE.

- mode:

  character. One of "in", "out" or "all". "in" returns the upstream
  catchment, "out" returns the downstream catchment (all catchments that
  are reachable from the given input segment), and "all" returns both.

- as_graph:

  logical. If TRUE, the output will be a new graph or a list of new
  graphs with the original attributes. If FALSE, the output will be a
  new data.table or a list of data.tables. List objects are named after
  the subc_ids. Default is FALSE.

- n_cores:

  numeric. Number of cores used for parallelisation in the case of
  multiple stream segments / s. Default is 1. Currently, the
  parallelisation process requires copying the data to each core. In
  case the graph is very large, and many segments are used as an input,
  setting n_cores to a higher value can speed up the computation. This
  comes however at the cost of possible RAM limitations and even slower
  processing since the large data will be copied to each core. Hence
  consider testing with n_cores = 1 first. Optional.

- max_size:

  numeric. Specifies the maximum size of the data passed to the parallel
  back-end in MB. Default is 1500 (1.5 GB). Consider a higher value for
  large study areas (more than one 20°x20° tile). Optional.

## Value

A graph or data.table that reports all subc_ids. In case of multiple
input segments, the results are stored in a list.

## Note

Currently the attributes are not provided for the (the selected subc_id
segment). If the attributes are also needed for the outlet subc_id, then
the next downstream sub_id can be selected (enlarge the study area)

## References

Csardi G, Nepusz T: The igraph software package for complex network
research, InterJournal, Complex Systems 1695. 2006. <https://igraph.org>

## See also

[`read_geopackage()`](read_geopackage.md) to create a network graph.

## Author

Sami Domisch

## Examples

``` r
# Download test data into the temporary R folder
# or define a different directory
my_directory <- tempdir()
download_test_data(my_directory)

# Load stream network as a graph
my_graph <- read_geopackage(gpkg = paste0(my_directory,
                                         "/hydrography90m_test_data",
                                         "/order_vect_59.gpkg"),
                           import_as = "graph")

# Pick a random subc_id
subc_id = "513855877"
# Get the upstream catchment as a graph
g_up <- get_catchment_graph(g = my_graph, subc_id = subc_id, mode = "in",
                            use_outlet = FALSE, as_graph = TRUE, n_cores = 1)

# Get the downstream segments as a data.table,
g_down <- get_catchment_graph(g = my_graph, subc_id = subc_id, mode = "out",
                              use_outlet = FALSE, as_graph = FALSE, n_cores = 1)

# Get the catchments of all outlets in the study area as a graph
g_all <- get_catchment_graph(g = my_graph, mode = "in", use_outlet = TRUE,
                             as_graph = TRUE, n_cores = 1)
```
