# Get all upstream distances for all subc_id

Calculates the upstream distance from each subc_id to all upstream
subc_id. The output can be directly used in spatial prioritization
analyses for e.g. Marxan, Gurobi etc. to specify the longitudinal
connectivity. Note that the stream segment and sub-catchment IDs are
identical, and for consistency, we use the term "subc_id".

Note that the distance can be extremely long and for the subsequent
spatial prioritization analyses you might want to consider setting a cap
at a certain distance.

## Usage

``` r
get_all_upstream_distances(network_table = network_table, n_cores = 1)
```

## Arguments

- network_table:

  A data.table that includes the columns
  `c(stream, next_stream, out_dist`, the latter specifying the distance
  to the outlet (which is included in the Hydrography90m vector
  attribute table).

- n_cores:

  numeric. Number of cores used for parallelisation in the case of
  multiple stream segments / s. Default is 1. Currently, the
  parallelisation process requires copying the data to each core. In
  case the graph is very large, and many segments are used as an input,
  setting n_cores to a higher value can speed up the computation. This
  comes however at the cost of possible RAM limitations and even slower
  processing since the large data will be copied to each core. Hence
  consider testing with n_cores = 1 first. Optional.

## Value

A data.table that reports the distance (in meters) from each subc_id to
all upstream subc_ids.

## Note

Currently the attributes are not provided for the (the selected subc_id
segment). If the attributes are also needed for the outlet subc_id, then
the next downstream sub_id can be selected (enlarge the study area)

## References

Csardi G, Nepusz T: The igraph software package for complex network
research, InterJournal, Complex Systems 1695. 2006. <https://igraph.org>

## See also

[`read_geopackage()`](read_geopackage.md) and
[`get_catchment_graph()`](get_catchment_graph.md) to create a network
graph. Alternatively, see
[`get_segment_neighbours()`](get_segment_neighbours.md) to obtain the
upstream variables for a specified neighbourhood, or
[`get_upstream_variable()`](get_upstream_variable.md) to aggregate a set
of variables across the upstream catchment.

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
subc_id = "513867228"
# Get the upstream catchment as a data.table
network_table <- hydrographr::get_catchment_graph(g = my_graph ,
                                                  subc_id = subc_id,
                                                  mode = "in",
                                                  use_outlet = FALSE,
                                                  as_graph = FALSE,
                                                  n_cores = 1)

## Condense the table supplied to the function to save RAM
keep_these <- c("stream", "next_stream", "out_dist")
network_table <- network_table[, ..keep_these]

## Change to integers
network_table$stream <- as.integer(network_table$stream)
network_table$next_stream <- as.integer(network_table$next_stream)


## Calculate the network distance (in meter) from each subc_id to
## all upstream subc_id using four CPUs for the parallelization
result <- get_all_upstream_distances(network_table = network_table,
                                      n_cores = 4)
```
