% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_segment_neighbours.R
\name{get_segment_neighbours}
\alias{get_segment_neighbours}
\title{Get stream segment neighbours}
\usage{
get_segment_neighbours(
  g,
  subc_id = NULL,
  var_layer = NULL,
  stat = NULL,
  attach_only = FALSE,
  order = 5,
  mode = "in",
  n_cores = 1,
  max_size = 1500
)
}
\arguments{
\item{g}{igraph object. A directed graph.}

\item{subc_id}{numeric vector of the input sub-catchment IDs
(=stream segment IDs) for which to search the connected segments.}

\item{var_layer}{character vector. One or more attributes (variable layers)
of the input graph that should be reported for each output segment_id
("to_stream"). Optional.}

\item{stat}{one of the functions mean, median, min, max, sd (without quotes).
Aggregates (or summarizes) the variables for the neighbourhood of each input
segment ("stream", e.g., the average land cover in the next five upstream
segments or sub-catchments). Default is NULL.}

\item{attach_only}{logical. If TRUE, the selected variables will be only
attached to each segment without any further aggregation. Default is FALSE.}

\item{order}{numeric. The neighbouring order as in igraph::ego.
Order = 1 would be immediate neighbours of the input sub-catchment IDs,
order = 2 would be the order 1 plus the immediate neighbours of
those sub-catchment IDs in order 1, and so on.}

\item{mode}{character. One of "in", "out", or "all". "in" returns only
upstream neighbouring segments, "out" returns only the downstream segments,
and "all" returns both.}

\item{n_cores}{numeric. Number of cores used for parallelization
in the case of multiple stream segments / outlets. Default is 1.
Currently, the parallelization process requires copying the data to each
core. In case the graph is very large, and many segments are
used as an input, setting n_cores to a higher value can speed up the
computation. This comes however at the cost of possible RAM limitations
and even slower processing since the large data will be copied to each core.
Hence consider testing with n_cores = 1 first. Optional.}

\item{max_size}{numeric. Specifies the maximum size of the data passed to the
parallel back-end in MB. Default is 1500 (1.5 GB). Consider a higher value
for large study areas (more than one 20°x20° tile). Optional.}
}
\description{
For each segment, reports those upstream, downstream, or up-and
downstream segments that are connected to one or multiple input
segments within a specified neighbour order, with the option to
summarize attributes across these segments. Note that the stream
segment and sub-catchment IDs are identical, and for consistency,
we use the term "subc_id".

This function can also be used to create the connectivity table
for Marxan by using var_layer="length" and attach_only=TRUE.
The resulting table reports the connectivity from each segment,
along with the stream length for all connected segments.
}
\examples{
# Download test data into temporary R folder
# or define a different directory
my_directory <- tempdir()
download_test_data(my_directory)

# Load the stream network as graph
g <- read_geopackage(paste0(my_directory, "/hydrography90m_test_data",
                             "/order_vect_59.gpkg"),
                      type = "net",
                      as_graph = TRUE)

# Get the upstream segment neighbours in the 5th order
# and report the length and source elevation
# for the neighbours of each input segment
get_segment_neighbours(g, subc_id = subc_id,
                       order = 5, mode = "in", n_cores = 1,
                       var_layer = c("length", "source_elev"),
                       attach_only = TRUE)

# Get the downstream segment neighbours in the 5th order
# and calculate the median length and source elevation
# across the neighbours of each input segment
get_segment_neighbours(g, subc_id = subc_id,
                       order = 2, mode ="out", n_cores = 1,
                       var_layer = c("length", "source_elev"),
                       stat = median)

# Get the up-and downstream segment neighbours in the 5th order
# and report the median length and source elevation
# for the neighbours of each input segment
get_segment_neighbours(g, subc_id = subc_id,order = 2,
                       mode = "all", n_cores = 1,
                       var_layer = c("length", "source_elev"),
                       stat = mean, attach_only = TRUE)

}
\references{
Csardi G, Nepusz T: The igraph software package for complex network research,
InterJournal, Complex Systems 1695. 2006. \url{https://igraph.org}
}
\author{
Sami Domisch
}