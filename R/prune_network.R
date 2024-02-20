

### EXAMPLE USING GRASS / EXAMPLE DATASET

## Get the raster sub-catchments after pruning the network

## requires the following
# - stream raster
# - stream_order raster
# - direction raster
# - number of stream order to remove
# - r.stream.basins GRASS add-on



## Create GRASS DB
export DIR=/mnt/d/projects/hydrographr/hydrographr_data/prune_test
mkdir $DIR
cd $DIR
mkdir grass_db

## set up GRASS session
# grass -c $DIR/stream_1264942.tif  $DIR/grass_db/hydro90m_test/
grass  $DIR/grass_db/hydro90m_test/PERMANENT




## import data
r.in.gdal input=$DIR/stream_1264942.tif  output=stream --o # stream IDs
r.in.gdal input=$DIR/order_strahler_h18v04.tif  output=stream_order --o # stream order
r.in.gdal input=$DIR/direction_1264942.tif  output=direction --o


## Get the stream segment raster map without the given stream order:
export VAR=2 # specified by user)
r.mapcalc "stream_reduced = if(stream_order > $VAR, stream, null() )" --o
# r.out.gdal  input=stream_reduced    output=$DIR/stream_reduced.tif   type=Int32  nodata=-9999  --o  -c  -m -f createopt="COMPRESS=LZW,ZLEVEL=9"

## Create new sub-catchments
# g.extension r.stream.basins # check if available?
r.stream.basins stream_rast=stream_reduced  direction=direction   basins=basins --o   # -c to create new categories

## Write out for checking
# r.out.gdal  input=basins    output=$DIR/basins_deleted_until_ord_${VAR}.tif   type=Int32  nodata=-9999  --o  -c  -m -f createopt="COMPRESS=LZW,ZLEVEL=9"

## Write out the lookup table: basin and segment IDs
r.stats input=basins,stream  output=$DIR/lookup.txt -n --o #  new, old




#======= Continue in R ========================================================
library(hydrographr)
library(igraph)
library(data.table)
"%ni%" <- Negate("%in%")


my_directory <- "D:/projects/hydrographr/hydrographr_data/prune_test"
# my_directory <- tempdir()
# download_test_data(my_directory)
setwd(my_directory)

## read as graph to subset area in next step
g <- read_geopackage(gpkg = paste0(my_directory,
                                   "/hydrography90m_test_data",
                                   "/order_vect_59.gpkg"),
                     import_as = "graph")


# Prepare everything on tables, not on graphs:
g_up <- get_catchment_graph(g = g, subc_id = subc_id, mode = "in",
                            outlet = FALSE, as_graph = FALSE, n_cores = 1)

## Prepare the look-up table, import data from GRASS
lookup <- fread("lookup.txt", h=F)
setnames(lookup, c("new_id", "stream"))
# lookup$new_id <- as.character(lookup$new_id)
# lookup$stream <- as.character(lookup$stream)
new_id <- lookup$new_id

## Merge the old network and the new IDs. Make sure that all old IDs are kept.
g_up$stream <- as.numeric(g_up$stream)
g_up$next_stream <- as.numeric(g_up$next_stream)
g_up <- lookup[g_up, on="stream", ]

## First two columns have to be the following for building the graph:
setcolorder(g_up, c("stream", "next_stream"))

## Create graph
g_up <- graph_from_data_frame(g_up, directed = TRUE)
# Note that there is one vertex more than edges (=the outflow, "-1")

## Attach the new basin_id to the vertices. Add "-1" for the outlet
## (there is always one vertex more than edges).
V(g_up)$stream_new <- append(E(g_up)$new_id, "-1")

### remove edges by subc_id. Delete all segments (edges) that are not in the
## new basin id file. Basin id are taken from the remaining segments.
g_up_reduced <- delete_edges(g_up, E(g_up)[.inc(E(g_up)[new_id %ni% lookup$new_id ])])

## merge, i.e. collapse all segments belonging to a new subc_id to new entities
g2 <- contract(g_up_reduced, factor(V(g_up_reduced)$stream_new),
               vertex.attr.comb = function(x) levels(factor(x)))

## Remove redundant segments and keep attributes (merge not really working...)
g_sim <- simplify(g2, edge.attr.comb = list("sum"),
                  remove.multiple = TRUE,
                  remove.loops = TRUE)


## Use the following to drop all old attributes
g_sim <- simplify(g2, edge.attr.comb = list(weight="sum", "ignore"),
                  remove.multiple = TRUE,
                  remove.loops = TRUE)

## Get as datatable
table_out <- setDT(as_long_data_frame(g_sim))
setcolorder(table_out, c("from_stream_new", "to_stream_new"))
table_out

## Which columns should be kept?
# some attributes need to be removed, e.g.
# distance from source,
# cumulative length
# ...
# check if the others are actually combined..


## OPTIONAL: Produce the raster stream network and the new sub-catchments






