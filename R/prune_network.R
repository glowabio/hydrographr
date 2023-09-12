

### EXAMPLE USING GRASS / EXAMPLE DATASET

## Get the raster sub-catchments after pruning the network

## Create GRASS DB
export DIR=/mnt/d/projects/hydrographr/hydrographr_data/prune_test
mkdir $DIR
cd $DIR
mkdir grass_db
stream_1264942.tif

## Specify the reclass rules
export RECLASS_RULES=$DIR/reclass_rules.txt
# 1 = NULL

## set up GRASS session
# grass -c $DIR/stream_1264942.tif  $DIR/grass_db/hydro90m_test/
grass  $DIR/grass_db/hydro90m_test/PERMANENT

## import data
r.in.gdal input=$DIR/stream_1264942.tif  output=stream --o # stream IDs
r.in.gdal input=$DIR/order_strahler_h18v04.tif  output=stream_order_input --o # stream order
r.in.gdal input=$DIR/direction_1264942.tif  output=direction --o


## Get the stream segment raster map without the 1st order streams (only for checking)
r.mapcalc "stream_reduced = if(stream_order_input!=1, stream, null() )" --o
r.out.gdal  input=stream_reduced    output=$DIR/stream_reduced.tif   type=Int32  nodata=-9999  --o  -c  -m -f createopt="COMPRESS=LZW,ZLEVEL=9"


## r.mapcalc "stream_reduced = if(stream_order_input >2, stream, null() )" --o
## r.out.gdal  input=stream_reduced    output=$DIR/stream_reduced_ord3.tif   type=Int32  nodata=-9999  --o  -c  -m -f createopt="COMPRESS=LZW,ZLEVEL=9"

## Create new sub-catchments
# g.extension r.stream.basins # check if available?
r.stream.basins stream_rast=stream_reduced  direction=direction   basins=basins --o   # -c to create new categories

r.out.gdal  input=basins    output=$DIR/basins_ord3.tif   type=Int32  nodata=-9999  --o  -c  -m -f createopt="COMPRESS=LZW,ZLEVEL=9"


## get those IDs that should be deleted (in case of Strahler order..)
# r.mapcalc "stream_prune = if(stream_order_input==1, stream, null() )" --o # not needed
## r.mapcalc "stream_prune = if(stream_order_input>2, stream, null() )" --o

## Write out the segments to be removed
# r.stats input=stream_prune   output=$DIR/stream_prune.txt -n --o # not needed

## Write out the lookup table: basin and segment IDs
r.stats input=basins,stream  output=$DIR/lookup.txt -n --o #  new, old




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




# Pick a random subc_id --> needs to be within those IDs that will be kept :-)
subc_id = "513867228"

# Prepare everything on tables, not on graphs:
g_up <- get_catchment_graph(g = g, subc_id = subc_id, mode = "in",
                            outlet = FALSE, as_graph = FALSE, n_cores = 1)


## Prepare the look-up table, import data from GRASS
lookup <- fread("lookup.txt", h=F)
setnames(lookup, c("new_id", "stream"))
# lookup$new_id <- as.character(lookup$new_id)
# lookup$stream <- as.character(lookup$stream)
new_id <- lookup$new_id

## Prepare streams to delete
# stream_prune <-  fread("stream_prune.txt", h=F)
# setnames(stream_prune, c("stream"))
# stream_prune$stream <- as.character(stream_prune$stream)

## Attach the new basin (or subc_id). Convert back to numeric..
# g_up <- g_up[lookup, on= .(stream)]  # not yet correct
g_up$stream <- as.numeric(g_up$stream)
g_up$next_stream <- as.numeric(g_up$next_stream)

## Merge the old network and the new IDs. Make sure that all old IDs are kept.
g_up <- merge(g_up, lookup, by="stream", all.x=TRUE)

# make sure that the first two columns are the following for building the graph:
setcolorder(g_up, c("stream", "next_stream"))

# create graph
g_up <- graph_from_data_frame(g_up, directed = TRUE)
# Note that there is one vertex more than edges (=the outflow)

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


# some attributes need to be removed, e.g.
# distance from source,
# cumulative length
# ...
# check if the others are actually combined..







###################






### TOY EXAMPLE ON TEST DATA SET:

library(hydrographr)
library(igraph)
library(data.table)

my_directory <- tempdir()
download_test_data(my_directory)


# Read the stream network as a graph
my_graph <- read_geopackage(gpkg = paste0(my_directory,
                                          "/hydrography90m_test_data",
                                          "/order_vect_59.gpkg"),
                            import_as = "graph")


# Pick a random subc_id
subc_id = "513863026"

# get as data.table
g_up <- get_catchment_graph(g = my_graph, subc_id = subc_id, mode = "in",
                            outlet = FALSE, as_graph = FALSE, n_cores = 1)

# assign new IDs: 3 new IDs in total = 1, 2, 3 (#10 is the 3rd order segment)
new_id <- data.frame(stream=g_up$stream,
                     new_id = c(0,0,1,1,1,0,0,10,2,2,3,0,0,0,0,0))



# this would need the r.stram.basin output and a look-up table of
# old subc_id | new subc_id

## attach the new subc_id
g_up <- g_up[new_id, on= .(stream)]

# make sure that the first two columns are:
setcolorder(g_up, c("stream", "next_stream"))

# create graph
g_up <- graph_from_data_frame(g_up, directed = TRUE)

# remove 1st order streams
# this shows which segments to remove (keep these as the attributes need to be merged)
# g_up_reduced <- induced.subgraph(g_up, which(E(g_up)$strahler>1) )

# Summarize the attributes across the edges and combine those with the same id
# V(g_up)$group <- append(E(g_up)$new_id, 10)
V(g_up)$stream_new <- append(E(g_up)$new_id, 10)
# E(g_up)$weight <- E(g_up)$new_id
# E(g_up)
# E(g_up)$weight

### remove edges

# use e.g. a Strahler number
g_up_reduced <- delete_edges(g_up, E(g_up)[.inc(E(g_up)[strahler==1])])

# use a custom id
"%ni%" <- Negate("%in%")
g_up_reduced <- delete_edges(g_up, E(g_up)[.inc(E(g_up)[new_id %ni% c(1, 2, 3, 10)])])


## merge, i.e. collapse all segments belonging to a new subc_id to new entities
g2 <- contract(g_up_reduced, factor(V(g_up_reduced)$stream_new),
               vertex.attr.comb = function(x) levels(factor(x)))

## remove redundant segments and keep attributes (merge done by averaging)
g_sim <- simplify(g2, edge.attr.comb = list("mean"),
                  remove.multiple = TRUE,
                  remove.loops = TRUE)

## old attributes are preserved. How to deal with new ones?
edge_attr(g_sim)

# some attributes need to be removed, e.g.
# distance from source,
# cumulative length
# ...


## Get as datatable
table_out <- setDT(as_long_data_frame(g_sim))
setcolorder(table_out, c("from_stream_new", "to_stream_new"))
table_out




