#' Delineate Pfafstetter basins
#'
#' Subset a basin or catchment into smaller units followng the Pfafstetter
#' basin delineation.
#'
#' The function calculates internally the area of each sub-catchment
#'
#' @param g A directed graph with one outlet
#' @param outlet The stream or sub-catchment ID that serves as the outlet of
#' the basin for which the Pfafstetter basins should be delineated.
#' @param cpu The number of CPUs used for the parallelization.
#' @importFrom data.table ..
#' @importFrom processx run
#' @export
#'





## TESTING
path <- "D:/projects/hydrographr/hydrographr_data"
setwd(path)
library(hydrographr)
## END TESTING

!!! the graph and table need to match, else the IDs are not found during the graph-subsetting !!!

todo:
- check if could run on any given outlet, as long as the datatable is created from this graph?
- implement GRASS functions

#
# if (!require("RSQLite")) { install.packages("RSQLite", dependencies = TRUE) ; library(RSQLite, quietly=T)}
# if (!require("data.table")) { install.packages("data.table", dependencies = TRUE) ; library(data.table, quietly=T)}
# if (!require("igraph")) { install.packages("igraph", dependencies = TRUE) ; library(igraph, quietly=T)}
#
# cat(">>>>>>>> Loading the network.... <<<<<<<<", "\n")
# ### Use geopackage database from GRASS
# con <- dbConnect(drv=RSQLite::SQLite(), dbname="order_vect_59.gpkg") # get connection
# tmp <- dbListTables(con) # ## list all tables
# tmp <- tmp[tmp == "merged"] # exclude sqlite_sequence (contains table information)
#
#
# stream_dt <- dbGetQuery(conn=con, statement=paste0("SELECT * FROM '", tmp, "'")); rm(tmp)
# dbDisconnect(con); rm(con) # close db connection
# stream_dt <- as.data.table(stream_dt)
# stream_dt$geom <- NULL # delete the geometry, messy
# stream_dt$fid <-  NULL # else dulicate-search does not work
# stream_dt <- stream_dt[!is.na(stream_dt$stream),] # remove all empty rows (from the point data layer)
#
#
# stream_dt <- unique(stream_dt) # should not be duplicated...
# setkey(stream_dt, stream)
#
#
# tmp_names <- c("stream", "next_stream")
# g_old <- graph.data.frame(stream_dt[,..tmp_names], directed = T); rm(tmp_names) #what="edges"  what="vertices"
# g_old
# g_old_catchment <- get_catchment_graph(g_old, stream= 513867228, outlet=FALSE, mode="in", as_graph = TRUE)



#
# g
# g_old_catchment
#
#
# V(g)
# V(g_old_catchment)
#
# g <- g_old_catchment





# not yet implemented: threshold for small/large basins to avoid splitting, or prefer splitting sooner
# input can be a graph or table
# users can specify n_cores?
# is moveme-function needed? if yes, check citation

# test data
g <- read_geopackage("order_vect_59.gpkg", import_as ="graph")
# subset
g <- get_catchment_graph(g, stream= 513867228, outlet=FALSE, mode="in", as_graph = TRUE)




## old, small test file
g <- read_geopackage("stream_vect.gpkg", import_as ="graph")
# subset
g <- get_catchment_graph(g, stream= 11382, outlet=FALSE, mode="in", as_graph = TRUE)





cat(">>>>>>>> Loading the network.... <<<<<<<<", "\n")
### Use geopackage database from GRASS
con <- dbConnect(drv=RSQLite::SQLite(), dbname="stream_vect.gpkg") # get connection
tmp <- dbListTables(con) # ## list all tables
tmp <- tmp[tmp == "stream_vect"] # exclude sqlite_sequence (contains table information)




stream_dt <- dbGetQuery(conn=con, statement=paste0("SELECT * FROM '", tmp, "'")); rm(tmp)
dbDisconnect(con); rm(con) # close db connection
stream_dt <- as.data.table(stream_dt)
stream_dt$geom <- NULL # delete the geometry, messy
stream_dt$fid <-  NULL # else dulicate-search does not work
stream_dt <- stream_dt[!is.na(stream_dt$stream),] # remove all empty rows (from the point data layer)


stream_dt <- unique(stream_dt) # should not be duplicated...
setkey(stream_dt, stream)


### Attach the area of each stream-reach
tmp_area <- fread("sub_catchment_area_stats.txt", h=F)
setnames(tmp_area, c("stream", "V2", "area_km2", "area_cells")) # still in m2
tmp_area$area_km2 <- round(tmp_area$area_km2 / 1000000, 4) # convert to km2
tmp_area$V2 <- NULL
### Merge to big table
stream_dt <- tmp_area[stream_dt, on="stream"]

stream_dt$flow_accum <- NULL


### Create graph based on two columns only
tmp_names <- c("stream", "next_stream")
g <- graph.data.frame(stream_dt[,..tmp_names], directed = T); rm(tmp_names) #what="edges"  what="vertices"

g2 <- graph_from_data_frame(stream_dt[,..tmp_names], directed = T); rm(tmp_names)



# large file: part of the Amazon
# g <- read_geopackage("order_vect_segment_h10v08.gpkg", import_as ="graph")
# # subset
# g <- get_catchment_graph(g, stream= 171719949, outlet=FALSE, mode="in", as_graph = TRUE)
# save(g, file="single_large_catchment.RData")
load("single_large_catchment.RData")
all_simple_paths(g, from = outlet, to = "", "in")

171719949


# g <- as.data.table(as_data_frame(g))
# names(g)[1:2] <- c("stream", "next_stream")

# stream_dt <- read_geopackage("order_vect_59.gpkg")


# ### Create graph based on two columns only
# g_cols <- c("stream", "next_stream")
# g <- graph.data.frame(g[,..g_cols], directed = T)

# get_largest_catchment()
# if outlet is specified by the user
#  then use this,
#  else take outlet from graph
#  --> but headwaters need to be connected to the outlet, how to check this?

# g <- read_geopackage("order_vect_59.gpkg", graph=T)
# g <- get_largest_catchment(g)
#
# do function: get graph for a specific outlet id



usePackage <- function(p){
  if (!is.element(p, installed.packages()[,1])) install.packages(p, dep = TRUE)
  library(p, character.only = TRUE)
}

usePackage("DBI")
usePackage("foreach")
usePackage("doParallel")
usePackage("dplyr")
usePackage("data.table")
usePackage("RSQLite")
usePackage("igraph")
usePackage("tidyr")
usePackage("future.apply")
usePackage("doFuture")
usePackage("progressr")
usePackage("sjmisc")
usePackage("R.utils")
usePackage("tidyverse")



pfafstetter <- function(g, sub_catch=sub_catch, flow_accum=flow_accum, outlet=outlet)

# Avoid exponential numbers in the reclassification, only set this only within the function
options(scipen=999)

cat("Setting up parallel backend...\n")
# Set up parallel backend
n_cores <- detectCores(logical=F)-2
registerDoFuture()
plan(multisession, workers = n_cores)


### Get the path number as an additional column in the graph list.vs
mapply_fun <- function(element,name){
  mutate(element,connected_to = name)
}

### Fast subsetting by vector elements
"%ni%" <- Negate("%in%")



# ### Change column order
# moveme <- function (invec, movecommand) {
#   movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]],
#                                  ",|\\s+"), function(x) x[x != ""])
#   movelist <- lapply(movecommand, function(x) {
#     Where <- x[which(x %in% c("before", "after", "first",
#                               "last")):length(x)]
#     ToMove <- setdiff(x, Where)
#     list(ToMove, Where)
#   })
#   myVec <- invec
#   for (i in seq_along(movelist)) {
#     temp <- setdiff(myVec, movelist[[i]][[1]])
#     A <- movelist[[i]][[2]][1]
#     if (A %in% c("before", "after")) {
#       ba <- movelist[[i]][[2]][2]
#       if (A == "before") {
#         after <- match(ba, temp) - 1
#       }
#       else if (A == "after") {
#         after <- match(ba, temp)
#       }
#     }
#     else if (A == "first") {
#       after <- 0
#     }
#     else if (A == "last") {
#       after <- length(myVec)
#     }
#     myVec <- append(temp, values = movelist[[i]][[1]], after = after)
#   }
#   myVec
# }






# Needs a data-table to assign results
stream_dt <- as.data.table(as_long_data_frame(g))
setnames(stream_dt, c("ver[el[, 1], ]"), c("stream"))
stream_dt <- stream_dt[,"stream"]
stream_dt$stream <- as.numeric(as.character(stream_dt$stream))
# Calculate the area of each sub-catchment (which is more accurate than the flow_accum)





### How to implement this? ----------------------------------#

# # start GRASS session
# # server3
# export DIR=/data/domisch/hydrographr_data
# grass78  -text -c -e   subcatchment_1264942.tif  $DIR/grass_location # rectangle
# # grass78  -text -c -e   subc_1264942.tif   $DIR/grass_location
# export GRASSEXEC="grass78 $DIR/grass_location/PERMANENT/ --exec" # create alias
#
# $GRASSEXEC  r.in.gdal  input=$DIR/subcatchment_1264942.tif  output=scatch --o
# $GRASSEXEC  r.stats input=scatch  output=$DIR/sub_catchment_area_stats.txt  separator=tab  -aclnC --o # rectangle
# # $GRASSEXEC  r.stats input=$DIR/subc_1264942.tif output=$DIR/sub_cachment_area_stats.txt  separator=tab  -aclnC --o




555555555555555555
# Use the hydrograpr-function for this:

### Attach the area of each stream-reach
# scatch_area <- fread("basins_area_stats.txt", h=F) # small, old test file
scatch_area <- fread("sub_catchment_area_stats.txt", h=F)
setnames(scatch_area, c("stream", "V2", "area_km2", "area_cells")) # still in m2
scatch_area$area_km2 <- round(scatch_area$area_km2 / 1000000, 4) # convert to km2
scatch_area$V2 <- NULL
### Merge to stream table --> keeps only those in the input graph, tosses the rectangle
stream_dt <- scatch_area[stream_dt, on="stream"]




### Get all possible paths from the outlet
# find root and leaves --> vertices
outlet = which(degree(g, v = V(g), mode = "out")==0, useNames = T)
headwater = which(degree(g, v = V(g), mode = "in")==0, useNames = T)

# Stop if too many outlets
if (length(outlet)!=1) {
  stop("Only one outlet possible. You may want to run get_catchment_graph() to get a single drainage basin as the input graph, or you use the same input graph but specify the outlet using outlet=stream, corresponding to your segment ID.")
}


outlet = which(degree(g, v = V(g), mode = "out")==0, useNames = T)
headwater = which(degree(g, v = V(g), mode = "in")==0, useNames = T)


cat("Finding the most contributing (=main) stream....", "\n")
### Run without foreach if only small basin (otherwise the split might not work for very small basins)

# registerDoFuture()
# plan(multisession, workers = NUMBER_CPU_FUTUREAPPLY) # Parallelize

### For very small catchments run subcomponent sequentially
l <- foreach (i=names(headwater), .inorder=F, .packages = "igraph")  %dopar% {
  out <-subcomponent(g, i, mode = c("out"))
}

### Get into datatable format
names(l) <- names(headwater)
l <- future_lapply(l, as_ids)
l <- future_lapply(l, as.data.table)
### Get the path number as an additional column
l <- future_mapply(mapply_fun,l,names(l),SIMPLIFY = F)
### Merge as datatable
dt <- rbindlist(l); rm(l)
names(dt)[1] <- "stream"

### Remove the outlet
setkey(dt, stream)
# dt <- dt[!(dt$stream==as.numeric(names(outlet))),]
dt$seq_id <- seq.int(1:nrow(dt))

dt$stream <- as.numeric(dt$stream)
dt$PATH_ID <- as.numeric(dt$PATH_ID)

### Join
dt <- streams_dt[dt, on="stream"] # left join, preserves ordering

### Get maximum flow acc per PATH_ID
sum_flow_acc_PATH_ID <- dt[,.(sum_flow_acc=sum(area_km2)), by="PATH_ID"]

### Identify the main stream = max contributing drainage
setorder(sum_flow_acc_PATH_ID, -sum_flow_acc)
main_headwater_id <- sum_flow_acc_PATH_ID[1]$PATH_ID # headwater tip ID
gc()



### Get the single stream reaches of the main stream
main_stream <- all_simple_paths(g, from = outlet, to = as.character(main_headwater_id), "in") # edges, ordered from down-to upstream
# main_stream <- future_lapply(main_stream, function(x) x[-1]) # remove the "-1", not a stream reach
main_stream <- as_ids(unlist(main_stream[[1]]))
# if(main_stream[1]=="-1") {main_stream <- main_stream[-1] } # remove the first item, which is the outlet (in first round "-1", due to "next stream" column)
main_stream <- main_stream[-1]

rm(list=ls(pattern="^tmp_")) # cleanup






###---------------------------------------------------------------------#
###--- Get up to 4 most contributing tributaries along main stream -----
###---------------------------------------------------------------------#



cat("Finding the next 4 most contributing tributaries....", "\n")
### Delete the edges of the main stream (not the vertices, else any single stream reaches, i.e. stand-alone connected to main stream, yield an error as there is nothing to connect to)

g_del <- delete_edges(g, V(g)[paste(main_stream)]) # requires the outlet to be removed

### From each main stream vertex, walk to the headwaters to get the entire tributary
## (note: previous version was finding only main tributary. Entire tributary is faster and allows to re-code
## the streams_dt for the next-smaller division, without GRASS)


l2 <- future_sapply(as.character(main_stream), function(x) subcomponent(g_del, x, mode = c("in")))

### Get into datatable format
l2 <- future_lapply(l2, function(x) names(x))
# l2 <- future_sapply(l2, as_ids)
l2 <- future_lapply(l2, as.data.table)
l2 <- future_mapply(mapply_fun,l2,names(l2),SIMPLIFY = F)

### Merge as datatable
dt2 <- rbindlist(l2)
names(dt2)[1] <- "stream"
setkey(dt2, stream)

dt2$stream <- as.numeric(as.character(dt2$stream))
dt2$PATH_ID <- as.numeric(as.character(dt2$PATH_ID))


### Remove the main stream ID from each subcomponent
dt2 <- dt2[dt2$stream %ni% as.numeric(as.character(main_stream)) ,]

### Join
dt2 <- streams_dt[dt2, on="stream"] # left join, preserves ordering

### Get maximum flow acc per PATH_ID
flow_acc_per_PATH_ID_trib <- dt2[,.(sum_flow_acc=sum(area_km2)), by="PATH_ID"]

### Identify the major tributaries flowing in to main stream = max contributing drainage
setorder(flow_acc_per_PATH_ID_trib, -sum_flow_acc)
### this identifies the top 4 tributaries



###--------------------------------------------------------------------#
###--- Identify the stream ID of the contributing tributaries ----
###--------------------------------------------------------------------#

### Get the main stream id at the branches = the position
trib_order <- flow_acc_per_PATH_ID_trib[1:4,1]
main_stream_branch_id <- na.omit(trib_order) # could be less than four
main_stream_branch_id <- as.character(t(main_stream_branch_id))


###------Save intermediate output in txt file ------------
res_ID=data.table(MAIN_STR_BRANCH_ID1=main_stream_branch_id[1],
                  MAIN_STR_BRANCH_ID2=main_stream_branch_id[2],
                  MAIN_STR_BRANCH_ID3=main_stream_branch_id[3],
                  MAIN_STR_BRANCH_ID4=main_stream_branch_id[4])








### When only few headwaters, subcomponent() gives strange results --> use sequential code in foreach below

### Get all id of single streams belonging to the entire tributary (note: main stream is deleted in g_del)
main_trib_all_id <- future_sapply(main_stream_branch_id, function(x) subcomponent(g_del, x, mode = c("in")))
# main_trib_all_id <- subcomponent(g_del, main_stream_branch_id, mode = c("in")) # takes only first argument!


### Get into datatable format
main_trib_all_id <- future_lapply(main_trib_all_id, function(x) names(x))
main_trib_all_id <- future_lapply(main_trib_all_id, as.data.table)
main_trib_all_id <- future_mapply(mapply_fun,main_trib_all_id,names(main_trib_all_id),SIMPLIFY = F)

### Merge as datatable
main_trib_all_id <- rbindlist(main_trib_all_id)
names(main_trib_all_id)[1] <- "stream"
setkey(main_trib_all_id, stream)

main_trib_all_id$stream <- as.numeric(main_trib_all_id$stream)
main_trib_all_id$PATH_ID <- as.numeric(main_trib_all_id$PATH_ID)



### Remove the main stream ID
# main_trib_all_id <- main_trib_all_id[main_trib_all_id$stream %ni% main_stream_branch_id ,]

### At which position are the tributaries? Use the ordered main stream
main_dt <- data.table(stream=as.numeric(main_stream),
                      seq_id=seq.int(1:length(main_stream)))
main_dt$PATH_ID <- main_dt$stream # copy for join only


### Get tributary position along main stream
# trib_order <- flow_acc_per_PATH_ID_trib[1:4,1] # done above
trib_order <- main_dt[trib_order, on="PATH_ID"]
setorder(trib_order, seq_id)
### In case of less than four tributaries
trib_order <- na.omit(trib_order)


###---- Assign Pfafstetter code -----
cat("Assigning the Pfafstetter codes....", "\n")
### Generate the sequence - can be shorter for small basins
tmp_odd <- seq(1, 7, 2)
tmp_even <- seq(2, 8, 2)

### Insert the tributary and interbasin code. Note that interbasin here is simply inserted as same position (as in the end, this specific main stream ID will be an interbasin, not belonging to the tributary)
N_TRIBUTARIES <- length(unique(trib_order$PATH_ID))
trib_order$code_trib <- tmp_even[1:N_TRIBUTARIES]

N_INTERBASIN <- nrow(trib_order)
trib_order$code_inter <-  tmp_odd[1:N_INTERBASIN]

### Attach to the main stream table
tmp_names <- c("PATH_ID", "code_trib", "code_inter")
main_trib_all_id <-  trib_order[,..tmp_names] [main_trib_all_id, on="PATH_ID"]
setnames(main_trib_all_id, "code_trib", "code") # change name to match other table


### Insert the position of tributaries in main_dt
setkey(main_dt, seq_id)
main_dt <- trib_order[,..tmp_names] [main_dt, on="PATH_ID"]

### Add last odd upstream code manually in case it's missing (only if 4 or less stream ID present --> there can't be a "9"
if (max(main_dt$code_trib, na.rm=T)<=6) {
  main_dt$code_inter <- ifelse(!is.na(main_dt$PATH_ID) & is.na(main_dt$code_inter), max(main_dt$code_inter, na.rm=T)+2, main_dt$code_inter)
}

### Fill in the interbasin Pfafstetter code along the main stream (ordered from down- to upstream)
main_dt <- as.data.table(main_dt %>% fill(code_inter, .direction = c("up")))

###--- Assign the "9" code = the upstream part of the main stream ----
main_dt$code <- ifelse(is.na(main_dt$code_inter), 9, main_dt$code_inter)



###--- Get all stream ID of the interbasins -----
### Get a network without tributary streams, and without the main stream ID to which the tributaries connect (=need to break the network)
### Delete the tributary streams ID and the branching stream ID from g = limit the subcomponent search
delete_these <- append(as.character(main_trib_all_id$stream), as.character(trib_order$stream) )
delete_these <- unique(delete_these)
# g_del_for_interbas <- delete_edges(g, V(g)[paste(delete_these)]) #555555555555555555555555 uncomment


### TEST
# delete_these %in% as_ids(V(g))
# g_del_for_interbas <- delete_edges(g, as.numeric(delete_these))
g_del_for_interbas <- delete_vertices(g, V(g)[paste(delete_these)])
### END TEST





# work-around
##############################################################
# g_del_for_interbas <- g - V(g)[delete_these]
##############################################################

### Get the most downstream interbasin stream reach for which to search
first_interbas_id <- main_dt[match(unique(main_dt$code), main_dt$code),]
names(first_interbas_id)[1] <- "first_interbas_id"

###---- Find all streams belonging to the interbasins ----
first_interbas_id_char <- as.character(first_interbas_id$first_interbas_id)

5555555555555555555555555555
first_interbas_id_char %ni% as_ids(V(g_del_for_interbas))


tmp <- future_sapply(first_interbas_id_char, function(x) subcomponent(g_del_for_interbas, x, mode = c("in")))
### Get into datatable format
tmp <- future_lapply(tmp, function(x) names(x))
tmp <- future_lapply(tmp, as.data.table)
tmp <- future_mapply(mapply_fun,tmp,names(tmp),SIMPLIFY = F)

### Merge as datatable
dt3 <- rbindlist(tmp)
names(dt3)[1] <- "stream"
setnames(dt3, "PATH_ID", "first_interbas_id")

dt3$stream <- as.numeric(as.character(dt3$stream))
dt3$first_interbas_id <- as.numeric(as.character(dt3$first_interbas_id))
setkey(dt3, stream)


### Correct the subcomponents: if two interbasin-IDs share the same vertex, the ID located lower will find the same subcomponent as the one located more upstream. Keep only those that belong to the upstream-ID
tmp_adj <- adjacent_vertices(g_del_for_interbas, first_interbas_id_char, mode = c("in")) # mode = c("out", "in", "all", "total"))
tmp_adj <- lapply(tmp_adj, function(x) names(x))
tmp_adj <- lapply(tmp_adj, as.data.table)
tmp_adj <- mapply(mapply_fun,tmp_adj,names(tmp_adj),SIMPLIFY = F)
tmp_adj <- rbindlist(tmp_adj)
tmp_adj_names <- c("stream_in_catchment", "PATH_ID")
setnames(tmp_adj, names(tmp_adj), tmp_adj_names)
tmp_adj$stream_in_catchment <- as.numeric(as.character(tmp_adj$stream_in_catchment))
tmp_adj$PATH_ID <- as.numeric(as.character(tmp_adj$PATH_ID))

### Add the information of stream position
tmp_names <- c("PATH_ID", "seq_id")
tmp_adj <- main_dt[,..tmp_names] [tmp_adj, on="PATH_ID"]

### If on the same row, it means that it's duplicated
### Replace the catchmant (or subcomponent) stream IDs with the main outlet ID
fix_these <- tmp_adj[tmp_adj$stream_in_catchment %in% tmp_adj$PATH_ID,]$PATH_ID
## --> these should have not and cachment streams, but only by themselves


dt3$fix_these <- ifelse(dt3$first_interbas_id %in% fix_these, NA, 0)
dt3$fix_these <- ifelse(dt3$stream %in% fix_these, 0, dt3$fix_these) # take out the main stream reach itself, keep it
### Remove all rows where the lower intervbasin main stream id points to the catchment stream IDs (i.e., these belong to the next Pfaf-basin...)
dt3 <- na.omit(dt3)
dt3$fix_these=NULL



### Get the Pfafstetter code for these interbasin subcomponent streams
tmp_names <- c("first_interbas_id", "code")
dt3 <- first_interbas_id[,..tmp_names] [dt3, on="first_interbas_id"]
### Add the branching stream ID as well - not needed, done in next step...

### Delete duplicate main stream ID in the interbasin stream IDs
dt3 <- dt3[dt3$stream %ni% main_dt$stream,]


###---- Add the tributary stream and interbasin id to the final table ------
tmp_names <- c("stream", "code")

all_stream_code <- rbind(main_trib_all_id[main_trib_all_id$stream %ni% trib_order$stream,][,..tmp_names],
                         main_dt[,..tmp_names],
                         dt3[,..tmp_names])

## Check if still duplicated
all_stream_code <- all_stream_code[!duplicated(all_stream_code),]
setorder(all_stream_code, stream)
all_stream_code <- all_stream_code[, lapply(.SD, as.integer)]
# all_stream_code[duplicated(all_stream_code$stream),] # check
# sort(unique(all_stream_code$code)) # check

cat("Creating Pfafstetter basin raster...", "\n")


recl_raster <- paste0(DIR, "/pfaf_raster.tif")

# stream_raster <- paste0(DIR, "/stream_raster.tif")
stream_raster <- paste0(DIR, "/sub_catchment_h34v10.tif")
# stream_raster <- paste0(DIR, "/segment_h34v10.tif")

pfaf_rast <- reclass_raster(data = all_stream_code,
                            rast_val = "stream",
                            new_val = "code",
                            raster_layer = stream_raster,
                            recl_layer = recl_raster,
                            read = TRUE)

cat("Pfafstetter basin delineation done", "\n")
stopCluster(cl)




return() raster layer, table, or graph, or all
