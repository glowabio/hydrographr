#' Delineate Pfafstetter basins
#'
#' Using a graph object, subset the input catchment into smaller, so-called Pfafstetter basins.
#'
#' The function calculates internally the area of each sub-catchment
#'
#' @param g A directed graph with one outlet
#' @param outlet A different, user-specified (upstream) outlet
#' @importFrom data.table ..
#' @importFrom processx run
#' @export
#'





## TESTING
path <- "D:/projects/hydrographr/hydrographr_data"
setwd(path)
## END TESTING

!!! the graph and table need to match, else the IDs are not found during the graph-subsetting !!!

todo:
- check if could run on any given outlet, as long as the datatable is created from this graph?
- implement GRASS functions

-



# not yet implemented: threshold for small/large basins to avoid splitting, or prefer splitting sooner
# input can be a graph or table
# users can specify n_cores?
# is moveme-function needed? if yes, check citation



g <- read_geopackage("order_vect_59.gpkg", graph=T)
g <- get_largest_catchment(g)

g <- as.data.table(as_data_frame(g))
names(g)[1:2] <- c("stream", "next_stream")

# streams_dt <- read_geopackage("order_vect_59.gpkg")


### Create graph based on two columns only
g_cols <- c("stream", "next_stream")
g <- graph.data.frame(g[,..g_cols], directed = T)

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
streams_dt <- as.data.table(as_long_data_frame(g))
setnames(streams_dt, c("ver[el[, 1], ]"), c("stream"))
streams_dt <- streams_dt[,"stream"]
streams_dt$stream <- as.numeric(as.character(streams_dt$stream))
# Calculate the area of each sub-catchment (which is more accurate than the flow_accum)






# start GRASS session
# server3
export DIR=/data/domisch/hydrographr_data
grass78  -text -c -e   subcatchment_1264942.tif  $DIR/grass_location # rectangle
# grass78  -text -c -e   subc_1264942.tif   $DIR/grass_location
export GRASSEXEC="grass78 $DIR/grass_location/PERMANENT/ --exec" # create alias

$GRASSEXEC  r.in.gdal  input=$DIR/subcatchment_1264942.tif  output=scatch --o
$GRASSEXEC  r.stats input=scatch  output=$DIR/sub_catchment_area_stats.txt  separator=tab  -aclnC --o # rectangle
# $GRASSEXEC  r.stats input=$DIR/subc_1264942.tif output=$DIR/sub_cachment_area_stats.txt  separator=tab  -aclnC --o






### Attach the area of each stream-reach
scatch_area <- fread("sub_catchment_area_stats.txt", h=F)
setnames(scatch_area, c("stream", "V2", "area_km2", "area_cells")) # still in m2
scatch_area$area_km2 <- round(scatch_area$area_km2 / 1000000, 4) # convert to km2
scatch_area$V2 <- NULL
### Merge to stream table --> keeps only those in the input graph, tosses the rectangle
streams_dt <- scatch_area[streams_dt, on="stream"]




### Get all possible paths from the outlet
# find root and leaves --> vertices
outlet = which(degree(g, v = V(g), mode = "out")==0, useNames = T)
headwater = which(degree(g, v = V(g), mode = "in")==0, useNames = T)

# Stop if too many outlets
if (length(outlet)!=1) {
  stop("Only one outlet possible. You may want to run get_largest_catchment() to get a single drainage basin as the input graph, or you use the same input graph but specify the outlet using outlet=your_segment_id. ")
}


### 3 options depending on the amount of headwaters, all return the identical objects
### Several options as else some methods fail for very few headwaters...

cat("Finding the main stream....\n")
### Run without foreach if only small basin (otherwise the split might not work for very small basins)

registerDoFuture()
plan(multisession, workers = n_cores) # Parallelize

### For very small catchments run subcomponent sequentially
if (length(headwater) < 10) {

  l <- foreach (i=names(headwater), .inorder=T)  %dopar% {
    # print(i)
    out <-subcomponent(g, i, mode = c("out"))
    # out <- list(out)
    # names(out) <- i
    return(out)
  } # close foreach

  ### Get into datatable format
  names(l) <- names(headwater)
  # l <- future_lapply(l, function(x) names(x))
  # names(l) <- seq.int(1:length(l)) # specify names of list elements = connected_to
  l <- future_lapply(l, as_ids)
  l <- future_lapply(l, as.data.frame)
  ### Get the path number as an additional column
  l <- future_mapply(mapply_fun,l,names(l),SIMPLIFY = F)
  ### Merge as datatable
  dt <- rbindlist(l); rm(l)
  names(dt)[1] <- "stream"

  ### Remove the outlet
  setkey(dt, stream)
  dt <- dt[!(dt$stream==as.numeric(names(outlet))),]
  dt$seq_id <- seq.int(1:nrow(dt))

  dt$stream <- as.numeric(as.character(dt$stream))
  dt$connected_to <- as.numeric(as.character(dt$connected_to))

  ### Join
  dt <- streams_dt[dt, on="stream"] # left join, preserves ordering

  ### Get maximum flow acc per connected_to
  sum_flow_acc_connected_to <- dt[,.(sum_flow_acc=sum(area_km2)), by="connected_to"]

  ### Identify the main stream = max contributing drainage
  setorder(sum_flow_acc_connected_to, -sum_flow_acc)
  main_headwater_id <- sum_flow_acc_connected_to[1]$connected_to # headwater tip ID
  rm(dt); gc()


  ### For medium sized run with future_sapply
} else if (length(headwater) >=10 & length(headwater) < 1000) {

  l <- future_sapply(names(headwater), function(x) subcomponent(g, x, mode = c("out")))

  ### Get into datatable format
  l <- future_lapply(l, function(x) names(x))
  # names(l) <- seq.int(1:length(l)) # specify names of list elements = connected_to
  l <- future_lapply(l, as.data.frame)
  ### Get the path number as an additional column
  l <- future_mapply(mapply_fun,l,names(l),SIMPLIFY = F)
  ### Merge as datatable
  dt <- rbindlist(l); rm(l)
  names(dt)[1] <- "stream"

  ### Remove the outlet
  setkey(dt, stream)
  dt <- dt[!(dt$stream==as.numeric(names(outlet))),]
  dt$seq_id <- seq.int(1:nrow(dt))

  dt$stream <- as.numeric(as.character(dt$stream))
  dt$connected_to <- as.numeric(as.character(dt$connected_to))

  ### Join
  dt <- streams_dt[dt, on="stream"] # left join, preserves ordering

  ### Get maximum flow acc per connected_to
  sum_flow_acc_connected_to <- dt[,.(sum_flow_acc=sum(area_km2)), by="connected_to"]

  ### Identify the main stream = max contributing drainage
  setorder(sum_flow_acc_connected_to, -sum_flow_acc)
  main_headwater_id <- sum_flow_acc_connected_to[1]$connected_to # headwater tip ID
  rm(dt); gc()


  ### For very large basins create chunks, run future_sapply with foreach
} else {
  ### Split the headwater file and run in parallel
  ### check free CPUs --> NUMBER_CPU X future.apply
  tmp_headwater <- names(headwater)
  tmp <- seq_along(tmp_headwater)
  tmp_chunks <- split(tmp_headwater, ceiling(tmp/2000)); length(tmp_chunks)
  rm(tmp, tmp_headwater); gc()

  registerDoFuture()
  plan(multisession, workers = NUMBER_CPU_FOREACH)
  # batchtools_multicore(workers = NUMBER_CPU_FOREACH) # for batch (background) process ?

  ### Check size of objects
  # data.frame(sort(sapply(ls(),function(x){object.size(get(x))})))


  # for (k in 1:length(tmp_chunks)) {

  ### Show progress bar:
  with_progress({
    p <- progressor(along = 1:length(tmp_chunks) )

    ### Start foreach
    flow_acc_per_connected_to <-
      foreach(mychunk=tmp_chunks, .combine=rbind, .inorder=T,.errorhandling="stop", .verbose=T) %dopar% {
        # mychunk is a list and each list object serves as the task for each worker
        # not needed in doFuture: .packages=c("igraph", "data.table", "future.apply", "dplyr")


        p(sprintf("x=%g", 1:length(tmp_chunks))) # print progress
        # cat("Running chunk", i, "of", length(tmp_chunks), "\n")
        plan(multisession, workers = NUMBER_CPU_FUTUREAPPLY)  # need to tell within foreach again...
        # batchtools_multicore(workers = NUMBER_CPU_FUTUREAPPLY)


        ### Find each path from headwater to outlet
        tmp <- future_sapply(mychunk, function(x) subcomponent(g, x, mode = c("out")))

        ### Get into datatable format
        tmp <- future_lapply(tmp, function(x) names(x))
        tmp <- future_lapply(tmp, as.data.frame)
        tmp <- future_mapply(mapply_fun,tmp,names(tmp),SIMPLIFY = F)


        ### Merge as datatable
        tmp <- rbindlist(tmp)
        names(tmp)[1] <- "stream"

        ### Remove the outlet
        setkey(tmp, stream)
        tmp <- tmp[!(tmp$stream==as.numeric(names(outlet))),]

        tmp$stream <- as.numeric(as.character(tmp$stream))
        tmp$connected_to <- as.numeric(as.character(tmp$connected_to))

        ### Join
        tmp <- streams_dt[tmp, on="stream"] # left join, preserves ordering

        ### Get maximum flow acc per connected_to
        sum_flow_acc_connected_to <- tmp[,.(sum_flow_acc=sum(area_km2)), by="connected_to"]

        return(sum_flow_acc_connected_to)
        rm(tmp, sum_flow_acc_connected_to); gc()

      } # close foreach
  }) # close progressr

  # stopCluster(cl) # stop parallel backend
  plan(sequential, .cleanup = T)


  ### Identify the main stream = max contributing drainage
  setorder(flow_acc_per_connected_to, -sum_flow_acc)
  main_headwater_id <- flow_acc_per_connected_to[1]$connected_to

} # close if

### Write to disk
# write.table(main_headwater_id, paste0(DIR, "/main_headwater_id.txt"), row.names=F, col.names=F, quote=F)

### Read table (in case it crashed...)
# main_headwater_id <- read.table(paste0(DIR, "/main_headwater_id.txt"))$V1



# } # close if: Read the main headwater id if already produced



### Get the single stream reaches of the main stream
main_stream <- all_simple_paths(g, from = outlet, to = as.character(main_headwater_id), "in") # edges, ordered from down-to upstream
# main_stream <- future_lapply(main_stream, function(x) x[-1]) # remove the "-1", not a stream reach
main_stream <- as_ids(unlist(main_stream[[1]]))

main_stream <- main_stream[-1] # remove the first item, which is the outlet (in first round "-1", due to "next stream" column)

rm(list=ls(pattern="^tmp_")) # cleanup




###---------------------------------------------------------------------#
###--- Get up to 4 most contributing tributaries along main stream -----
###---------------------------------------------------------------------#


### Skip if the four main branches are defined in a previous run saved on disk:
# if(!file.exists(paste0(DIR, "/main_stream_branch_id.txt")) && LEVEL==1) {





cat("Finding the next 4 most contributing tributaries....", "\n")
### Delete the edges of the main stream (not the vertices, else any single stream reaches, i.e. stand-alone connected to main stream, yield an error as there is nothing to connect to)
g_del <- delete_edges(g, V(g)[paste(main_stream)])
# plot_net(g_del)


### From each main stream vertex, walk to the headwaters to get the entire tributary
## (note: previous version was finding only main tributary. Entire tributary is faster and allows to re-code
## the streams_dt for the next-smaller division, without GRASS)

if(length(main_stream) < 1000) {

  l2 <- future_sapply(as.character(main_stream), function(x) subcomponent(g_del, x, mode = c("in")))

  ### Get into datatable format
  l2 <- future_lapply(l2, function(x) names(x))
  # l2 <- future_sapply(l2, as_ids)
  l2 <- future_lapply(l2, as.data.frame)
  l2 <- future_mapply(mapply_fun,l2,names(l2),SIMPLIFY = F)

  ### Merge as datatable
  dt2 <- rbindlist(l2)
  names(dt2)[1] <- "stream"
  setkey(dt2, stream)

  dt2$stream <- as.numeric(as.character(dt2$stream))
  dt2$connected_to <- as.numeric(as.character(dt2$connected_to))


  ### Remove the main stream ID from each subcomponent
  dt2 <- dt2[dt2$stream %ni% as.numeric(as.character(main_stream)) ,]

  ### Join
  dt2 <- streams_dt[dt2, on="stream"] # left join, preserves ordering

  ### Get maximum flow acc per connected_to
  flow_acc_per_connected_to_trib <- dt2[,.(sum_flow_acc=sum(area_km2)), by="connected_to"]

  ### Identify the major tributaries flowing in to main stream = max contributing drainage
  setorder(flow_acc_per_connected_to_trib, -sum_flow_acc)

  ### this idemntifies the top 4 tributaries



} else {

  tmp_main_stream <- as.character(main_stream)
  tmp <- seq_along(tmp_main_stream)
  tmp_chunks <- split(tmp_main_stream, ceiling(tmp/500)); length(tmp_chunks)
  rm(tmp, tmp_main_stream); gc()


  registerDoFuture()
  plan(multisession, workers = NUMBER_CPU_FOREACH)

  ### Show progress bar:
  with_progress({
    p <- progressor(along = 1:length(tmp_chunks) )

    ### Start foreach
    flow_acc_per_connected_to_trib <-
      foreach(mychunk=tmp_chunks, .combine=rbind, .inorder=T,.errorhandling="stop", .verbose=T) %dopar% {
        # mychunk is a list and each list object serves as the task for each worker
        # not needed in doFuture: .packages=c("igraph", "data.table", "future.apply", "dplyr")


        p(sprintf("x=%g", 1:length(tmp_chunks))) # print progress
        # cat("Running chunk", i, "of", length(tmp_chunks), "\n")
        plan(multisession, workers = NUMBER_CPU_FUTUREAPPLY)  # need to tell within foreach again...

        mychunk <- unlist(mychunk, use.names = F)
        tmp <- future_sapply(mychunk, function(x) subcomponent(g_del, x, mode = c("in")))

        ### Get into datatable format
        tmp <- future_lapply(tmp, function(x) names(x))
        tmp <- future_lapply(tmp, as.data.frame)
        tmp <- future_mapply(mapply_fun,tmp,names(tmp),SIMPLIFY = F)

        ### Merge as datatable
        dt2 <- rbindlist(tmp)
        names(dt2)[1] <- "stream"
        setkey(dt2, stream)

        dt2$stream <- as.numeric(as.character(dt2$stream))
        dt2$connected_to <- as.numeric(as.character(dt2$connected_to))


        ### Remove the main stream ID from each subcomponent
        dt2 <- dt2[dt2$stream %ni% main_stream,]

        ### Join
        dt2 <- streams_dt[dt2, on="stream"] # left join, preserves ordering

        ### Get maximum flow acc per connected_to
        res <- dt2[,.(sum_flow_acc=sum(area_km2)), by="connected_to"]

        return(res)
        rm(res, tmp, dt2); gc()

      } # close foreach
  }) # close progressr

  # stopCluster(cl) # stop parallel backend
  plan(sequential, .cleanup = T)


  ### Identify the major tributaries flowing in to main stream = max contributing drainage
  setorder(flow_acc_per_connected_to_trib, -sum_flow_acc)



} # close if



### Save to disk (not needed to save space..)
# write.table(flow_acc_per_connected_to_trib, paste0(DIR, "/flow_acc_per_connected_to_trib.txt"), row.names=F, col.names=F, quote=F)




###--------------------------------------------------------------------#
###--- Identify the stream ID of the contributing tributaries ----
###--------------------------------------------------------------------#

### Get the main stream id at the branches = the position
trib_order <- flow_acc_per_connected_to_trib[1:4,1]
main_stream_branch_id <- na.omit(trib_order) # could be less than four
main_stream_branch_id <- as.character(t(main_stream_branch_id))


###------Save intermediate output in txt file ------------
res_ID=data.table(MAIN_HEADWATER_ID=main_headwater_id,
                  MAIN_STR_BRANCH_ID1=main_stream_branch_id[1],
                  MAIN_STR_BRANCH_ID2=main_stream_branch_id[2],
                  MAIN_STR_BRANCH_ID3=main_stream_branch_id[3],
                  MAIN_STR_BRANCH_ID4=main_stream_branch_id[4])

# if(ITERATOR==0) { # if first round
#   write.table(res_ID, paste0(DIR, "/lbasinID_", MACROBASIN_ID, "_intermediate_IDs.txt"), row.names = F, col.names = T, quote = F)
# } else {
#   # if(Sys.info()[["sysname"]]=="Windows") {
#   write.table(res_ID, paste0(DIR, "/lbasinID_", MACROBASIN_ID, "_intermediate_IDs.txt"), row.names = F, col.names = F, quote = F,append=TRUE)
# }




### When only few headwaters, subcomponent() gives strange results --> use sequential code in foreach below
if (length(headwater)>50) {
  ### Get all id of single streams belonging to the entire tributary (note: main stream is deleted in g_del)
  main_trib_all_id <- future_sapply(main_stream_branch_id, function(x) subcomponent(g_del, x, mode = c("in")))
  # main_trib_all_id <- subcomponent(g_del, main_stream_branch_id, mode = c("in")) # takes only first argument!


  ### Get into datatable format
  main_trib_all_id <- future_lapply(main_trib_all_id, function(x) names(x))
  main_trib_all_id <- future_lapply(main_trib_all_id, as.data.frame)
  main_trib_all_id <- future_mapply(mapply_fun,main_trib_all_id,names(main_trib_all_id),SIMPLIFY = F)

  ### Merge as datatable
  main_trib_all_id <- rbindlist(main_trib_all_id)
  names(main_trib_all_id)[1] <- "stream"
  setkey(main_trib_all_id, stream)

  main_trib_all_id$stream <- as.numeric(as.character(main_trib_all_id$stream))
  main_trib_all_id$connected_to <- as.numeric(as.character(main_trib_all_id$connected_to))


} else {

  registerDoFuture()

  main_trib_all_id <- foreach (i=main_stream_branch_id, .inorder=T)  %dopar% {
    out <-subcomponent(g_del, i, mode = c("in"))
    # names(out) <- i
    return(out)
  } # close foreach

  ### Get into datatable format
  names(main_trib_all_id) <- main_stream_branch_id # needs this line after the foreach subcomponent()
  main_trib_all_id <- future_lapply(main_trib_all_id, function(x) names(x))
  main_trib_all_id <- future_lapply(main_trib_all_id, as.data.frame)
  main_trib_all_id <- future_mapply(mapply_fun,main_trib_all_id,names(main_trib_all_id),SIMPLIFY = F)

  ### Merge as datatable
  main_trib_all_id <- rbindlist(main_trib_all_id)
  names(main_trib_all_id)[1] <- "stream"
  setkey(main_trib_all_id, stream)

  main_trib_all_id$stream <- as.numeric(as.character(main_trib_all_id$stream))
  main_trib_all_id$connected_to <- as.numeric(as.character(main_trib_all_id$connected_to))


} # close if




### Remove the main stream ID
# main_trib_all_id <- main_trib_all_id[main_trib_all_id$stream %ni% main_stream_branch_id ,]

### At which position are the tributaries? Use the ordered main stream
main_dt <- data.table(stream=as.numeric(as.character(main_stream)),
                      seq_id=seq.int(1:length(main_stream)))
main_dt$connected_to <- main_dt$stream # copy for join only





### Get tributary position along main stream
# trib_order <- flow_acc_per_connected_to_trib[1:4,1] # done above
trib_order <- main_dt[trib_order, on="connected_to"]
setorder(trib_order, seq_id)
### In case of less than four tributaries
trib_order <- na.omit(trib_order)




###---- Assign Pfafstetter code -----
cat("Assigning the Pfafstetter codes.... ", "\n")
### Generate the sequence - can be shorter for small basins
tmp_odd <- seq(1, 7, 2)
tmp_even <- seq(2, 8, 2)

### Insert the tributary and interbasin code. Note that interbasin here is simply inserted as same position (as in the end, this specific main stream ID will be an interbasin, not belonging to the tributary)
N_TRIBUTARIES <- length(unique(trib_order$connected_to))
trib_order$code_trib <- tmp_even[1:N_TRIBUTARIES]

N_INTERBASIN <- nrow(trib_order)
trib_order$code_inter <-  tmp_odd[1:N_INTERBASIN]

### Attach to the main stream table
tmp_names <- c("connected_to", "code_trib", "code_inter")
main_trib_all_id <-  trib_order[,..tmp_names] [main_trib_all_id, on="connected_to"]
setnames(main_trib_all_id, "code_trib", "code") # change name to match other table


### Insert the position of tributaries in main_dt
setkey(main_dt, seq_id)
main_dt <- trib_order[,..tmp_names] [main_dt, on="connected_to"]

### Add last odd upstream code manually in case it's missing (only if 4 or less stream ID present --> there can't be a "9"
if (max(main_dt$code_trib, na.rm=T)<=6) {
  main_dt$code_inter <- ifelse(!is.na(main_dt$connected_to) & is.na(main_dt$code_inter), max(main_dt$code_inter, na.rm=T)+2, main_dt$code_inter)
}

### Fill in the interbasin Pfafstetter code along the main stream (ordered from down- to upstream)
main_dt <- as.data.table(main_dt %>% fill(code_inter, .direction = c("up")))

###--- Assign the "9" code = the upstream part of the main stream ----
main_dt$code <- ifelse(is.na(main_dt$code_inter), 9, main_dt$code_inter)



###--- Get all stream ID of the interbasins -----
### Get a network without tributary streams, and without the main stream ID to which the tributaries connect (=need to break the network)
### Delete the tributary streams ID and the branching stream ID from g --> limit the subcomponent search
delete_these <- append(as.character(main_trib_all_id$stream), as.character(trib_order$stream) )
g_del_for_interbas <- delete_edges(g, V(g)[paste(delete_these)])

# work-around
# g_del_for_interbas <- g - V(g)[paste(delete_these)]


### Get the most downstream interbasin stream reach for which to search

# get_interbas_stream$keep <- ifelse(is.na(get_interbas_stream$code_trib) & !is.na(get_interbas_stream$code), 1, 0)
# interbas_find_streams <- get_interbas_stream[get_interbas_stream$keep==1,]
# interbas_add_later <- get_interbas_stream[get_interbas_stream$keep==0,] # double check, these are the branching IDs
# first_interbas_id <- get_interbas_stream[match(unique(get_interbas_stream$code), get_interbas_stream$code),]

first_interbas_id <- main_dt[match(unique(main_dt$code), main_dt$code),]
names(first_interbas_id)[1] <- "first_interbas_id"

###---- Find all streams belonging to the interbasins ----
first_interbas_id_char <- as.character(first_interbas_id$first_interbas_id)
tmp <- future_sapply(first_interbas_id_char, function(x) subcomponent(g_del_for_interbas, x, mode = c("in")))
### Get into datatable format
tmp <- future_lapply(tmp, function(x) names(x))
tmp <- future_lapply(tmp, as.data.frame)
tmp <- future_mapply(mapply_fun,tmp,names(tmp),SIMPLIFY = F)

### Merge as datatable
dt3 <- rbindlist(tmp)
names(dt3)[1] <- "stream"
setnames(dt3, "connected_to", "first_interbas_id")

dt3$stream <- as.numeric(as.character(dt3$stream))
dt3$first_interbas_id <- as.numeric(as.character(dt3$first_interbas_id))
setkey(dt3, stream)


### Correct the subcomponents: if two interbasin-IDs share the same vertex, the ID located lower will find the same subcomponent as the one located more upstream. Keep only those that belong to the upstream-ID
tmp_adj <- adjacent_vertices(g_del_for_interbas, first_interbas_id_char, mode = c("in")) # mode = c("out", "in", "all", "total"))
tmp_adj <- lapply(tmp_adj, function(x) names(x))
tmp_adj <- lapply(tmp_adj, as.data.frame)
tmp_adj <- mapply(mapply_fun,tmp_adj,names(tmp_adj),SIMPLIFY = F)
tmp_adj <- rbindlist(tmp_adj)
tmp_adj_names <- c("stream_in_catchment", "connected_to")
setnames(tmp_adj, names(tmp_adj), tmp_adj_names)
tmp_adj$stream_in_catchment <- as.numeric(as.character(tmp_adj$stream_in_catchment))
tmp_adj$connected_to <- as.numeric(as.character(tmp_adj$connected_to))

### Add the information of stream position
tmp_names <- c("connected_to", "seq_id")
tmp_adj <- main_dt[,..tmp_names] [tmp_adj, on="connected_to"]

### If on the same row, it means that it's duplicated
### Replace the catchmant (or subcomponent) stream IDs with the main outlet ID
fix_these <- tmp_adj[tmp_adj$stream_in_catchment %in% tmp_adj$connected_to,]$connected_to
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
# all_stream_code[duplicated(all_stream_code$stream),] # check
# sort(unique(all_stream_code$code)) # check


## Run these lines to export the last Pfafstetter basin reclassification for GRASS:
GRASS_format <- rbind(all_stream_code,
                      data.table(stream="*", code="NULL")) # all other streams should be deleted (if any)

GRASS_format$GRASS <- paste0(GRASS_format$stream, " = ", GRASS_format$code)

### Write to disk
write.table(GRASS_format[,"GRASS"], paste0("pfafstetter_for_GRASS_reclass.txt"), row.names = F, col.names = F, quote = F)
rm(list=ls(pattern="^tmp_")) # cleanup
rm(list=ls(pattern="^dt"))
rm(g, g_del, g_del_for_interbas)



# Run reclassification in GRASS



reclass_raster <- function(rast_val, recl_val, rast_path,
                           recl_path, recl_read = TRUE,
                           nodata = -9999, type = "Int32",
                           compress = "DEFLATE", quiet = TRUE)



# start GRASS session
# server3
export DIR=/data/domisch/hydrographr_data
# grass78  -text -c -e   subcatchment_1264942.tif  $DIR/grass_location # rectangle
# grass78  -text -c -e   subc_1264942.tif   $DIR/grass_location
export GRASSEXEC="grass78 $DIR/grass_location/PERMANENT/ --exec" # create alias

### reclassify sub-catchments directly
# $GRASSEXEC  r.in.gdal  input=$DIR/subcatchment_1264942.tif  output=scatch --o
$GRASSEXEC  r.reclass input=scatch   output=pfafstetter_scatch  rules=$DIR/pfafstetter_for_GRASS_reclass.txt --o --q
$GRASSEXEC  r.out.gdal input=pfafstetter_scatch  output=$DIR/pfafstetter_scatch_rast.tif  type=Int32  nodata=-9999  --o  -c  -m  createopt="COMPRESS=LZW,ZLEVEL=9" --q

### reclassify streams and run r.basins
$GRASSEXEC  r.in.gdal input=$DIR/stream_1264942.tif   output=stream  --o
$GRASSEXEC  r.in.gdal input=$DIR/direction_1264942.tif   output=dir  --o
$GRASSEXEC  r.reclass input=stream   output=pfafstetter_stream   rules=$DIR/pfafstetter_for_GRASS_reclass.txt --o --q
$GRASSEXEC  r.stream.basins dir=dir  stream=pfafstetter_stream   basins=pfafstetter_basin  --o --q
$GRASSEXEC  r.out.gdal input=pfafstetter_basin  output=$DIR/pfafstetter_basin_rast.tif  type=Int32  nodata=-9999  --o  -c  -m  createopt="COMPRESS=LZW,ZLEVEL=9" --q
$GRASSEXEC  r.out.gdal input=pfafstetter_stream  output=$DIR/pfafstetter_stream_rast.tif  type=Int32  nodata=-9999  --o  -c  -m  createopt="COMPRESS=LZW,ZLEVEL=9" --q


# delete intermediate files from disk

# Import raster layer into R


cat("Pfafstetter basin delineation done \n")

return() raster layer, table, or graph, or all
