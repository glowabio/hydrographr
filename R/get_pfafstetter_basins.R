#' Get Pfafstetter sub-basins
#'
#' Subset a basin or catchment into up to nine smaller sub-basins following the
#' Pfafstetter basin delineation scheme. The functions takes a network graph as
#' the input and splits it into smaller sub-basins following a
#' hierarchical topological coding scheme (see Verdin & Verdin (1999) for
#' details), using the flow accumulation as the basis. The user has to define the
#' sub-catchment (stream segment) ID that serves as the outlet of the basin.
#' Note that this can be any stream segment that has an upstream catchment.
#' The input graph can be created with \code{\link{read_geopackage()}} and
#' \code{\link{get_catchment_graph()}}.
#'
#' @param g igraph object. A directed graph of a basin with one outlet.
#' The outlet can be any stream / sub-catchment for which the upstream basin
#' should be split into smaller sub-basins. The input graph can be created with
#' \code{\link{read_geopackage()}} and \code{\link{get_catchment_graph()}}.
#'
#' @param subc_raster character. Full path to the sub-catchment raster file of
#' the basin. Does not need to be cropped / masked to the basin, but the IDs
#' of the sub-catchments need to match with those in the input graph.
#' @param data_table Logical. If TRUE, then the result will be loaded into R
#' as a 2-column data.table (sub-catchment ID and Pfafstetter code). If FALSE,
#' the result is loaded as a raster (terra object) in R and written to disk.
#' Default is FALSE.
#' @param out_dir character. The path of the output directory where the
#' Pfafstetter raster layer will be written. Only needed when data.table=FALSE.
#' @param file_name character. The filename and extension of the Pfafstetter
#' raster layer (e.g. 'pfafstetter_raster.tif"). Only needed when
#' data.table=FALSE.
#' @param n_cores numeric. Number of cores used for parallelisation. Default is
#' NULL (= detectCores(logical=FALSE)-1). Optional.
#'
#' @returns Either a data.table, or a raster (terra object) loaded into R. In
#' case the result is a raster, then a .tif file is written to disk.
#'
#' @importFrom data.table setDT rbindlist setorder setnames as.data.table setkey
#' @importFrom foreach getDoParWorkers %dopar%
#' @importFrom parallel detectCores stopCluster makePSOCKcluster
#' @importFrom dplyr mutate
#' @importFrom igraph graph.data.frame is_directed subcomponent V degree
#' all_simple_paths as_ids delete_edges delete_vertices adjacent_vertices gsize
#' @importFrom future.apply future_lapply  future_mapply future_sapply
#' @importFrom tidyr fill
#' @importFrom doParallel registerDoParallel
#' @importFrom doFuture registerDoFuture
#' @importFrom future plan multisession multicore sequential
#' @importFrom terra rast expanse
#' @export
#'
#' @author Sami Domisch
#'
#' @references Verdin, K.L. & Verdin, J.P. (1999). A topological system
#' for delineation and codification of the Earth’s river basins. Journal
#' of Hydrology, 218(1-2), 1-12. doi:10.1016/s0022-1694(99)00011-6
#'
#' @seealso
#' \code{\link{read_geopackage()}} and \code{\link{get_catchment_graph.()}} to
#' create the input graph.
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Import the stream network as a graph
#' # Load stream network as a graph
#' my_graph <- read_geopackage(gpkg = paste0(my_directory,
#'                                          "/hydrography90m_test_data",
#'                                          "/order_vect_59.gpkg"),
#'                            import_as = "graph")
#'
#' # Subset the graph such that it contains only one basin. You can use
#' # a random ID, i.e. it does not need to be the real outlet of the basin.
#' g_subset <- get_catchment_graph(g = my_graph,
#'                          subc_id = "513867227",
#'                          use_outlet = FALSE,
#'                          mode = "in",
#'                          as_graph = TRUE)
#'
#' # Specify the sub-catchment raster file
#' subc_raster <- paste0(my_directory,"/hydrography90m_test_data",
#'                      "/subcatchment_1264942.tif")
#'
#' # Specify the output directory
#' out_dir <- my_directory
#'
#' # Calculate the Pfafstetter sub-basins and write the raster layer to disk (
#' # and import into R)
#' pfafstetter <- get_pfafstetter_basins(g = g_subset ,
#'                                       subc_raster = subc_raster,
#'                                       out_dir = out_dir,
#'                                       file_name = "pfafstetter_raster.tif",
#'                                       data_table = FALSE,
#'                                       n_cores = 4)
#'
#'
#' @note You can use the online map at https://geo.igb-berlin.de/maps/351/view
#' to identify an ID of a stream segment (use the "Stream segment ID" layer to
#' the left)
#'



get_pfafstetter_basins <- function(g, subc_raster, out_dir, file_name,
                                   data_table = FALSE, n_cores = NULL) {



  # Check input arguments
  if (class(g) != "igraph")
    stop("Input must be an igraph object.")

  if (!is_directed(g))
    stop("The input graph must be a directed graph.")

  if (missing(subc_raster))
    stop("Please provide the full path to the sub-catchment raster layer of
         the basin.")

  if (missing(data_table))
    stop("Please select the output type with data_table=TRUE or
         data_table=FALSE (output will be a raster layer,")

  if (missing(out_dir) && data_table==FALSE)
    stop("Please provide the full path of the output directory.")

  if (missing(file_name) && data_table==FALSE)
    stop("Please provide the file name of the output file.")


cat("Setting up parallel backend...\n")

  # Setting up parallelization if n_cores is not provided
  if (is.null(n_cores)) {

  #  Detect number of available cores
  n_cores <- detectCores(logical = FALSE) - 1

  }

  # Check parallel backend depending on the OS
  if (get_os() == "windows") {
    plan(multisession, workers = n_cores)
  }

  if (get_os() == "osx" || get_os() == "linux")  {
    plan(multicore, workers = n_cores)
  }

# Foreach
cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)
cat("Using", foreach::getDoParWorkers(), "CPUs...", "\n")

# Avoid exponential numbers in the reclassification
options(scipen=999)

### Get the path number as an additional column in the graph list.vs
mapply_fun <- function(element,name){
  mutate(element,PATH_ID = name)
}

### Fast subsetting by vector elements
"%ni%" <- Negate("%in%")



cat("Preparing the graph...\n")
# Need to re-define the graph to overcome any errors due to different IDs,
# depending on how the graph was initially created
g_dt_tmp <- setDT(igraph::as_data_frame(g))
g <- graph.data.frame(g_dt_tmp[,c("from", "to")], directed = T)
# Get all possible paths from the outlet
outlet = which(degree(g, v = V(g), mode = "out")==0, useNames = T)
#headwater = which(degree(g, v = V(g), mode = "in")==0, useNames = T) # all, slow


### Get a pre-selection of those 100 main stream candidates that have the
### longest distance to the outlet (the most contributing stream is likely to
### to be one of these)
headwater <- g_dt_tmp[order(-out_dist)]$from[1:100]
headwater <- headwater[!is.na(headwater)] # remove NAs if less than 100 subc_id



# Stop if too few stream segments (currently three, need to go lower?)
if (gsize(g_subset)<3) {
  stop("The input basin has too few stream segments / sub-catchments. Please
       select another outlet segment with get_catchment_graph()")
}


# Stop if too many outlets
if (length(outlet)!=1) {
  stop("Only one outlet possible. You may want to run get_catchment_graph()
       to get a single drainage basin.")
}



cat("Calculating the area of single sub-catchments...\n")
# Needs a data-table to assign results
# IDs have to match, hence run this again
streams_dt <- setDT(data.frame(stream=as.numeric(as_ids(V(g)))))

# Attach the area of each stream-reach
tmp_r <- rast(subc_raster)
tmp_scatch_area <- expanse(tmp_r, byValue=T, unit="km")
setDT(tmp_scatch_area)
tmp_scatch_area <- tmp_scatch_area[,-1]
setnames(tmp_scatch_area, c("stream", "area_km2"))
streams_dt <- tmp_scatch_area[streams_dt, on="stream"]




cat("Finding the most contributing stream...", "\n")
### Run only with foreach (future / lapply results in strange results
# for very small basins)

l <- foreach::foreach (i=names(headwater), .inorder=FALSE,
              .packages = "igraph")  %dopar% {
              out <-subcomponent(g, i, mode = c("out"))
              }

### Get into datatable format
names(l) <- names(headwater)
l <- future_lapply(l, as_ids)
l <- future_lapply(l, as.data.table)
### Get the path number as an additional column
l <- future_mapply(mapply_fun,l,names(l),SIMPLIFY = F)
### Merge as datatable
tmp_dt <- rbindlist(l); rm(l)
names(tmp_dt)[1] <- "stream"

### Remove the outlet
setkey(tmp_dt, stream)
tmp_dt <- tmp_dt[!(tmp_dt$stream==as.numeric(names(outlet))),]
tmp_dt$seq_id <- seq.int(1:nrow(tmp_dt))

tmp_dt$stream <- as.numeric(tmp_dt$stream)
tmp_dt$PATH_ID <- as.numeric(tmp_dt$PATH_ID)

### Join
tmp_dt <- streams_dt[tmp_dt, on="stream"] # left join, preserves ordering

### Get maximum flow acc per PATH_ID
sum_flow_acc_PATH_ID <- tmp_dt[,.(sum_flow_acc=sum(area_km2)), by="PATH_ID"]

### Identify the main stream = max contributing drainage
setorder(sum_flow_acc_PATH_ID, -sum_flow_acc)
main_headwater_id <- sum_flow_acc_PATH_ID[1]$PATH_ID # headwater tip ID


### Get the single stream reaches of the main stream
# edges, ordered from down-to upstream
main_stream <- all_simple_paths(g, from = outlet,
                                to = as.character(main_headwater_id), "in")

main_stream <- as_ids(unlist(main_stream[[1]]))
# remove the first item, which is the outlet
main_stream <- main_stream[-1]
rm(list=ls(pattern="^tmp_")) # cleanup
rm(g_dt_tmp, sum_flow_acc_PATH_ID, headwater)
gc()

###---------------------------------------------------------------------#
###--- Get up to 4 most contributing tributaries along main stream -----
###---------------------------------------------------------------------#

cat("Finding the next 4 most contributing tributaries....", "\n")
# Delete the edges of the main stream (not the vertices, else any single
# stream reaches, i.e. stand-alone connected to main stream, yield an error
# as there is nothing to connect to)

# requires the outlet to be removed
g_del <- delete_edges(g, V(g)[paste(main_stream)])

# From each main stream vertex, walk to the headwaters to get entire tributary
l2 <- future_sapply(as.character(main_stream),
                    function(x) subcomponent(g_del, x, mode = c("in")))
# Get into datatable format
l2 <- future_lapply(l2, function(x) names(x))
l2 <- future_lapply(l2, as.data.table)
l2 <- future_mapply(mapply_fun,l2,names(l2),SIMPLIFY = F)

### Merge as datatable
dt2 <- rbindlist(l2)
names(dt2)[1] <- "stream"
setkey(dt2, stream)
# Convert to character IDs
dt2$stream <- as.numeric(dt2$stream)
dt2$PATH_ID <- as.numeric(dt2$PATH_ID)

### Remove the main stream ID from each subcomponent
dt2 <- dt2[dt2$stream %ni% as.numeric(main_stream) ,]

### Join
dt2 <- streams_dt[dt2, on="stream"] # left join, preserves ordering

### Get maximum flow acc per PATH_ID
flow_acc_per_PATH_ID_trib <- dt2[,.(sum_flow_acc=sum(area_km2)), by="PATH_ID"]

### Identify the top 4 tributaries flowing into main stream
setorder(flow_acc_per_PATH_ID_trib, -sum_flow_acc)





###--- Identify the stream ID of the contributing tributaries ----
# Get the main stream id at the branches = the position
trib_order <- flow_acc_per_PATH_ID_trib[1:4,1]
main_stream_branch_id <- na.omit(trib_order) # could be less than four
main_stream_branch_id <- as.character(t(main_stream_branch_id))


# Remove IDs of single streams belonging to the entire tributary
# (note: main stream is deleted in g_del)
main_trib_all_id <- future_sapply(main_stream_branch_id,
                          function(x) subcomponent(g_del, x, mode = c("in")))

### Get into datatable format
main_trib_all_id <- future_lapply(main_trib_all_id, function(x) names(x))
main_trib_all_id <- future_lapply(main_trib_all_id, as.data.table)
main_trib_all_id <- future_mapply(mapply_fun,main_trib_all_id,
                                  names(main_trib_all_id),SIMPLIFY = F)

### Merge as datatable
main_trib_all_id <- rbindlist(main_trib_all_id)
names(main_trib_all_id)[1] <- "stream"
setkey(main_trib_all_id, stream)

main_trib_all_id$stream <- as.numeric(main_trib_all_id$stream)
main_trib_all_id$PATH_ID <- as.numeric(main_trib_all_id$PATH_ID)



# Remove the main stream ID
# At which position are the tributaries? Use the ordered main stream
main_dt <- data.table(stream=as.numeric(main_stream),
                      seq_id=seq.int(1:length(main_stream)))
main_dt$PATH_ID <- main_dt$stream # copy for join only

# Get tributary position along main stream
trib_order <- main_dt[trib_order, on="PATH_ID"]
setorder(trib_order, seq_id)
### In case of less than four tributaries
trib_order <- na.omit(trib_order)


###---- Assign Pfafstetter code -----
cat("Assigning the Pfafstetter codes....", "\n")
### Generate the sequence - can be shorter for small basins
tmp_odd <- seq(1, 7, 2)
tmp_even <- seq(2, 8, 2)

### Insert the tributary and interbasin code. Note that interbasins are is
# simply inserted at the same position (as in the end, this specific main
# stream ID will be an interbasin, not belonging to the tributary)
N_TRIBUTARIES <- length(unique(trib_order$PATH_ID))
trib_order$code_trib <- tmp_even[1:N_TRIBUTARIES]

N_INTERBASIN <- nrow(trib_order)
trib_order$code_inter <-  tmp_odd[1:N_INTERBASIN]

# Attach to the main stream table
tmp_names <- c("PATH_ID", "code_trib", "code_inter")
main_trib_all_id <-  trib_order[,..tmp_names] [main_trib_all_id, on="PATH_ID"]
# change name to match other table
setnames(main_trib_all_id, "code_trib", "code")


### Insert the position of tributaries in main_dt
setkey(main_dt, seq_id)
main_dt <- trib_order[,..tmp_names] [main_dt, on="PATH_ID"]

### Add last odd upstream code manually in case it's missing
# (only if 4 or less stream ID present --> there can't be a "9"
if (max(main_dt$code_trib, na.rm=T)<=6) {
  main_dt$code_inter <- ifelse(!is.na(main_dt$PATH_ID) &
                                 is.na(main_dt$code_inter),
                               max(main_dt$code_inter, na.rm=T)+2,
                               main_dt$code_inter)
                                        }

# Fill in the interbasin Pfafstetter code along the main stream
# (ordered from down- to upstream)
main_dt <- as.data.table(main_dt %>% fill(code_inter, .direction = c("up")))

# Assign the "9" code = the upstream part of the main stream
main_dt$code <- ifelse(is.na(main_dt$code_inter), 9, main_dt$code_inter)


#--- Get all stream ID of the interbasins -----
### Get a network without tributary streams, and without the main stream ID
# to which the tributaries connect (=need to break the network)
# Delete the tributary streams ID and the branching stream ID
# from g = limit the subcomponent search
delete_these <- append(as.character(main_trib_all_id$stream),
                       as.character(trib_order$stream) )
delete_these <- unique(delete_these)

g_del_for_interbas <- igraph::delete_vertices(g, V(g)[paste(delete_these)])

# Get the most downstream interbasin stream reach for which to search
first_interbas_id <- main_dt[match(unique(main_dt$code), main_dt$code),]
names(first_interbas_id)[1] <- "first_interbas_id"

# Find all streams belonging to the interbasins
first_interbas_id_char <- as.character(first_interbas_id$first_interbas_id)
tmp <- future_sapply(first_interbas_id_char,
                     function(x) subcomponent(g_del_for_interbas, x,
                                              mode = c("in")))
# Get into datatable format
tmp <- future_lapply(tmp, function(x) names(x))
tmp <- future_lapply(tmp, as.data.table)
tmp <- future_mapply(mapply_fun,tmp,names(tmp),SIMPLIFY = F)

# Merge as datatable
dt3 <- rbindlist(tmp)
names(dt3)[1] <- "stream"
setnames(dt3, "PATH_ID", "first_interbas_id")

dt3$stream <- as.numeric(dt3$stream)
dt3$first_interbas_id <- as.numeric(dt3$first_interbas_id)
setkey(dt3, stream)


# Correct the subcomponents: if two interbasin-IDs share the same vertex,
# the ID located lower will find the same subcomponent as the one located
# more upstream. Keep only those that belong to the upstream-ID
tmp_adj <- adjacent_vertices(g_del_for_interbas,
                             first_interbas_id_char, mode = c("in"))
tmp_adj <- lapply(tmp_adj, function(x) names(x))
tmp_adj <- lapply(tmp_adj, as.data.table)
tmp_adj <- mapply(mapply_fun,tmp_adj,names(tmp_adj),SIMPLIFY = F)
tmp_adj <- rbindlist(tmp_adj)
tmp_adj_names <- c("stream_in_catchment", "PATH_ID")
setnames(tmp_adj, names(tmp_adj), tmp_adj_names)
tmp_adj$stream_in_catchment <- as.numeric(tmp_adj$stream_in_catchment)
tmp_adj$PATH_ID <- as.numeric(tmp_adj$PATH_ID)

### Add the information of stream position
tmp_names <- c("PATH_ID", "seq_id")
tmp_adj <- main_dt[,..tmp_names] [tmp_adj, on="PATH_ID"]

# If on the same row, it means that it's duplicated
# Replace the catchmant (or subcomponent) stream IDs with the main outlet ID
fix_these <- tmp_adj[tmp_adj$stream_in_catchment %in% tmp_adj$PATH_ID,]$PATH_ID

dt3$fix_these <- ifelse(dt3$first_interbas_id %in% fix_these, NA, 0)
# take out the main stream reach itself, keep it
dt3$fix_these <- ifelse(dt3$stream %in% fix_these, 0, dt3$fix_these)
### Remove all rows where the lower interbasin main stream id points to the
# catchment stream IDs (i.e., these belong to the next Pfaf-basin...)
dt3 <- na.omit(dt3)
dt3$fix_these=NULL


### Get the Pfafstetter code for these interbasin subcomponent streams
tmp_names <- c("first_interbas_id", "code")
dt3 <- first_interbas_id[,..tmp_names] [dt3, on="first_interbas_id"]
### Add the branching stream ID as well - not needed, done in next step...

### Delete duplicate main stream ID in the interbasin stream IDs
dt3 <- dt3[dt3$stream %ni% main_dt$stream,]


# Add the tributary stream and interbasin id to the final table
tmp_names <- c("stream", "code")

all_stream_code <- rbind(main_trib_all_id[main_trib_all_id$stream %ni%
                                            trib_order$stream,][,..tmp_names],
                                            main_dt[,..tmp_names],
                                            dt3[,..tmp_names])

## Check if still duplicated
all_stream_code <- all_stream_code[!duplicated(all_stream_code),]
setorder(all_stream_code, stream)
all_stream_code <- all_stream_code[, lapply(.SD, as.integer)]

# Return data.table or raster
if (data_table==TRUE) {
  return(all_stream_code)
} else {
cat("Creating Pfafstetter basin raster...", "\n")
# Reclassify the sub-catchment raster and get the map
pfafstetter_raster <- reclass_raster(data = all_stream_code,
                            rast_val = "stream",
                            new_val = "code",
                            raster_layer = subc_raster,
                            recl_layer = paste0(out_dir, "/", file_name),
                            read = TRUE)


cat("Pfafstetter basin layer written to", paste0(out_dir, "/", file_name), "\n")
return(pfafstetter_raster)

 }
cat("Pfafstetter basin delineation done", "\n")
stopCluster(cl)
plan(sequential)
}


