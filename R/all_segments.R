#' All upstream catchments
#'
#' For each segment, find all upstream connected segments and return a data.table.
#'
#' In addition, variables can be selected that should be aggregated, e.g. the upstream landcover. This function can also be used to create the connectivity table for Marxan by using agg_var="length" and agg_type="sum". The resulting table reports the upstream connectivity from each segment, along with the distance to all upstream segments.
#'
#' @param g A directed graph (igraph object).
#' @param agg_var Optional. The stream segment or sub-catchments IDs for which to delineate the upstream drainage area. Can be a single ID or a vector of multiple IDs (c(ID1, ID2, ID3, ...). If empty, then outlets will be used as segment IDs (with outlet=TRUE).
#' @param agg_type Logical. If TRUE, then the outlets of the given network graph will be used as additional input segmentIDs. Outlets will be identified internally as those stream segments that do not have any downstream donnected segment.
#' @param graph Logical. If TRUE then the output will be a new graph or a list of new graphs with the original attributes, If FALSE (the default), then the output will be a new data.table, or a list of data.tables. List objects are named after the segmentIDs.
#' @param n_cores Optional. Specify the number of CPUs for internal parallelization in the case of multiple stream segments / outlets. Defaults to the all available CPUs minus two. In case the graph is very large, and many segments are used as an input, setting n_cores to 1 might might be useful to avoid any RAM errors, while still achieving a fast computation. This is because the large data will be copied to each CPU which might slow things down.
#' @param n_cores Optional. Specify the maximum size of the data passed to the parallel backend in MB. Defaults to 1500 (1.5 GB). Consider a higher value for large study areas (more than one 20°x20° tile).

#'
#' @importFrom future plan
#' @importFrom doFuture registerDoFuture
#' @importFrom parallel detectCores
#' @importFrom data.table setDT setnames
#' @importFrom igraph subcomponent subgraph as_data_frame
#' @importFrom future.apply future_lapply
#' @importFrom memuse Sys.meminfo
#' @export
#'




# todo:
- use quotes or no quotes?


usePackage <- function(p){
  if (!is.element(p, installed.packages()[,1])) install.packages(p, dep = TRUE)
  library(p, character.only = TRUE)
}

usePackage("future.apply")
usePackage("doFuture")
usePackage("future.batchtools")
usePackage("dplyr")
usePackage("parallel")


g <- my_catchment

# find_upstream <- function(g, aggregate_var=c("length", "flow_accum"), aggregate_type="sum") {
upstream_segments <- function(g, agg_var=NULL, agg_type=NULL) {

  ### Get the path number as an additional column in the graph list.vs
  mapply_fun <- function(element,name){
    mutate(element,connected_to = name)
  }
  cat("Setting up parallel backend...\n")
  # Set up parallel backend
  n_cores <- detectCores(logical=F)-2
  registerDoFuture()
  plan(multisession, workers = n_cores) # windows / linux

  cat("Finding all upstream segments...\n")
  # Get all subcomponents
  l <- future_lapply(V(g), function(x) subcomponent(g, x, mode = c("out")))
  ### Get into datatable format
  l <- future_lapply(l, function(x) names(x))
  # specify names of list elements = connected_to
  l <- future_lapply(l, as.data.frame)
  ### Get the stream (from) ID as an additional column
  l <- future_mapply(mapply_fun,l,names(l),SIMPLIFY = F)
  ### Merge as datatable
  dt <- rbindlist(l)
  names(dt)[1] <- "stream"
  setkey(dt, stream)

  # If aggregation was defined:
  if(hasArg(agg_var)) {

  cat("Attaching the selected attributes...\n")
  ### Sort and remove the self-connection of the source stream
  dt <- dt[order(stream, connected_to),]
  # dt <- subset(dt, stream != connected_to)

  # Get the attributes for each edge
  lookup_dt <- as.data.table(as_long_data_frame(g)[c("ver[el[, 1], ]", agg_var)])
  names(lookup_dt)[1] <- "stream"

  # Merge the network attributes. The "connected_to" may have NAs which means that these are headwaters without upstream segments.
  # Keep these my coyping the edge ID into the column
  dt <- dt[lookup_dt, on="stream"] # left join, preserves ordering

  cat("Aggregating attributes...\n")

  dt_agg <- dt[,lapply(.SD, agg_type, na.rm=TRUE),
                         .SDcols=agg_var,
                         by="stream"]

                      return(dt_agg)
                                }  else {
                                   return(dt)
                                        }

  cat("Clean up...\n")
  # Stop parallel backend
  plan(sequential, .cleanup = T)

}



# test functions

# provides only the upstream connection to each stream
all_segments_out <- all_segments(my_catchment)

# # provides besides the upstream connection to each stream, also the aggregation of variables
all_segments_out <- all_segments(my_catchment,
                                 agg_var=c("length", "flow_accum"),
                                 agg_type="sum") # or mean, sd, ...


