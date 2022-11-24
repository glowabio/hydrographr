#' All stream segments within a given distance
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


g <- my_graph
segmentID=c(371698007, 371698005, 371698008)

# find_upstream <- function(g, aggregate_var=c("length", "flow_accum"), aggregate_type="sum") {
segment_neighbours <- function(g, segmentID=NULL, order=5, mode="in", n_cores=NULL, maxsize=1500) {


  # Check input arguments
  if ( class(g) != "igraph")     stop("Input must be an igraph object. Please run table_to_graph() first.")

  if ( !is_directed(g)) stop("The input graph must be a directed graph.")

  if ( missing(segmentID)) stop("Please provide at least one segment ID of the input graph. The segmentID must be a numeric vector.")

  if (is.data.frame(segmentID)==TRUE) stop("The segmentID must be a numeric vector.")

  if(hasArg(n_cores)) {
    if (length(segmentID)>1 & n_cores==0)  stop("You have specified multiple segments but zero workers. Please specify at least n_cores=1, or leave it empty to allow an automatic setup.")
  }



  # Set available RAM for future.apply
  # maxmem <- memuse::Sys.meminfo()$totalram@size-1
  # Define the size of the onjects passed to future:
  # 1500*1024^2=1572864000 , i.e. 1.5GB for one tile
  options(future.globals.maxSize=maxsize*1024^2)

  #  Remove any duplicate segmenrIDs
  segmentID <- segmentID[!duplicated(segmentID)]

  # Set up parallel backend if multiple segments
  if(length(segmentID)>1) {
    cat("Setting up parallel backend...\n")
    registerDoFuture()
    # If n_cores not specified, use all-2
    if(length(segmentID)>1 & missing(n_cores)) {
      n_cores <- detectCores(logical=F)-2
    }
    # Check parallel backend depending on the OS
    if(get_os()=="windows") {
      plan(multisession, workers = n_cores)
    }

    if(get_os()=="osx" || get_os()=="linux")  {
      plan(multisession, workers = n_cores) # multicore?
    }
  }



  cat("Finding stream segments within neighbourhood order", order, "\n")

  l <- future_sapply(as.character(segmentID), function(x) ego(g, nodes=x, order, mode = mode))
  # ego_out <- ego(g, nodes=as.character(segmentID), order, mode = mode)

  # Reduce list items
  l <- future_lapply(l, as_ids)

  # As data.frame
  l <- future_lapply(l, as.data.table)

  ### Get the path number as an additional column in the graph list.vs
  mapply_fun <- function(element,name){
    mutate(element,stream = name)
  }
  ### Get the stream (from) ID as an additional column
  l <- future_mapply(mapply_fun,l,names(l),SIMPLIFY = F)
  # Get into one file
  dt <- rbindlist(l)
  # Rename and set key
  names(dt)[1] <- "to_stream"
  setkey(dt, stream)
  # Remove the from-stream, self-reference
  dt <- dt[dt$stream != dt$to_stream,]
  dt <- unique(dt) # remove any duplicates
  # Set col order
  setcolorder(dt, c("stream", "to_stream"))


  # If aggregation was defined:
  if(hasArg(agg_var)) {

  cat("Attaching the attribute(s)", agg_var, "\n")
  # Get the attributes for each edge
  lookup_dt <- as.data.table(as_long_data_frame(g)[c("ver[el[, 1], ]", agg_var)])
  names(lookup_dt)[1] <- "stream"

  # Merge the network attributes.
  dt <- lookup_dt[dt, on="stream"] # left join, preserves ordering
  # Set col order
  setcolorder(dt, c("stream", "to_stream", agg_var))


  cat("Aggregating attribute(s)", agg_var, "for each segment. \n")

  dt_agg <- dt[,lapply(.SD, agg_type, na.rm=TRUE),
                         .SDcols=agg_var,
                         by="stream"]

            return(dt_agg)
          }  else {
           return(dt)
     }
}



out <- segment_neighbours(my_graph, segmentID=as_ids(V(my_graph)), order=1, mode="in", n_cores=5)



  ### Sort and remove the self-connection of the source stream
  dt <- dt[order(stream, connected_to),]
  # dt <- subset(dt, stream != connected_to)







  cat("Clean up...\n")
  # Stop parallel backend
  plan(sequential, .cleanup = T)




  cat("Setting up parallel backend...\n")
  # Set up parallel backend
  n_cores <- detectCores(logical=F)-2
  registerDoFuture()
  plan(multisession, workers = n_cores) # windows / linux




  ### Get into datatable format

  # specify names of list elements = connected_to
  l <- future_sapply(l, setDT)

  ### Merge as datatable
  dt <- rbindlist(l)
  names(dt)[1] <- "stream"



}



# test functions

# provides only the upstream connection to each stream



# # provides besides the upstream connection to each stream, also the aggregation of variables
all_segments_out <- all_segments(my_catchment,
                                 agg_var=c("length", "flow_accum"),
                                 agg_type="sum") # or mean, sd, ...

agg_var=c("length")
