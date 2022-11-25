#' Stream segment neighbours
#'
#' For each segment, report those segments that are connected to one or multiple input segments within a specified neighbour order, with the option to summarize attributes across these segments.
#'
#' This function can also be used to create the connectivity table for Marxan by using agg_var="length" and attach_only=TRUE. The resulting table reports the connectivity from each segment, along with the stream length for all connected segments.
#'
#' @param g A directed graph (igraph object).
#' @param segmentID The input segment IDs as a numerical vector for which to search the connected segments.
#' @param order The neighbouring order. Order=1 would be immediate neighbours of the input segementID, order=2 would be the order 1 plus the immediate neighbours of the segementIDs in order 1.
#' @param mode One of "in", "out", or "all". "in" reports only upstream neighbour segments, "out" reports only the downstream segments, and "all" does both.
#' @param variable Optional. One or more attribute(s) or variable(s) of the input graph that should be reported for each output segmentID ("to_stream").
#' @param attach_only Logical. If TRUE then the selected variables will be only attached to each for each segment without any further aggregation.
#' @param stat One of mean, median, min, max, sd (without quotes). Aggregates (or summarizes) the variables for the neighbourhood of each input segment ("stream", e.g., the average land cover in the next five upstream segments or sub-catchments).
#' @param n_cores Optional. Specify the number of CPUs for internal parallelization in the case of multiple stream segments / outlets. Defaults to 1. In case the graph is very large, and many segments are used as an input, setting n_cores to a higher value can speed up the coputatation. This comes however at the cost of possible RAM limitations and even slower processing since the large data will be copied to each CPU.
#' @param maxsize Optional. Specify the maximum size of the data passed to the parallel backend in MB. Defaults to 1500 (1.5 GB). Consider a higher value for large study areas (more than one 20°x20° tile).

#'
#' @importFrom future plan
#' @importFrom doFuture registerDoFuture
#' @importFrom parallel detectCores
#' @importFrom data.table setDT setnames rbindlist setcolorder setkey
#' @importFrom igraph ego as_ids
#' @importFrom future.apply future_lapply future_sapply future_mapply
#' @importFrom dplyr mutate
#' @importFrom memuse Sys.meminfo
#' @export
#'



segment_neighbours <- function(g, segmentID=NULL, variable=NULL, stat=NULL,
                               attach_only=F, order=5, mode="in", n_cores=1,
                               maxsize=1500) {

  # Check input arguments
  if ( class(g) != "igraph")     stop("Input must be an igraph object. Please run table_to_graph() first.")

  if ( !is_directed(g)) stop("The input graph must be a directed graph.")

  if ( missing(segmentID)) stop("Please provide at least one segment ID of the input graph. The segmentID must be a numeric vector.")

  if (is.data.frame(segmentID)==TRUE) stop("The segmentID must be a numeric vector.")

  if(hasArg(n_cores)) {
    if (length(segmentID)>1 & n_cores==0)  stop("You have specified multiple segments but zero workers. Please specify at least n_cores=1, or leave it empty to allow an automatic setup.")
  }

  if (attach_only==TRUE & missing(variable)) stop("No variable specified that should be attached to the stream segments. Please provide at least one variables from the input graph.")


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
      # n_cores <- detectCores(logical=F)-2
      n_cores <- 1
    }
    # Check parallel backend depending on the OS
    if(get_os()=="windows") {
      plan(multisession, workers = n_cores)
    }
    if(get_os()=="osx" || get_os()=="linux")  {
      plan(multicore, workers = n_cores) # multicore?
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
    mutate(element,stream = name) }
  ### Get the stream (from) ID as an additional column
  l <- future_mapply(mapply_fun,l,names(l),SIMPLIFY = F)
  # Get into one file
  dt <- rbindlist(l)
  # Rename and set key
  names(dt)[1] <- "to_stream"
  setkey(dt, stream)
  dt <- unique(dt) # remove any duplicates, if any

  # If aggregation was defined:
  if(!missing(variable)) {


  cat("Attaching the attribute(s)", variable, "\n")
  # Get the attributes for all edges of the full graph
  lookup_dt <- as.data.table(as_long_data_frame(g)[c("ver[el[, 1], ]", variable)])
  names(lookup_dt)[1] <- "to_stream"

  # Merge the network attributes and sort:
  # dt_join <- merge(dt, lookup_dt, by="to_stream", all.x=TRUE)
  # dt_join <- dt[lookup_dt, on="to_stream"] # lookup_dt[dt, on="stream"]  gives NAs
  dt_join <- lookup_dt[dt, on="to_stream"]
  dt_join <- dt_join[order(-rank(stream))]
  # Remove the from-stream, self-reference
  dt_join <- dt_join[dt_join$stream != dt_join$to_stream,]
  # Set col order
  setcolorder(dt_join, c("stream", "to_stream", variable))

  # Export only attached data
  if(attach_only==TRUE) {
    return(dt_join)
# Else aggregate the variables to each "from" stream
  } else if(attach_only==FALSE & !missing(stat))   {
  cat("Aggregating variable(s)", variable, "for each segmentID.\n")

  dt_agg <- dt_join[,lapply(.SD, stat, na.rm=TRUE),
                         .SDcols=variable,
                         by="stream"]
  dt_agg <- dt_agg[order(-rank(stream))]
  return(dt_agg)
          } else stop("Please provide the summary statistic for the aggregation.")
    }  else {
           return(dt)
          }
  plan(sequential)
}





# # Test function
# usePackage <- function(p){
#   if (!is.element(p, installed.packages()[,1])) install.packages(p, dep = TRUE)
#   library(p, character.only = TRUE)
# }
#
# usePackage("future.apply")
# usePackage("doFuture")
# usePackage("future.batchtools")
# usePackage("dplyr")
# usePackage("parallel")


# g <- my_catchment
# segmentID=c(371901515)
#
# segmentID=(as_ids(V(my_graph)))
#
#
# out <- segment_neighbours(my_graph, segmentID=segmentID,
#                           order=5, mode="in", n_cores=1,
#                           variable=c("length", "source_elev"),
#                           attach_only=T)
#
# out <- segment_neighbours(my_graph, segmentID=segmentID,
#                           order=2, mode="out", n_cores=1,
#                           variable=c("length", "source_elev"),
#                           stat=median)
#
#
# out <- segment_neighbours(my_graph, segmentID=segmentID,
#                           order=2, mode="all", n_cores=1,
#                           variable=c("length", "source_elev"),
#                           stat=mean,
#                           attach_only=T)






