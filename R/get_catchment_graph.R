#' Get catchment from graph
#'
#' Subset the network graph by extracting the upstream sub-catchments, i.e. the drainage basin, for one or multiple stream segments. The function will return either one or more data.tables or graph objects for each inout stream segment.
#'
#' @param g A directed graph (igraph object).
#' @param segmentID Optional. The stream segment or sub-catchments IDs for which to delineate the upstream drainage area. Can be a single ID or a vector of multiple IDs (c(ID1, ID2, ID3, ...). If empty, then outlets will be used as segment IDs (with outlet=TRUE).
#' @param outlet Logical. If TRUE, then the outlets of the given network graph will be used as additional input segmentIDs. Outlets will be identified internally as those stream segments that do not have any downstream donnected segment.
#' @param graph Logical. If TRUE then the output will be a new graph or a list of new graphs with the original attributes, If FALSE (the default), then the output will be a new data.table, or a list of data.tables. List objects are named after the segmentIDs.
#' @param n_cores Optional. Specify the number of CPUs for internal parallelization, in the case of multiple stream segments / outlets.

#'
#' @importFrom future plan
#' @importFrom doFuture registerDoFuture
#' @importFrom parallel detectCores
#' @importFrom data.table setDT setnames
#' @importFrom igraph subcomponent subgraph as_data_frame
#' @importFrom future.apply future_lapply
#' @export
#'



# usePackage <- function(p){
#   if (!is.element(p, installed.packages()[,1])) install.packages(p, dep = TRUE)
#   library(p, character.only = TRUE)
# }
#
# usePackage("future.apply")
# usePackage("doFuture")
# usePackage("data.table")
# usePackage("parallel")
# usePackage("parallel")
# g <- my_graph

get_catchment_graph <- function(g, segmentID=NULL, outlet=F, graph=F, n_cores=NULL) {

  # Check input arguments
  if ( class(g) != "igraph")     stop("Input must be an igraph object. Please run table_to_graph() first.")

  if ( !is_directed(g)) stop("The input graph must be a directed graph.")

  if ( missing(segmentID) & outlet==FALSE) stop("Please provide at least one segment ID of the input graph, or set outlet=TRUE. The segmentID must be a numeric vector.")

  if (is.data.frame(segmentID)==TRUE) stop("The segmentID must be a numeric vector.")

  if(hasArg(n_cores)) {
  if (length(segmentID)>1 & n_cores==0)  stop("You have specified multiple segments but zero workers. Please specify at least n_cores=1, or leave it empty to allow enable the automatic setup.")
  }

  # Use the outlets as the segmentIDs?
  if(outlet==TRUE) {
  cat("Using outlets as segmentIDs...\n")
    # Identify outlets
    # Which vertices are connected to only one inflowing stream reach?
    # The Hydrograhy90m outlets are coded as "-1"
    outlet = which(degree(g, v = V(g), mode = "out")==0, useNames = T)
    # Stop if no outlets found.
    if(length(outlet)==0) stop("No outlets found.")

    # If no segmentIDs provided, then use the outlets as the segmentIDs
    if(missing(segmentID) & length(outlet)>=1) {
      segmentID <-  as.numeric(as.character(names(outlet)))
        } else if (length(segmentID)>=1 & length(outlet)>=1) {
        # If segmentId and outlets are specified, take both and remove duplicates
        segmentID <- c(segmentID, as.numeric(as.character(names(outlet))))
        segmentID <- segmentID[!duplicated(segmentID)]
    }
  }


  # Set up parallel backend if multiple segments
  if(length(segmentID)>1) {
  cat("Setting up parallel backend...\n")
    registerDoFuture()
    # If n_cores not specified, use all-2
     if(length(segmentID)>1 & missing(n_cores)) {
        n_cores <- detectCores(logical=F)-2
     }
   # Checl parallel backend depending on the OS
    if(get_os()=="windows") {
      plan(multisession, workers = n_cores)
    }

    if(get_os()=="osx" || get_os()=="linux")  {
      plan(multisession, workers = n_cores) # multicore?
    }
  }


  # Extract the catchments
    if(length(segmentID)==1) {
      cat("Calculating the upstream drainage basin...\n")
  # Get subcomponent
  l <- subcomponent(g, as.character(segmentID), mode = c("in"))
  # Subset the graph
  cat("Subsetting the original graph to get all attributes...\n")
  g_sub <- subgraph(g,l)
  # Return graph or datatable
  if (graph==TRUE) {
    return(g_sub)
  } else if (graph==FALSE) {
    # Get into datatable format
    dt <- setDT(igraph::as_data_frame(g_sub))
    setnames(dt, c("from", "to"), c("stream", "next_stream")) # use same col names
    return(dt)
  }

  # if multiple segmentIDs, run in parallel
    } else if(length(segmentID)>1) {
      cat("Calculating the upstream drainage basins...\n")
      segmentID <- segmentID[!duplicated(segmentID)] # remove any duplicates
    # Get all subcomponents
    l <- future_lapply(as.character(segmentID),
                       function(x) subcomponent(g, x, mode = c("in")))
    # Subset the graphs
    cat("Subsetting the original graph to get all attributes...\n")
    g_sub <- future_lapply(l, function(x) subgraph(g,x))
     names(g_sub) <- segmentID
      # Remove the empty graphs (no upstream segments due to headwaters, or because
      # of cutline of the outer extent)
       gsub_size <- future_lapply(g_sub, function(x) gsize(x))
       g_sub <- g_sub[gsub_size!=0] # delete the graph objects with zero upstream segments
    # Return graph
    if (graph==TRUE) {
           return(g_sub)
    } else if (graph==FALSE) {
      # Convert to data.frame and data.table
      dt <- future_lapply(g_sub, function(x) igraph::as_data_frame(x))
      # Convert to data.table
      dt <- future_lapply(dt, function(x) data.table::setDT(x))
      # Rename columns
      dt <- future_lapply(dt, function(x) setnames(x, c("from", "to"), c("stream", "next_stream")))
      return(dt)
      }
    }
  # Close parallel backend
  plan(sequential)
}




# Test function
segmentID = 513868395

my_seg = 513868395
my_seg = c(513853532, 513833203, 513853533, 513853535)
segmentID = c(513853532, 513833203, 513853533, 513853535)

my_seg = data.frame(ID=c(513853532, 513833203, 513853533, 513853535,513853532, 513833203, 513853533, 513853535))


my_catchment <- get_catchment_graph(my_graph, segmentID = my_seg, outlet=F, graph=F, n_cores=3)
my_catchment <- get_catchment_graph(my_graph, segmentID = my_seg, outlet=T, graph=F, n_cores=3)


my_catchment <- get_catchment_graph(my_graph, segmentID = my_seg$ID, outlet=F, graph=F, n_cores=2)
my_catchment <- get_catchment_graph(my_graph, segmentID = my_seg$ID, outlet=T, graph=T, n_cores=2)
my_catchment <- get_catchment_graph(my_graph, segmentID = my_seg$ID, outlet=T, graph=T)
my_catchment <- get_catchment_graph(my_graph, outlet=F) # OK
my_catchment <- get_catchment_graph(my_graph, outlet=T)
