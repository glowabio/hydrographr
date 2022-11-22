#' Get catchment from graph
#'
#' Subset the network graph by extracting the upstream sub-catchments, i.e. the drainage basin, for one or multiple stream segments. The function will return either one or more data.tables or graph objects for each inout stream segment.
#'
#' @param g The igraph object
#' @param segmentID The stream segment or sub-catchments IDs for you you want to get the entire upstream drainage basin. Can be a single ID or a vector of multiple IDs (c(ID1, ID2, ID3, ...)
#' @param graph logical. TRUE if the output should a graph, FALSE if it should be a data.table (default)
#' @param n_cores Specify the number of CPUs for internal parallelization, in the case of multiple stream segments.

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




get_catchment_graph <- function(g, segmentID=NULL, graph=F, n_cores=NULL) {
  # Check input
  if ( class(g) != "igraph")     stop("Input must be an igraph object. Please run table_to_graph() first.")

  if ( hasArg(segmentID)==FALSE) stop("Please provide at least one segment ID of the input graph. The segmentID must be a numeric vector.")

  if (is.data.frame(segmentID)==TRUE) stop("The segmentID must be a numeric vector.")

  if (length(segmentID)>1 & n_cores==0)  stop("You have specified multiple segments but zero workers. Please specify at least n_cores=1, or leave it empty to allow enable the automatic setup.")


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
  cat("Calculating the upstream drainage basin...\n")
  if(length(segmentID)==1) {
  # Get subcomponent
  l <- subcomponent(g, as.character(segmentID), mode = c("out"))
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
    # Get all subcomponents
    l <- future_lapply(as.character(segmentID),
                       function(x) subcomponent(g, x, mode = c("out")))
    # Subset the graphs
    cat("Subsetting the original graph to get all attributes...\n")
    g_sub <- future_lapply(l, function(x) subgraph(g,x))
    # Return graph
    if (graph==TRUE) {
      return(g_sub)
    } else if (graph==FALSE) {
      # Convert to data.frame and data.table
      dt <- future_lapply(g_sub, function(x) igraph::as_data_frame(x))
      # Specify the segmentIDs as the list element names
      names(dt) <- segmentID
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
segmentID = 513833203
my_seg = 513853532
my_seg = c(513853532, 513833203, 513853533, 513853535)
segmentID = c(513853532, 513833203, 513853533, 513853535)

my_seg = data.frame(ID=c(513853532, 513833203, 513853533, 513853535,513853532, 513833203, 513853533, 513853535))


my_catchment <- get_catchment_graph(my_graph, segmentID = my_seg, graph=T, n_cores=3)
my_catchment <- get_catchment_graph(my_graph, segmentID = my_seg$ID, graph=F, n_cores=2)

