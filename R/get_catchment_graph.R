#' Get catchment from graph
#'
#' Subset the network graph by extracting the upstream, downstream or entire
#' catchment, for one or multiple stream segments. The function will return
#' either one or more data.tables or graph objects for each input stream
#' segment. Note that the stream segment and sub-catchment IDs are identical,
#' and for consistency, we use the term "subc_id".
#'
#' By switching the mode to either "in", "out" or "all", only the upstream,
#' downstream or all connected segments will be returned.
#'
#' @param g A directed graph (igraph object).
#' @param subc_id Optional.
#' The sub-catchments (equivalent to stream segment) IDs for which
#' to delineate the upstream drainage area. Can be a single ID or a vector of
#' multiple IDs (c(ID1, ID2, ID3, ...).
#' If empty, then outlets will be used as sub-catchment IDs
#' (with outlet=TRUE). Note that you can browse the entire network online at
#' https://geo.igb-berlin.de/maps/351/view and to left hand side, select the
#' "Stream segment ID"  layer and click on the map to get the ID.
#' @param outlet Logical. If TRUE, then the outlets of the given network graph
#' will be used as additional input subc_ids.
#' Outlets will be identified internally as those stream segments that do not
#' have any downstream connected segment.
#' @param as_graph Logical. If TRUE then the output will be a new graph
#' or a list of new graphs with the original attributes,
#' If FALSE (the default), then the output will be a new data.table,
#' or a list of data.tables. List objects are named after the subc_ids.
#' @param mode Can be either "in", "out" or "all". "in" will delineate
#' the upstream catchment, "out" delineates the downstream catchment
#' (all segments that are reachable from the given input segment), and
#' "all" does both.
#' @param n_cores Optional. Specify the number of CPUs for internal
#' parallelization in the case of multiple stream segments / outlets. Defaults
#' to 1. Setting a higher number is might be slower in the end, as the data has
#' to be provided to each CPU (worker) which can take time.
#' @param maxsize Optional. Specify the maximum size of the data passed to the
#' parallel backend in MB. Defaults to 1500 (1.5 GB). Consider a higher value
#' for large study areas (more than one 20??x20?? tile).
#'
#' @return A graph or datatable that reports all subc_ids.
#' In case of multiple input segments, the results are stored in a list.
#'
#' @importFrom future plan multisession multicore sequential
#' @importFrom doFuture registerDoFuture
#' @importFrom parallel detectCores
#' @importFrom data.table setDT setnames
#' @importFrom igraph subcomponent subgraph as_data_frame is_directed degree V
#' gsize
#' @importFrom future.apply future_lapply
#' @importFrom memuse Sys.meminfo
#' @export
#'
#' @examples
#' library(hydrographr)
#'
#' # Download test data into temporary R folder
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Load stream network as a graph
#' g <- read_geopackage(paste0(my_directory, "/order_vect_59.gpkg"),
#'                             type = "net", as_graph = TRUE)
#'
#' # Pick a random subc_id
#' subc_id = "513855877"
#' # Get the upstream catchment as a graph
#' g_up <- get_catchment_graph(g, subc_id = subc_id, mode = "in",
#'                             outlet = FALSE, as_graph = TRUE, n_cores =1)
#'
#' # Get the downstream segments as a data.table,
#' g_down <- get_catchment_graph(g, subc_id = subc_id, mode = "out",
#'                               outlet = FALSE, as_graph = FALSE, n_cores = 1)
#'
#' # Get the catchments of all outlets in the study area as a graph
#' g_all <- get_catchment_graph(g, mode = "in", outlet = TRUE, as_graph = TRUE,
#'                              n_cores=1)
#'
#' @author Sami Domisch


get_catchment_graph <- function(g, subc_id = NULL, outlet = FALSE,
mode = NULL, as_graph = FALSE, n_cores = 1, maxsize = 1500) {

  # Check input arguments
  if (class(g) != "igraph")     stop("Input must be an igraph object.")

  if (!is_directed(g)) stop("The input graph must be a directed graph.")

  if (missing(subc_id) && outlet == FALSE) stop(
        "Please provide at least one segment ID of the input graph,
        or set outlet=TRUE. The subc_id must be a numeric vector."
        )


  if (missing(mode)) stop("Please provide the mode as 'in', 'out' or 'all'.")

  if (is.data.frame(subc_id) == TRUE) stop(
    "The subc_id must be a numeric vector.")

  if (hasArg(n_cores)) {
  if (length(subc_id) > 1 && n_cores == 0)  stop(
    "You have specified multiple segments but zero workers.
    Please specify at least n_cores=1,
     or leave it empty to allow enable the automatic setup.")
  }

  # Set available RAM for future.apply
  # maxmem <- memuse::Sys.meminfo()$totalram@size-1
  # Define the size of the onjects passed to future:
  # 1500*1024^2=1572864000 , i.e. 1.5GB for one tile
  options(future.globals.maxSize = maxsize * 1024^2)
  # Avoid exponential numbers in the table and IDs,
  # only set this only within the function
  options(scipen = 999)

  # Use the outlets as the subc_ids?
  if (outlet == TRUE) {
  cat("Using outlets as (additional) subc_ids...\n")
    # Identify outlets
    # Which vertices are connected to only one inflowing stream reach?
    # The Hydrograhy90m outlets are coded as "-1"
    outlet <- which(degree(g, v = V(g), mode = "out") == 0, useNames = TRUE)
    # Stop if no outlets found.
    if (length(outlet) == 0) stop("No outlets found.")

    # If no subc_ids provided, then use the outlets as the subc_ids
    if (missing(subc_id) && length(outlet) >= 1) {
      subc_id <-  as.numeric(as.character(names(outlet)))
        } else if (length(subc_id) >= 1 && length(outlet) >= 1) {
        # If subc_id and outlets are specified, take both
        subc_id <- c(subc_id, as.numeric(as.character(names(outlet))))
    }
  }

  #  Remove any duplicate segmenrIDs
  subc_id <- subc_id[!duplicated(subc_id)]

  # Set up parallel backend if multiple segments
  if (length(subc_id) > 1) {
  cat("Setting up parallel backend...\n")
    registerDoFuture()
    # If n_cores not specified, use 1
     if (length(subc_id) > 1 && missing(n_cores)) {
        # n_cores <- detectCores(logical=F)-2
        n_cores <- 1
     }
   # Check parallel backend depending on the OS
    if (get_os() == "windows") {
      plan(multisession, workers = n_cores)
    }

    if (get_os() == "osx" || get_os() == "linux")  {
      plan(multicore, workers = n_cores)
    }
  }


  # Extract the catchments
    if (length(subc_id) == 1) {
      cat("Delineating the catchment for", length(subc_id),
      "segment using mode=", mode, "...\n")
  # Get subcomponent
  l <- subcomponent(g, as.character(subc_id), mode = mode)
  # Subset the graph
  cat("Subsetting the original graph to get all attributes...\n")
  g_sub <- subgraph(g, l)
  # Return graph or datatable
  if (as_graph == TRUE) {
    return(g_sub)
  } else if (as_graph == FALSE) {
    # Get into datatable format
    dt <- setDT(igraph::as_data_frame(g_sub))
    setnames(dt, c("from", "to"),
    c("stream", "next_stream")) # use same col names
    return(dt)
  }

  # if multiple subc_ids, run in parallel
    } else if (length(subc_id) > 1) {
      cat("Delineating the catchment for",
      length(subc_id), "segments using mode=", mode, "...\n")
      subc_id <- subc_id[!duplicated(subc_id)] # remove any duplicates
    # Get all subcomponents
    l <- future_lapply(as.character(subc_id),
                       function(x) subcomponent(g, x, mode = mode))
    # Subset the graphs
    cat("Subsetting the original graph to get all attributes...\n")
    g_sub <- future_lapply(l, function(x) subgraph(g, x))
    # numeric to allow quick referencing of the output list
    names(g_sub) <- as.numeric(subc_id)
    # Remove the empty graphs
    # (no upstream segments due to headwaters, or because
    # of cutline of the outer extent)
    gsub_size <- future_lapply(g_sub, function(x) gsize(x))
    # delete the graph objects with zero upstream segments
    g_sub <- g_sub[gsub_size != 0]
    # Return graph
    if (as_graph == TRUE) {
           return(g_sub)
    } else if (as_graph == FALSE) {
      # Convert to data.frame and data.table
      dt <- future_lapply(g_sub, function(x) igraph::as_data_frame(x))
      # Convert to data.table
      dt <- future_lapply(dt, function(x) data.table::setDT(x))
      # Rename columns
      dt <- future_lapply(dt, function(x) {
        setnames(x, c("from", "to"), c("stream", "next_stream"))
      })
      return(dt)
      }
    }
  # Close parallel backend
  plan(sequential)
}
