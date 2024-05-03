#' @title Calculate upstream variable for each sub-catchment
#'
#' @description For each input sub-catchment, the function calculates the
#' upstream mean, median, min, max, sd or sum for one or more variables. Note
#' that the stream segment and sub-catchment IDs are identical. The input
#' graph can be created with \code{\link{read_geopackage()}}
#' and \code{\link{get_catchment_graph()}}.
#'
#' This function can be used to obtain all upstream stream segments /
#' sub-catchments for species distribution modelling, or for spatial
#' prioritization analyses (e.g. Marxan/Gurobi) to specify the connectivity
#' (and in this case the "length" attribute can be used).
#'
#' The function accepts as an input a graph with one ore more outlets (e.g.
#' an entire tile with many drainage basins). The variable that should be
#' aggregated upstream should be prepared as a data.table, e.g.
#' with \code{\link{extract_zonal_stat()}}.
#'
#' @param g igraph object. A directed graph.
#' @param variable_table a data.table that includes the "stream" column
#' (corresponding to the subc_id) as well as the attributes that should be
#' aggregated across the the upstream network subc_id. Default is NULL.
#' @param subc_id A vector of subc_id for which to calculate the
#' upstream variables. Optional; default is to use all subc_id of the input graph.
#' @param var_layer character vector. One or more attributes (variable layers)
#' of the variable_table should be reported for each output subc_id.
#' Default is NULL.
#' @param upstream_stat one of the functions mean, median, min, max, sd, sum
#' (without quotes). The function will be used to aggregate (or summarize)
#' the upstream variables for each subc_id (e.g., the average
#' land cover acorss the entire upstream area. Default is NULL.
#' @param include_focal Whether the focal subc_id should be
#' included in the aggregation (include_focal = TRUE, which is the default). Set
#' to FALSE if the focal subc_id should not be included.
#' @param save_up_conn Set to TRUE if you want to save the intermediate result
#' consisting of a data.table that includes all upstream connections for each
#' subc_id. Useful for large study areas as it avoids re-running
#' the possibly time-consuming pre-processing. The data.table will be saved as
#' an .RData file to getwd(). Default is FALSE.
#' @param n_cores numeric. Number of cores used for parallelisation. Default is 1.
#' In case the graph is very large, and many segments are
#' used as an input, setting n_cores to a higher value can speed up the
#' computation. Note however that the parallelisation process requires copying
#' the input graph to each core. This may result in possible RAM limitations
#' and even slower processing. Hence consider testing first with a lower numver
#' of cores. Default is n_cores = 1. Optional.
#' @param max_size numeric. Specifies the maximum size of the data passed to the
#' parallel back-end in MB. Default is 1500 (1.5 GB). Consider a higher value
#' for large study areas (more than one 20°x20° tile). Optional.
#'
#' @importFrom foreach foreach getDoParWorkers
#' @importFrom doFuture registerDoFuture
#' @importFrom parallel makePSOCKcluster stopCluster
#' @importFrom doParallel
#' @importFrom data.table data.table setDT setnames  rbindlist
#' @importFrom igraph  subcomponent as_ids is_directed degree edge_attr_names
#' delete_edge_attr
#' @importFrom future.apply future_lapply future_mapply
#' @importFrom dplyr mutate
#' @importFrom memuse Sys.meminfo
#' @export
#'
#'
#' @returns
#' A data.table indicating the "stream" (=subc_id) and the upstream variables
#' which have the same column names as the \code{var_layer} argument. The
#' intermediate output can be saved to disk (requires setting \code{save_up_conn} = TRUE).
#'
#'
#' @author Sami Domisch
#'
#' @references
#' Csardi G, Nepusz T: The igraph software package for complex network research,
#' InterJournal, Complex Systems 1695. 2006. \url{https://igraph.org}
#'
#'
#' @seealso
#' \code{\link{read_geopackage()}} and \code{\link{get_catchment_graph()}} to
#' create the input graph. Alternatively, see
#' \code{\link{get_segment_neighbours()}} to obtain the upstream variables for
#' a specified neighbourhood.
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' setwd(my_directory)
#' download_test_data(my_directory)
#'
#' # Load the stream network as graph
#' my_graph <- read_geopackage(gpkg = paste0(my_directory,
#'                                          "/hydrography90m_test_data",
#'                                          "/order_vect_59.gpkg"),
#'                                           import_as = "graph")
#'
#' # Subset the graph and get a smaller catchment
#' my_graph <- get_catchment_graph(g = my_graph,
#'                                 subc_id = 513867228,
#'                                 mode = "in",
#'                                 outlet = FALSE,
#'                                 as_graph = TRUE,
#'                                 n_cores = 1)
#'
#'
#' ## Prepare the variables that should be accumulated.
#' ## Load the table
#' variable_table <- read_geopackage(gpkg = paste0(my_directory,
#'                                                "/hydrography90m_test_data",
#'                                               "/order_vect_59.gpkg"),
#'                                               import_as = "data.table")
#'
#' ## Specify the layers for the upstream aggregation
#' var_layer= c("length", "flow_accum")
#'
#' ## Subset the table
#' keep_these <- c("stream", var_layer)
#' variable_table <- variable_table[, ..keep_these]
#'
#'
#' ## Get the upstream sum of the variables "length" and "flow_accum"
#' result <- get_upstream_variable(my_graph,
#'                                 variable_table = variable_table,
#'                                 var_layer = var_layer,
#'                                 upstream_stat=sum,
#'                                 include_focal = TRUE,
#'                                 save_up_conn = FALSE)
#'
#'
#' ## Map the new variable across the network
#'
#' ## Specify tif-layer for reclassification
#' subc_raster <- paste0(my_directory, "/hydrography90m_test_data/subcatchment_1264942.tif")
#' recl_raster <- paste0(my_directory, "/upstream_sum.tif")
#'
#' ## Set columns as integer
#' result <- result[, names(result) := lapply(.SD, as.integer)]
#'
#' ### Not working in case not all subc_is are present in the raster
#'
#' ### Create raster - select the "to" column which represents the unique subc_id
#' reclass_raster(data = result,
#'                rast_val = "stream",
#'                new_val = "length",
#'                raster_layer = subc_raster,
#'               recl_layer = recl_raster)
#'
#'
#'
#'






get_upstream_variable <- function(g, variable_table = NULL, subc_id = NULL,
                                  var_layer = NULL, upstream_stat = NULL,
                                  include_focal = FALSE, save_up_conn = FALSE,
                                  n_cores = 1, max_size = 3000) {



  ### Upstream variables
  # library(hydrographr)
  # library(doFuture)
  # library(doParallel)
  # library(future.apply)
  # library(foreach)
  # library(parallel)
  # library(data.table)
  # library(dplyr)
  # library(igraph)



  ## Function for getting the path number as an additional column in the
  ## graph list
  mapply_fun <- function(element,name){
    mutate(element, PATH_ID = name)
  }

  ## Make sure that long integers are not in exponential mode
  options(scipen = 999)

  # Check input arguments
  if (class(g) != "igraph")
    stop("Input must be an igraph object. Please create the graph first.")

  if (!is_directed(g))
    stop("The input graph must be a directed graph.")

  if (missing(variable_table))
    stop("Please provide a table that includes the variable for each
         sub-catchment, and which should be used for the upstream aggregation.")

  if (missing(subc_id))
    cat("No subc_id specified. Using all subc_id of the entire network. \n")

  if (missing(var_layer))
    stop("Please provide the name(s) of the variable that should be should be
         used for the upstream aggregation.")

  if (missing(upstream_stat))
    stop("Please provide the aggregation statistic, either mean, min, max, sd, or sum.")




  cat("Setting up parallel backend...\n")

  # Set available RAM for future.apply
  # maxmem <- memuse::Sys.meminfo()$totalram@size-1
  # Define the size of the onjects passed to future:
  # 1500*1024^2=1572864000 , i.e. 1.5GB for one tile
  options(future.globals.max_size = max_size * 1024^2)

  cl <- makePSOCKcluster(n_cores, outfile="")
  registerDoParallel(cl) # register parallel backend


  cat("using", getDoParWorkers(), "CPUs..\n")  # show number of workers
  registerDoFuture()

  ## Drop graph attributes to reduce object size and avoid a bottleneck in foreach
  cat("Preparing graph...", "\n")
  for(i in edge_attr_names(g)) {
    g <- delete_edge_attr(g, i)
  }

  ### Specify all headwater segments
  headwater = which(degree(g, v = V(g), mode = "in")==0, useNames = T)
  headwater <- names(headwater)

  ###Specify all segments except headwaters
  ## Check if the user supplied the subc_id. If not, use all subc_id of the graph
  if(!missing(subc_id)){
    if (is.data.frame(subc_id) == TRUE)
      stop("The subc_id must be a numeric vector.")

    subc_id <- as.character(subc_id)

    } else {
      # Take all non-headwater subc_id of the graph
  subc_id = which(degree(g, v = V(g), mode = "in")>0, useNames = T)
  subc_id <- names(subc_id)
  }

  start1 <- Sys.time()
  cat("Finding all upstream contributing areas...", "\n")
  ### Get the components of each subc_id
  l <- foreach::foreach(i=subc_id, .inorder=FALSE,
                        .packages = "igraph")  %dopar% {
                        subcomponent(g, i, mode = c("in"))
                        }
  end1 <- Sys.time()

  ### Get into data.table format
  names(l) <- subc_id
  l <- future_lapply(l, as_ids)
  l <- future_lapply(l, as.data.table)

  ### Get the path number as an additional column
  l <- future_mapply(mapply_fun,l,names(l),SIMPLIFY = F)

  ### Merge as data.table
  upstream_dt <- rbindlist(l)
  rm(l); gc() # free RAM
  setnames(upstream_dt, c("stream", "base"))

  ## Remove the rows that have the same subc_id in from and to
  ## Check which opttion was chosen by the user
  if(include_focal == FALSE) { # default
    upstream_dt <- upstream_dt[!upstream_dt$stream == upstream_dt$base]
  }

  ### Attach the headwaters
  headwater <- data.table(base = headwater,
                          stream = NA)

  upstream_dt <- rbind(upstream_dt, headwater)

  ## Write the base (subc_id) into the upstream_subc_id-column --> fill the
  ## headwaters that do not have any upstream subc_id and were not needed for
  ## the previous calculation
  upstream_dt$stream <- ifelse(is.na(upstream_dt$stream),
                                         upstream_dt$base,
                                         upstream_dt$stream)


  ## Change to integers
  upstream_dt$stream <- as.integer(upstream_dt$stream)
  upstream_dt$base <- as.integer(upstream_dt$base)

  ## Optional: save the data.table as a .RData file
  if(save_up_conn == TRUE) {
    save(upstream_dt, file=paste(getwd(), "/tmp_all_upstream_connections.RData"))
  }

  ## Attach the attributes that should be aggregated. Must have the same subc_ids
  ## as the "base" in the table
  cat("Attaching the attributes...", "\n")
  upstream_dt <- upstream_dt[variable_table, on = "stream"]


  ### Aggregate the variables for each subc_id
  cat("Aggregating upstream variable(s):", var_layer, "\n")

  if(!missing(upstream_stat)) {
  dt_agg <- upstream_dt[, lapply(.SD, upstream_stat, na.rm = TRUE),
                        .SDcols = var_layer,
                        by = "base"]

  setnames(dt_agg, c("stream", var_layer))

  } else {
    stop("Please provide the summary statistic for the aggregation.")
  }

  return(dt_agg)

  ## Stop the cluster object
  stopCluster(cl)
  plan(sequential)

end2 <- Sys.time()


  # if(nrow(variable_table) != nrow(dt_agg) {
  #   cat("Please note that the output has a different number or rows than the
  #       input table.", "\n")
  # }

}

