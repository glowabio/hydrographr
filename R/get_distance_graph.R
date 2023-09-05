#' @title Get the distance in meters between stream segments along a network graph
#'
#' @description Given a set of input sub-catchment IDs, the function will
#' calculate the network distance in meters between all input pairs.
#' Alternatively, only the number of stream segments (sub-catchment) along the
#' paths are reported. Note that the stream segment and sub-catchment
#' IDs are identical, and for consistency, we use the term "subc_id".
#'
#' See the example code under "Note" that explains how to use standard igraph
#' functions to obtain the actual stream segment IDs along the path. The
#' function \code{\link{read_geopackage()}} can be used to create the input
#' network graph, and the function \code{\link{get_catchment_graph()}} can be
#' used to subset a network graph. Note that graph-based function includes also
#' the entire length of the "from" and "to" stream segment, opposed to the
#' \code{\link{get_distance()}} which, depending on the snapping method,
#' includes only the part of the "from" and "to" segement where the points are
#' located, resulting in minor differences.
#'
#' @param g igraph object. A directed graph.
#' @param subc_id numeric. Vector of a single or multiple IDs,
#' e.g (c(ID1, ID2, ID3, ...). The sub-catchment (equivalent to stream segment)
#' IDs for calculating the distances. Note that you can browse the entire
#' network online at \url{https://geo.igb-berlin.de/maps/351/view} and to the
#' left hand side, select the "Stream segment ID" layer and click on the map
#' to get the ID.
#' @param variable character. Specify the attribute / column name in the graph
#' object that should be cumulated along the network path. Default is "length"
#' to be used with the Hydrography90m dataset. Not needed when
#' using "distance_m = FALSE".
#' @param distance_m logical. If TRUE, and in case of the Hydrography90m dataset,
#' the length (in meters) of each network segment along the path will be
#' cumulated and the total length between all pairs will be reported in
#' data.table. If FALSE, only the number of segments that are traversed
#' through will be reported in the output matrix. If the subc_ids of the actual
#'  path is needed, please see the example code under "Note". Default is TRUE.
#' @param max_size numeric. Specifies the maximum size of the data passed to the
#' parallel back-end in MB. Default is 1500 (1.5 GB). Consider a higher value
#' for large study areas (more than one 20°x20° tile). Optional.
#'
#' @returns A data.table that reports either the distance or the number of
#' segments between sub_ids.
#'
#' @importFrom igraph all_shortest_paths edge_attr distances
#' @importFrom data.table setDT
#' @importFrom memuse Sys.meminfo
#' @export
#'
#' @author Sami Domisch
#'
#' @references
#' Csardi G, Nepusz T: The igraph software package for complex network research,
#' InterJournal, Complex Systems 1695. 2006. \url{https://igraph.org}
#'
#'
#' @seealso
#' * \code{\link{read_geopackage()}} to create a network graph.
#' * \code{\link{get_catchment_graph()}} to subset a network graph.
#' @md
#'
#'
#'
#' @note
#' For getting the actual IDs of the path between two sub-catchments,
#' you can use the igraph function "all_shortest_paths":
#'
#' Specify the subc_ids:
#'
#' from_subc_id = 513866854
#'
#' to_subc_id = 513867238
#'
#' subc_path <- all_shortest_paths(graph = my_graph,
#'                                 from = as.character(from_subc_id),
#'                                 to = as.character(to_subc_id),
#'                                 mode = "all")
#'
#' Extract only the subc_ids from the output:
#'
#' subc_path <- as.numeric(as_ids(subc_path$res[[1]]))
#'
#'
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#'
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Load stream network as a graph
#' my_graph <- read_geopackage(gpkg = paste0(my_directory,
#'                                          "/hydrography90m_test_data",
#'                                          "/order_vect_59.gpkg"),
#'                            import_as = "graph")
#'
#' # Assume we have some point data in the following sub-catchment IDs:
#' subc_id <- c("513863746", "513866851", "513867238")
#'
#' # ... if you have some point data, use the extract_ids() function:
#' # (the coordinates correspond to the same three subc_ids)
#' my_points <- data.frame(longitude = c(8.885996642821286,
#'                                       8.873352770456831,
#'                                       8.898060225276105),
#'                         latitude = c(42.26533822791161,
#'                             	       42.26307509726402,
#'                             	       42.25513157316764),
#'                         occurrence_id = c(1, 2, 3))
#'
#' # Define the path of the sub-catchment raster
#' subc_raster <- paste0(my_directory,
#'                       "/hydrography90m_test_data/subcatchment_1264942.tif")
#'
#'
#' # Extract the sub-catchment IDs for the points:
#' my_points_subc_id <- extract_ids(data = my_points,
#'                                  lon = "longitude",
#'                                  lat = "latitude",
#'                                  id = "occurrence_id",
#'                                  subc_layer = subc_raster)
#'
#'
#' # Get a vector of the sub-catchment IDs:
#' subc_id <- my_points_subc_id$subcatchment_id
#'
#' # Get the network distance (in meters) between all input pairs:
#' subc_distances <- get_distance_graph(my_graph,
#'                                      subc_id = subc_id,
#'                                      variable = "length",
#'                                      distance_m = TRUE)
#' subc_distances
#'
#' # Get the number of stream segments that are along the network path:
#' number_segments <- get_distance_graph(my_graph,
#'                                       subc_id = subc_id,
#'                                       variable = "length",
#'                                       distance_m = FALSE)
#' number_segments



get_distance_graph <- function(g, subc_id = NULL, variable = "length",
                               distance_m = TRUE, max_size = 1500) {


  # Check input arguments
  if (class(g) != "igraph")
    stop("Input must be an igraph object.")

  # if (!is_directed(g))
  #   stop("The input graph must be a directed graph.")

  if (missing(subc_id) | length(subc_id) <2)
    stop("Please provide at least two unique segment IDs of the input graph.
         The subc_id must be a numeric vector.")

  if (is.data.frame(subc_id) == TRUE)
    stop("The subc_id must be a numeric vector.")



  # Set available RAM for future.apply
  # maxmem <- memuse::Sys.meminfo()$totalram@size-1
  # Define the size of the objects passed to future:
  # 1500*1024^2=1572864000 , i.e. 1.5GB for one tile
  options(future.globals.max_size = max_size * 1024^2)
  # Avoid exponential numbers in the table and IDs,
  # only set this only within the function
  options(scipen = 999)

  # Remove any duplicates from the input subc_ids
  subc_id <- subc_id[!duplicated(subc_id)]

  if (length(subc_id) <2)
    stop("Please provide at least two unique segment IDs of the input graph.
         The subc_id must be a numeric vector.")

  if(distance_m == TRUE) { # default
    # get the actual distance in meters along the paths

    # Rename the length column for igraph::distances
    names_edg_attr <- names(edge_attr(g))
    names_edg_attr[which(names_edg_attr == variable)] <- "weight"

    # Get a matrix of the distances [m] between the input IDs
    dist_out <- distances(g,
                          v = as.character(subc_id),
                          to = as.character(subc_id),
                          mode = c("all"),
                          weights = NULL,
                          algorithm = c("automatic"))


    # get only the number of segments along the paths
    } else if(distance_m == FALSE) {

    dist_out <- distances(g,
                          v = as.character(subc_id),
                          to = as.character(subc_id),
                          mode = c("all"),
                          weights = NA)
    }

    # Convert the upper triangle to a 3-column long table
    dist_out_ind <- which(upper.tri(dist_out, diag = TRUE), arr.ind = TRUE)
    dist_out_ind_name <- dimnames(dist_out)
    dist_out_table <- data.frame(from = dist_out_ind_name[[1]][dist_out_ind[, 1]],
                                 to = dist_out_ind_name[[2]][dist_out_ind[, 2]],
                                 distance = dist_out[dist_out_ind])

    # Remove the zero self-distances
    dist_out_table <- dist_out_table[-which(dist_out_table["from"] == dist_out_table["to"]),]

    # Return as data.table
    dist_out_table <- setDT(dist_out_table)
    return(dist_out_table)

}




