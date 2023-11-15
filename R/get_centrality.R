#' @title Get centrality indexes from stream network graph
#'
#' @description Calculate centrality indexes from a directed stream network
#' graph.
#' By switching the mode to either "in", "out" or "all", only the upstream,
#' downstream or all connected segments will be considered, respectively. The
#' function \code{\link{read_geopackage()}} can be used to create the input
#' network graph.
#'
#'
#' @param g igraph object. A directed graph.
#' @param index character. One of "all", "closeness", "farness",
#' "betweenness", "degree", "eccentricity". See @Details
#' @param mode character. One of "in", "out" or "all". Defines whether the
#' shortest paths to (upstream) or from (downstream) the given
#' segments/sub-catchments should be calculated.
#' If "out", then only downstream segments will be considered. If "in", then
#' only upstream segments will be considered. If "all", then the flow direction
#' will be ignored and all streams will be considered.
#'
#'
#' @returns A data.table that reports all subc_id and their centrality values.
#'
#' @importFrom data.table setDT
#' @importFrom igraph is_directed V degree closeness betweenness eccentricity
#' @export
#'
#' @author Afroditi Grigoropoulou
#'
#' @references
#' Csardi G, Nepusz T: The igraph software package for complex network research,
#' InterJournal, Complex Systems 1695. 2006. \url{https://igraph.org}
#'
#' @seealso
#' \code{\link{read_geopackage()}} to create a network graph.
#'
#'@details
#' The degree of a node is the number of its adjacent edges.
#' Closeness centrality measures how many steps are required to access every
#' other node from a given node
#' Farness centrality is the sum of the length of the shortest paths between the
#' node and all other nodes. It is the reciprocal of closeness (Altermatt, 2013).
#' The eccentricity of a node is its shortest path distance from the farthest
#' other node in the graph (West, 1996).
#' The node betweenness is (roughly) defined by the number of geodesics
#' (shortest paths) going through a node.
#'
#'
#' @examples
#' # Download test data into the temporary R folder
#' # or define a different directory
#' my_directory <- tempdir()
#' download_test_data(my_directory)
#'
#' # Load stream network as a graph
#' my_graph <- read_geopackage(gpkg = paste0(my_directory,
#'                                          "/hydrography90m_test_data",
#'                                          "/order_vect_59.gpkg"),
#'                            import_as = "graph")
#'
#' # Get the all the centrality indexes
#' centrality <- get_centrality(g = my_graph, index = "all", mode = "in")
#'


get_centrality <- function(g, index = "all", mode = NULL) {

  # Check input arguments
  if (class(g) != "igraph")
    stop("Input must be an igraph object.")

  if (!is_directed(g))
    stop("The input graph must be a directed graph.")

  match.arg(arg = index, choices = c("all", "closeness", "farness",
  "betweenness", "degree", "eccentricity"))

  if (index %in% c("all", "degree", "closeness", "farness", "eccentricity")) {
    if (missing(mode)){
            stop("Please provide the mode as 'in', 'out' or 'all'.")
    }
    match.arg(arg = mode, choices = c("in", "out", "all"))
  }


  # Avoid exponential numbers in the table and IDs,
  # only set this only within the function
  options(scipen = 999)

  if (index == "degree") {
    centr_ind <- degree(graph = g,  v = V(g),  mode = mode,  loops = TRUE,
                     normalized = FALSE) %>%
    as.data.frame() %>%
    setDT(keep.rownames = T) %>%
    rename(subc_id = "rn", degree = ".") %>%
      filter(subc_id != -1)
  }

  if (index == "closeness") {
    centr_ind <- closeness(graph = g, vids = V(g), mode = mode, weights = NULL,
    normalized = FALSE, cutoff = -1) %>%
    as.data.frame() %>%
    setDT(keep.rownames = T) %>%
    rename(subc_id = "rn", closeness = ".") %>%
      filter(subc_id != -1)
  }

  if (index == "farness") {
    closeness <- closeness(graph = g, vids = V(g), mode = mode, weights = NULL,
                           normalized = FALSE, cutoff = -1) %>%
    as.data.frame() %>%
    setDT(keep.rownames = T) %>%
    rename(subc_id = "rn", closeness = ".") %>%
      filter(subc_id != -1)

    centr_ind <- closeness %>%
      mutate(farness = 1/closeness) %>%
      select(subc_id, farness) %>%
      filter(subc_id != -1)
  }

  if (index == "betweenness") {
    centr_ind <- betweenness(graph = g) %>%
      as.data.frame() %>%
      setDT(keep.rownames = T) %>%
      rename(subc_id = "rn", betweeness = ".") %>%
      filter(subc_id != -1)
  }

  if (index == "eccentricity") {
    centr_ind <- eccentricity(graph = g, v = V(g), mode = mode) %>%
      as.data.frame() %>%
      setDT(keep.rownames = T) %>%
      rename(subc_id = "rn", eccentricity = ".") %>%
      filter(subc_id != -1)
  }

  if (index == "all") {
    degree <- degree(graph = g,  v = V(g),  mode = mode, loops = TRUE,
                        normalized = FALSE) %>%
      as.data.frame() %>%
      setDT(keep.rownames = T) %>%
      rename(subc_id = "rn", degree = ".") %>%
      filter(subc_id != -1)

    closeness <- closeness(graph = g, vids = V(g), mode = mode, weights = NULL,
                           normalized = FALSE, cutoff = -1) %>%
      as.data.frame() %>%
      setDT(keep.rownames = T) %>%
      rename(subc_id = "rn", closeness = ".") %>%
      filter(subc_id != -1)

    farness <- closeness %>%
      mutate(farness = 1/closeness) %>%
      select(subc_id, farness) %>%
      filter(subc_id != -1)

    betweenness <- betweenness(graph = g) %>%
      as.data.frame() %>%
      setDT(keep.rownames = T) %>%
      rename(subc_id = "rn", betweeness = ".") %>%
      filter(subc_id != -1)

    eccentricity <- eccentricity(graph = g, v = V(g), mode = mode) %>%
      as.data.frame() %>%
      setDT(keep.rownames = T) %>%
      rename(subc_id = "rn", eccentricity = ".") %>%
      filter(subc_id != -1)

    centr_ind_list <- list(degree, closeness, farness, betweenness, eccentricity)
    centr_ind <- Reduce(function(...)
      merge(..., by='subc_id', all.x=TRUE), centr_ind_list)

  }

  return(centr_ind)

}
