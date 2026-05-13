#' @title Fill gaps between suitable habitat patches in a stream network
#'
#' @description For a given set of suitable stream segments (e.g. from a
#' thresholded species distribution model), the function identifies short
#' unsuitable reaches that interrupt otherwise continuous suitable habitat
#' patches, and fills them in. This consolidates fragmented habitat predictions
#' into ecologically meaningful patches, under the assumption that fish can
#' move through short unsuitable reaches to access suitable habitat on either
#' side.
#'
#' A gap is defined as a sequence of consecutive unsuitable reaches between
#' two suitable patches. A gap is filled if its total length is less than the
#' specified threshold (sigma_mob, max_gap_m, or the minimum of both).
#'
#' @param g igraph object. A directed stream network graph. Edge direction
#'   should go from upstream to downstream (subc_id -> target).
#' @param suitable_ids numeric or character vector. The subc_ids of reaches
#'   classified as suitable habitat (above the suitability threshold).
#' @param edge_length named numeric vector. Reach lengths in metres, named
#'   by subc_id. Used to compute gap lengths.
#' @param sigma_mob numeric. Species-specific dispersal distance in metres
#'   (e.g. from fishmove). Gaps shorter than sigma_mob are filled.
#'   Optional if max_gap_m is provided.
#' @param max_gap_m numeric. Maximum gap length in metres to fill, regardless
#'   of species dispersal distance. Optional if sigma_mob is provided.
#'   If both sigma_mob and max_gap_m are provided, the minimum of the two
#'   is used as the gap threshold.
#'
#' @return A character vector of subc_ids of all suitable reaches after gap
#'   filling (original suitable reaches + filled gap reaches).
#'
#' @details
#' The gap-filling algorithm works as follows:
#' \enumerate{
#'   \item Build a subgraph of all unsuitable reaches.
#'   \item Find connected components of unsuitable reaches (potential gaps).
#'   \item For each component, check if it is flanked by suitable reaches
#'     on both sides (upstream and downstream).
#'   \item If the total length of the component is less than the gap threshold,
#'     fill it (reclassify as suitable).
#' }
#'
#' @importFrom igraph induced_subgraph components neighbors V E edge_attr
#'   as_ids is_directed
#' @importFrom data.table data.table rbindlist
#'
#' @author Afroditi Grigoropoulou
#'
#' @examples
#' \dontrun{
#' # Load stream network as graph
#' network_g <- read_geopackage("stream_network.gpkg", import_as = "graph")
#'
#' # Set edge weights to reach length
#' E(network_g)$weight <- reach_lengths[match(E(network_g)$subc_id,
#'                                             names(reach_lengths))]
#'
#' # Define suitable reaches (e.g. from thresholded SDM)
#' suitable <- network_dt %>%
#'   filter(idw_Salmo_farioides >= 0.7) %>%
#'   pull(subc_id)
#'
#' # Fill gaps shorter than sigma_mob
#' suitable_filled <- fill_habitat_gaps(
#'   g           = network_g,
#'   suitable_ids = suitable,
#'   edge_length  = setNames(E(network_g)$weight, V(network_g)$name),
#'   sigma_mob    = 12585,
#'   max_gap_m    = 5000
#' )
#' }
#'
#' @export

fill_habitat_gaps <- function(g,
                              suitable_ids,
                              edge_length,
                              sigma_mob  = NULL,
                              max_gap_m  = NULL) {

  # ---- Input checks ----
  if (!inherits(g, "igraph"))
    stop("g must be an igraph object.")

  if (!is_directed(g))
    stop("g must be a directed graph.")

  if (is.null(sigma_mob) && is.null(max_gap_m))
    stop("At least one of sigma_mob or max_gap_m must be provided.")

  # Determine gap threshold
  if (!is.null(sigma_mob) && !is.null(max_gap_m)) {
    gap_threshold <- min(sigma_mob, max_gap_m)
    message("  Using gap threshold = min(sigma_mob, max_gap_m) = ",
            round(gap_threshold), "m")
  } else if (!is.null(sigma_mob)) {
    gap_threshold <- sigma_mob
    message("  Using gap threshold = sigma_mob = ", round(gap_threshold), "m")
  } else {
    gap_threshold <- max_gap_m
    message("  Using gap threshold = max_gap_m = ", round(gap_threshold), "m")
  }

  # Convert suitable_ids to character for igraph compatibility
  suitable_ids  <- as.character(suitable_ids)
  all_node_ids  <- V(g)$name

  # Unsuitable reaches = all nodes not in suitable_ids
  unsuitable_ids <- setdiff(all_node_ids, suitable_ids)

  message("  Suitable reaches:   ", length(suitable_ids))
  message("  Unsuitable reaches: ", length(unsuitable_ids))

  if (length(unsuitable_ids) == 0) {
    message("  No unsuitable reaches — nothing to fill")
    return(suitable_ids)
  }

  # ---- Build undirected subgraph of unsuitable reaches ----
  # Use undirected for component detection so upstream/downstream
  # unsuitable sequences are treated as single gaps
  g_unsuitable <- induced_subgraph(g, unsuitable_ids)
  g_unsuitable_undir <- as.undirected(g_unsuitable, mode = "collapse")

  # Find connected components of unsuitable reaches
  comps <- components(g_unsuitable_undir)
  n_comps <- comps$no
  message("  Unsuitable connected components (potential gaps): ", n_comps)

  filled_ids <- character(0)
  n_filled   <- 0

  for (comp_id in seq_len(n_comps)) {

    # Nodes in this component
    comp_nodes <- names(which(comps$membership == comp_id))

    # Total gap length
    gap_length <- sum(edge_length[comp_nodes], na.rm = TRUE)

    # Check if gap is flanked by suitable reaches on both sides
    # A gap qualifies if at least one suitable reach is upstream
    # AND at least one suitable reach is downstream
    has_suitable_upstream   <- FALSE
    has_suitable_downstream <- FALSE

    for (node in comp_nodes) {
      # Upstream neighbours (edges pointing TO this node)
      up_neighbours <- as_ids(neighbors(g, node, mode = "in"))
      if (any(up_neighbours %in% suitable_ids))
        has_suitable_upstream <- TRUE

      # Downstream neighbours (edges pointing FROM this node)
      down_neighbours <- as_ids(neighbors(g, node, mode = "out"))
      if (any(down_neighbours %in% suitable_ids))
        has_suitable_downstream <- TRUE

      if (has_suitable_upstream && has_suitable_downstream) break
    }

    # Fill gap if: flanked on both sides AND shorter than threshold
    if (has_suitable_upstream && has_suitable_downstream &&
        gap_length <= gap_threshold) {
      filled_ids <- c(filled_ids, comp_nodes)
      n_filled   <- n_filled + 1
    }
  }

  message("  Gaps filled: ", n_filled, " (of ", n_comps, " components)")
  message("  Reaches added by gap filling: ", length(filled_ids))

  # Return all suitable reaches: original + filled gaps
  result <- unique(c(suitable_ids, filled_ids))
  message("  Total suitable reaches after gap filling: ", length(result))

  return(result)
}
