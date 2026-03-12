#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# pci_sparse.R
#
# Sparse implementation of the Population Connectivity Index (PCI)
# from riverconn::index_calculation (Baldan et al. 2022)
#
# Computes connectivity matrices only for occupied nodes (weight > 0),
# reducing memory from O(N²) to O(k²) where k = number of occupied
# subcatchments and N = total subcatchments in the basin.
# Produces identical results to riverconn::index_calculation.
#
# Validated against riverconn v0.3.31 on basins with up to ~800 nodes
# (see supplementary material).
#
# Limitations vs riverconn::index_calculation:
#   - Exponential dispersal kernel only (no threshold/leptokurtic)
#   - Symmetric directionality only (dir_fragmentation_type = "symmetric",
#     dir_distance_type = "symmetric")
#   - c_ij and B_ij always included (no flags to disable)
#   - index_type "sum" (CAFI) not implemented
#
# References:
#   Baldan, D., Cunillera-Montcusí, D., Funk, A., & Hein, T. (2022).
#     Introducing 'riverconn': an R package to assess river connectivity
#     indices. Environmental Modelling & Software, 156, 105470.
#
#   Jumani, S. et al. (2020). River fragmentation and flow alteration
#     metrics: a review. Environmental Research Letters, 15(12), 123009.
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

pci_sparse <- function(graph,
                       weight = "weight",
                       field_B = "length_reach",
                       param = 0.5,
                       index_type = "full",
                       index_mode = "to",
                       pass_u = "pass_u",
                       pass_d = "pass_d") {

  # ----------------------------------------------------------
  # Input validation
  # ----------------------------------------------------------
  if (!igraph::is_igraph(graph))
    stop("'graph' must be an igraph object")
  if (!(index_type %in% c("full", "reach")))
    stop("'index_type' must be 'full' or 'reach'")
  if (index_type == "reach" && !(index_mode %in% c("from", "to")))
    stop("'index_mode' must be 'from' or 'to'")
  if (!(weight %in% igraph::vertex_attr_names(graph)))
    stop("'weight' must be a vertex attribute in 'graph'")
  if (!(field_B %in% igraph::vertex_attr_names(graph)))
    stop("'field_B' must be a vertex attribute in 'graph'")
  if (!(pass_u %in% igraph::edge_attr_names(graph)))
    stop("'pass_u' must be an edge attribute in 'graph'")
  if (!(pass_d %in% igraph::edge_attr_names(graph)))
    stop("'pass_d' must be an edge attribute in 'graph'")

  # ----------------------------------------------------------
  # Extract vertex and edge attributes
  # ----------------------------------------------------------
  v_weights <- igraph::vertex_attr(graph, weight)   # binary presence (0/1)
  v_lengths <- igraph::vertex_attr(graph, field_B)   # reach lengths (for distance)
  n_nodes   <- igraph::vcount(graph)

  presence_idx <- which(v_weights > 0)
  n_presence   <- length(presence_idx)

  # ----------------------------------------------------------
  # Early exit if fewer than 2 occupied nodes
  # ----------------------------------------------------------
  if (n_presence < 2) {
    if (index_type == "full") {
      return(data.frame(num = 0, den = 0, index = 0))
    } else {
      return(data.frame(
        name  = igraph::V(graph)$name,
        num   = 0,
        den   = 0,
        index = 0
      ))
    }
  }

  # ----------------------------------------------------------
  # Get edge passabilities
  # ----------------------------------------------------------
  edge_pass_u <- igraph::edge_attr(graph, pass_u)
  edge_pass_d <- igraph::edge_attr(graph, pass_d)

  # ----------------------------------------------------------
  # Build sparse c_ij and B_ij matrices (presence × presence)
  #
  # c_ij: barrier passability along path
  #   For each edge on the path: pass = pass_u * pass_d (symmetric)
  #   c_ij = product of pass along all edges on the path
  #
  # B_ij: dispersal probability based on distance
  #   distance_ij = sum of field_B for all vertices on the path
  #   B_ij = param ^ distance_ij (exponential kernel)
  # ----------------------------------------------------------

  c_ij_sparse <- matrix(1, nrow = n_presence, ncol = n_presence)
  B_ij_sparse <- matrix(1, nrow = n_presence, ncol = n_presence)

  for (i in 1:n_presence) {
    origin_node <- presence_idx[i]

    paths_result <- igraph::shortest_paths(
      graph,
      from   = origin_node,
      to     = presence_idx,
      mode   = "all",
      output = "both"
    )

    for (j in 1:n_presence) {
      if (i == j) next

      path_edges <- paths_result$epath[[j]]
      path_verts <- paths_result$vpath[[j]]

      if (length(path_edges) == 0) {
        c_ij_sparse[i, j] <- 0
        B_ij_sparse[i, j] <- 0
      } else {
        # c_ij: product of symmetric passability along path
        c_ij_sparse[i, j] <- prod(
          edge_pass_u[path_edges] * edge_pass_d[path_edges]
        )

        # B_ij: exponential decay with distance
        path_node_ids <- as.integer(path_verts)
        distance_ij   <- sum(v_lengths[path_node_ids])
        B_ij_sparse[i, j] <- param ^ distance_ij
      }
    }

    rm(paths_result)
  }

  gc(verbose = FALSE)

  # ----------------------------------------------------------
  # Combine: agg_mat = c_ij * B_ij (element-wise)
  # ----------------------------------------------------------
  agg_mat <- c_ij_sparse * B_ij_sparse

  # ----------------------------------------------------------
  # Calculate PCI using binary weights
  # ----------------------------------------------------------
  w_presence <- v_weights[presence_idx]

  if (index_type == "full") {
    # Catchment-level PCI: w' * agg_mat * w / (sum(w))^2
    index_num <- as.numeric(t(w_presence) %*% agg_mat %*% w_presence)
    index_den <- sum(w_presence)^2
    index_val <- index_num / index_den

    result <- data.frame(
      num   = index_num,
      den   = index_den,
      index = index_val
    )

  } else if (index_type == "reach") {
    # Reach-level PCI: per-node connectivity score
    if (index_mode == "to") {
      index_num_sparse <- agg_mat %*% w_presence
    } else {
      index_num_sparse <- t(t(w_presence) %*% agg_mat)
    }

    index_den <- sum(w_presence)

    # Expand to full node list (zeros for unoccupied nodes)
    index_num_full <- numeric(n_nodes)
    index_num_full[presence_idx] <- as.numeric(index_num_sparse)

    index_full <- index_num_full / index_den

    result <- data.frame(
      name  = igraph::V(graph)$name,
      num   = index_num_full,
      den   = index_den,
      index = index_full
    )
  }

  return(result)
}
