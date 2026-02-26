#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# MEMORY-EFFICIENT index_calculation
# Only computes connectivity for nodes with presence (weight > 0)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

index_calculation_efficient <- function(graph,
                                        weight = "weight",
                                        field_B = "length_reach",
                                        param = 0.5,
                                        index_type = "full",
                                        index_mode = "to",
                                        pass_u = "pass_u",
                                        pass_d = "pass_d") {

  # Input validation
  if (!igraph::is_connected(graph)) {
    stop("'graph' must be connected")
  }

  # Get vertex attributes
  v_weights <- vertex_attr(graph, weight)
  v_lengths <- vertex_attr(graph, field_B)
  n_nodes <- vcount(graph)

  # Identify presence nodes (weight > 0)
  presence_idx <- which(v_weights > 0)
  n_presence <- length(presence_idx)

  # Early exit if insufficient presence
  if (n_presence < 2) {
    if (index_type == "full") {
      return(data.frame(num = 0, den = 0, index = 0))
    } else {
      return(data.frame(
        name = V(graph)$name,
        num = 0,
        den = 0,
        index = 0
      ))
    }
  }

  # ============================================================
  # Convert index_mode to shortest_paths mode
  # ============================================================
  sp_mode <- if (index_mode == "to") "out" else "in"

  # ============================================================
  # Build SPARSE connectivity matrix (only for presence nodes)
  # LOOP-BASED APPROACH (more memory efficient + handles disconnected nodes)
  # ============================================================

  # Initialize sparse matrix (only presence × presence)
  c_ij_sparse <- matrix(0, nrow = n_presence, ncol = n_presence)
  diag(c_ij_sparse) <- 1  # Self-connectivity = 1

  # Get edge passabilities
  edge_pass_u <- edge_attr(graph, pass_u)
  edge_pass_d <- edge_attr(graph, pass_d)

  # Calculate connectivity for each origin node
  for (i in 1:n_presence) {
    origin_node <- presence_idx[i]

    # Get paths from this origin to all other presence nodes
    paths_from_i <- shortest_paths(
      graph,
      from = origin_node,
      to = presence_idx,
      mode = sp_mode,
      output = "epath"
    )$epath

    # Process each destination
    for (j in 1:n_presence) {
      if (i == j) next  # Already set diagonal to 1

      path_edges <- paths_from_i[[j]]

      if (length(path_edges) == 0) {
        c_ij_sparse[i, j] <- 0  # Not connected
      } else {
        # Calculate passability along path
        if (index_mode == "to") {
          pass_path <- prod(edge_pass_d[path_edges])
        } else {
          pass_path <- prod(edge_pass_u[path_edges])
        }

        # Apply dispersal decay: c_ij = passability ^ param
        c_ij_sparse[i, j] <- pass_path ^ param
      }
    }

    # Clean up after each origin to save memory
    rm(paths_from_i)
  }

  # Force garbage collection
  gc(verbose = FALSE)

  # ============================================================
  # Calculate index based on type
  # ============================================================

  if (index_type == "full") {
    # Catchment-level index (single value)

    # Get weights and lengths for presence nodes only
    v_weights_presence <- v_weights[presence_idx]
    v_lengths_presence <- v_lengths[presence_idx]

    # Weight by habitat length (not just presence)
    weighted_connectivity <- c_ij_sparse * outer(v_lengths_presence, v_lengths_presence)

    # Index = sum(c_ij * L_i * L_j) / (sum(L))^2
    index_num <- sum(weighted_connectivity)
    index_den <- sum(v_lengths_presence)^2
    index_val <- index_num / index_den

    result <- data.frame(
      num = index_num,
      den = index_den,
      index = index_val
    )

  } else if (index_type == "reach") {
    # Per-node contribution

    v_weights_presence <- v_weights[presence_idx]
    v_lengths_presence <- v_lengths[presence_idx]

    if (index_mode == "to") {
      # How well can node i reach others?
      # Sum across columns (destinations), weighted by their habitat
      index_num_sparse <- c_ij_sparse %*% v_lengths_presence
    } else {
      # How well can others reach node i?
      # Sum across rows (origins), weighted by their habitat
      index_num_sparse <- t(t(v_lengths_presence) %*% c_ij_sparse)
    }

    index_den <- sum(v_lengths_presence)

    # Expand back to full node list (zeros for non-presence nodes)
    index_num_full <- numeric(n_nodes)
    index_num_full[presence_idx] <- index_num_sparse

    index_full <- index_num_full / index_den

    result <- data.frame(
      name = V(graph)$name,
      num = index_num_full,
      den = index_den,
      index = index_full
    )
  }

  return(result)
}
