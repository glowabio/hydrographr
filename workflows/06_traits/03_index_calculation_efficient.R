#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# MEMORY-EFFICIENT index_calculation
# Matches riverconn::index_calculation behavior but only computes
# connectivity for nodes with presence (weight > 0)
#
# FIXES vs previous version:
#   1. B_ij (distance decay) now computed: B_ij = param ^ distance_ij
#   2. c_ij (passability) computed separately: product of edge passabilities
#   3. Uses binary weight attribute (not length_reach) for PCI formula
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

index_calculation_efficient <- function(graph,
                                        weight = "weight",
                                        field_B = "length_reach",
                                        param = 0.5,
                                        index_type = "full",
                                        index_mode = "to",
                                        pass_u = "pass_u",
                                        pass_d = "pass_d") {

  # Get vertex attributes
  v_weights  <- vertex_attr(graph, weight)    # binary presence (0/1)
  v_lengths  <- vertex_attr(graph, field_B)   # reach lengths (for distance calc)
  n_nodes    <- vcount(graph)

  # Identify presence nodes (weight > 0)
  presence_idx <- which(v_weights > 0)
  n_presence   <- length(presence_idx)

  # Early exit if insufficient presence
  if (n_presence < 2) {
    if (index_type == "full") {
      return(data.frame(num = 0, den = 0, index = 0))
    } else {
      return(data.frame(
        name  = V(graph)$name,
        num   = 0,
        den   = 0,
        index = 0
      ))
    }
  }

  # Get edge passabilities
  edge_pass_u <- edge_attr(graph, pass_u)
  edge_pass_d <- edge_attr(graph, pass_d)

  # ============================================================
  # Build c_ij (passability) and B_ij (distance decay) matrices
  # Only for presence × presence pairs
  # ============================================================

  c_ij_sparse <- matrix(1, nrow = n_presence, ncol = n_presence)
  B_ij_sparse <- matrix(1, nrow = n_presence, ncol = n_presence)

  # For symmetric dispersal, use undirected shortest paths
  # (riverconn default: dir_fragmentation_type = "symmetric", dir_distance_type = "symmetric")
  # This means: use both pass_u and pass_d, and distance is undirected

  for (i in 1:n_presence) {
    origin_node <- presence_idx[i]

    # Get shortest paths (both vertex and edge paths)
    paths_result <- shortest_paths(
      graph,
      from = origin_node,
      to = presence_idx,
      mode = "all",           # undirected (symmetric)
      output = "both"         # get both vpath and epath
    )

    for (j in 1:n_presence) {
      if (i == j) next  # diagonal stays 1

      path_edges <- paths_result$epath[[j]]
      path_verts <- paths_result$vpath[[j]]

      if (length(path_edges) == 0) {
        # Not reachable
        c_ij_sparse[i, j] <- 0
        B_ij_sparse[i, j] <- 0
      } else {

        # --- c_ij: passability along path ---
        # Symmetric: use geometric mean of pass_u and pass_d per edge
        # (matches riverconn symmetric mode: pass = pass_u * pass_d per edge,
        #  then c_ij = product of sqrt(pass) along path...
        #  but actually riverconn symmetric c_ij_fun computes it differently)
        # Simplest symmetric approach: product of (pass_u * pass_d) for each edge
        pass_along_path <- prod(edge_pass_u[path_edges] * edge_pass_d[path_edges])
        c_ij_sparse[i, j] <- pass_along_path

        # --- B_ij: distance decay ---
        # Distance = sum of length_reach for all INTERMEDIATE nodes on path
        # (riverconn sums field_B for vertices along the path)
        path_node_ids <- as.integer(path_verts)
        distance_ij <- sum(v_lengths[path_node_ids])

        # B_ij = param ^ distance
        B_ij_sparse[i, j] <- param ^ distance_ij
      }
    }

    rm(paths_result)
  }

  gc(verbose = FALSE)

  # ============================================================
  # Combine: agg_mat = c_ij * B_ij
  # ============================================================
  agg_mat <- c_ij_sparse * B_ij_sparse

  # ============================================================
  # Calculate index using binary WEIGHTS (not lengths!)
  # ============================================================

  # Weights for presence nodes only
  w_presence <- v_weights[presence_idx]

  if (index_type == "full") {
    # PCI = w' * agg_mat * w / (sum(w))^2
    index_num <- as.numeric(t(w_presence) %*% agg_mat %*% w_presence)
    index_den <- sum(w_presence)^2
    index_val <- index_num / index_den

    result <- data.frame(
      num   = index_num,
      den   = index_den,
      index = index_val
    )

  } else if (index_type == "reach") {

    if (index_mode == "to") {
      # For each node i: how well can others reach it?
      index_num_sparse <- agg_mat %*% w_presence
    } else {
      # For each node i: how well can it reach others?
      index_num_sparse <- t(t(w_presence) %*% agg_mat)
    }

    index_den <- sum(w_presence)

    # Expand back to full node list
    index_num_full <- numeric(n_nodes)
    index_num_full[presence_idx] <- as.numeric(index_num_sparse)

    index_full <- index_num_full / index_den

    result <- data.frame(
      name  = V(graph)$name,
      num   = index_num_full,
      den   = index_den,
      index = index_full
    )
  }

  return(result)
}
