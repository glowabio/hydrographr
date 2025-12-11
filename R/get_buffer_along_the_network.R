#' Get upstream and downstream network buffer along stream segments
#'
#' @description
#' Extracts all segments along a stream network within specified upstream
#' and/or downstream distances from either segment IDs or sf points.
#'
#' @param lines_sf An `sf` object representing the stream network. Must have
#'   columns `subc_id`, `target`, `length`, and LINESTRING geometry.
#' @param target_ids Optional vector of segment IDs to start from (exclusive with start_points)
#' @param start_points Optional sf POINT object to start from (exclusive with target_ids)
#' @param up_radius Numeric distance to traverse upstream (default 0)
#' @param down_radius Numeric distance to traverse downstream (default 0)
#'
#' @return An sf object containing all segments within requested distances.
#'   Columns include:
#'   - `point_id`: identifier of starting point
#'   - `target_segment`: segment used as starting point
#'   - `start_fraction`: fraction along starting segment
#'   - `direction`: "upstream" or "downstream"
#'   - `search_distance`: requested buffer distance
#'
#' @export
get_buffer_along_the_network <- function(lines_sf, target_ids = NULL, start_points = NULL,
                                         up_radius = 0, down_radius = 0) {

  # ---- Pre-checks ----
  if (!("subc_id" %in% colnames(lines_sf)))
    stop("lines_sf must have 'subc_id' column")
  if (!("target" %in% colnames(lines_sf)))
    stop("lines_sf must have 'target' column")
  if (!("length" %in% colnames(lines_sf)))
    stop("lines_sf must have 'length' column")

  if (!is.numeric(up_radius) || !is.numeric(down_radius) ||
      up_radius < 0 || down_radius < 0)
    stop("up_radius and down_radius must be non-negative numbers")

  if (up_radius == 0 && down_radius == 0)
    warning("Both up_radius and down_radius are 0, returning empty result")

  # ---- Determine input type and get starting information ----
  if (!is.null(target_ids) && !is.null(start_points)) {
    stop("Please provide either target_ids OR start_points, not both")
  }

  if (is.null(target_ids) && is.null(start_points)) {
    stop("Please provide either target_ids or start_points")
  }

  # Structure to store starting information
  start_info <- list()

  if (!is.null(target_ids)) {
    # Case 1: Direct segment IDs provided - start from beginning of each segment
    target_ids <- unique(as.character(target_ids))
    missing_ids <- target_ids[!target_ids %in% lines_sf$subc_id]
    if (length(missing_ids) > 0)
      stop("target_ids not found in network: ", paste(missing_ids, collapse = ", "))

    for (target_id in target_ids) {
      seg <- lines_sf %>% filter(subc_id == target_id)
      if (nrow(seg) > 0) {
        # Start from beginning of segment (fraction = 0)
        start_info[[length(start_info) + 1]] <- list(
          segment_id = target_id,
          start_fraction = 0,  # Start at beginning
          point_id = target_id  # Use segment ID as point ID
        )
      }
    }

  } else if (!is.null(start_points)) {
    # Case 2: Coordinates provided - trust the user that they're correctly snapped
    if (!inherits(start_points, "sf")) {
      stop("start_points must be an sf object")
    }

    # Check if all geometries are points
    if (!all(st_is(start_points, "POINT"))) {
      stop("All start_points must be POINT geometry")
    }

    # Ensure same CRS - convert if needed
    if (st_crs(start_points) != st_crs(lines_sf)) {
      start_points <- st_transform(start_points, st_crs(lines_sf))
    }

    # Extract coordinates from points for distance calculation
    point_coords <- st_coordinates(start_points)

    # For each point, find nearest segment
    for (i in 1:nrow(start_points)) {
      point <- start_points[i, ]
      point_xy <- point_coords[i, ]

      # Find nearest segment using distance calculation
      # Calculate distances manually to avoid st_nearest_points issues
      distances <- numeric(nrow(lines_sf))
      segment_coords_list <- list()

      for (j in 1:nrow(lines_sf)) {
        seg <- lines_sf[j, ]
        seg_coords <- st_coordinates(st_geometry(seg)[[1]])
        segment_coords_list[[j]] <- seg_coords

        # Calculate minimum distance from point to any vertex of the segment
        vertex_dists <- sqrt(rowSums((seg_coords[, 1:2] -
                                        matrix(point_xy, nrow = nrow(seg_coords),
                                               ncol = 2, byrow = TRUE))^2))
        distances[j] <- min(vertex_dists)
      }

      nearest_idx <- which.min(distances)
      seg <- lines_sf[nearest_idx, ]
      seg_coords <- segment_coords_list[[nearest_idx]]
      seg_geom <- st_geometry(seg)[[1]]

      # Calculate approximate fraction along segment
      # Find which vertex is closest
      vertex_dists <- sqrt(rowSums((seg_coords[, 1:2] -
                                      matrix(point_xy, nrow = nrow(seg_coords),
                                             ncol = 2, byrow = TRUE))^2))
      closest_vertex_idx <- which.min(vertex_dists)
      closest_vertex <- seg_coords[closest_vertex_idx, 1:2]

      # Calculate cumulative distance along the segment to the closest vertex
      seg_length_total <- as.numeric(st_length(seg_geom))

      # Calculate distance along line to the closest vertex
      if (closest_vertex_idx == 1) {
        dist_to_vertex <- 0
      } else {
        # Sum distances of all segments up to this vertex
        dist_to_vertex <- 0
        for (k in 1:(closest_vertex_idx - 1)) {
          p1 <- seg_coords[k, 1:2]
          p2 <- seg_coords[k + 1, 1:2]
          dist_to_vertex <- dist_to_vertex + sqrt(sum((p2 - p1)^2))
        }
      }

      # Adjust for distance from point to the closest vertex
      dist_point_to_vertex <- vertex_dists[closest_vertex_idx]

      # If point is very close to start of segment, add distance
      # If point is very close to end of segment, subtract distance
      # This is a simplification but should work for snapped points
      if (closest_vertex_idx == 1) {
        # Point near start of segment
        dist_along <- dist_point_to_vertex
      } else if (closest_vertex_idx == nrow(seg_coords)) {
        # Point near end of segment
        dist_along <- dist_to_vertex - dist_point_to_vertex
      } else {
        # Point somewhere in middle - use distance to previous vertex plus adjustment
        # Determine if point is closer to segment before or after the vertex
        if (closest_vertex_idx > 1) {
          prev_vertex <- seg_coords[closest_vertex_idx - 1, 1:2]
          seg_vector <- closest_vertex - prev_vertex
          point_vector <- point_xy - prev_vertex

          # Project point onto segment
          seg_length <- sqrt(sum(seg_vector^2))
          if (seg_length > 0) {
            projection <- sum(point_vector * seg_vector) / seg_length
            projection <- max(0, min(seg_length, projection))  # Clamp to segment

            # Distance along to this projected point
            dist_along <- dist_to_vertex - seg_length + projection
          } else {
            dist_along <- dist_to_vertex
          }
        } else {
          dist_along <- dist_to_vertex
        }
      }

      # Calculate fraction (ensure it's between 0 and 1)
      fraction <- dist_along / seg_length_total
      fraction <- max(0, min(1, fraction))

      # Get point ID if available
      point_id <- if ("site_id" %in% colnames(start_points)) {
        as.character(start_points$site_id[i])
      } else if ("id" %in% colnames(start_points)) {
        as.character(start_points$id[i])
      } else {
        paste0("point_", i)
      }

      start_info[[length(start_info) + 1]] <- list(
        segment_id = seg$subc_id,
        start_fraction = fraction,
        point_id = point_id
      )

      # Debug output
      message(sprintf("Point %s: segment_id = %s, fraction = %.3f",
                      point_id, seg$subc_id, fraction))
    }
  }

  if (length(start_info) == 0) {
    stop("No valid starting points could be determined")
  }

  # Helper function for downstream traversal from a specific point on a segment
  traverse_downstream <- function(lines_sf, start_segment_id, start_fraction, X) {

    out_geoms <- list()
    out_data <- list()
    cumdist <- 0
    current_id <- start_segment_id

    # First segment: start from start_fraction
    seg <- lines_sf %>% filter(subc_id == current_id)
    if (nrow(seg) == 0) break
    if (nrow(seg) > 1) {
      warning("Multiple segments found with same subc_id: ", current_id,
              " — using the first one")
      seg <- seg[1, ]
    }

    seg_len <- seg$length

    if (nrow(seg) == 0) return(list(geoms = list(), data = list()))

    seg_len <- seg$length
    remaining_in_first_seg <- seg_len * (1 - start_fraction)

    # Check if we can stay within first segment
    if (cumdist + remaining_in_first_seg <= X) {
      # Use the remaining part of first segment
      clipped_geom <- st_linesubstring(
        st_geometry(seg)[[1]],
        from = start_fraction,
        to = 1
      )
      out_geoms <- append(out_geoms, list(clipped_geom))

      # Update segment data
      seg_data <- st_drop_geometry(seg)
      seg_data$length <- remaining_in_first_seg
      out_data <- append(out_data, list(seg_data))

      cumdist <- cumdist + remaining_in_first_seg

      # Move to next downstream segment
      next_id <- seg$target
      if (is.na(next_id) || next_id == "" || !next_id %in% lines_sf$subc_id) {
        return(list(geoms = out_geoms, data = out_data))
      }
      current_id <- next_id

    } else if (cumdist < X) {
      # Need to clip inside first segment
      remain <- X - cumdist
      frac_in_segment <- remain / seg_len
      end_fraction <- start_fraction + frac_in_segment

      clipped_geom <- st_linesubstring(
        st_geometry(seg)[[1]],
        from = start_fraction,
        to = min(end_fraction, 1)
      )
      out_geoms <- append(out_geoms, list(clipped_geom))

      # Update segment data
      seg_data <- st_drop_geometry(seg)
      seg_data$length <- seg_len * (end_fraction - start_fraction)
      out_data <- append(out_data, list(seg_data))

      return(list(geoms = out_geoms, data = out_data))
    }

    # Continue with remaining segments (whole segments from beginning)
    while (TRUE) {
      seg <- lines_sf %>% filter(subc_id == current_id)
      if (nrow(seg) == 0) break
      if (nrow(seg) > 1) {
        warning("Multiple segments found with same subc_id: ", current_id,
                " — using the first one")
        seg <- seg[1, ]
      }

      seg_len <- seg$length


      # Case: whole segment fits
      if (cumdist + seg_len <= X) {
        out_geoms <- append(out_geoms, list(st_geometry(seg)[[1]]))
        out_data <- append(out_data, list(st_drop_geometry(seg)))
        cumdist <- cumdist + seg_len

        # Move to next downstream segment
        next_id <- seg$target
        if (is.na(next_id) || next_id == "" || !next_id %in% lines_sf$subc_id) break
        current_id <- next_id

      } else if (cumdist < X) {
        # Need to clip inside the segment
        remain <- X - cumdist
        frac <- remain / seg_len

        clipped_geom <- st_linesubstring(
          st_geometry(seg)[[1]],
          from = 0,
          to = min(frac, 1)
        )
        out_geoms <- append(out_geoms, list(clipped_geom))

        # Update segment data
        seg_data <- st_drop_geometry(seg)
        seg_data$length <- seg_len * min(frac, 1)
        out_data <- append(out_data, list(seg_data))

        break

      } else {
        break
      }
    }

    return(list(geoms = out_geoms, data = out_data))
  }

  # Helper function for upstream traversal from a specific point on a segment
  traverse_upstream <- function(lines_sf, start_segment_id, start_fraction, X) {

    # Create upstream adjacency list
    upstream_map <- list()
    for (i in 1:nrow(lines_sf)) {
      seg <- lines_sf[i, ]
      target <- as.character(seg$target)
      if (!is.na(target) && target != "") {
        if (!target %in% names(upstream_map)) {
          upstream_map[[target]] <- c()
        }
        upstream_map[[target]] <- c(upstream_map[[target]], as.character(seg$subc_id))
      }
    }

    # First segment: start from start_fraction (going backward/upstream)
    out_geoms <- list()
    out_data <- list()
    cumdist <- 0

    # Process the first segment (the one containing the start point)
    seg <- lines_sf %>% filter(subc_id == start_segment_id)
    if (nrow(seg) == 0) return(list(geoms = list(), data = list()))

    seg_len <- seg$length
    available_in_first_seg <- seg_len * start_fraction  # Distance from point to start of segment

    # Check if we can stay within first segment going upstream
    if (cumdist + available_in_first_seg <= X) {
      # Use the upstream part of first segment
      clipped_geom <- st_linesubstring(
        st_geometry(seg)[[1]],
        from = 0,
        to = start_fraction
      )
      out_geoms <- append(out_geoms, list(clipped_geom))

      # Update segment data
      seg_data <- st_drop_geometry(seg)
      seg_data$length <- available_in_first_seg
      out_data <- append(out_data, list(seg_data))

      cumdist <- cumdist + available_in_first_seg

      # Check for upstream branches from the START of this segment
      # FIX: Check if start_segment_id exists in upstream_map first
      upstream_ids <- if (!is.null(upstream_map[[as.character(start_segment_id)]])) {
        upstream_map[[as.character(start_segment_id)]]
      } else {
        NULL
      }

      if (!is.null(upstream_ids) && length(upstream_ids) > 0) {
        # Initialize queue for upstream branches
        queue <- list()
        for (up_id in upstream_ids) {
          queue[[length(queue) + 1]] <- list(
            cumdist = cumdist,
            current_id = up_id
          )
        }

        # Process upstream branches
        best_distance <- list()

        while (length(queue) > 0) {
          current <- queue[[1]]
          queue <- queue[-1]

          seg_id <- as.character(current$current_id)
          seg <- lines_sf %>% filter(subc_id == seg_id)

          if (nrow(seg) == 0) next

          seg_len <- seg$length
          seg_data <- st_drop_geometry(seg)

          # Check if we've already reached this segment with a shorter or equal distance
          if (seg_id %in% names(best_distance) && best_distance[[seg_id]] <= current$cumdist) {
            # Still continue upstream from here if it's a branching point
            # FIX: Check if seg_id exists in upstream_map first
            upstream_ids <- if (!is.null(upstream_map[[seg_id]])) {
              upstream_map[[seg_id]]
            } else {
              NULL
            }

            if (!is.null(upstream_ids) && length(upstream_ids) > 0) {
              for (up_id in upstream_ids) {
                queue[[length(queue) + 1]] <- list(
                  cumdist = current$cumdist + seg_len,
                  current_id = up_id
                )
              }
            }
            next
          }

          # Record the best distance to this segment
          best_distance[[seg_id]] <- current$cumdist

          # Check if we can add this whole segment
          if (current$cumdist + seg_len <= X) {
            # Store whole segment
            out_geoms <- append(out_geoms, list(st_geometry(seg)[[1]]))
            out_data <- append(out_data, list(seg_data))

            # Check for upstream branches
            # FIX: Check if seg_id exists in upstream_map first
            upstream_ids <- if (!is.null(upstream_map[[seg_id]])) {
              upstream_map[[seg_id]]
            } else {
              NULL
            }

            new_cumdist <- current$cumdist + seg_len

            if (!is.null(upstream_ids) && length(upstream_ids) > 0) {
              for (up_id in upstream_ids) {
                queue[[length(queue) + 1]] <- list(
                  cumdist = new_cumdist,
                  current_id = up_id
                )
              }
            }

          } else if (current$cumdist < X) {
            # Need to clip this segment - keep the END for upstream
            remain <- X - current$cumdist
            frac <- remain / seg_len

            clipped_geom <- st_linesubstring(
              st_geometry(seg)[[1]],
              from = max(0, 1 - frac),
              to = 1
            )

            # Update segment data with new length
            seg_data$length <- seg_len * min(frac, 1)

            out_geoms <- append(out_geoms, list(clipped_geom))
            out_data <- append(out_data, list(seg_data))

          }
        }
      }

    } else if (cumdist < X) {
      # Need to clip inside first segment going upstream
      remain <- X - cumdist
      frac_in_segment <- remain / seg_len
      start_fraction_clip <- start_fraction - frac_in_segment

      clipped_geom <- st_linesubstring(
        st_geometry(seg)[[1]],
        from = max(0, start_fraction_clip),
        to = start_fraction
      )
      out_geoms <- append(out_geoms, list(clipped_geom))

      # Update segment data
      seg_data <- st_drop_geometry(seg)
      seg_data$length <- seg_len * (start_fraction - max(0, start_fraction_clip))
      out_data <- append(out_data, list(seg_data))
    }

    return(list(geoms = out_geoms, data = out_data))
  }

  # ---- Process each starting point ----
  results <- list()

  for (i in seq_along(start_info)) {
    info <- start_info[[i]]
    segment_id <- info$segment_id
    start_fraction <- info$start_fraction
    point_id <- info$point_id

    # Process upstream if radius > 0
    if (up_radius > 0) {
      upstream_result <- traverse_upstream(lines_sf, segment_id, start_fraction, up_radius)
      upstream_geoms <- upstream_result$geoms
      upstream_data <- upstream_result$data

      if (length(upstream_data) > 0) {
        upstream_df <- bind_rows(upstream_data)
        upstream_df$geometry <- st_sfc(upstream_geoms, crs = st_crs(lines_sf))
        upstream_df$point_id <- point_id
        upstream_df$target_segment <- segment_id
        upstream_df$start_fraction <- start_fraction
        upstream_df$direction <- "upstream"
        upstream_df$search_distance <- up_radius

        # Remove duplicates (keep first occurrence = shortest distance)
        upstream_df <- upstream_df[!duplicated(upstream_df[, c("point_id", "subc_id")]), ]

        results <- append(results, list(st_as_sf(upstream_df)))
      }
    }

    # Process downstream if radius > 0
    if (down_radius > 0) {
      downstream_result <- traverse_downstream(lines_sf, segment_id, start_fraction, down_radius)
      downstream_geoms <- downstream_result$geoms
      downstream_data <- downstream_result$data

      if (length(downstream_data) > 0) {
        downstream_df <- bind_rows(downstream_data)
        downstream_df$geometry <- st_sfc(downstream_geoms, crs = st_crs(lines_sf))
        downstream_df$point_id <- point_id
        downstream_df$target_segment <- segment_id
        downstream_df$start_fraction <- start_fraction
        downstream_df$direction <- "downstream"
        downstream_df$search_distance <- down_radius

        results <- append(results, list(st_as_sf(downstream_df)))
      }
    }
  }

  # ---- Combine all results ----
  if (length(results) == 0) {
    # Return empty sf with correct structure
    empty_df <- lines_sf[0, ]
    empty_df$point_id <- character(0)
    empty_df$target_segment <- character(0)
    empty_df$start_fraction <- numeric(0)
    empty_df$direction <- character(0)
    empty_df$search_distance <- numeric(0)
    return(empty_df)
  }

  # Combine results
  final_result <- do.call(rbind, results)

  # Reorder columns nicely
  final_cols <- c("point_id", "target_segment", "subc_id", "start_fraction", "direction",
                  "search_distance", setdiff(colnames(lines_sf), c("subc_id", "geometry")))

  # Ensure all columns exist
  missing_cols <- setdiff(final_cols, colnames(final_result))
  for (col in missing_cols) {
    if (col %in% colnames(lines_sf)) {
      final_result[[col]] <- NA
    }
  }

  # Select and order columns
  final_result <- final_result %>%
    select(any_of(final_cols), geometry)

  return(final_result)
}
