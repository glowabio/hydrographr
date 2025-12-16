#' Get upstream and downstream network buffer along stream segments
#'
#' @description
#' Given a stream network and starting points (either segment IDs or sf points),
#' this function extracts the segments along the network within a specified
#' upstream and/or downstream distance (buffer). It handles points snapped to
#' stream segments and can clip partial segments based on the start fraction.
#'
#' @param lines_sf An `sf` object representing the stream network. Must have
#'   columns `subc_id` (segment ID), `target` (downstream segment), `length`
#'   (segment length), and LINESTRING geometry.
#' @param target_ids Optional integer or character vector of segment IDs to start
#'   the buffer from. Provide either `target_ids` OR `start_points`, not both.
#' @param start_points Optional `sf` object of POINT geometries representing
#'   starting locations on the network. Provide either `target_ids` OR `start_points`.
#' @param up_radius Numeric. Distance (in units of `lines_sf`) to trace upstream.
#'   Default is 0 (no upstream buffer).
#' @param down_radius Numeric. Distance (in units of `lines_sf`) to trace downstream.
#'   Default is 0 (no downstream buffer).
#'
#' @return An `sf` object containing all segments within the requested upstream
#'   and downstream distances for each starting point. Additional columns:
#'   - `point_id`: identifier of starting point
#'   - `target_segment`: ID of segment used as starting point
#'   - `start_fraction`: fraction along starting segment where the point lies
#'   - `direction`: "upstream" or "downstream"
#'   - `search_distance`: the requested buffer distance
#'
#' @details
#' - Supports both single segments (`target_ids`) or arbitrary points (`start_points`)
#' - Clips segments proportionally if the buffer does not reach the full segment length
#' - Handles branching upstream segments and avoids duplicate segments for the same point
#' - Requires `sf`, `dplyr`, and helper functions `traverse_upstream()` and `traverse_downstream()`
#'
#' @export
get_buffer_along_the_network <- function(
    lines_sf,
    target_ids = NULL,
    start_points = NULL,
    up_radius = 0,
    down_radius = 0
) {

  # ---- Pre-checks ----
  if (!("subc_id" %in% colnames(lines_sf)))
    stop("lines_sf must have 'subc_id' column")
  if (!("target" %in% colnames(lines_sf)))
    stop("lines_sf must have 'target' column")
  if (!("length" %in% colnames(lines_sf)))
    stop("lines_sf must have 'length' column")

  if (!is.numeric(up_radius) || !is.numeric(down_radius) || up_radius < 0 || down_radius < 0)
    stop("up_radius and down_radius must be non-negative numbers")

  if (up_radius == 0 && down_radius == 0)
    warning("Both up_radius and down_radius are 0, returning empty result")

  if (!is.null(target_ids) && !is.null(start_points)) {
    stop("Please provide either target_ids OR start_points, not both")
  }
  if (is.null(target_ids) && is.null(start_points)) {
    stop("Please provide either target_ids or start_points")
  }

  # ---- Determine starting points ----
  start_info <- list()

  if (!is.null(target_ids)) {
    target_ids <- unique(as.character(target_ids))
    missing_ids <- target_ids[!target_ids %in% lines_sf$subc_id]
    if (length(missing_ids) > 0)
      stop("target_ids not found in network: ", paste(missing_ids, collapse = ", "))

    for (tid in target_ids) {
      start_info[[length(start_info) + 1]] <- list(
        segment_id = tid,
        start_fraction = 0,
        point_id = tid
      )
    }

  } else if (!is.null(start_points)) {
    if (!inherits(start_points, "sf"))
      stop("start_points must be an sf object")
    if (!all(sf::st_is(start_points, "POINT")))
      stop("All start_points must be POINT geometries")
    if (sf::st_crs(start_points) != sf::st_crs(lines_sf))
      start_points <- sf::st_transform(start_points, sf::st_crs(lines_sf))

    # Store the target CRS for later use
    target_crs <- sf::st_crs(lines_sf)

    # Check if start_points has subc_id_after column (already snapped)
    if ("subc_id_after" %in% colnames(start_points)) {
      # Use the already-snapped segment IDs
      for (i in seq_len(nrow(start_points))) {
        pt <- start_points[i, ]
        segment_id <- as.character(pt$subc_id_after)

        # Find the segment in lines_sf
        seg <- lines_sf %>% dplyr::filter(subc_id == segment_id)

        if (nrow(seg) == 0) {
          warning(sprintf("Point %s: subc_id_after '%s' not found in network, skipping", i, segment_id))
          next
        }

        # Calculate fraction along segment
        # Extract geometries as sfc (not sfg) to preserve CRS
        pt_geom <- sf::st_geometry(pt)
        line_geom <- sf::st_geometry(seg)

        # Find nearest point on line to the snapped point
        nearest_pt <- sf::st_nearest_points(pt_geom, line_geom)
        nearest_pt_on_line <- sf::st_cast(nearest_pt, "POINT")[2]

        # Calculate distance from start of line to nearest point
        line_coords <- sf::st_coordinates(line_geom[[1]])
        start_pt <- sf::st_sfc(sf::st_point(line_coords[1, c("X", "Y")]), crs = target_crs)

        dist_from_start <- sf::st_distance(start_pt, nearest_pt_on_line)[1]
        total_length <- seg$length[1]

        start_fraction <- as.numeric(dist_from_start / total_length)
        start_fraction <- max(0, min(1, start_fraction))  # Clamp to [0,1]

        # Create point ID
        point_id <- if ("site_id" %in% colnames(pt)) {
          as.character(pt$site_id)
        } else {
          paste0("point_", i)
        }

        start_info[[length(start_info) + 1]] <- list(
          segment_id = segment_id,
          start_fraction = start_fraction,
          point_id = point_id
        )
      }
    } else {
      # Need to find nearest segment for each point
      for (i in seq_len(nrow(start_points))) {
        pt <- start_points[i, ]
        pt_geom <- sf::st_geometry(pt)

        # Find nearest segment
        distances <- sf::st_distance(pt_geom, lines_sf)
        nearest_idx <- which.min(distances[1, ])
        nearest_seg <- lines_sf[nearest_idx, ]

        segment_id <- as.character(nearest_seg$subc_id)
        line_geom <- sf::st_geometry(nearest_seg)

        # Calculate fraction along segment
        nearest_pt <- sf::st_nearest_points(pt_geom, line_geom)
        nearest_pt_on_line <- sf::st_cast(nearest_pt, "POINT")[2]

        line_coords <- sf::st_coordinates(line_geom[[1]])
        start_pt <- sf::st_sfc(sf::st_point(line_coords[1, c("X", "Y")]), crs = target_crs)

        dist_from_start <- sf::st_distance(start_pt, nearest_pt_on_line)[1]
        total_length <- nearest_seg$length[1]

        start_fraction <- as.numeric(dist_from_start / total_length)
        start_fraction <- max(0, min(1, start_fraction))

        point_id <- if ("site_id" %in% colnames(pt)) {
          as.character(pt$site_id)
        } else {
          paste0("point_", i)
        }

        start_info[[length(start_info) + 1]] <- list(
          segment_id = segment_id,
          start_fraction = start_fraction,
          point_id = point_id
        )
      }
    }
  }

  if (length(start_info) == 0)
    stop("No valid starting points could be determined")

  # ---- Process each starting point ----
  results <- list()
  for (info in start_info) {
    segment_id <- info$segment_id
    start_fraction <- info$start_fraction
    point_id <- info$point_id

    if (up_radius > 0) {
      upstream_result <- traverse_upstream(lines_sf, segment_id, start_fraction, up_radius)
      for (seg_info in upstream_result) {
        seg_row <- lines_sf %>% dplyr::filter(subc_id == seg_info$subc_id)
        if (nrow(seg_row) > 0) {
          seg_row$point_id <- point_id
          seg_row$target_segment <- segment_id
          seg_row$start_fraction <- start_fraction
          seg_row$direction <- "upstream"
          seg_row$search_distance <- up_radius
          seg_row$fraction_start <- seg_info$fraction_start
          seg_row$fraction_end <- seg_info$fraction_end
          results[[length(results) + 1]] <- seg_row
        }
      }
    }

    if (down_radius > 0) {
      downstream_result <- traverse_downstream(lines_sf, segment_id, start_fraction, down_radius)
      for (seg_info in downstream_result) {
        seg_row <- lines_sf %>% dplyr::filter(subc_id == seg_info$subc_id)
        if (nrow(seg_row) > 0) {
          seg_row$point_id <- point_id
          seg_row$target_segment <- segment_id
          seg_row$start_fraction <- start_fraction
          seg_row$direction <- "downstream"
          seg_row$search_distance <- down_radius
          seg_row$fraction_start <- seg_info$fraction_start
          seg_row$fraction_end <- seg_info$fraction_end
          results[[length(results) + 1]] <- seg_row
        }
      }
    }
  }

  # ---- Combine all results ----
  if (length(results) == 0) {
    empty_df <- lines_sf[0, ]
    empty_df$point_id <- character(0)
    empty_df$target_segment <- character(0)
    empty_df$start_fraction <- numeric(0)
    empty_df$direction <- character(0)
    empty_df$search_distance <- numeric(0)
    return(empty_df)
  }

  final_result <- do.call(rbind, results)

  return(final_result)
}
