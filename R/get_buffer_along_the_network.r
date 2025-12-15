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
#' - Requires `sf`, `dplyr`, `purrr`, and `st_linesubstring` (from `lwgeom`)
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
      seg <- lines_sf %>% filter(subc_id == tid)
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

    # Calculate fraction along nearest segment for each point
    point_coords <- sf::st_coordinates(start_points)
    for (i in seq_len(nrow(start_points))) {
      # (Logic to compute closest segment and fraction along segment)
      # Store in start_info[[i]] as list(segment_id, start_fraction, point_id)
      # ...
    }
  }

  if (length(start_info) == 0)
    stop("No valid starting points could be determined")

  # ---- Helper functions for upstream/downstream traversal ----
  # traverse_downstream <- function(...) { /* your code */ }
  # traverse_upstream <- function(...) { /* your code */ }

  # ---- Process each starting point ----
  results <- list()
  for (info in start_info) {
    segment_id <- info$segment_id
    start_fraction <- info$start_fraction
    point_id <- info$point_id

    if (up_radius > 0) {
      upstream_result <- traverse_upstream(lines_sf, segment_id, start_fraction, up_radius)
      # convert results to sf, add metadata
      # append to results
    }
    if (down_radius > 0) {
      downstream_result <- traverse_downstream(lines_sf, segment_id, start_fraction, down_radius)
      # convert results to sf, add metadata
      # append to results
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
  final_result <- final_result %>% dplyr::select(any_of(c(
    "point_id", "target_segment", "subc_id", "start_fraction", "direction", "search_distance",
    setdiff(colnames(lines_sf), c("subc_id", "geometry"))
  )), geometry)

  return(final_result)
}
