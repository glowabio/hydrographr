#' Cascade Snapping of Points to Stream Segments by Strahler Order
#'
#' @description
#' Iteratively snaps points to stream segments of decreasing Strahler order.
#' After each snapping round, points whose snapping distance exceeds
#' `distance_threshold` are passed to the next (finer) Strahler level.
#'
#' This "cascade snapping" approach improves snapping accuracy while preventing
#' points from being snapped too far away from their original location.
#'
#' @param df A data.frame containing the input point coordinates and site IDs.
#' @param col_lat Character string. Column name for original latitude values.
#' @param col_lon Character string. Column name for original longitude values.
#' @param col_site Character string. Column name for the unique site identifier.
#' @param strahler_seq Numeric vector of Strahler orders to snap to (in
#'   descending order). Default is `c(4, 3, 2)`.
#' @param distance_threshold Numeric. Maximum allowed snapping distance (in
#'   meters). Points exceeding this distance are forwarded to the next
#'   Strahler order. Default is 150 m.
#'
#' @return A `data.frame` with:
#'   * input coordinates and site_id
#'   * snapped coordinates (`lon_after`, `lat_after`)
#'   * `subc_id_after`
#'   * `dist_to_snapped` (meters, Haversine distance)
#'   * `strahler` (Strahler order where snapping succeeded)
#'
#' @details
#' This function requires the following hydrographr API helpers:
#' * `standardize_input()`
#' * `api_get_snapped_points_strahler()`
#'
#' Snapping distance is computed using `geosphere::distHaversine()`.
#'
#' @examples
#' \dontrun{
#' snapped <- snap_points_cascade(
#'   df = pts,
#'   col_lat = "lat",
#'   col_lon = "lon",
#'   col_site = "id",
#'   strahler_seq = c(5, 4, 3),
#'   distance_threshold = 100
#' )
#' }
#'
#' @importFrom dplyr mutate left_join select filter
#' @importFrom geosphere distHaversine
#' @export
snap_points_cascade <- function(df,
                                col_lat = "lat_before",
                                col_lon = "lon_before",
                                col_site = "site_id",
                                strahler_seq = c(4, 3, 2),
                                distance_threshold = 150) {

  # ---- 1. Standardize column names -----------------------------------------
  df_std <- standardize_input(
    df,
    col_lon = col_lon,
    col_lat = col_lat,
    col_site_id = col_site
  )

  # Output container
  snapped_df <- df_std %>%
    mutate(
      lon_after = NA_real_,
      lat_after = NA_real_,
      subc_id_after = NA_character_,
      dist_to_snapped = NA_real_,
      strahler = NA_integer_
    )

  # Points to process
  points_to_snap <- df_std

  # ---- 2. Iterate over Strahler orders -------------------------------------
  for (strahler in strahler_seq) {

    message("Snapping points to Strahler ", strahler, "...")

    snapped <- api_get_snapped_points_strahler(
      df = points_to_snap,
      colname_lat = "lat_before",
      colname_lon = "lon_before",
      colname_site_id = "site_id",
      strahler = strahler,
      geometry_only = FALSE
    )

    # Haversine distance (meters)
    dist_to_snapped <- geosphere::distHaversine(
      cbind(snapped$lon_before, snapped$lat_before),
      cbind(snapped$lon_after, snapped$lat_after)
    )

    # Update main df
    idx <- match(snapped$site_id, snapped_df$site_id)

    snapped_df$lon_after[idx]      <- snapped$lon_after
    snapped_df$lat_after[idx]      <- snapped$lat_after
    snapped_df$subc_id_after[idx]  <- snapped$subc_id_after
    snapped_df$dist_to_snapped[idx]<- dist_to_snapped
    snapped_df$strahler[idx]       <- strahler

    # ---- Points exceeding the threshold ------------------------------------
    too_far_idx <- which(dist_to_snapped > distance_threshold)

    if (length(too_far_idx) == 0) {
      message("All remaining points within threshold. Stopping.")
      break
    }

    site_ids_too_far <- snapped$site_id[too_far_idx]

    points_to_snap <- df_std[df_std$site_id %in% site_ids_too_far, ]

    if (!nrow(points_to_snap)) {
      message("No points remain above threshold. Stopping cascade.")
      break
    }

    message(
      length(too_far_idx), " points exceed threshold → next Strahler order."
    )

    if (strahler == tail(strahler_seq, 1)) {
      message("Reached finest Strahler in sequence. Returning final snapped points.")
      break
    }
  }

  # Final formatting
  snapped_df %>%
    mutate(subc_id_after = as.integer(subc_id_after))
}
