#' Snap Points to Stream Network (Cascade Workflow) - JSON Input
#'
#' @description
#' Snaps points to stream network across descending Strahler orders using
#' data.frame input. Points exceeding distance threshold
#' at higher orders are re-snapped to lower orders until successful or all
#' levels exhausted.
#'
#' @family ocgapi
#' @param data A data.frame containing point data with coordinates and site IDs.
#' @param colname_lon Character. Longitude column name in data.frame.
#'   Default: `"longitude"`.
#' @param colname_lat Character. Latitude column name in data.frame.
#'   Default: `"latitude"`.
#' @param colname_site_id Character. Site ID column name in data.frame.
#'   Default: `"site_id"`.
#' @param strahler_seq Integer vector. Descending Strahler orders to try
#'   (e.g., `c(4, 3, 2)`). Higher values tried first.
#' @param distance_threshold Numeric. Maximum snapping distance in meters.
#'   Points exceeding this are re-snapped to next lower Strahler level.
#'   Default: `150`.
#' @param async_threshold Numeric. Number of points threshold for auto-async.
#'   Default: `500`.
#' @param poll_interval Numeric. Seconds between async job status checks.
#'   Default: `10`.
#' @param max_wait Numeric. Maximum seconds to wait for job completion.
#'   Default: `7200` (2 hours).
#'
#' @return A data.frame with successfully snapped points containing:
#'   \describe{
#'     \item{site_id}{Site identifier from input data.}
#'     \item{subc_id}{Subcatchment ID of snapped location.}
#'     \item{strahler}{Strahler order of stream segment.}
#'     \item{longitude_snapped, latitude_snapped}{Snapped coordinates.}
#'     \item{longitude_original, latitude_original}{Original coordinates.}
#'     \item{distance_metres}{Snapping distance in meters.}
#'     \item{...}{Any other columns from original data.}
#'   }
#'   Returns empty data.frame if no points successfully snapped.
#'
#' @details
#' **Workflow per Strahler level:**
#' \enumerate{
#'   \item Convert data.frame to GeoJSON and snap to current Strahler level
#'   \item Poll until job completes (if async)
#'   \item Separate points within vs exceeding distance threshold
#'   \item Store successful snaps, prepare failed snaps for next level
#'   \item Re-snap failures from original coordinates at next lower Strahler
#' }
#'
#' Points always snap from original coordinates to ensure each attempt starts fresh.
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' sites <- data.frame(
#'   site_id = c("site_1", "site_2", "site_3"),
#'   longitude = c(9.931555, 9.921555, 9.941555),
#'   latitude = c(54.695070, 54.295070, 54.495070),
#'   species = c("Trout", "Salmon", "Pike")
#' )
#'
#' # Run cascade
#' result <- api_get_snapped_points_cascade_df(
#'   data = sites,
#'   strahler_seq = c(5, 4, 3),
#'   distance_threshold = 200
#' )
#'
#' # Large dataset (auto-async)
#' gbif_data <- data.table::fread("my_fish_data.csv")
#' result <- api_get_snapped_points_cascade_df(
#'   data = gbif_data,
#'   colname_lon = "decimalLongitude",
#'   colname_lat = "decimalLatitude",
#'   colname_site_id = "gbifID",
#'   strahler_seq = c(4, 3, 2)
#' )
#' }
#'
#' @export
#' @importFrom data.table fread rbindlist
api_get_snapped_points_cascade <- function(
    data,
    colname_lon = "longitude",
    colname_lat = "latitude",
    colname_site_id = "site_id",
    strahler_seq = c(4, 3, 2),
    distance_threshold = 150,
    async_threshold = 500,
    poll_interval = 10,
    max_wait = 7200
) {

  # ---------- INPUT VALIDATION ----------

  if (missing(data) || !is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }

  if (nrow(data) == 0) {
    stop("'data' is empty (0 rows)")
  }

  # Check required columns
  required_cols <- c(colname_lon, colname_lat, colname_site_id)
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Remove NA coordinates
  original_nrow <- nrow(data)
  data <- data[!is.na(data[[colname_lon]]) & !is.na(data[[colname_lat]]), ]

  if (nrow(data) == 0) {
    stop("No valid coordinates remaining after removing NAs")
  }

  if (nrow(data) < original_nrow) {
    message(sprintf("Removed %d rows with NA coordinates", original_nrow - nrow(data)))
  }

  # Store original data for reference
  original_data <- data

  message(sprintf("\n=== Starting Cascade for %d points ===", nrow(data)))
  message(sprintf("Strahler sequence: %s", paste(strahler_seq, collapse = " → ")))
  message(sprintf("Distance threshold: %d meters", distance_threshold))

  # ---------- CASCADE WORKFLOW ----------

  successful_points <- list()
  current_data <- data

  for (i in seq_along(strahler_seq)) {
    strahler <- strahler_seq[i]

    message(sprintf("\n========================================"))
    message(sprintf("=== Strahler %d (Level %d/%d) ===", strahler, i, length(strahler_seq)))
    message(sprintf("========================================"))
    message(sprintf("Points to snap: %d", nrow(current_data)))

    # 1. SNAP POINTS TO CURRENT STRAHLER LEVEL
    snapped_df <- api_get_snapped_points_strahler_df(
      data = current_data,
      colname_lon = colname_lon,
      colname_lat = colname_lat,
      colname_site_id = colname_site_id,
      min_strahler = strahler,
      add_distance = TRUE,
      async_threshold = async_threshold,
      poll_interval = poll_interval,
      max_wait = max_wait
    )

    message(sprintf("✓ Snapped %d points", nrow(snapped_df)))

    # Check if any points were snapped
    if (nrow(snapped_df) == 0) {
      message("⚠ No points were snapped at this level")
      if (i == length(strahler_seq)) {
        message("Last Strahler level reached with no snaps")
        break
      }
      # Continue to next level with same data
      next
    }

    # 2. SEPARATE WITHIN AND OVER THRESHOLD
    within_threshold <- snapped_df[snapped_df$distance_metres < distance_threshold, ]
    over_threshold <- snapped_df[snapped_df$distance_metres >= distance_threshold, ]

    message(sprintf("\nDistance summary:"))
    message(sprintf("  Range: %.1f - %.1f meters",
                    min(snapped_df$distance_metres, na.rm = TRUE),
                    max(snapped_df$distance_metres, na.rm = TRUE)))
    message(sprintf("  Within threshold (< %dm): %d points",
                    distance_threshold, nrow(within_threshold)))
    message(sprintf("  Over threshold (>= %dm): %d points",
                    distance_threshold, nrow(over_threshold)))

    # 3. STORE SUCCESSFUL SNAPS
    if (nrow(within_threshold) > 0) {
      successful_points[[as.character(strahler)]] <- within_threshold
      message(sprintf("✓ Collected %d successful snaps", nrow(within_threshold)))
    }

    # 4. CHECK IF LAST LEVEL
    if (i == length(strahler_seq)) {
      message(sprintf("\nLast Strahler level reached"))
      if (nrow(over_threshold) > 0) {
        message(sprintf("⚠ %d points exceeded threshold at final level",
                        nrow(over_threshold)))
        message("  These points will not be included in final results")
      }
      break
    }

    # 5. PREPARE FOR NEXT LEVEL
    if (nrow(over_threshold) == 0) {
      message("\n✓ All points successfully snapped!")
      break
    }

    message(sprintf("\nPreparing %d points for Strahler %d...",
                    nrow(over_threshold), strahler_seq[i + 1]))

    # Get site IDs that need re-snapping
    failed_ids <- unique(over_threshold[[colname_site_id]])

    # Filter original data to get original coordinates for these sites
    current_data <- original_data[original_data[[colname_site_id]] %in% failed_ids, ]

    message(sprintf("✓ Retrieved original coordinates for %d sites", nrow(current_data)))

    # Verify all IDs were found
    if (nrow(current_data) != length(failed_ids)) {
      warning(sprintf("Expected %d points but got %d from original data",
                      length(failed_ids), nrow(current_data)))
    }
  }

  # ---------- COMBINE RESULTS ----------

  message(sprintf("\n========================================"))
  message(sprintf("=== Cascade Complete ==="))
  message(sprintf("========================================"))

  if (length(successful_points) == 0) {
    message("⚠ No points were successfully snapped")
    return(data.frame())
  }

  # Combine all successful points
  final_df <- data.table::rbindlist(successful_points, fill = TRUE)

  # Summary by Strahler level
  message("\nResults by Strahler level:")
  for (strahler in names(successful_points)) {
    n_points <- nrow(successful_points[[strahler]])
    message(sprintf("  Strahler %s: %d points", strahler, n_points))
  }

  # Overall summary
  message(sprintf("\nOverall summary:"))
  message(sprintf("  Total input: %d points", nrow(original_data)))
  message(sprintf("  Successfully snapped: %d points", nrow(final_df)))
  message(sprintf("  Success rate: %.1f%%",
                  100 * nrow(final_df) / nrow(original_data)))
  message(sprintf("  Failed to snap: %d points",
                  nrow(original_data) - nrow(final_df)))

  return(final_df)
}
