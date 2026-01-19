#' Cascade Snapping Workflow with Multiple Strahler Levels (Debug Version)
#'
#' @description
#' Performs iterative snapping across a descending sequence of Strahler orders
#' with extensive debugging output to diagnose issues in the cascade workflow.
#'
#' Points are first snapped to the highest Strahler level, and any points that
#' exceed the maximum snapping distance threshold are re-snapped to the next
#' lower Strahler level. This process continues until all points are successfully
#' snapped or the lowest Strahler level is reached.
#'
#' The function uses asynchronous API calls for snapping operations to handle
#' large datasets efficiently, but presents a synchronous interface to the user
#' by automatically polling and waiting for job completion at each step.
#'
#' **This is a debug version** that provides detailed diagnostic output at each
#' step to help identify data loss or filtering issues.
#'
#' @family ocgapi
#' @param csv_url Character. URL to a CSV file containing point data to snap.
#'   The CSV must be publicly accessible via HTTP/HTTPS.
#' @param colname_lon Character. Name of the longitude column in the CSV.
#'   Default: "longitude".
#' @param colname_lat Character. Name of the latitude column in the CSV.
#'   Default: "latitude".
#' @param colname_site_id Character. Name of the site ID column in the CSV.
#'   Default: "site_id".
#' @param strahler_seq Integer vector. Descending sequence of Strahler stream
#'   orders to try (e.g., `c(4, 3, 2)`). The function starts with the first
#'   (highest) value and proceeds to lower values as needed.
#' @param distance_threshold Numeric. Maximum allowed snapping distance in
#'   meters. Points snapped at a distance greater than this threshold will be
#'   re-snapped to the next lower Strahler level. Default: 150.
#' @param poll_interval Numeric. Seconds to wait between status checks when
#'   polling async jobs. Default: 10.
#' @param max_wait Numeric. Maximum seconds to wait for an async job to complete.
#'   Default: 7200 (2 hours).
#' @param debug Logical. Enable verbose debugging output. Default: TRUE.
#'
#' @return
#' A data.frame containing all successfully snapped points with columns:
#' \describe{
#'   \item{site_id}{Site identifier from original data}
#'   \item{subc_id}{Sub-catchment identifier of snapped location}
#'   \item{strahler}{Strahler order of the stream segment}
#'   \item{longitude_snapped, latitude_snapped}{Coordinates of snapped location}
#'   \item{longitude_original, latitude_original}{Original input coordinates}
#'   \item{distance_metres}{Snapping distance in meters}
#'   \item{strahler_level}{The Strahler level at which this point was successfully snapped}
#' }
#' If no points were successfully snapped, returns an empty data.frame.
#'
#' @details
#' The workflow proceeds as follows for each Strahler level:
#' 1. Snap all remaining points to the current Strahler level (async)
#' 2. Wait for snapping job to complete
#' 3. Download ALL snapped results for verification
#' 4. Filter out points snapped within `distance_threshold` (successful)
#' 5. Filter out points that exceeded the threshold
#' 6. Verify counts and detect any data loss
#' 7. If points remain that exceeded the threshold:
#'    - Extract their site IDs
#'    - Filter the original CSV to get only those points (with original coordinates)
#'    - Verify the filtered CSV contains expected points
#'    - Proceed to next Strahler level
#'
#' Points are always snapped from their original coordinates, not from previously
#' snapped locations. This ensures each snapping attempt starts fresh.
#'
#' **Debug Output Includes:**
#' - Row counts at each step
#' - Distance statistics (min, max, NA/infinite values)
#' - Verification of filter operations (expected vs actual counts)
#' - ID matching checks (missing/extra IDs)
#' - File size and content validation
#' - Success rate calculations
#'
#' @note
#' - All intermediate processing happens server-side via the pygeoAPI; only
#'   final results are downloaded to R.
#' - The function blocks until the entire cascade completes, which may take
#'   several minutes for large datasets.
#' - Async jobs are used internally for all snapping operations regardless of
#'   dataset size to ensure consistency and prevent timeouts.
#' - Debug mode downloads ALL snapped results at each level for verification,
#'   which may increase runtime slightly.
#' - This is a diagnostic version; use the standard `api_get_snapped_points_cascade_plural`
#'   for production workflows once issues are resolved.
#'
#' @examples
#' \dontrun{
#' # Debug cascade snapping with default settings
#' result <- api_get_snapped_points_cascade_debug(
#'   csv_url = "https://example.com/points.csv",
#'   distance_threshold = 200
#' )
#'
#' # Custom Strahler sequence with detailed logging
#' result <- api_get_snapped_points_cascade_debug(
#'   csv_url = "https://example.com/points.csv",
#'   strahler_seq = c(5, 4, 3),
#'   distance_threshold = 100,
#'   debug = TRUE
#' )
#'
#' # Check results and diagnostic output
#' head(result)
#' summary(result$distance_metres)  # Snapping distances
#' table(result$strahler_level)     # Distribution across Strahler levels
#'
#' # Review console output for:
#' # - Data loss warnings (⚠️)
#' # - Count mismatches
#' # - Missing IDs
#' }
#'
#' @seealso
#' \code{\link{api_get_snapped_points_cascade_plural}} for the production version
#' \code{\link{api_get_snapped_points_strahler_async}} for single-level async snapping
#' \code{\link{api_filter_by_attribute}} for attribute-based filtering
#'
#' @importFrom data.table fread
#' @importFrom utils download.file
#' @export
#' @author Afroditi Grigoropoulou, Merret Buurman
api_get_snapped_points_cascade_async <- function(
    csv_url,
    colname_lon = "longitude",
    colname_lat = "latitude",
    colname_site_id = "site_id",
    strahler_seq = c(4, 3, 2),
    distance_threshold = 150,
    poll_interval = 10,
    max_wait = 7200,
    debug = TRUE
) {

  message("=== DEBUG MODE ===")
  successful_points <- list()
  current_url <- csv_url

  # Download original to check total
  if (debug) {
    message("\n1. Checking original dataset...")
    original_df <- data.table::fread(csv_url)
    message(sprintf("   Original has %d points", nrow(original_df)))
    message(sprintf("   Columns: %s", paste(names(original_df), collapse = ", ")))
  }

  for (i in seq_along(strahler_seq)) {
    strahler <- strahler_seq[i]

    message(sprintf("\n=== STRAHLER %d (Level %d/%d) ===", strahler, i, length(strahler_seq)))

    if (debug) {
      message(sprintf("Current input URL points to CSV with:"))
      tryCatch({
        current_df <- data.table::fread(current_url)
        message(sprintf("   %d points", nrow(current_df)))
        message(sprintf("   First 3 IDs: %s", paste(head(current_df[[colname_site_id]], 3), collapse = ", ")))
      }, error = function(e) {
        message(sprintf("   Could not read: %s", e$message))
      })
    }

    # 1. Snap
    message(sprintf("Snapping to Strahler %d...", strahler))
    snap_result <- api_get_snapped_points_strahler_async(
      csv_url = current_url,
      colname_lon = colname_lon,
      colname_lat = colname_lat,
      colname_site_id = colname_site_id,
      min_strahler = strahler,
      add_distance = TRUE,
      force_async = TRUE
    )

    # 2. Wait and get URL
    if (snap_result$async) {
      job_status <- api_poll_job(snap_result$jobID, wait = TRUE,
                                 poll_interval = poll_interval, max_wait = max_wait)
      snap_url <- job_status$href
    } else {
      snap_url <- snap_result$href
    }

    # 3. Download ALL snapped results for debugging
    message("Downloading ALL snapped results...")
    all_snapped_df <- data.table::fread(snap_url)

    if (debug) {
      message(sprintf("   Total snapped: %d points", nrow(all_snapped_df)))
      message(sprintf("   Distance range: %.1f to %.1f m",
                      min(all_snapped_df$distance_metres, na.rm = TRUE),
                      max(all_snapped_df$distance_metres, na.rm = TRUE)))

      # Check for NAs/infinity
      na_dist <- sum(is.na(all_snapped_df$distance_metres))
      inf_dist <- sum(is.infinite(all_snapped_df$distance_metres))
      if (na_dist > 0) message(sprintf("   ⚠️ %d NA distances", na_dist))
      if (inf_dist > 0) message(sprintf("   ⚠️ %d infinite distances", inf_dist))
    }

    # 4. Filter: points within threshold
    message("Filtering successful points...")
    within_url <- tryCatch({
      api_filter_by_attribute(
        csv_url = snap_url,
        conditions = list(distance_metres = sprintf("x < %s", distance_threshold))
      )
    }, error = function(e) {
      message(sprintf("   Filter error: %s", e$message))
      return(NULL)
    })

    n_within <- 0
    if (!is.null(within_url)) {
      within_df <- data.table::fread(within_url)
      n_within <- nrow(within_df)

      if (debug) {
        message(sprintf("   Successfully snapped: %d points (< %d m)",
                        n_within, distance_threshold))

        # Verify all are actually within threshold
        over_threshold_in_within <- sum(within_df$distance_metres >= distance_threshold, na.rm = TRUE)
        if (over_threshold_in_within > 0) {
          message(sprintf("   ⚠️ BUG: %d 'successful' points have distance >= %d!",
                          over_threshold_in_within, distance_threshold))
        }
      }

      if (n_within > 0) {
        within_df$strahler_level <- strahler
        successful_points[[as.character(strahler)]] <- within_df
      }
    }

    # 5. Check if last level
    if (i == length(strahler_seq)) {
      if (debug) {
        message("\n=== FINAL CHECK ===")
        message(sprintf("Last level reached. Points snapped here: %d", n_within))

        # Check what happened to the rest
        remaining_after_snap <- nrow(all_snapped_df) - n_within
        message(sprintf("Points not snapped (<%dm): %d", distance_threshold, remaining_after_snap))

        if (remaining_after_snap > 0) {
          message("\nSample of failed points (distance >= threshold):")
          failed_sample <- all_snapped_df[all_snapped_df$distance_metres >= distance_threshold, ]
          if (nrow(failed_sample) > 0) {
            print(head(failed_sample[, c(colname_site_id, "distance_metres"), with = FALSE], 10))
          }
        }
      }
      break
    }

    # 6. Filter: points over threshold
    message("Filtering over-threshold points...")
    over_url <- tryCatch({
      api_filter_by_attribute(
        csv_url = snap_url,
        conditions = list(distance_metres = sprintf("x >= %s", distance_threshold))  # Note: >=
      )
    }, error = function(e) {
      message(sprintf("   Filter error: %s", e$message))
      return(NULL)
    })

    if (is.null(over_url)) {
      message("   No points over threshold")
      break
    }

    # 7. Check over-threshold file
    message("Checking over-threshold file...")
    tryCatch({
      # First check file size
      temp_file <- tempfile(fileext = ".csv")
      utils::download.file(over_url, temp_file, quiet = TRUE, mode = "wb")
      file_size <- file.info(temp_file)$size
      message(sprintf("   File size: %d bytes", file_size))

      # Read first few lines
      lines <- readLines(temp_file, n = 5, warn = FALSE)
      message(sprintf("   First %d lines:", length(lines)))
      for (line in lines) message(sprintf("     %s", line))

      # Try to read the data
      if (file_size > 50) {  # More than just header
        over_df <- data.table::fread(over_url)
        message(sprintf("   Read %d rows from over-threshold file", nrow(over_df)))

        # COMPARE: Should match all_snapped_df filtered by distance
        expected_over <- all_snapped_df[all_snapped_df$distance_metres >= distance_threshold, ]
        message(sprintf("   Expected (from all_snapped_df): %d rows", nrow(expected_over)))
        message(sprintf("   Actual (from filter): %d rows", nrow(over_df)))

        if (nrow(over_df) != nrow(expected_over)) {
          message("   ⚠️ MISMATCH! Filter returned different number of rows!")

          # Check IDs
          over_ids <- unique(over_df[[colname_site_id]])
          expected_ids <- unique(expected_over[[colname_site_id]])

          missing_ids <- setdiff(expected_ids, over_ids)
          extra_ids <- setdiff(over_ids, expected_ids)

          if (length(missing_ids) > 0) {
            message(sprintf("   Missing %d IDs in filter output", length(missing_ids)))
            message(sprintf("   First 3 missing: %s", paste(head(missing_ids, 3), collapse = ", ")))
          }
          if (length(extra_ids) > 0) {
            message(sprintf("   Extra %d IDs in filter output", length(extra_ids)))
          }
        }

        remaining_ids <- unique(over_df[[colname_site_id]])
        message(sprintf("   Unique site IDs: %d", length(remaining_ids)))

      } else {
        message("   File appears empty (only header)")
        remaining_ids <- NULL
      }

      unlink(temp_file)

    }, error = function(e) {
      message(sprintf("   Error reading over-threshold file: %s", e$message))
      remaining_ids <- NULL
    })

    if (is.null(remaining_ids) || length(remaining_ids) == 0) {
      message("No points need next level")
      break
    }

    message(sprintf("%d points need Strahler %d", length(remaining_ids), strahler_seq[i + 1]))

    # 8. Verify IDs exist in original
    if (debug) {
      message("Verifying IDs exist in original dataset...")
      missing_in_original <- setdiff(remaining_ids, original_df[[colname_site_id]])
      if (length(missing_in_original) > 0) {
        message(sprintf("   ⚠️ %d IDs not found in original dataset!", length(missing_in_original)))
        message(sprintf("   First 3 missing: %s", paste(head(missing_in_original, 3), collapse = ", ")))
      }
    }

    # 9. Prepare next level input
    message("Preparing input for next level...")
    next_input_url <- tryCatch({
      api_filter_by_attribute(
        csv_url = csv_url,
        keep = setNames(list(remaining_ids), colname_site_id)
      )
    }, error = function(e) {
      message(sprintf("   Filter error: %s", e$message))
      return(NULL)
    })

    if (!is.null(next_input_url)) {
      # Verify the filtered CSV
      tryCatch({
        next_input_df <- data.table::fread(next_input_url)
        message(sprintf("   Next input has %d rows (expected %d)",
                        nrow(next_input_df), length(remaining_ids)))

        if (nrow(next_input_df) != length(remaining_ids)) {
          message("   ⚠️ COUNT MISMATCH!")

          # Check which IDs are missing
          next_ids <- unique(next_input_df[[colname_site_id]])
          missing_next <- setdiff(remaining_ids, next_ids)

          if (length(missing_next) > 0) {
            message(sprintf("   Missing %d IDs in next input", length(missing_next)))
            message(sprintf("   Sample missing: %s", paste(head(missing_next, 3), collapse = ", ")))

            # Check if they exist at all
            if (debug && exists("original_df")) {
              missing_in_original <- intersect(missing_next, original_df[[colname_site_id]])
              message(sprintf("   Of those, %d DO exist in original", length(missing_in_original)))
            }
          }
        }
      }, error = function(e) {
        message(sprintf("   Could not verify next input: %s", e$message))
      })
    }

    current_url <- next_input_url
    if (is.null(current_url)) {
      message("Failed to prepare next level input")
      break
    }
  }

  # Final summary
  if (length(successful_points) > 0) {
    final_df <- do.call(rbind, successful_points)
    message(sprintf("\n=== FINAL RESULTS ==="))
    message(sprintf("Total snapped: %d points", nrow(final_df)))

    if (debug && exists("original_df")) {
      original_count <- nrow(original_df)
      success_rate <- round(nrow(final_df) / original_count * 100, 1)
      message(sprintf("Original points: %d", original_count))
      message(sprintf("Success rate: %.1f%%", success_rate))
      message(sprintf("Missing: %d points (%.1f%%)",
                      original_count - nrow(final_df),
                      100 - success_rate))
    }

    return(final_df)
  }

  return(data.frame())
}
