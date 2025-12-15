#' Cascade snapping workflow with multiple Strahler levels and custom column names
#'
#' @description
#' Performs iterative snapping across a sequence of Strahler orders.
#' After each snapping round, the API's distance column is used to filter sites.
#' Sites within the threshold are collected, while sites exceeding the threshold
#' are filtered and passed to the next Strahler level for re-snapping with their
#' original coordinates.
#'
#' @family ocgapi
#' @param csv_url Character. URL of the input CSV containing original points.
#' @param col_lon Character. Column name for longitude in the input CSV.
#' @param col_lat Character. Column name for latitude in the input CSV.
#' @param col_site Character. Column name for site ID.
#' @param strahler_seq Integer vector. Sequence of Strahler values (e.g. c(4,3,2)).
#' @param distance_threshold Numeric. Maximum allowed snapping distance in meters.
#' @param comment Optional comment for API logging.
#'
#' @return A list containing:
#'   \item{snap_urls}{Character vector with the .csv URLs of the snapped points per round}
#'   \item{filtered_within_urls}{Character vector with URLs of points within threshold per round}
#'   \item{snapped_points_df}{A data frame with the final snapped points}
#'
#' @export
api_get_snapped_points_cascade_plural <- function(
    csv_url,
    col_lon = "longitude",
    col_lat = "latitude",
    col_site = "site_id",
    strahler_seq = c(4, 3, 2),
    distance_threshold = 150,
    comment = NULL
) {
  current_url <- csv_url  # Points to snap in this round (with original coordinates)
  snap_urls <- c()
  filtered_within_urls <- c()
  snapped_points_list <- list()

  for (i in seq_along(strahler_seq)) {
    s <- strahler_seq[i]

    message(sprintf("[Strahler %s] Starting snapping round...", s))

    # ---------- 1. SNAPPING REQUEST ----------
    # Snap current points to streams of Strahler order s
    # IMPORTANT: add_distance = TRUE to get the distance_metres column
    href_snap <- api_get_snapped_points_strahler_plural(
      csv_url = current_url,
      colname_lon = col_lon,
      colname_lat = col_lat,
      colname_site_id = col_site,
      min_strahler = s,
      add_distance = TRUE  # Request distance calculation
    )$href

    snap_urls <- c(snap_urls, href_snap)
    message(sprintf("[Strahler %s] Snapping complete. URL: %s", s, href_snap))

    # ---------- 2. FILTER POINTS WITHIN THRESHOLD (ONLINE) ----------
    # The snapped CSV has a 'distance_metres' column with the snapping distance
    href_within <- tryCatch({
      api_filter_by_attribute(
        csv_url = href_snap,
        conditions = list(distance_metres = sprintf("x<%s", distance_threshold)),
        comment = paste0(comment, " - within threshold, Strahler ", s)
      )
    }, error = function(e) {
      message(sprintf("[Strahler %s] Warning: Could not filter within threshold: %s", s, e$message))
      return(NULL)
    })

    if (!is.null(href_within)) {
      filtered_within_urls <- c(filtered_within_urls, href_within)

      # Download and store successful snaps
      temp_within <- tempfile(fileext = ".csv")
      utils::download.file(href_within, temp_within, quiet = TRUE)
      within_df <- utils::read.csv(temp_within)

      if (nrow(within_df) > 0) {
        # within_df$strahler_snapped <- s  # Track which Strahler level succeeded
        snapped_points_list[[i]] <- within_df
        message(sprintf(
          "[Strahler %s] %s points snapped within threshold (distance <= %s m).",
          s, nrow(within_df), distance_threshold
        ))
      }

      unlink(temp_within)
    }

    # ---------- 3. FILTER POINTS OVER THRESHOLD (ONLINE) ----------
    # Get sites that exceeded the threshold
    href_over <- tryCatch({
      api_filter_by_attribute(
        csv_url = href_snap,
        conditions = list(distance_metres = sprintf("x>%s", distance_threshold)),
        comment = paste0(comment, " - over threshold, Strahler ", s)
      )
    }, error = function(e) {
      message(sprintf("[Strahler %s] No points exceeded threshold or filter error.", s))
      return(NULL)
    })

    # Check if we have points to continue with
    if (is.null(href_over)) {
      message(sprintf("[Strahler %s] All points snapped successfully. Cascade stops.", s))
      break
    }

    # Download to check how many points exceeded and get their site IDs
    temp_over <- tempfile(fileext = ".csv")
    download_success <- tryCatch({
      utils::download.file(href_over, temp_over, quiet = TRUE)
      over_df <- utils::read.csv(temp_over)
      n_over <- nrow(over_df)
      n_over > 0
    }, error = function(e) {
      message(sprintf("[Strahler %s] Could not download over-threshold points.", s))
      FALSE
    })

    if (!download_success) {
      message(sprintf("[Strahler %s] All points snapped successfully. Cascade stops.", s))
      break
    }

    # Read the over-threshold points to extract site IDs
    over_df <- utils::read.csv(temp_over)
    over_threshold_ids <- unique(over_df[[col_site]])
    unlink(temp_over)

    message(sprintf(
      "[Strahler %s] %s points exceeded threshold, preparing for next round.",
      s, length(over_threshold_ids)
    ))

    # Stop if this was the last Strahler level
    if (i == length(strahler_seq)) {
      message(sprintf(
        "[Strahler %s] Last Strahler level reached. %s points could not be snapped within threshold.",
        s, length(over_threshold_ids)
      ))
      break
    }

    # ---------- 4. GET ORIGINAL COORDINATES FOR NEXT ROUND ----------
    # Filter the ORIGINAL input CSV to get only the sites that exceeded threshold
    # This ensures we snap from original coordinates, not from previous snapped coordinates
    current_url <- api_filter_by_attribute(
      csv_url = csv_url,  # Use original CSV, not href_snap!
      keep = setNames(list(over_threshold_ids), col_site),
      comment = paste0(comment, " - next round input")
    )

    message(sprintf("[Strahler %s] Filtered original CSV for next round.", s))
  }

  # ---------- 5. COMBINE ALL SUCCESSFUL SNAPS ----------
  if (length(snapped_points_list) > 0) {
    snapped_points_df <- dplyr::bind_rows(snapped_points_list)
    message(sprintf(
      "\nCascade complete! Total points successfully snapped: %s",
      nrow(snapped_points_df)
    ))
  } else {
    snapped_points_df <- data.frame()
    message("\nCascade complete! No points were successfully snapped.")
  }

  return(list(
    snap_urls = snap_urls,
    filtered_within_urls = filtered_within_urls,
    snapped_points_df = snapped_points_df
  ))
}
