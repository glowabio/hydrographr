#' Cascade snapping workflow with multiple Strahler levels and custom column names
#'
#' @description
#' Performs iterative snapping across a sequence of Strahler orders.
#' After each snapping round, distances are computed locally using Haversine formula.
#' Sites exceeding the threshold are sent to pygeoapi `filter-by-attribute` for the
#' next round. Successfully snapped sites are saved per iteration.
#'
#' @family ocgapi
#' @param csv_url Character. URL of the input CSV containing original points.
#' @param col_lon Character. Column name for longitude in the input CSV.
#' @param col_lat Character. Column name for latitude in the input CSV.
#' @param col_site Character. Column name for site ID.
#' @param strahler_seq Integer vector. Sequence of Strahler values (e.g. c(4,3,2)).
#' @param distance_threshold Numeric. Maximum allowed snapping distance.
#' @param comment Optional comment for API logging.
#'
#' @return A list containing:
#'   \item{snap_urls}{Character vector with the .csv URLs of the snapped points per round}
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

  current_url <- csv_url
  snap_urls <- c()
  filtered_urls <- c()
  snapped_points_list <- list()

  for (i in seq_along(strahler_seq)) {
    s <- strahler_seq[i]

    # ---------- 1. SNAPPING REQUEST ----------
    colname_lon <- col_lon #else paste(col_lon, paste0(replicate(i-1, "original"), collapse = "_"), sep = "_")
    colname_lat <- col_lat #else paste(col_lat, paste0(replicate(i-1, "original"), collapse = "_"), sep = "_")
    href_snap <- api_get_snapped_points_strahler_plural(csv_url = current_url,
      colname_lon = "longitude",
      colname_lat = "latitude",
      colname_site_id = "site_id",
      min_strahler = s
    )$href

    snap_urls <- c(snap_urls, href_snap)

    # Download silently
    temp_snapped <- tempfile(fileext = ".csv")
    utils::download.file(href_snap, temp_snapped, quiet = TRUE)
    snapped_df <- utils::read.csv(temp_snapped)


    # ---------- 2. RELIABLE COLUMN DETECTION ----------
    lon_before <- colnames(snapped_df)[5]
    lat_before <- colnames(snapped_df)[7]
    lon_after  <- colnames(snapped_df)[4]
    lat_after  <- colnames(snapped_df)[6]

    if (length(lon_before) != 1 || length(lat_before) != 1 ||
        length(lon_after)  != 1 || length(lat_after)  != 1) {
      stop("Could not uniquely identify before/after longitude/latitude columns.")
    }


    # ---------- 3. HAVERSINE DISTANCE ----------
    snapped_df$snap_distance <- geosphere::distHaversine(
      cbind(snapped_df[[lon_before]], snapped_df[[lat_before]]),
      cbind(snapped_df[[lon_after]],  snapped_df[[lat_after]])
    )


    # ---------- 4. STORE SUCCESSFUL POINTS ----------
    in_threshold <- snapped_df %>%
      dplyr::filter(snap_distance <= distance_threshold)

    message(sprintf(
      "[Strahler %s] Snapped %s / %s points within threshold.",
      s, nrow(in_threshold), nrow(snapped_df)
    ))

    snapped_points_list[[i]] <- in_threshold


    # ---------- 5. PREPARE NEXT ROUND ----------
    over_threshold_ids <- snapped_df %>%
      dplyr::filter(snap_distance > distance_threshold) %>%
      dplyr::pull(col_site) %>%
      unique()

    if (length(over_threshold_ids) == 0) {
      message(sprintf("All points snapped by Strahler %s. Cascade stops.", s))
      break
    }

    href_filtered <- api_filter_by_attribute(
      csv_url = csv_url,
      keep = setNames(list(over_threshold_ids), col_site),
      comment = comment
    )

    filtered_urls <- c(filtered_urls, href_filtered)

    current_url <- href_filtered
  }

  snapped_points_df <- bind_rows(snapped_points_list)

  return(list(
    snap_urls = snap_urls,
    snapped_points_df = snapped_points_df
  ))
}
