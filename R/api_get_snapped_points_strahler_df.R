#' Snap Points to River Network Using GeoJSON Input
#'
#' Sends point data as GeoJSON to the GeoFRESH API process
#' `get-snapped-points-strahler-plural`, which snaps each point to the nearest
#' stream segment above a given minimum Strahler order.
#'
#' @description
#' This function accepts a data.frame with coordinates and converts it to GeoJSON
#' format for the API. Unlike the CSV version, this doesn't require uploading
#' files to a server first. For datasets with >= 500 points (or custom threshold),
#' automatically uses async mode with built-in polling.
#'
#' @family ocgapi
#' @param data A data.frame containing point data with coordinates and site IDs.
#' @param colname_lon Character. Name of the longitude column in the data.frame.
#'   Default: "longitude".
#' @param colname_lat Character. Name of the latitude column in the data.frame.
#'   Default: "latitude".
#' @param colname_site_id Character. Name of the site ID column in the data.frame.
#'   Default: "site_id".
#' @param min_strahler Integer. Minimum Strahler order to snap to (e.g., 5).
#' @param add_distance Logical. If TRUE, includes a 'distance_metres' column with
#'   the snapping distance in meters. Default: TRUE.
#' @param force_async Logical. If TRUE, forces async mode. If FALSE, forces sync.
#'   If NULL (default), uses async for datasets >= async_threshold.
#' @param async_threshold Numeric. Number of points threshold for auto-async.
#'   Default: 500.
#' @param poll_interval Numeric. Seconds between status checks when polling
#'   async jobs. Default: 10.
#' @param max_wait Numeric. Maximum wait time in seconds for async jobs.
#'   Default: 3600 (1 hour).
#'
#' @return A data.frame with the snapped output including:
#'   - Original columns from input data
#'   - longitude_snapped, latitude_snapped (snapped coordinates)
#'   - subc_id (subcatchment ID)
#'   - strahler (Strahler order of snapped segment)
#'   - distance_metres (if add_distance = TRUE)
#'
#' @import httr
#' @import jsonlite
#' @import data.table
#' @export
#' @author Afroditi Grigoropoulou, Merret Buurman
#'
#' @examples
#' \dontrun{
#' # Small dataset (sync mode)
#' sites <- data.frame(
#'   site_id = c("site_1", "site_2", "site_3"),
#'   longitude = c(9.931555, 9.921555, 9.941555),
#'   latitude = c(54.695070, 54.295070, 54.495070),
#'   species = c("Trout", "Salmon", "Pike")
#' )
#'
#' result <- api_get_snapped_points_strahler_df(
#'   data = sites,
#'   min_strahler = 4,
#'   add_distance = TRUE
#' )
#'
#' # Large dataset (auto-async)
#' gbif_data <- fread("my_fish_occurrences.csv")
#'
#' result <- api_get_snapped_points_strahler_df(
#'   data = gbif_data,
#'   colname_lon = "decimalLongitude",
#'   colname_lat = "decimalLatitude",
#'   colname_site_id = "gbifID",
#'   min_strahler = 4
#' )
#'
#' # Force async mode
#' result <- api_get_snapped_points_strahler_df(
#'   data = sites,
#'   min_strahler = 4,
#'   force_async = TRUE
#' )
#'
#' # Custom threshold (async if > 100 points)
#' result <- api_get_snapped_points_strahler_df(
#'   data = sites,
#'   min_strahler = 4,
#'   async_threshold = 100
#' )
#' }
api_get_snapped_points_strahler_df <- function(
    data,
    colname_lon = "longitude",
    colname_lat = "latitude",
    colname_site_id = "site_id",
    min_strahler = NULL,
    add_distance = TRUE,
    force_async = NULL,
    async_threshold = 500,
    poll_interval = 10,
    max_wait = 3600
) {

  # ---------- INPUT VALIDATION ----------

  if (missing(data) || !is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }

  if (nrow(data) == 0) {
    stop("'data' is empty (0 rows)")
  }

  if (is.null(min_strahler)) {
    stop("min_strahler must be provided.")
  }

  # Check required columns exist
  required_cols <- c(colname_lon, colname_lat, colname_site_id)
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Check for NA values in coordinates
  n_na_lon <- sum(is.na(data[[colname_lon]]))
  n_na_lat <- sum(is.na(data[[colname_lat]]))

  if (n_na_lon > 0 || n_na_lat > 0) {
    warning(sprintf("Found %d NA values in coordinates. These rows will be excluded.",
                    max(n_na_lon, n_na_lat)))

    # Remove rows with NA coordinates
    data <- data[!is.na(data[[colname_lon]]) & !is.na(data[[colname_lat]]), ]

    if (nrow(data) == 0) {
      stop("No valid coordinates remaining after removing NAs")
    }
  }

  # ---------- DETERMINE ASYNC MODE ----------

  use_async <- force_async

  if (is.null(use_async)) {
    use_async <- nrow(data) >= async_threshold

    if (use_async) {
      message(sprintf("Dataset has %d points (>= %d threshold). Using async mode.",
                      nrow(data), async_threshold))
    } else {
      message(sprintf("Dataset has %d points (< %d threshold). Using sync mode.",
                      nrow(data), async_threshold))
    }
  } else {
    message(sprintf("Using %s mode (forced by user)", if (use_async) "async" else "sync"))
  }

  message(sprintf("Processing %d points...", nrow(data)))

  # ---------- CONVERT DATAFRAME TO GEOJSON ----------

  message("Converting data.frame to GeoJSON...")

  # Create features list
  features <- lapply(1:nrow(data), function(i) {
    list(
      type = "Feature",
      geometry = list(
        type = "Point",
        coordinates = c(as.numeric(data[[colname_lon]][i]),
                        as.numeric(data[[colname_lat]][i]))
      ),
      properties = setNames(
        list(as.character(data[[colname_site_id]][i])),
        colname_site_id
      )
    )
  })


  # Build FeatureCollection
  points_geojson <- list(
    type = "FeatureCollection",
    features = features
  )

  message("✓ GeoJSON created")

  # ---------- BUILD REQUEST ----------

  process_url <- "https://aqua.igb-berlin.de/pygeoapi/processes/get-snapped-points-strahler-plural/execution"

  # Build POST body
  body <- list(
    inputs = list(
      colname_site_id = colname_site_id,
      colname_lon = colname_lon,
      colname_lat = colname_lat,
      min_strahler = as.integer(min_strahler),
      add_distance = add_distance,
      result_format = "csv",
      points_geojson = points_geojson
    ),
    outputs = list(
      transmissionMode = "reference"
    )
  )

  # Build headers
  headers <- c("Content-Type" = "application/json")
  if (use_async) {
    headers <- c(headers, "Prefer" = "respond-async")
  }

  # ---------- SEND REQUEST ----------

  message("Sending request to API...")

  response <- httr::POST(
    url = process_url,
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    httr::add_headers(.headers = headers)
  )

  status <- httr::status_code(response)

  if (status != 200 && status != 201) {
    stop(sprintf("API request failed (HTTP %s): %s",
                 status,
                 httr::content(response, as = "text")))
  }

  result <- httr::content(response, as = "parsed", simplifyVector = TRUE)

  # ---------- HANDLE ASYNC RESPONSE ----------

  if (use_async && !is.null(result$jobID)) {
    message(sprintf("\nAsync job submitted successfully!"))
    message(sprintf("  Job ID: %s", result$jobID))
    message(sprintf("  Status: %s", result$status))
    message(sprintf("\nPolling for completion (checking every %d seconds)...", poll_interval))

    # Poll for completion
    job_result <- api_poll_job(
      jobID = result$jobID,
      wait = TRUE,
      poll_interval = poll_interval,
      max_wait = max_wait
    )

    csv_url <- job_result$href

  } else {
    # ---------- HANDLE SYNC RESPONSE ----------
    csv_url <- result$href
    }

  # ---------- DOWNLOAD RESULTS ----------

  if (is.null(csv_url)) {
    stop("API response does not include a download link (href).")
  }

  message("✓ Snapping complete. Downloading results...")

  # Download CSV using data.table::fread
  df <- data.table::fread(csv_url)

  message(sprintf("✓ Downloaded %d rows, %d columns", nrow(df), ncol(df)))

  # Check if all points were snapped
  if (nrow(df) < nrow(data)) {
    warning(sprintf("Only %d/%d points were snapped. Some points may be outside the stream network.",
                    nrow(df), nrow(data)))
  }

  return(df)
}


# Helper for NULL coalescing (if not already defined)
`%||%` <- function(x, y) if (is.null(x)) y else x
