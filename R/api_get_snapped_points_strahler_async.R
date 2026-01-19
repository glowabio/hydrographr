#' Snap Many Points to River Network Using Strahler Threshold (CSV Input)
#'
#' Sends a CSV file URL with multiple points to the GeoFRESH API process
#' `get-snapped-points-strahler-plural`, which snaps each point to the nearest
#' stream segment above a given minimum Strahler order.
#'
#' For large datasets (>= async_threshold points), automatically uses async mode.
#' In async mode, the function returns a job object that can be polled for results
#' using api_poll_job().
#'
#' @family ocgapi
#' @param csv_url URL to a CSV file containing the point data.
#' @param colname_lon Name of the longitude column inside the CSV. Default: "longitude".
#' @param colname_lat Name of the latitude column inside the CSV. Default: "latitude".
#' @param colname_site_id Name of the site ID column inside the CSV. Default: "site_id".
#' @param min_strahler Minimum Strahler order to snap to (e.g., 5).
#' @param add_distance Logical. If TRUE, includes a 'distance_metres' column with the snapping
#'   distance in meters. Default: FALSE.
#' @param force_async Logical. NULL = auto-detect based on dataset size (default),
#'   TRUE = force async mode, FALSE = force sync mode.
#' @param async_threshold Numeric. Number of points threshold for auto-async (default: 1000).
#'
#' @return A list with:
#'   \describe{
#'     \item{data}{A data.frame with the snapped output (if sync mode)}
#'     \item{href}{The download link returned by the API (if sync mode)}
#'     \item{jobID}{Job ID for async jobs (if async mode)}
#'     \item{status}{Job status (if async mode)}
#'     \item{async}{Logical indicating whether async mode was used}
#'   }
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
#' result <- api_get_snapped_points_strahler(
#'   csv_url = "https://aqua.igb-berlin.de/referencedata/aqua90m/spdata_barbus.csv",
#'   min_strahler = 5,
#'   add_distance = TRUE
#' )
#' head(result$data)
#'
#' # Large dataset (async mode)
#' job <- api_get_snapped_points_strahler(
#'   csv_url = "https://example.com/large_dataset.csv",
#'   min_strahler = 4,
#'   add_distance = TRUE
#' )
#' # Poll for results
#' result <- api_poll_job(job$jobID)
#' df <- api_get_job_results(result$href)
#' }
api_get_snapped_points_strahler_async <- function(
    csv_url,
    colname_lon = "longitude",
    colname_lat = "latitude",
    colname_site_id = "site_id",
    min_strahler = NULL,
    add_distance = FALSE,
    force_async = NULL,
    async_threshold = 1000
) {
  if (is.null(min_strahler)) {
    stop("min_strahler must be provided.")
  }

  # ---------- AUTO-DETECT ASYNC MODE ----------
  use_async <- force_async

  if (is.null(use_async)) {
    message("Checking dataset size to determine sync/async mode...")

    temp_csv <- tempfile(fileext = ".csv")
    tryCatch({
      utils::download.file(csv_url, temp_csv, quiet = TRUE, method = "auto")

      # Count total rows efficiently
      n_rows <- length(readLines(temp_csv)) - 1  # Subtract header

      unlink(temp_csv)

      use_async <- n_rows >= async_threshold

      if (use_async) {
        message(sprintf(
          "Dataset has %d points (>= %d threshold). Using async mode.",
          n_rows, async_threshold
        ))
      } else {
        message(sprintf(
          "Dataset has %d points (< %d threshold). Using sync mode.",
          n_rows, async_threshold
        ))
      }

    }, error = function(e) {
      message(sprintf("Warning: Could not determine dataset size: %s", e$message))
      message("Defaulting to sync mode. Use force_async=TRUE to override.")
      use_async <- FALSE
      if (file.exists(temp_csv)) unlink(temp_csv)
    })
  } else {
    message(sprintf("Using %s mode (forced by user)", if (use_async) "async" else "sync"))
  }

  # ---------- BUILD REQUEST ----------
  process_url <- "https://aqua.igb-berlin.de/pygeoapi/processes/get-snapped-points-strahler-plural/execution"

  # Build POST body
  body <- list(
    inputs = list(
      csv_url = csv_url,
      colname_lon = colname_lon,
      colname_lat = colname_lat,
      colname_site_id = colname_site_id,
      min_strahler = min_strahler,
      add_distance = add_distance
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
    if (!is.null(result$started)) {
      message(sprintf("  Started: %s", result$started))
    }
    message(sprintf("\nTo check status and retrieve results, use:"))
    message(sprintf("  api_poll_job('%s')", result$jobID))

    return(structure(
      list(
        jobID = result$jobID,
        status = result$status,
        started = result$started %||% NA,
        processID = result$processID %||% NA,
        async = TRUE,
        href = NULL,
        data = NULL
      ),
      class = "ocg_async_job"
    ))
  }

  # ---------- HANDLE SYNC RESPONSE ----------
  if (is.null(result$href)) {
    stop("API response does not include a download link (href).")
  }

  message(sprintf("Snapping complete (sync mode). Downloading results..."))

  # Download CSV using data.table::fread
  df <- data.table::fread(result$href)

  message(sprintf("Downloaded %d rows, %d columns", nrow(df), ncol(df)))

  return(list(
    data = df,
    href = result$href,
    async = FALSE,
    jobID = NULL,
    status = "successful"
  ))
}

#' Print method for async job objects
#' @export
print.ocg_async_job <- function(x, ...) {
  cat(sprintf(
    "OCG Async Job\n  ID: %s\n  Status: %s\n  Started: %s\n\nUse api_poll_job('%s') to check status\n",
    x$jobID, x$status, x$started, x$jobID
  ))
  invisible(x)
}
