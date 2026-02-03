#' Retrieve Local IDs for Sampling Sites
#'
#' @description
#' Queries the GeoFRESH API to obtain regional unit, basin, and subcatchment
#' IDs for sampling locations provided via a CSV file. Useful for linking
#' observational data to the stream network hierarchy.
#'
#' For large datasets (>= async_threshold points), automatically uses async mode,
#' polls for completion, and returns the final data.
#'
#' @family ocgapi
#' @param csv_url Character. URL to a CSV file hosted online (must be publicly
#'   accessible via HTTP/HTTPS).
#' @param colname_lat Character. Name of the latitude column in CSV.
#'   Default: `"latitude"`.
#' @param colname_lon Character. Name of the longitude column in CSV.
#'   Default: `"longitude"`.
#' @param colname_site_id Character. Name of the site ID column in CSV.
#'   Default: `"site_id"`.
#' @param colname_subc_id Character (optional). Name of the subcatchment ID
#'   column in CSV if pre-computed IDs should be used. Default: `NULL`.
#' @param which_ids Character. ID type(s) to retrieve. Options: `"reg_id"`,
#'   `"basin_id"`, `"subc_id"`. Default: `"reg_id"`.
#' @param comment Character (optional). Comment for request logging.
#'   Default: `NULL`.
#' @param force_async Logical. NULL = auto-detect based on dataset size (default),
#'   TRUE = force async mode, FALSE = force sync mode.
#' @param async_threshold Numeric. Number of points threshold for auto-async (default: 1000).
#' @param poll_interval Numeric. Seconds between status checks when polling async jobs.
#'   Default: 10.
#' @param max_wait Numeric. Maximum wait time in seconds for async jobs. Default: 3600 (1 hour).
#'
#' @return A list containing:
#'   \describe{
#'     \item{data}{data.frame. Retrieved IDs with one row per site.}
#'     \item{href}{Character. URL of the output CSV file.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Small dataset (sync mode) - returns immediately
#' result <- api_get_local_ids(
#'   csv_url = "https://example.com/sites.csv",
#'   which_ids = "basin_id"
#' )
#' head(result$data)
#'
#' # Large dataset (async mode) - polls automatically
#' result <- api_get_local_ids(
#'   csv_url = "https://example.com/large_sites.csv",
#'   which_ids = "basin_id",
#'   poll_interval = 30  # Check every 30 seconds
#' )
#' head(result$data)
#' }
#'
#' @export
#' @importFrom httr POST add_headers status_code content
#' @importFrom jsonlite toJSON
#' @importFrom data.table fread
#' @author Afroditi Grigoropoulou, Merret Buurman
api_get_local_ids_async <- function(csv_url,
                              colname_lat = "latitude",
                              colname_lon = "longitude",
                              colname_site_id = "site_id",
                              colname_subc_id = NULL,
                              which_ids = "reg_id",
                              comment = NULL,
                              force_async = NULL,
                              async_threshold = 1000,
                              poll_interval = 10,
                              max_wait = 3600) {

  # ---- Validation ----
  if (is.null(csv_url)) {
    stop("A 'csv_url' must be provided.")
  }
  if (!is.character(csv_url) || !grepl("^https?://", csv_url)) {
    stop("csv_url must be a valid HTTP or HTTPS URL.")
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

  # ---- Define process URL ----
  process_url <- "https://aqua.igb-berlin.de/pygeoapi/processes/get-local-ids-plural/execution"

  # ---- Build request body ----
  inputs <- list(
    csv_url = csv_url,
    colname_lat = colname_lat,
    colname_lon = colname_lon,
    colname_site_id = colname_site_id,
    which_ids = which_ids
  )

  # Add optional arguments only if provided
  if (!is.null(colname_subc_id)) inputs$colname_subc_id <- colname_subc_id
  if (!is.null(comment)) inputs$comment <- comment

  body <- list(
    inputs = inputs,
    outputs = list(transmissionMode = "reference")
  )

  # Build headers
  headers <- c("Content-Type" = "application/json")
  if (use_async) {
    headers <- c(headers, "Prefer" = "respond-async")
  }

  # ---- Send POST request ----
  response <- httr::POST(
    url = process_url,
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    httr::add_headers(.headers = headers)
  )

  # ---- Handle errors ----
  status <- httr::status_code(response)
  if (status != 200 && status != 201) {
    stop(sprintf("API request failed (HTTP %s): %s",
                 status,
                 httr::content(response, as = "text")))
  }

  # ---- Parse results ----
  result <- httr::content(response, as = "parsed", simplifyVector = TRUE)

  # ---------- HANDLE ASYNC RESPONSE ----------
  if (use_async && !is.null(result$jobID)) {
    message(sprintf("\nAsync job submitted successfully!"))
    message(sprintf("  Job ID: %s", result$jobID))
    message(sprintf("  Status: %s", result$status))
    if (!is.null(result$started)) {
      message(sprintf("  Started: %s", result$started))
    }
    message(sprintf("  Which IDs: %s", paste(which_ids, collapse = ", ")))

    # Poll for completion
    message(sprintf("\nPolling for job completion (checking every %d seconds)...", poll_interval))

    job_result <- api_poll_job(
      jobID = result$jobID,
      wait = TRUE,
      poll_interval = poll_interval,
      max_wait = max_wait
    )

    # Get the CSV URL
    csv_download_url <- job_result$href

    if (is.null(csv_download_url)) {
      stop("Job completed but no download link was returned.")
    }

    message(sprintf("\nJob complete! Downloading results..."))

    # Download the data
    df_out <- tryCatch({
      data.table::fread(csv_download_url)
    }, error = function(e) {
      stop("Error downloading CSV: ", e$message)
    })

    message(sprintf("Downloaded %d rows, %d columns", nrow(df_out), ncol(df_out)))

    return(list(
      data = df_out,
      href = csv_download_url
    ))
  }

  # ---------- HANDLE SYNC RESPONSE ----------
  csv_download_url <- result$href

  if (is.null(csv_download_url) || !grepl("\\.csv$", csv_download_url)) {
    stop("No valid CSV download link found in the results.")
  }

  message(sprintf("ID retrieval complete (sync mode). Downloading results..."))

  # ---- Read CSV ----
  df_out <- tryCatch({
    data.table::fread(csv_download_url)
  }, error = function(e) {
    stop("Error downloading CSV: ", e$message)
  })

  message(sprintf("Downloaded %d rows, %d columns", nrow(df_out), ncol(df_out)))

  # ---- Return ----
  return(list(
    data = df_out,
    href = csv_download_url
  ))
}

# Helper for NULL coalescing (if not already defined)
`%||%` <- function(x, y) if (is.null(x)) y else x
