#' Get Subcatchment IDs for a Basin
#'
#' @description
#' Retrieves all subcatchment IDs within a basin from the GeoFRESH API,
#' optionally filtered by minimum Strahler order. Accepts either a basin ID
#' or point coordinates. Large requests automatically use async mode.
#'
#' **Input modes:**
#' \itemize{
#'   \item Direct `basin_id` (if known)
#'   \item Point coordinates (lon/lat) - basin determined from location
#' }
#'
#' @family ocgapi
#' @param basin_id Integer (optional). Basin ID to query. If `NULL`, must
#'   provide `longitude` and `latitude`. Default: `NULL`.
#' @param longitude Numeric (optional). Point longitude to determine basin.
#'   Used only if `basin_id` is `NULL`. Default: `NULL`.
#' @param latitude Numeric (optional). Point latitude to determine basin.
#'   Used only if `basin_id` is `NULL`. Default: `NULL`.
#' @param min_strahler Integer (optional). Minimum Strahler order filter.
#'   Only subcatchments with Strahler >= this value returned.
#'   Default: `NULL` (no filtering).
#' @param comment Character (optional). Comment for API logging.
#'   Default: `NULL`.
#' @param force_async Logical (optional). Force async/sync mode:
#'   `NULL` = auto-detect, `TRUE` = force async, `FALSE` = force sync.
#'   Default: `TRUE`.
#'
#' @return A list with elements depending on execution mode:
#'   \describe{
#'     \item{async}{Logical. Whether async mode was used.}
#'     \item{jobID}{Character. Job ID (async mode only).}
#'     \item{status}{Character. Job status (async mode only).}
#'     \item{data}{data.frame. Subcatchment IDs (sync mode only).}
#'     \item{href}{Character. Download URL (sync mode only).}
#'   }
#'   For async jobs, use `api_poll_job(result$jobID)` to check completion.
#'
#' @examples
#' \dontrun{
#' # Example 1: Using basin ID
#' job <- api_get_basin_subcids(
#'   basin_id = 1288419,
#'   min_strahler = 6
#' )
#' result <- api_poll_job(job$jobID)
#' subcids <- api_get_job_results(result$href)
#'
#' # Example 2: Using point coordinates
#' job <- api_get_basin_subcids(
#'   longitude = 8.278198,
#'   latitude = 53.549107,
#'   min_strahler = 6
#' )
#' }
#'
#' @export
#' @importFrom httr POST add_headers status_code content
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom data.table fread
api_get_basin_subcids <- function(
    basin_id = NULL,
    longitude = NULL,
    latitude = NULL,
    min_strahler = NULL,
    comment = NULL,
    force_async = TRUE  # Default to async since this can return many results
) {

  # ---------- INPUT VALIDATION ----------

  # Check that either basin_id OR coordinates are provided
  has_basin_id <- !is.null(basin_id)
  has_coords <- !is.null(latitude) && !is.null(longitude)

  if (!has_basin_id && !has_coords) {
    stop("Must provide either 'basin_id' OR both 'latitude' and 'longitude'.",
         call. = FALSE)
  }

  if (has_basin_id && has_coords) {
    message("Both basin_id and coordinates provided. Using basin_id (ignoring coordinates).")
    has_coords <- FALSE
  }

  # Validate basin_id
  if (has_basin_id && !is.numeric(basin_id)) {
    stop("`basin_id` must be a numeric value.", call. = FALSE)
  }

  # Validate coordinates
  if (has_coords) {
    if (!is.numeric(latitude) || !is.numeric(longitude)) {
      stop("`latitude` and `longitude` must be numeric values.", call. = FALSE)
    }
    if (latitude < -90 || latitude > 90) {
      stop("`latitude` must be between -90 and 90.", call. = FALSE)
    }
    if (longitude < -180 || longitude > 180) {
      stop("`longitude` must be between -180 and 180.", call. = FALSE)
    }
  }

  # Validate min_strahler
  if (!is.null(min_strahler)) {
    if (!is.numeric(min_strahler) || min_strahler < 1) {
      stop("`min_strahler` must be a positive numeric value or NULL.", call. = FALSE)
    }
  }

  # ---------- DETERMINE ASYNC MODE ----------

  use_async <- force_async

  if (is.null(use_async)) {
    # Auto-detect: use async for low Strahler thresholds (more results expected)
    if (is.null(min_strahler) || min_strahler <= 4) {
      use_async <- TRUE
      message("Low/no Strahler filter detected. Using async mode.")
    } else {
      use_async <- FALSE
      message("High Strahler filter detected. Using sync mode.")
    }
  } else {
    message(sprintf("Using %s mode (forced by user)", if (use_async) "async" else "sync"))
  }

  # ---------- BUILD REQUEST ----------

  process_url <- "https://aqua.igb-berlin.de/pygeoapi-dev/processes/get-basin-subcids/execution"

  # Build inputs based on input type
  if (has_basin_id) {
    inputs <- list(
      basin_id = as.integer(basin_id)
    )
  } else {
    # Point input with GeoJSON structure
    inputs <- list(
      point = list(
        type = "Point",
        coordinates = c(longitude, latitude)  # [lon, lat] order for GeoJSON
      )
    )
  }

  # Add optional parameters
  if (!is.null(min_strahler)) {
    inputs$min_strahler <- as.integer(min_strahler)
  }

  if (!is.null(comment)) {
    inputs$comment <- comment
  }

  # Build body
  body <- list(
    inputs = inputs,
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

    # Add context about what was requested
    if (has_basin_id) {
      message(sprintf("  Basin ID: %d", basin_id))
    } else {
      message(sprintf("  Point: [%.6f, %.6f]", longitude, latitude))
    }
    if (!is.null(min_strahler)) {
      message(sprintf("  Min Strahler: %d", min_strahler))
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
        data = NULL,
        basin_id = if (has_basin_id) basin_id else NA,
        point = if (has_coords) c(longitude, latitude) else NA
      ),
      class = "ocg_async_job"
    ))
  }

  # ---------- HANDLE SYNC RESPONSE ----------

  if (is.null(result$href)) {
    stop("API response does not include a download link (href).")
  }

  message(sprintf("Subcatchment retrieval complete (sync mode). Downloading results..."))

  # Download CSV using data.table::fread
  df <- data.table::fread(result$href)

  message(sprintf("Downloaded %d subcatchment IDs", nrow(df)))

  return(list(
    data = df,
    href = result$href,
    async = FALSE,
    jobID = NULL,
    status = "successful"
  ))
}

