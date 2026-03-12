#' Get Subcatchment IDs for Basin(s)
#'
#' @description
#' Retrieves all subcatchment IDs within one or more basins from the GeoFRESH API,
#' optionally filtered by minimum Strahler order. Supports three input modes:
#' \itemize{
#'   \item Basin IDs (`basin_ids`)
#'   \item Subcatchment IDs (`subc_ids`)
#'   \item Point coordinates (as GeoJSON FeatureCollection)
#' }
#'
#' The function automatically handles async jobs by polling until completion and
#' returning the final results.
#'
#' @family ocgapi
#' @param basin_ids Integer or integer vector (optional). One or more basin IDs
#'   to query. Default: `NULL`.
#' @param subc_ids Integer or integer vector (optional). One or more subcatchment IDs
#'   to query. Default: `NULL`.
#' @param points Data.frame or matrix (optional). Points with longitude and latitude.
#'   Must have columns matching `colname_lon` and `colname_lat`. Default: `NULL`.
#' @param colname_lon Character. Name of longitude column in `points`.
#'   Default: `"longitude"`.
#' @param colname_lat Character. Name of latitude column in `points`.
#'   Default: `"latitude"`.
#' @param min_strahler Integer (optional). Minimum Strahler order filter.
#'   Only subcatchments with Strahler >= this value returned.
#'   Default: `NULL` (no filtering).
#' @param comment Character (optional). Comment for API logging.
#'   Default: `NULL`.
#' @param force_async Logical (optional). Force async/sync mode:
#'   `NULL` = auto-detect, `TRUE` = force async, `FALSE` = force sync.
#'   Default: `TRUE`.
#' @param poll_interval Numeric. Seconds to wait between status checks when
#'   polling async jobs. Default: `10`.
#' @param max_wait Numeric. Maximum seconds to wait for job completion.
#'   Default: `3600` (1 hour).
#'
#' @return A list containing:
#'   \describe{
#'     \item{data}{data.frame. Summary with basin_id, reg_id, num_subcatchments.}
#'     \item{subc_ids}{Named list. Subcatchment IDs per basin (basin_id as names).}
#'     \item{href}{Character. Download URL (if provided by API).}
#'     \item{async}{Logical. Whether async mode was used.}
#'     \item{jobID}{Character. Job ID (if async mode was used).}
#'     \item{status}{Character. Final job status.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Example 1: Using basin IDs
#' result <- api_get_basin_subcids(
#'   basin_ids = c(1293500, 1293501),
#'   min_strahler = 4,
#'   comment = "Testing basin IDs"
#' )
#'
#' # Example 2: Using subcatchment IDs
#' result <- api_get_basin_subcids(
#'   subc_ids = c(506319029, 509342352),
#'   min_strahler = 4,
#'   comment = "Testing subc IDs"
#' )
#'
#' # Example 3: Using point coordinates
#' points_df <- data.frame(
#'   longitude = c(10.217977, 11.5),
#'   latitude = c(54.301799, 53.2)
#' )
#' result <- api_get_basin_subcids(
#'   points = points_df,
#'   min_strahler = 6,
#'   comment = "Testing points"
#' )
#'
#' # Access results
#' head(result$data)
#' result$subc_ids
#' }
#'
#' @export
#' @importFrom httr POST GET add_headers status_code content
#' @importFrom jsonlite toJSON fromJSON

api_get_basin_subcids <- function(
    basin_ids = NULL,
    subc_ids = NULL,
    points = NULL,
    colname_lon = "longitude",
    colname_lat = "latitude",
    min_strahler = NULL,
    comment = NULL,
    force_async = TRUE,
    poll_interval = 10,
    max_wait = 3600
) {

  # ---------- INPUT VALIDATION ----------

  # Count input methods
  input_count <- sum(!is.null(basin_ids), !is.null(subc_ids), !is.null(points))

  if (input_count == 0) {
    stop("Must provide one of: 'basin_ids', 'subc_ids', or 'points'.",
         call. = FALSE)
  }

  if (input_count > 1) {
    stop("Only one input method allowed: 'basin_ids', 'subc_ids', or 'points'.",
         call. = FALSE)
  }

  # Validate basin_ids
  if (!is.null(basin_ids)) {
    if (!is.numeric(basin_ids)) {
      stop("`basin_ids` must be a numeric value or vector.", call. = FALSE)
    }
    basin_ids <- as.integer(basin_ids)
  }

  # Validate subc_ids
  if (!is.null(subc_ids)) {
    if (!is.numeric(subc_ids)) {
      stop("`subc_ids` must be a numeric value or vector.", call. = FALSE)
    }
    subc_ids <- as.integer(subc_ids)
  }

  # Validate points
  if (!is.null(points)) {
    if (!is.data.frame(points) && !is.matrix(points)) {
      stop("`points` must be a data.frame or matrix.", call. = FALSE)
    }

    # Convert to data.frame if matrix
    if (is.matrix(points)) {
      points <- as.data.frame(points)
    }

    # Check for required columns
    if (!colname_lon %in% colnames(points)) {
      stop(sprintf("`points` must have a '%s' column.", colname_lon), call. = FALSE)
    }
    if (!colname_lat %in% colnames(points)) {
      stop(sprintf("`points` must have a '%s' column.", colname_lat), call. = FALSE)
    }

    # Validate coordinate ranges
    lons <- points[[colname_lon]]
    lats <- points[[colname_lat]]

    if (any(lats < -90 | lats > 90, na.rm = TRUE)) {
      stop("All latitudes must be between -90 and 90.", call. = FALSE)
    }
    if (any(lons < -180 | lons > 180, na.rm = TRUE)) {
      stop("All longitudes must be between -180 and 180.", call. = FALSE)
    }
    if (any(is.na(lons)) || any(is.na(lats))) {
      stop("`points` cannot contain NA values.", call. = FALSE)
    }
  }

  # Validate min_strahler
  if (!is.null(min_strahler)) {
    if (!is.numeric(min_strahler) || min_strahler < 1) {
      stop("`min_strahler` must be a positive numeric value or NULL.", call. = FALSE)
    }
    min_strahler <- as.integer(min_strahler)
  }

  # ---------- DETERMINE ASYNC MODE ----------

  use_async <- force_async

  if (is.null(use_async)) {
    # Auto-detect based on request complexity
    if (is.null(min_strahler) || min_strahler <= 4 ||
        (!is.null(basin_ids) && length(basin_ids) > 1) ||
        (!is.null(subc_ids) && length(subc_ids) > 1) ||
        (!is.null(points) && nrow(points) > 1)) {
      use_async <- TRUE
      message("Large request detected. Using async mode.")
    } else {
      use_async <- FALSE
      message("Small request detected. Using sync mode.")
    }
  } else {
    message(sprintf("Using %s mode (forced by user)", if (use_async) "async" else "sync"))
  }

  # ---------- BUILD REQUEST ----------

  process_url <- "https://aqua.igb-berlin.de/pygeoapi-dev/processes/get-basin-subcids/execution"

  # Build inputs based on input type
  inputs <- list()

  if (!is.null(basin_ids)) {
    # Basin IDs mode
    inputs$basin_ids <- basin_ids
    input_type <- "basin_ids"

  } else if (!is.null(subc_ids)) {
    # Subcatchment IDs mode
    inputs$subc_ids <- subc_ids
    input_type <- "subc_ids"

  } else if (!is.null(points)) {
    # Points mode - build GeoJSON FeatureCollection
    features <- lapply(1:nrow(points), function(i) {
      list(
        type = "Feature",
        geometry = list(
          type = "Point",
          coordinates = c(points[[colname_lon]][i], points[[colname_lat]][i])
        ),
        properties = list()
      )
    })

    inputs$points_geojson <- list(
      type = "FeatureCollection",
      features = features
    )
    input_type <- "points"
  }

  # Add optional parameters
  if (!is.null(min_strahler)) {
    inputs$min_strahler <- min_strahler
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
                 httr::content(response, as = "text")),
         call. = FALSE)
  }

  result <- httr::content(response, as = "parsed", simplifyVector = TRUE)

  # ---------- HANDLE ASYNC RESPONSE WITH POLLING ----------

  if (use_async && !is.null(result$jobID)) {
    message(sprintf("\nAsync job submitted successfully!"))
    message(sprintf("  Job ID: %s", result$jobID))
    message(sprintf("  Status: %s", result$status))

    # Add context about what was requested
    if (input_type == "basin_ids") {
      if (length(basin_ids) == 1) {
        message(sprintf("  Basin ID: %d", basin_ids))
      } else {
        message(sprintf("  Basin IDs: %s", paste(basin_ids, collapse = ", ")))
      }
    } else if (input_type == "subc_ids") {
      if (length(subc_ids) == 1) {
        message(sprintf("  Subcatchment ID: %d", subc_ids))
      } else {
        message(sprintf("  Subcatchment IDs: %s", paste(subc_ids, collapse = ", ")))
      }
    } else if (input_type == "points") {
      message(sprintf("  Number of points: %d", nrow(points)))
    }

    if (!is.null(min_strahler)) {
      message(sprintf("  Min Strahler: %d", min_strahler))
    }

    message("\nPolling for job completion...")

    # Poll the job until completion
    job_id <- result$jobID
    start_time <- Sys.time()

    repeat {
      # Check if max wait time exceeded
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      if (elapsed > max_wait) {
        stop(sprintf("Job did not complete within %d seconds. Job ID: %s",
                     max_wait, job_id),
             call. = FALSE)
      }

      # Poll job status (assumes api_poll_job function exists)
      job_status <- api_poll_job(job_id)

      if (job_status$status == "successful") {
        message(sprintf("✓ Job completed successfully after %.1f seconds", elapsed))

        if (!is.null(job_status$href)) {
          message("Downloading results...")
          # Assumes parse_job_results function exists
          parsed_results <- parse_job_results(job_status$href)

          message(sprintf("Retrieved %d subcatchments across %d basin(s)",
                          parsed_results$total_subcatchments,
                          nrow(parsed_results$summary)))

          return(list(
            data = parsed_results$summary,
            subc_ids = parsed_results$subc_ids,
            href = job_status$href,
            async = TRUE,
            jobID = job_id,
            status = "successful"
          ))
        } else {
          stop("Job completed but no download link (href) provided.",
               call. = FALSE)
        }
      } else if (job_status$status == "failed") {
        stop(sprintf("Job failed. Job ID: %s", job_id),
             call. = FALSE)
      } else if (job_status$status == "dismissed") {
        stop(sprintf("Job was dismissed. Job ID: %s", job_id),
             call. = FALSE)
      }

      # Still running, wait and poll again
      message(sprintf("  Status: %s (%.1fs elapsed, checking again in %ds...)",
                      job_status$status, elapsed, poll_interval))
      Sys.sleep(poll_interval)
    }
  }

  # ---------- HANDLE SYNC RESPONSE (JSON) ----------

  if (!is.null(result$href)) {
    # Sync mode with href
    message(sprintf("Subcatchment retrieval complete (sync mode). Downloading results..."))
    parsed_results <- parse_job_results(result$href)

    message(sprintf("Retrieved %d subcatchments across %d basin(s)",
                    parsed_results$total_subcatchments,
                    nrow(parsed_results$summary)))

    return(list(
      data = parsed_results$summary,
      subc_ids = parsed_results$subc_ids,
      href = result$href,
      async = FALSE,
      jobID = NULL,
      status = "successful"
    ))
  } else {
    # Direct sync response without href (fallback)
    stop("Sync mode without href not yet implemented. Use async mode or contact support.",
         call. = FALSE)
  }
}
