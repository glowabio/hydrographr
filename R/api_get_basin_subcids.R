#' Get Subcatchment IDs for Basin(s)
#'
#' @description
#' Retrieves all subcatchment IDs within one or more basins from the GeoFRESH API,
#' optionally filtered by minimum Strahler order. Supports multiple input modes:
#' direct basin ID(s), point coordinates, or CSV file with basin IDs, subcatchment
#' IDs, or coordinates.
#'
#' The function automatically handles async jobs by polling until completion and
#' returning the final results.
#'
#' **Input modes:**
#' \itemize{
#'   \item Single `basin_id` or vector of `basin_ids`
#'   \item Point coordinates (`longitude` + `latitude`)
#'   \item CSV with basin ID column
#'   \item CSV with subcatchment ID column
#'   \item CSV with coordinate columns (lon/lat)
#' }
#'
#' @family ocgapi
#' @param basin_id Integer or integer vector (optional). One or more basin IDs
#'   to query. If `NULL`, must provide coordinates, `csv_url`, or use CSV mode.
#'   Default: `NULL`.
#' @param longitude Numeric (optional). Point longitude to determine basin.
#'   Used only if `basin_id` and `csv_url` are `NULL`. Default: `NULL`.
#' @param latitude Numeric (optional). Point latitude to determine basin.
#'   Used only if `basin_id` and `csv_url` are `NULL`. Default: `NULL`.
#' @param csv_url Character (optional). URL to CSV file with basin IDs,
#'   subcatchment IDs, or coordinates. Default: `NULL`.
#' @param colname_basin_id Character (optional). Name of basin ID column in CSV.
#'   Use when CSV contains basin IDs. Default: `NULL`.
#' @param colname_subc_id Character (optional). Name of subcatchment ID column
#'   in CSV. Use when CSV contains subcatchment IDs. Default: `NULL`.
#' @param colname_lon Character (optional). Name of longitude column in CSV.
#'   Required when using coordinate-based CSV input. Default: `"longitude"`.
#' @param colname_lat Character (optional). Name of latitude column in CSV.
#'   Required when using coordinate-based CSV input. Default: `"latitude"`.
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
#' # Example 1: Single basin ID
#' result <- api_get_basin_subcids(
#'   basin_id = 1288419,
#'   min_strahler = 6
#' )
#' head(result$data)
#'
#' # Example 2: Multiple basin IDs
#' result <- api_get_basin_subcids(
#'   basin_id = c(1288419, 1288420),
#'   min_strahler = 4
#' )
#' # Access subcatchments for basin 1288419
#' result$subc_ids$`1288419`
#'
#' # Example 3: CSV with basin IDs
#' result <- api_get_basin_subcids(
#'   csv_url = "https://example.com/sites.csv",
#'   colname_basin_id = "basin_id"
#' )
#' }
#'
#' @export
#' @importFrom httr POST GET add_headers status_code content
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom data.table fread

api_get_basin_subcids <- function(
    basin_id = NULL,
    longitude = NULL,
    latitude = NULL,
    csv_url = NULL,
    colname_basin_id = NULL,
    colname_subc_id = NULL,
    colname_lon = "longitude",
    colname_lat = "latitude",
    min_strahler = NULL,
    comment = NULL,
    force_async = TRUE,
    poll_interval = 10,
    max_wait = 3600
) {

  # ---------- INPUT VALIDATION ----------

  # Determine input mode
  has_basin_id <- !is.null(basin_id)
  has_coords <- !is.null(latitude) && !is.null(longitude)
  has_csv <- !is.null(csv_url)
  has_csv_basin <- has_csv && !is.null(colname_basin_id)
  has_csv_subc <- has_csv && !is.null(colname_subc_id)
  has_csv_coords <- has_csv && !is.null(colname_lon) && !is.null(colname_lat)

  # Count input methods
  input_methods <- sum(has_basin_id, has_coords, has_csv_basin, has_csv_subc, has_csv_coords)

  if (input_methods == 0) {
    stop("Must provide one of: 'basin_id', coordinates ('latitude' + 'longitude'), ",
         "or 'csv_url' with appropriate column names.",
         call. = FALSE)
  }

  if (input_methods > 1) {
    message("Multiple input methods provided. Priority: basin_id > csv > coordinates.")
    if (has_basin_id) {
      has_coords <- FALSE
      has_csv <- FALSE
      has_csv_basin <- FALSE
      has_csv_subc <- FALSE
      has_csv_coords <- FALSE
    } else if (has_csv) {
      has_coords <- FALSE
    }
  }

  # Validate basin_id(s)
  if (has_basin_id) {
    if (!is.numeric(basin_id)) {
      stop("`basin_id` must be a numeric value or vector.", call. = FALSE)
    }
    basin_id <- as.integer(basin_id)
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

  # Validate CSV URL
  if (has_csv) {
    if (!is.character(csv_url) || !grepl("^https?://", csv_url)) {
      stop("`csv_url` must be a valid HTTP or HTTPS URL.", call. = FALSE)
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
    # Auto-detect: use async for low Strahler thresholds or multiple basins
    if (is.null(min_strahler) || min_strahler <= 4 || (has_basin_id && length(basin_id) > 1)) {
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

  if (has_basin_id) {
    # Always use basin_ids (plural) - API expects this even for single basin
    inputs$basin_ids <- basin_id

  } else if (has_csv_basin) {
    # CSV with basin ID column
    inputs$csv_url <- csv_url
    inputs$colname_basin_id <- colname_basin_id
  } else if (has_csv_subc) {
    # CSV with subcatchment ID column
    inputs$csv_url <- csv_url
    inputs$colname_subc_id <- colname_subc_id
  } else if (has_csv_coords) {
    # CSV with coordinate columns
    inputs$csv_url <- csv_url
    inputs$colname_lon <- colname_lon
    inputs$colname_lat <- colname_lat
  } else if (has_coords) {
    # Single point with GeoJSON structure
    inputs$point <- list(
      type = "Point",
      coordinates = c(longitude, latitude)  # [lon, lat] order for GeoJSON
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

  # Ensure basin_ids is always an array in JSON (not auto-unboxed)
  if (!is.null(inputs$basin_ids) && length(inputs$basin_ids) == 1) {
    body$inputs$basin_ids <- I(inputs$basin_ids)
  }

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

  # ---------- HANDLE ASYNC RESPONSE WITH POLLING ----------

  if (use_async && !is.null(result$jobID)) {
    message(sprintf("\nAsync job submitted successfully!"))
    message(sprintf("  Job ID: %s", result$jobID))
    message(sprintf("  Status: %s", result$status))

    # Add context about what was requested
    if (has_basin_id) {
      if (length(basin_id) == 1) {
        message(sprintf("  Basin ID: %d", basin_id))
      } else {
        message(sprintf("  Basin IDs: %s", paste(basin_id, collapse = ", ")))
      }
    } else if (has_csv) {
      message(sprintf("  CSV: %s", basename(csv_url)))
      if (has_csv_basin) message(sprintf("  Column: %s (basin IDs)", colname_basin_id))
      if (has_csv_subc) message(sprintf("  Column: %s (subcatchment IDs)", colname_subc_id))
      if (has_csv_coords) message(sprintf("  Columns: %s, %s (coordinates)", colname_lon, colname_lat))
    } else if (has_coords) {
      message(sprintf("  Point: [%.6f, %.6f]", longitude, latitude))
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
                     max_wait, job_id))
      }

      # Poll job status
      job_status <- api_poll_job(job_id)

      if (job_status$status == "successful") {
        message(sprintf("✓ Job completed successfully after %.1f seconds", elapsed))

        if (!is.null(job_status$href)) {
          message("Downloading results...")
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
          stop("Job completed but no download link (href) provided.")
        }
      } else if (job_status$status == "failed") {
        stop(sprintf("Job failed. Job ID: %s", job_id))
      } else if (job_status$status == "dismissed") {
        stop(sprintf("Job was dismissed. Job ID: %s", job_id))
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
    stop("Sync mode without href not yet implemented. Use async mode or contact support.")
  }
}
