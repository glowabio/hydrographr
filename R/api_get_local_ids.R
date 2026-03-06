#' Get Local IDs (Regional Unit, Basin, Subcatchment) for Sampling Sites
#'
#' @description
#' Retrieves regional unit IDs, basin IDs, and/or subcatchment IDs for sampling
#' locations. Supports two input modes:
#' \itemize{
#'   \item Data.frame with longitude/latitude columns
#'   \item CSV URL with longitude/latitude columns
#' }
#'
#' For large datasets, automatically uses async mode and polls for completion.
#'
#' @family ocgapi
#' @param data Data.frame (optional). Points data with longitude, latitude, and
#'   optionally site_id columns. Default: `NULL`.
#' @param csv_url Character (optional). URL to a CSV file with point data.
#'   Default: `NULL`.
#' @param colname_lon Character. Name of longitude column.
#'   Default: `"longitude"`.
#' @param colname_lat Character. Name of latitude column.
#'   Default: `"latitude"`.
#' @param colname_site_id Character (optional). Name of site ID column.
#'   If `NULL`, sequential IDs will be generated. Default: `"site_id"`.
#' @param colname_subc_id Character (optional). Name of existing subcatchment ID
#'   column (if available). Default: `NULL`.
#' @param which_ids Character vector. Which IDs to retrieve. Options:
#'   `"reg_id"`, `"basin_id"`, `"subc_id"`. Default: `c("subc_id", "basin_id", "reg_id")`.
#' @param comment Character (optional). Comment for API logging.
#'   Default: `NULL`.
#' @param force_async Logical (optional). Force async/sync mode:
#'   `NULL` = auto-detect, `TRUE` = force async, `FALSE` = force sync.
#'   Default: `NULL`.
#' @param async_threshold Numeric. Number of points threshold for auto-async.
#'   Default: `1000`.
#' @param poll_interval Numeric. Seconds to wait between status checks.
#'   Default: `10`.
#' @param max_wait Numeric. Maximum seconds to wait for job completion.
#'   Default: `3600` (1 hour).
#'
#' @return Data.frame with original columns plus requested ID columns
#'   (reg_id, basin_id, and/or subc_id).
#'
#' @examples
#' \dontrun{
#' # Example 1: From data.frame
#' sites <- data.frame(
#'   site_id = 1:3,
#'   longitude = c(10.698833, 12.808980, 11.915323),
#'   latitude = c(53.517107, 52.421871, 52.730867)
#' )
#' result <- api_get_local_ids(data = sites)
#' head(result)
#'
#' # Example 2: From CSV URL
#' result <- api_get_local_ids(
#'   csv_url = "https://example.com/sites.csv",
#'   which_ids = c("basin_id", "reg_id")
#' )
#'
#' # Example 3: Only regional IDs (faster)
#' result <- api_get_local_ids(
#'   data = sites,
#'   which_ids = "reg_id"
#' )
#' }
#'
#' @export
#' @importFrom httr POST add_headers status_code content
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom data.table fread

api_get_local_ids <- function(
    data = NULL,
    csv_url = NULL,
    colname_lon = "longitude",
    colname_lat = "latitude",
    colname_site_id = "site_id",
    colname_subc_id = NULL,
    which_ids = c("subc_id", "basin_id", "reg_id"),
    comment = NULL,
    force_async = NULL,
    async_threshold = 1000,
    poll_interval = 10,
    max_wait = 3600
) {

  # ---------- INPUT VALIDATION ----------

  # Count input methods
  input_count <- sum(!is.null(data), !is.null(csv_url))

  if (input_count == 0) {
    stop("Must provide either 'data' or 'csv_url'.",
         call. = FALSE)
  }

  if (input_count > 1) {
    stop("Only one input method allowed at a time.",
         call. = FALSE)
  }

  # Validate which_ids
  if (!is.character(which_ids) || length(which_ids) == 0) {
    stop("`which_ids` must be a non-empty character vector.", call. = FALSE)
  }

  valid_ids <- c("reg_id", "basin_id", "subc_id")
  if (!all(which_ids %in% valid_ids)) {
    stop(sprintf("`which_ids` must be one or more of: %s",
                 paste(valid_ids, collapse = ", ")),
         call. = FALSE)
  }

  # Store original data for later merging
  original_data <- NULL
  input_type <- NULL

  # ---------- HANDLE DATA.FRAME INPUT ----------

  if (!is.null(data)) {
    if (!is.data.frame(data) && !is.matrix(data)) {
      stop("`data` must be a data.frame or matrix.", call. = FALSE)
    }

    if (is.matrix(data)) {
      data <- as.data.frame(data)
    }

    # Check for required columns
    if (!colname_lon %in% colnames(data)) {
      stop(sprintf("`data` must have a '%s' column.", colname_lon),
           call. = FALSE)
    }
    if (!colname_lat %in% colnames(data)) {
      stop(sprintf("`data` must have a '%s' column.", colname_lat),
           call. = FALSE)
    }

    # Validate coordinates
    lons <- data[[colname_lon]]
    lats <- data[[colname_lat]]

    if (any(lats < -90 | lats > 90, na.rm = TRUE)) {
      stop("All latitudes must be between -90 and 90.", call. = FALSE)
    }
    if (any(lons < -180 | lons > 180, na.rm = TRUE)) {
      stop("All longitudes must be between -180 and 180.", call. = FALSE)
    }
    if (any(is.na(lons)) || any(is.na(lats))) {
      stop("`data` cannot contain NA values in coordinate columns.",
           call. = FALSE)
    }

    # Check/create site_id column
    if (!is.null(colname_site_id) && !colname_site_id %in% colnames(data)) {
      message(sprintf("Creating sequential site IDs in column '%s'", colname_site_id))
      data[[colname_site_id]] <- seq_len(nrow(data))
    }

    original_data <- data
    input_type <- "dataframe"
    num_points <- nrow(data)

  } else if (!is.null(csv_url)) {
    # Validate CSV URL
    if (!is.character(csv_url) || !grepl("^https?://", csv_url)) {
      stop("`csv_url` must be a valid HTTP or HTTPS URL.", call. = FALSE)
    }

    # Download the original CSV to merge with results later
    message("Downloading original CSV data...")
    original_data <- tryCatch({
      data.table::fread(csv_url)
    }, error = function(e) {
      stop(sprintf("Error downloading CSV: %s", e$message),
           call. = FALSE)
    })

    # Validate columns exist
    if (!colname_lon %in% colnames(original_data)) {
      stop(sprintf("CSV must have a '%s' column.", colname_lon),
           call. = FALSE)
    }
    if (!colname_lat %in% colnames(original_data)) {
      stop(sprintf("CSV must have a '%s' column.", colname_lat),
           call. = FALSE)
    }

    input_type <- "csv"
    num_points <- nrow(original_data)
  }

  # ---------- DETERMINE ASYNC MODE ----------

  use_async <- force_async

  if (is.null(use_async) && !is.null(num_points)) {
    use_async <- num_points >= async_threshold

    if (use_async) {
      message(sprintf(
        "Dataset has %d points (>= %d threshold). Using async mode.",
        num_points, async_threshold
      ))
    } else {
      message(sprintf(
        "Dataset has %d points (< %d threshold). Using sync mode.",
        num_points, async_threshold
      ))
    }
  } else if (is.null(use_async)) {
    # Default to sync if we can't determine size
    use_async <- FALSE
    message("Using sync mode (dataset size unknown).")
  } else {
    message(sprintf("Using %s mode (forced by user)",
                    if (use_async) "async" else "sync"))
  }

  # ---------- BUILD REQUEST ----------

  process_url <- "https://aqua.igb-berlin.de/pygeoapi/processes/get-local-ids-plural/execution"

  inputs <- list()

  # Handle different input types
  if (input_type == "dataframe") {
    # Upload data.frame as CSV to temporary location
    # Actually, we'll convert to GeoJSON for the API
    features <- lapply(seq_len(nrow(data)), function(i) {
      properties <- list()
      if (!is.null(colname_site_id) && colname_site_id %in% colnames(data)) {
        properties[[colname_site_id]] <- data[[colname_site_id]][i]
      }

      list(
        type = "Feature",
        properties = properties,
        geometry = list(
          type = "Point",
          coordinates = c(data[[colname_lon]][i], data[[colname_lat]][i])
        )
      )
    })

    inputs$points_geojson <- list(
      type = "FeatureCollection",
      features = features
    )

    if (!is.null(colname_site_id)) {
      inputs$colname_site_id <- colname_site_id
    }

  } else if (input_type == "csv") {
    inputs$csv_url <- csv_url
    inputs$colname_lon <- colname_lon
    inputs$colname_lat <- colname_lat

    if (!is.null(colname_site_id)) {
      inputs$colname_site_id <- colname_site_id
    }
    if (!is.null(colname_subc_id)) {
      inputs$colname_subc_id <- colname_subc_id
    }
  }

  # Add which_ids
  inputs$which_ids <- which_ids

  # Add optional comment
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
    body = jsonlite::toJSON(body, auto_unbox = TRUE, pretty = FALSE),
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
    message(sprintf("  Which IDs: %s", paste(which_ids, collapse = ", ")))

    if (input_type == "dataframe") {
      message(sprintf("  Number of points: %d", nrow(data)))
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

          # Download and parse the result
          result_df <- download_and_parse_result(job_status$href, input_type)

          message(sprintf("Retrieved %d rows with %d columns",
                          nrow(result_df), ncol(result_df)))

          # Merge with original data
          if (!is.null(original_data)) {
            # For CSV input, if result_df doesn't have lon/lat, add them from original
            if (input_type == "csv" &&
                !("longitude" %in% colnames(result_df)) &&
                !("latitude" %in% colnames(result_df))) {

              # The CSV result has site_id but not lon/lat
              # We need to merge original data with result to get complete info
              if ("site_id" %in% colnames(result_df) &&
                  colname_site_id %in% colnames(original_data)) {

                # Rename if needed
                if (colname_site_id != "site_id") {
                  names(result_df)[names(result_df) == "site_id"] <- colname_site_id
                }

                # Merge to get all columns
                result_df <- merge(
                  original_data,
                  result_df,
                  by = colname_site_id,
                  all.x = TRUE,
                  sort = FALSE
                )
              }
            } else {
              # Normal merge for dataframe input or when result has lon/lat
              result_df <- merge_with_original_data(
                original_data, result_df, colname_site_id, colname_lon, colname_lat
              )
            }
          }

          return(result_df)

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

  # ---------- HANDLE SYNC RESPONSE ----------

  download_url <- result$href

  if (is.null(download_url)) {
    stop("No download link found in the results.", call. = FALSE)
  }

  message("ID retrieval complete (sync mode). Downloading results...")

  # Download and parse the result
  result_df <- download_and_parse_result(download_url, input_type)

  message(sprintf("Retrieved %d rows with %d columns",
                  nrow(result_df), ncol(result_df)))

  # Merge with original data
  if (!is.null(original_data)) {
    # For CSV input, if result_df doesn't have lon/lat, add them from original
    if (input_type == "csv" &&
        !("longitude" %in% colnames(result_df)) &&
        !("latitude" %in% colnames(result_df))) {

      # The CSV result has site_id but not lon/lat
      # We need to merge original data with result to get complete info
      if ("site_id" %in% colnames(result_df) &&
          colname_site_id %in% colnames(original_data)) {

        # Rename if needed
        if (colname_site_id != "site_id") {
          names(result_df)[names(result_df) == "site_id"] <- colname_site_id
        }

        # Merge to get all columns
        result_df <- merge(
          original_data,
          result_df,
          by = colname_site_id,
          all.x = TRUE,
          sort = FALSE
        )
      }
    } else {
      # Normal merge for dataframe input or when result has lon/lat
      result_df <- merge_with_original_data(
        original_data, result_df, colname_site_id, colname_lon, colname_lat
      )
    }
  }

  return(result_df)
}


#' Download and Parse API Result
#'
#' @description
#' Internal helper function to download and parse results from the API.
#' Handles both CSV and JSON formats.
#'
#' @param url Character. Download URL
#' @param input_type Character. Type of input ("dataframe" or "csv")
#'
#' @return Data.frame with parsed results
#' @keywords internal

download_and_parse_result <- function(url, input_type) {

  # Try to determine format from URL
  is_csv <- grepl("\\.csv$", url, ignore.case = TRUE)
  is_json <- grepl("\\.json$", url, ignore.case = TRUE)

  if (is_csv) {
    # Parse as CSV
    result_df <- tryCatch({
      data.table::fread(url)
    }, error = function(e) {
      stop(sprintf("Error downloading CSV: %s", e$message),
           call. = FALSE)
    })

  } else if (is_json || input_type == "dataframe") {
    # Parse as JSON
    result_json <- tryCatch({
      jsonlite::fromJSON(url, simplifyVector = FALSE)
    }, error = function(e) {
      stop(sprintf("Error downloading JSON: %s", e$message),
           call. = FALSE)
    })

    # Convert JSON structure to data.frame
    result_df <- parse_json_to_dataframe(result_json)

  } else {
    # Try JSON first, then CSV
    result_df <- tryCatch({
      result_json <- jsonlite::fromJSON(url, simplifyVector = FALSE)
      parse_json_to_dataframe(result_json)
    }, error = function(e1) {
      tryCatch({
        data.table::fread(url)
      }, error = function(e2) {
        stop(sprintf("Error parsing result (tried JSON and CSV): %s, %s",
                     e1$message, e2$message),
             call. = FALSE)
      })
    })
  }

  return(result_df)
}


#' Parse JSON Result to Data.frame
#'
#' @description
#' Internal helper function to parse the JSON structure returned by the API
#' into a clean data.frame format.
#'
#' @param json_result List. Parsed JSON from API
#'
#' @return Data.frame with one row per site
#' @keywords internal

parse_json_to_dataframe <- function(json_result) {

  if (is.null(json_result$everything)) {
    stop("Unexpected JSON structure: 'everything' field not found.", call. = FALSE)
  }

  rows_list <- list()

  for (reg_id in names(json_result$everything)) {
    reg_data <- json_result$everything[[reg_id]]

    for (basin_id in names(reg_data)) {
      basin_data <- reg_data[[basin_id]]

      for (subc_id in names(basin_data)) {
        features <- basin_data[[subc_id]]

        for (feature in features) {
          # Get the ACTUAL property name (could be gbifID, site_id, etc.)
          props <- feature$properties

          # Extract the site_id value (use the actual property name)
          site_id <- if (!is.null(props) && length(props) > 0) {
            # Get the first property value (works for any property name)
            as.character(props[[1]])
          } else {
            NA_character_
          }

          coords <- feature$geometry$coordinates

          rows_list[[length(rows_list) + 1]] <- data.frame(
            site_id = site_id,
            longitude = as.numeric(coords[[1]]),
            latitude = as.numeric(coords[[2]]),
            # CRITICAL: Keep as character - "None" can't convert to numeric!
            subc_id = as.character(subc_id),
            basin_id = as.character(basin_id),
            reg_id = as.character(reg_id),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(rows_list) == 0) {
    stop("No features found in JSON result.", call. = FALSE)
  }

  result_df <- do.call(rbind, rows_list)
  rownames(result_df) <- NULL

  return(result_df)
}


#' Merge Result with Original Data
#'
#' @description
#' Internal helper function to merge API results back with the original
#' input data.frame, preserving all original columns and adding ID columns.
#'
#' @param original_data Data.frame. Original input data
#' @param result_df Data.frame. Results from API
#' @param colname_site_id Character. Site ID column name
#' @param colname_lon Character. Longitude column name
#' @param colname_lat Character. Latitude column name
#'
#' @return Data.frame with original columns plus ID columns
#' @keywords internal

merge_with_original_data <- function(original_data, result_df,
                                     colname_site_id, colname_lon, colname_lat) {

  # Determine merge keys
  merge_keys <- character(0)

  # Try to merge by site_id if available in both
  if (!is.null(colname_site_id) &&
      colname_site_id %in% colnames(original_data) &&
      "site_id" %in% colnames(result_df)) {

    # Need to align column names
    # If result has "site_id" but original has custom name, rename in result
    if (colname_site_id != "site_id") {
      names(result_df)[names(result_df) == "site_id"] <- colname_site_id
    }
    merge_keys <- colname_site_id

  }

  # If site_id merge didn't work, fall back to coordinates
  if (length(merge_keys) == 0) {
    # Check if result_df has longitude/latitude
    has_lon_lat <- "longitude" %in% colnames(result_df) &&
      "latitude" %in% colnames(result_df)

    # Check if original_data has the specified lon/lat columns
    has_custom_lon_lat <- colname_lon %in% colnames(original_data) &&
      colname_lat %in% colnames(original_data)

    if (has_lon_lat && has_custom_lon_lat) {
      # Rename result_df columns to match original_data if needed
      if (colname_lon != "longitude") {
        names(result_df)[names(result_df) == "longitude"] <- colname_lon
      }
      if (colname_lat != "latitude") {
        names(result_df)[names(result_df) == "latitude"] <- colname_lat
      }
      merge_keys <- c(colname_lon, colname_lat)
    } else {
      warning("Cannot merge results with original data: no common columns found. ",
              "Returning API results only.")
      return(result_df)
    }
  }

  # Get ID columns from result
  id_cols <- intersect(c("reg_id", "basin_id", "subc_id"), colnames(result_df))

  if (length(id_cols) == 0) {
    warning("No ID columns found in results. Returning original data.")
    return(original_data)
  }

  # Columns to add from result_df (exclude those already in original_data)
  cols_to_add <- setdiff(colnames(result_df), colnames(original_data))

  # Keep only columns that actually exist in result_df
  cols_to_add <- intersect(cols_to_add, colnames(result_df))

  # Perform merge
  merged <- merge(
    original_data,
    result_df[, c(merge_keys, cols_to_add), drop = FALSE],
    by = merge_keys,
    all.x = TRUE,
    sort = FALSE
  )

  return(merged)
}

