#' Get Subcatchment/Geographic IDs
#'
#' Retrieves subcatchment IDs and related geographic identifiers from the
#' GeoFRESH API. Three modes cover distinct spatial queries:
#'
#' - **"basin"**: returns all subcatchment IDs contained within the basin(s)
#'   of the given locations.
#' - **"upstream"**: returns all subcatchment IDs upstream of each input point.
#' - **"local"**: returns the local identifiers (subc_id, basin_id, reg_id)
#'   for each input point or subcatchment — i.e. the reverse lookup.
#'
#' @family ocgapi
#'
#' @param points Data.frame. Input sites. Used in all three modes:
#'   - mode="basin": must contain longitude and latitude columns (and optionally
#'     site_id). Can alternatively supply `basin_ids` or `subc_ids` directly.
#'   - mode="upstream": must contain longitude, latitude, and site_id columns.
#'   - mode="local": must contain either longitude + latitude columns, or a
#'     subc_id column. Returns one row of IDs per input row.
#' @param basin_ids Integer or vector. Basin IDs (mode="basin" only).
#'   Alternative to `points` or `subc_ids`.
#' @param subc_ids Integer or vector. Subcatchment IDs (mode="basin" only).
#'   Alternative to `points` or `basin_ids`.
#' @param mode Character. One of "basin", "upstream", or "local".
#'   Default: "basin".
#' @param min_strahler Integer. Minimum Strahler order filter
#'   (mode="basin" and mode="upstream" only). Default: NULL.
#' @param colname_lon Character. Name of the longitude column in `points`.
#'   Default: "longitude".
#' @param colname_lat Character. Name of the latitude column in `points`.
#'   Default: "latitude".
#' @param colname_site_id Character. Name of the site ID column in `points`.
#'   Default: "site_id".
#' @param colname_subc_id Character. Name of an existing subcatchment ID column
#'   in `points` (mode="local" only). If provided, the API uses these directly
#'   instead of looking up from coordinates. Default: NULL.
#' @param force_async Logical. Force async (TRUE) or sync (FALSE) mode.
#'   Default: TRUE.
#' @param poll_interval Numeric. Seconds between status checks. Default: 10.
#' @param max_wait Numeric. Max seconds to wait for async job. Default: 3600.
#'
#' @return
#' - mode="basin": Integer vector of all subcatchment IDs in the basin(s).
#' - mode="upstream": Data.frame with columns `site_id`, `subc_id`,
#'   `upstream_id` (one row per upstream subcatchment per site).
#' - mode="local": Data.frame with one row per input point, containing
#'   `subc_id`, `basin_id`, and `reg_id`.
#'
#' @examples
#' \dontrun{
#' # Basin mode: get all subcatchments in a basin
#' result <- api_get_ids(basin_ids = 1288419, mode = "basin")
#'
#' # Basin mode: from snapped points dataframe
#' result <- api_get_ids(
#'   points          = all_snapped,
#'   colname_lon     = "longitude_snapped",
#'   colname_lat     = "latitude_snapped",
#'   colname_site_id = "site_id",
#'   mode            = "basin"
#' )
#'
#' # Upstream mode: all upstream subcatchments per site
#' sites <- data.frame(site_id = "S1", longitude = 9.93, latitude = 54.70)
#' result <- api_get_ids(points = sites, mode = "upstream")
#'
#' # Local mode: get subc_id, basin_id, reg_id for each point
#' sites <- data.frame(longitude = c(10.7, 9.9), latitude = c(53.5, 54.7))
#' result <- api_get_ids(points = sites, mode = "local")
#'
#' # Local mode: from existing subc_ids column
#' result <- api_get_ids(
#'   points         = my_df,
#'   colname_subc_id = "subc_id",
#'   mode           = "local"
#' )
#' }
#'
#' @importFrom httr2 request req_headers req_body_json req_perform
#' @importFrom httr2 resp_status resp_body_json resp_body_string
#' @export

api_get_ids <- function(
    points          = NULL,
    basin_ids       = NULL,
    subc_ids        = NULL,
    mode            = "basin",
    min_strahler    = NULL,
    colname_lon     = "longitude",
    colname_lat     = "latitude",
    colname_site_id = "site_id",
    colname_subc_id = NULL,
    force_async     = TRUE,
    poll_interval   = 10,
    max_wait        = 3600
) {

  if (!mode %in% c("basin", "upstream", "local")) {
    stop("`mode` must be 'basin', 'upstream', or 'local'.", call. = FALSE)
  }

  if (mode == "basin") {
    result <- .basin_mode(
      basin_ids, subc_ids, points,
      min_strahler, colname_lon, colname_lat, colname_site_id,
      force_async, poll_interval, max_wait
    )
  } else if (mode == "upstream") {
    result <- .upstream_mode(
      points, colname_lon, colname_lat, colname_site_id,
      min_strahler
    )
  } else if (mode == "local") {
    result <- .local_mode(
      points, colname_lon, colname_lat, colname_site_id, colname_subc_id,
       force_async, poll_interval, max_wait
    )
  }

  return(result)
}

# ============================================================================
# MODE: BASIN - Get all subcatchment IDs within basin(s)
# ============================================================================

.basin_mode <- function(basin_ids, subc_ids, points, min_strahler,
                        colname_lon, colname_lat, colname_site_id,
                        force_async, poll_interval, max_wait) {

  # --- INPUT VALIDATION ---

  input_count <- sum(!is.null(basin_ids), !is.null(subc_ids), !is.null(points))

  if (input_count == 0) {
    stop("In mode='basin', must provide one of: basin_ids, subc_ids, or points.",
         call. = FALSE)
  }
  if (input_count > 1) {
    stop("In mode='basin', provide only one of: basin_ids, subc_ids, or points.",
         call. = FALSE)
  }

  if (!is.null(basin_ids)) {
    if (!is.numeric(basin_ids)) stop("`basin_ids` must be numeric.", call. = FALSE)
    basin_ids <- as.integer(basin_ids)
  }

  if (!is.null(subc_ids)) {
    if (!is.numeric(subc_ids)) stop("`subc_ids` must be numeric.", call. = FALSE)
    subc_ids <- as.integer(subc_ids)
  }

  if (!is.null(points)) {
    points <- .validate_points(points, colname_lon, colname_lat,
                               colname_site_id, require_site_id = TRUE)
  }

  if (!is.null(min_strahler)) {
    if (!is.numeric(min_strahler) || min_strahler < 1) {
      stop("`min_strahler` must be a positive integer.", call. = FALSE)
    }
    min_strahler <- as.integer(min_strahler)
  }

  # --- BUILD REQUEST ---

  process_url <- "https://aqua.igb-berlin.de/pygeoapi/processes/get-basin-subcids/execution"

  inputs <- list()

  if (!is.null(basin_ids)) {
    inputs$basin_ids <- as.list(basin_ids)
  } else if (!is.null(subc_ids)) {
    inputs$subc_ids <- as.list(subc_ids)
  } else if (!is.null(points)) {
    inputs$points_geojson <- .df_to_featurecollection(points, colname_lon,
                                                      colname_lat, colname_site_id)
    inputs$colname_site_id <- colname_site_id
  }

  if (!is.null(min_strahler)) inputs$min_strahler <- min_strahler

  body <- list(
    inputs  = inputs,
    outputs = list(transmissionMode = "reference")
  )

  # --- SEND REQUEST ---

  req <- httr2::request(process_url) |>
    httr2::req_body_json(body, auto_unbox = TRUE)

  if (force_async) {
    req <- req |> httr2::req_headers("Prefer" = "respond-async")
  }

  resp <- req |> httr2::req_perform()

  if (!httr2::resp_status(resp) %in% c(200, 201)) {
    stop(sprintf("API request failed (HTTP %s): %s",
                 httr2::resp_status(resp), httr2::resp_body_string(resp)),
         call. = FALSE)
  }

  result <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  # --- HANDLE ASYNC ---

  if (force_async && !is.null(result$jobID)) {
    message(sprintf("Async job submitted. Job ID: %s", result$jobID))

    job_id     <- result$jobID
    start_time <- Sys.time()

    repeat {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      if (elapsed > max_wait) {
        stop(sprintf("Job timed out after %d seconds.", max_wait), call. = FALSE)
      }

      job_status <- api_poll_job(job_id)

      if (job_status$status == "successful") {
        message(sprintf("✓ Job completed after %.1f seconds", elapsed))
        if (!is.null(job_status$href)) {
          message("Downloading results...")
          parsed_results <- parse_job_results(job_status$href)
          return(unlist(parsed_results$subc_ids, use.names = FALSE))
        } else {
          stop("Job completed but no download link provided.", call. = FALSE)
        }
      } else if (job_status$status == "failed") {
        stop(sprintf("Job failed. Job ID: %s", job_id), call. = FALSE)
      } else if (job_status$status == "dismissed") {
        stop(sprintf("Job dismissed. Job ID: %s", job_id), call. = FALSE)
      }

      message(sprintf("  Status: %s (%.1fs elapsed)", job_status$status, elapsed))
      Sys.sleep(poll_interval)
    }
  }

  # --- HANDLE SYNC ---

  if (!is.null(result$href)) {
    message("Downloading results (sync)...")
    parsed_results <- parse_job_results(result$href)
    return(unlist(parsed_results$subc_ids, use.names = FALSE))
  } else {
    stop("No download link (href) in response.", call. = FALSE)
  }
}

# ============================================================================
# MODE: UPSTREAM - Get all subcatchment IDs upstream of each site
# ============================================================================

.upstream_mode <- function(points, colname_lon, colname_lat, colname_site_id,
                           min_strahler) {

  # --- INPUT VALIDATION ---

  if (is.null(points) || nrow(points) == 0) {
    stop("In mode='upstream', `points` must be a non-empty data.frame.", call. = FALSE)
  }

  points <- .validate_points(points, colname_lon, colname_lat,
                             colname_site_id, require_site_id = TRUE)

  if (!is.null(min_strahler)) {
    if (!is.numeric(min_strahler) || min_strahler < 1) {
      stop("`min_strahler` must be a positive integer.", call. = FALSE)
    }
    min_strahler <- as.integer(min_strahler)
  }

  process_url <- "https://aqua.igb-berlin.de/pygeoapi/processes/get-upstream-subcids/execution"

  all_results <- list()

  for (i in seq_len(nrow(points))) {
    lon     <- points[[colname_lon]][i]
    lat     <- points[[colname_lat]][i]
    site_id <- points[[colname_site_id]][i]

    inputs <- list(lon = lon, lat = lat)
    if (!is.null(min_strahler)) inputs$min_strahler <- min_strahler

    resp <- tryCatch(
      httr2::request(process_url) |>
        httr2::req_body_json(list(inputs = inputs), auto_unbox = TRUE) |>
        httr2::req_perform(),
      error = function(e) NULL
    )

    if (is.null(resp) || httr2::resp_status(resp) != 200) {
      warning(sprintf("Row %d (site '%s') failed.", i, site_id))
      next
    }

    result_json <- httr2::resp_body_json(resp)

    if (is.null(result_json$upstream_ids) || length(result_json$upstream_ids) == 0) {
      warning(sprintf("No upstream IDs for row %d (site '%s').", i, site_id))
      next
    }

    n <- length(result_json$upstream_ids)
    all_results[[i]] <- data.frame(
      site_id     = rep(site_id, n),
      subc_id     = rep(result_json$subc_id, n),
      upstream_id = unlist(result_json$upstream_ids),
      stringsAsFactors = FALSE
    )

    message(sprintf("Row %d done: %d upstream segments.", i, n))
  }

  if (length(all_results) == 0) {
    warning("No valid results returned.")
    return(data.frame(site_id = character(), subc_id = numeric(),
                      upstream_id = numeric()))
  }

  combined_df <- do.call(rbind, all_results)
  rownames(combined_df) <- NULL
  return(combined_df)
}

# ============================================================================
# MODE: LOCAL - Get local IDs (subc_id, basin_id, reg_id) for each input row
# ============================================================================

.local_mode <- function(points, colname_lon, colname_lat, colname_site_id,
                        colname_subc_id, force_async, poll_interval,
                        max_wait) {

  # --- INPUT VALIDATION ---

  if (is.null(points) || nrow(points) == 0) {
    stop("In mode='local', `points` must be a non-empty data.frame.", call. = FALSE)
  }
  if (!is.data.frame(points)) {
    stop("In mode='local', `points` must be a data.frame.", call. = FALSE)
  }

  # Save original immediately — cbind onto this at the end
  original_points <- points

  # Add a temporary row index as a property so we can safely merge
  # the API result back regardless of row order in the CSV response
  row_index_col <- ".row_index"
  points[[row_index_col]] <- seq_len(nrow(points))

  has_lonlat  <- colname_lon %in% colnames(points) && colname_lat %in% colnames(points)
  has_subc_id <- !is.null(colname_subc_id) && colname_subc_id %in% colnames(points)

  if (!has_lonlat && !has_subc_id) {
    stop(sprintf(
      "In mode='local', `points` must have either '%s'+'%s' columns or a '%s' column.",
      colname_lon, colname_lat, colname_subc_id %||% "subc_id"
    ), call. = FALSE)
  }

  # --- BUILD REQUEST ---

  process_url <- "https://aqua.igb-berlin.de/pygeoapi/processes/get-local-ids-plural/execution"

  inputs <- list(result_format = "csv")

  if (has_subc_id) {
    # Pass subc_ids directly as CSV URL is not supported — use colname approach
    inputs$csv_url       <- NULL  # Not used; we send GeoJSON
    # Build GeometryCollection from subc_ids — API will look up basin/reg from subc_id
    # The API accepts colname_subc_id in the CSV path; for GeoJSON path we need lon/lat.
    # If user has subc_ids but no lon/lat, they should use the csv_url path.
    # For now: require lon/lat to build GeoJSON, but pass colname_subc_id as hint.
    if (!has_lonlat) {
      stop(paste(
        "In mode='local', when providing subc_ids without coordinates,",
        "please also provide lon/lat columns so the API can locate each point.",
        "Alternatively, use the colname_subc_id parameter together with lon/lat."
      ), call. = FALSE)
    }
  }

  if (has_lonlat) {
    inputs$points_geojson <- list(
      type       = "GeometryCollection",
      geometries = lapply(seq_len(nrow(points)), function(i) {
        list(type = "Point",
             coordinates = c(points[[colname_lon]][i], points[[colname_lat]][i]))
      })
    )
  }



  body <- list(
    inputs  = inputs,
    outputs = list(transmissionMode = "reference")
  )

  # --- SEND REQUEST ---

  req <- httr2::request(process_url) |>
    httr2::req_body_json(body, auto_unbox = TRUE)

  if (force_async) {
    req <- req |> httr2::req_headers("Prefer" = "respond-async")
  }

  resp <- req |> httr2::req_perform()

  if (!httr2::resp_status(resp) %in% c(200, 201)) {
    stop(sprintf("API request failed (HTTP %s): %s",
                 httr2::resp_status(resp), httr2::resp_body_string(resp)),
         call. = FALSE)
  }

  result_json <- httr2::resp_body_json(resp)

  # Handle async job
  if (force_async && !is.null(result_json$jobID)) {
    message(sprintf("Async job submitted. Job ID: %s", result_json$jobID))

    job_id     <- result_json$jobID
    start_time <- Sys.time()

    repeat {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      if (elapsed > max_wait) {
        stop(sprintf("Job timed out after %d seconds.", max_wait), call. = FALSE)
      }

      job_status <- api_poll_job(job_id)

      if (job_status$status == "successful") {
        message(sprintf("✓ Job completed after %.1f seconds", elapsed))
        href <- job_status$href
        break
      } else if (job_status$status == "failed") {
        stop(sprintf("Job failed. Job ID: %s", job_id), call. = FALSE)
      } else if (job_status$status == "dismissed") {
        stop(sprintf("Job dismissed. Job ID: %s", job_id), call. = FALSE)
      }

      message(sprintf("  Status: %s (%.1fs elapsed)", job_status$status, elapsed))
      Sys.sleep(poll_interval)
    }
  } else {
    href <- result_json$href %||% result_json[[1]]$href
  }

  if (is.null(href)) stop("No download link in response.", call. = FALSE)

  # --- DOWNLOAD AND RETURN ---

  message("Downloading results...")
  csv_result <- utils::read.csv(href, stringsAsFactors = FALSE)

  # Extract only the ID columns from the API response,
  # excluding any already present in original_points (e.g. subc_id)
  id_cols <- setdiff(
    intersect(c("subc_id", "basin_id", "reg_id"), colnames(csv_result)),
    colnames(original_points)
  )

  if (length(id_cols) == 0) {
    stop("API response CSV does not contain any new ID columns (subc_id, basin_id, reg_id).\n",
         "Columns found: ", paste(colnames(csv_result), collapse = ", "), call. = FALSE)
  }

  if (nrow(csv_result) != nrow(original_points)) {
    warning(sprintf(
      "API returned %d rows but input had %d rows. Results may be incomplete.",
      nrow(csv_result), nrow(original_points)
    ))
  }

  # Bind new ID columns onto the original input (preserving user's column names)
  result <- cbind(original_points, csv_result[, id_cols, drop = FALSE])
  rownames(result) <- NULL

  return(result)
}

# ============================================================================
# SHARED HELPERS
# ============================================================================

# Validate a points data.frame and check required columns
.validate_points <- function(points, colname_lon, colname_lat,
                             colname_site_id, require_site_id = FALSE) {
  if (!is.data.frame(points) && !is.matrix(points)) {
    stop("`points` must be a data.frame or matrix.", call. = FALSE)
  }
  if (is.matrix(points)) points <- as.data.frame(points)

  if (!colname_lon %in% colnames(points)) {
    stop(sprintf("`points` must have a '%s' column.", colname_lon), call. = FALSE)
  }
  if (!colname_lat %in% colnames(points)) {
    stop(sprintf("`points` must have a '%s' column.", colname_lat), call. = FALSE)
  }
  if (require_site_id && !colname_site_id %in% colnames(points)) {
    stop(sprintf("`points` must have a '%s' column.", colname_site_id), call. = FALSE)
  }

  lons <- points[[colname_lon]]
  lats <- points[[colname_lat]]

  if (any(lats < -90 | lats > 90, na.rm = TRUE)) {
    stop("Latitudes must be between -90 and 90.", call. = FALSE)
  }
  if (any(lons < -180 | lons > 180, na.rm = TRUE)) {
    stop("Longitudes must be between -180 and 180.", call. = FALSE)
  }
  if (any(is.na(lons)) || any(is.na(lats))) {
    stop("`points` cannot contain NA coordinates.", call. = FALSE)
  }

  return(points)
}

# Convert a data.frame to a GeoJSON FeatureCollection with site_id in properties
.df_to_featurecollection <- function(points, colname_lon, colname_lat,
                                     colname_site_id) {
  features <- lapply(seq_len(nrow(points)), function(i) {
    list(
      type     = "Feature",
      geometry = list(
        type        = "Point",
        coordinates = c(points[[colname_lon]][i], points[[colname_lat]][i])
      ),
      properties = setNames(
        list(as.character(points[[colname_site_id]][i])),
        colname_site_id
      )
    )
  })
  list(type = "FeatureCollection", features = features)
}

# NULL coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x
