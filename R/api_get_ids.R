#' Get Subcatchment/Geographic IDs (Basin, Upstream, or Point Location)
#'
#' Consolidated function to retrieve subcatchment IDs and geographic identifiers
#' from the GeoFRESH API. Replaces api_get_basin_subcids(), api_get_upstream_subcids(),
#' and api_get_local_ids() with a unified interface.
#'
#' @family ocgapi
#'
#' @param basin_ids Integer or vector. Basin IDs to query (mode="basin" only).
#' @param subc_ids Integer or vector. Subcatchment IDs (mode="basin" only).
#' @param points Data.frame. Sites with lon/lat/site_id columns (mode="upstream" only).
#' @param lon Numeric. Longitude for point location (mode="point" only).
#' @param lat Numeric. Latitude for point location (mode="point" only).
#' @param mode Character. "basin", "upstream", or "point". Default: "basin".
#' @param which_ids Character vector. Which IDs to return: "subc_id", "basin_id", "reg_id".
#' @param min_strahler Integer. Minimum Strahler order filter (mode="basin" only).
#' @param colname_lon,colname_lat,colname_site_id Character. Column names.
#' @param comment Character. API logging comment.
#' @param force_async Logical. Force async/sync mode. Default: TRUE.
#' @param poll_interval Numeric. Seconds between status checks. Default: 10.
#' @param max_wait Numeric. Max seconds to wait for job. Default: 3600.
#'
#' @return
#' - mode="basin": Integer vector of subcatchment IDs
#' - mode="upstream": Data.frame with `site_id`, `subc_id`, `upstream_id`
#' - mode="point": List with `subc_id`, `basin_id`, `reg_id`, `coordinates`
#'
#' @examples
#' \dontrun{
#' # Basin mode
#' result <- api_get_ids(basin_ids = 1288419, mode = "basin")
#'
#' # Upstream mode
#' sites <- data.frame(site_id = "S1", longitude = 20.5, latitude = 40.1)
#' result <- api_get_ids(points = sites, mode = "upstream")
#'
#' # Point mode
#' result <- api_get_ids(lon = 10.7, lat = 53.5, mode = "point")
#' }
#'
#' @importFrom httr2 request req_headers req_body_json req_perform
#' @importFrom httr2 resp_status resp_body_json resp_body_string
#' @export

api_get_ids <- function(
    basin_ids = NULL,
    subc_ids = NULL,
    points = NULL,
    lon = NULL,
    lat = NULL,
    mode = "basin",
    which_ids = c("subc_id", "basin_id", "reg_id"),
    min_strahler = NULL,
    colname_lon = "longitude",
    colname_lat = "latitude",
    colname_site_id = "site_id",
    comment = NULL,
    force_async = TRUE,
    poll_interval = 10,
    max_wait = 3600
) {

  if (!mode %in% c("basin", "upstream", "point")) {
    stop("`mode` must be 'basin', 'upstream', or 'point'.", call. = FALSE)
  }

  if (mode == "basin") {
    result <- .basin_mode(
      basin_ids, subc_ids, points, which_ids, min_strahler,
      colname_lon, colname_lat, comment, force_async, poll_interval, max_wait
    )
  } else if (mode == "upstream") {
    result <- .upstream_mode(
      points, colname_lon, colname_lat, colname_site_id, comment
    )
  } else if (mode == "point") {
    result <- .point_mode(lon, lat, which_ids, comment)
  }

  return(result)
}

# ============================================================================
# MODE: BASIN - Get all subcatchments in basin(s)
# ============================================================================

.basin_mode <- function(basin_ids, subc_ids, points, which_ids, min_strahler,
                        colname_lon, colname_lat, comment, force_async,
                        poll_interval, max_wait) {

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

  valid_ids <- c("subc_id", "basin_id", "reg_id")
  if (!all(which_ids %in% valid_ids)) {
    stop(sprintf("`which_ids` must be subset of: %s", paste(valid_ids, collapse = ", ")),
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
    if (!is.data.frame(points) && !is.matrix(points)) {
      stop("`points` must be data.frame or matrix.", call. = FALSE)
    }
    if (is.matrix(points)) points <- as.data.frame(points)
    if (!colname_lon %in% colnames(points)) {
      stop(sprintf("`points` must have '%s' column.", colname_lon), call. = FALSE)
    }
    if (!colname_lat %in% colnames(points)) {
      stop(sprintf("`points` must have '%s' column.", colname_lat), call. = FALSE)
    }
    lons <- points[[colname_lon]]
    lats <- points[[colname_lat]]
    if (any(lats < -90 | lats > 90, na.rm = TRUE)) stop("Latitudes must be -90 to 90.", call. = FALSE)
    if (any(lons < -180 | lons > 180, na.rm = TRUE)) stop("Longitudes must be -180 to 180.", call. = FALSE)
    if (any(is.na(lons)) || any(is.na(lats))) stop("`points` cannot contain NA.", call. = FALSE)
  }

  if (!is.null(min_strahler)) {
    if (!is.numeric(min_strahler) || min_strahler < 1) {
      stop("`min_strahler` must be positive numeric or NULL.", call. = FALSE)
    }
    min_strahler <- as.integer(min_strahler)
  }

  # --- BUILD REQUEST BODY ---

  process_url <- "https://aqua.igb-berlin.de/pygeoapi/processes/get-basin-subcids/execution"

  inputs <- list()

  if (!is.null(basin_ids)) {
    inputs$basin_ids <- as.list(basin_ids)   # as.list() preserves JSON array even for length-1
  } else if (!is.null(subc_ids)) {
    inputs$subc_ids <- as.list(subc_ids)     # as.list() preserves JSON array even for length-1
  } else if (!is.null(points)) {
    features <- lapply(seq_len(nrow(points)), function(i) {
      list(
        type     = "Feature",
        geometry = list(
          type        = "Point",
          coordinates = c(points[[colname_lon]][i], points[[colname_lat]][i])
        ),
        properties = list()
      )
    })
    inputs$points_geojson <- list(type = "FeatureCollection", features = features)
  }

  if (!is.null(min_strahler)) inputs$min_strahler <- min_strahler
  if (!is.null(comment))      inputs$comment      <- comment

  body <- list(
    inputs  = inputs,
    outputs = list(transmissionMode = "reference")
  )

  # --- SEND REQUEST ---

  req <- httr2::request(process_url) %>%
    httr2::req_body_json(body, auto_unbox = TRUE)

  if (force_async) {
    req <- req %>% httr2::req_headers("Prefer" = "respond-async")
  }

  resp <- req %>% httr2::req_perform()

  if (!httr2::resp_status(resp) %in% c(200, 201)) {
    stop(sprintf("API request failed (HTTP %s)", httr2::resp_status(resp)), call. = FALSE)
  }

  result <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  # --- HANDLE ASYNC RESPONSE ---

  if (force_async && !is.null(result$jobID)) {
    message(sprintf("Async job submitted. Job ID: %s", result$jobID))

    job_id     <- result$jobID
    start_time <- Sys.time()

    repeat {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      if (elapsed > max_wait) {
        stop(sprintf("Job did not complete within %d seconds.", max_wait), call. = FALSE)
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

  # --- HANDLE SYNC RESPONSE ---

  if (!is.null(result$href)) {
    message("Retrieving results (sync mode)...")
    parsed_results <- parse_job_results(result$href)
    return(unlist(parsed_results$subc_ids, use.names = FALSE))
  } else {
    stop("No download link (href) in response.", call. = FALSE)
  }
}

# ============================================================================
# MODE: UPSTREAM - Get upstream subcatchments for each site
# ============================================================================

.upstream_mode <- function(points, colname_lon, colname_lat,
                           colname_site_id, comment) {

  # --- INPUT VALIDATION ---

  if (is.null(points) || nrow(points) == 0) {
    stop("In mode='upstream', `points` must be non-empty data.frame.", call. = FALSE)
  }
  if (!is.data.frame(points)) {
    stop("In mode='upstream', `points` must be data.frame.", call. = FALSE)
  }
  if (!colname_lon %in% colnames(points)) {
    stop(sprintf("`points` must have '%s' column.", colname_lon), call. = FALSE)
  }
  if (!colname_lat %in% colnames(points)) {
    stop(sprintf("`points` must have '%s' column.", colname_lat), call. = FALSE)
  }
  if (!colname_site_id %in% colnames(points)) {
    stop(sprintf("`points` must have '%s' column.", colname_site_id), call. = FALSE)
  }

  process_url <- "https://aqua.igb-berlin.de/pygeoapi/processes/get-upstream-subcids/execution"

  all_results <- list()

  # --- LOOP THROUGH EACH SITE ---

  for (i in seq_len(nrow(points))) {
    lon     <- points[[colname_lon]][i]
    lat     <- points[[colname_lat]][i]
    site_id <- points[[colname_site_id]][i]

    inputs <- list(lon = lon, lat = lat)
    if (!is.null(comment)) inputs$comment <- paste0(comment, "_row_", i)

    resp <- tryCatch(
      httr2::request(process_url) %>%
        httr2::req_body_json(list(inputs = inputs), auto_unbox = TRUE) %>%
        httr2::req_perform(),
      error = function(e) NULL
    )

    if (is.null(resp) || httr2::resp_status(resp) != 200) {
      warning(sprintf("Row %d (site %s) failed.", i, site_id))
      next
    }

    result_json <- httr2::resp_body_json(resp)

    if (is.null(result_json$upstream_ids) || length(result_json$upstream_ids) == 0) {
      warning(sprintf("No upstream IDs for row %d (site %s).", i, site_id))
      next
    }

    result_df <- data.frame(
      site_id     = rep(site_id, length(result_json$upstream_ids)),
      subc_id     = rep(result_json$subc_id, length(result_json$upstream_ids)),
      upstream_id = unlist(result_json$upstream_ids),
      stringsAsFactors = FALSE
    )

    all_results[[length(all_results) + 1]] <- result_df
    message(sprintf("Row %d done: %d upstream segments.", i, nrow(result_df)))
  }

  if (length(all_results) == 0) {
    warning("No valid results returned.")
    return(data.frame(site_id = character(), subc_id = numeric(), upstream_id = numeric()))
  }

  combined_df <- do.call(rbind, all_results)
  rownames(combined_df) <- NULL

  return(combined_df)
}

# ============================================================================
# MODE: POINT - Get geographic IDs for a single point
# ============================================================================

.point_mode <- function(lon, lat, which_ids, comment) {

  # --- INPUT VALIDATION ---

  if (is.null(lon) || is.null(lat)) {
    stop("In mode='point', must provide both `lon` and `lat`.", call. = FALSE)
  }
  if (!is.numeric(lon) || !is.numeric(lat)) {
    stop("In mode='point', `lon` and `lat` must be numeric.", call. = FALSE)
  }
  if (lon < -180 || lon > 180) stop("`lon` must be -180 to 180.", call. = FALSE)
  if (lat < -90  || lat > 90)  stop("`lat` must be -90 to 90.",  call. = FALSE)

  process_url <- "https://aqua.igb-berlin.de/pygeoapi/processes/get-local-ids-plural/execution"

  # --- BUILD REQUEST BODY ---

  # GeometryCollection + result_format="csv" + transmissionMode="reference"
  # is the tested path that returns all IDs reliably (test cases 6 & 7 in Python source)
  inputs <- list(
    points_geojson = list(
      type       = "GeometryCollection",
      geometries = list(
        list(type = "Point", coordinates = c(lon, lat))
      )
    ),
    result_format = "csv"
  )

  if (!is.null(comment)) inputs$comment <- comment

  body <- list(
    inputs  = inputs,
    outputs = list(transmissionMode = "reference")
  )

  # --- SEND REQUEST ---

  resp <- httr2::request(process_url) %>%
    httr2::req_body_json(body, auto_unbox = TRUE) %>%
    httr2::req_perform()

  if (!httr2::resp_status(resp) %in% c(200, 201)) {
    stop(sprintf("API request failed (HTTP %s): %s",
                 httr2::resp_status(resp),
                 httr2::resp_body_string(resp)),
         call. = FALSE)
  }

  # Response contains a href to a CSV file — download and parse
  result_json <- httr2::resp_body_json(resp)
  href <- result_json$href %||% result_json[[1]]$href

  if (is.null(href)) stop("No download link in response.", call. = FALSE)

  csv_result <- utils::read.csv(href, stringsAsFactors = FALSE)

  # --- RETURN AS LIST ---
  list(
    coordinates = c(lon = lon, lat = lat),
    subc_id     = csv_result$subc_id[1]  %||% NA,
    basin_id    = csv_result$basin_id[1] %||% NA,
    reg_id      = csv_result$reg_id[1]   %||% NA
  )
}

# --- NULL COALESCING OPERATOR ---

`%||%` <- function(x, y) if (is.null(x)) y else x
