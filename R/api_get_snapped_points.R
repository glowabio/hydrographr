#' Snap Points to the River Network
#'
#' Snaps input point locations to the nearest stream segment at or above a
#' given Strahler order, using the GeoFRESH API.
#'
#' @family ocgapi
#'
#' @param data Data.frame. Must contain columns for longitude, latitude, and
#'   a site identifier.
#' @param colname_lon Character. Name of the longitude column. Default: "longitude".
#' @param colname_lat Character. Name of the latitude column. Default: "latitude".
#' @param colname_site_id Character. Name of the site ID column. Default: "site_id".
#' @param min_strahler Integer. Minimum Strahler order to snap to. Required.
#' @param add_distance Logical. If TRUE, includes a `distance_metres` column
#'   with the snapping distance. Default: TRUE.
#' @param force_async Logical. If NULL (default), async mode is used
#'   automatically for datasets >= `async_threshold`. Set TRUE or FALSE to
#'   force a mode regardless of dataset size.
#' @param async_threshold Integer. Number of points above which async mode is
#'   used automatically. Default: 500.
#' @param poll_interval Numeric. Seconds between status checks when polling
#'   async jobs. Default: 10.
#' @param max_wait Numeric. Maximum seconds to wait for async job completion.
#'   Default: 3600.
#'
#' @return Data.frame with one row per snapped point, containing:
#'   - All original columns from `data`
#'   - `longitude_snapped`, `latitude_snapped`: snapped coordinates
#'   - `subc_id`: subcatchment ID of the snapped segment
#'   - `strahler`: Strahler order of the snapped segment
#'   - `distance_metres`: distance from original to snapped point (if `add_distance = TRUE`)
#'
#' @examples
#' \dontrun{
#' sites <- data.frame(
#'   site_id   = c("S1", "S2", "S3"),
#'   longitude = c(9.931555, 9.921555, 9.941555),
#'   latitude  = c(54.695070, 54.295070, 54.495070)
#' )
#'
#' # Snap to streams with Strahler >= 4
#' result <- api_get_snapped_points(data = sites, min_strahler = 4)
#'
#' # Non-default column names
#' result <- api_get_snapped_points(
#'   data           = sites,
#'   colname_lon    = "decimalLongitude",
#'   colname_lat    = "decimalLatitude",
#'   colname_site_id = "gbifID",
#'   min_strahler   = 4
#' )
#' }
#'
#' @importFrom httr2 request req_headers req_body_json req_perform
#' @importFrom httr2 resp_status resp_body_json resp_body_string
#' @importFrom data.table fread
#' @export

api_get_snapped_points <- function(
    data,
    colname_lon     = "longitude",
    colname_lat     = "latitude",
    colname_site_id = "site_id",
    min_strahler    = NULL,
    add_distance    = TRUE,
    force_async     = NULL,
    async_threshold = 500,
    poll_interval   = 10,
    max_wait        = 3600
) {

  # --- INPUT VALIDATION ---

  if (missing(data) || !is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (nrow(data) == 0) {
    stop("`data` is empty (0 rows).", call. = FALSE)
  }
  if (is.null(min_strahler)) {
    stop("`min_strahler` must be provided.", call. = FALSE)
  }

  missing_cols <- setdiff(c(colname_lon, colname_lat, colname_site_id), names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing columns in `data`: %s", paste(missing_cols, collapse = ", ")),
         call. = FALSE)
  }

  # Remove rows with NA coordinates
  na_rows <- is.na(data[[colname_lon]]) | is.na(data[[colname_lat]])
  if (any(na_rows)) {
    warning(sprintf("Removing %d row(s) with NA coordinates.", sum(na_rows)))
    data <- data[!na_rows, ]
    if (nrow(data) == 0) stop("No valid coordinates remaining after removing NAs.", call. = FALSE)
  }

  # --- DETERMINE ASYNC MODE ---

  use_async <- force_async

  if (is.null(use_async)) {
    use_async <- nrow(data) >= async_threshold
    message(sprintf("%d points: using %s mode.",
                    nrow(data), if (use_async) "async" else "sync"))
  } else {
    message(sprintf("Using %s mode (forced).", if (use_async) "async" else "sync"))
  }

  # --- BUILD GEOJSON ---

  features <- lapply(seq_len(nrow(data)), function(i) {
    list(
      type     = "Feature",
      geometry = list(
        type        = "Point",
        coordinates = c(as.numeric(data[[colname_lon]][i]),
                        as.numeric(data[[colname_lat]][i]))
      ),
      properties = setNames(
        list(as.character(data[[colname_site_id]][i])),
        colname_site_id
      )
    )
  })

  points_geojson <- list(type = "FeatureCollection", features = features)

  # --- BUILD REQUEST ---

  process_url <- "https://aqua.igb-berlin.de/pygeoapi/processes/get-snapped-points-strahler-plural/execution"

  body <- list(
    inputs = list(
      colname_site_id = colname_site_id,
      colname_lon     = colname_lon,
      colname_lat     = colname_lat,
      min_strahler    = as.integer(min_strahler),
      add_distance    = add_distance,
      result_format   = "csv",
      points_geojson  = points_geojson
    ),
    outputs = list(transmissionMode = "reference")
  )

  # --- SEND REQUEST ---

  req <- httr2::request(process_url) |>
    httr2::req_body_json(body, auto_unbox = TRUE)

  if (use_async) {
    req <- req |> httr2::req_headers("Prefer" = "respond-async")
  }

  resp <- req |> httr2::req_perform()

  if (!httr2::resp_status(resp) %in% c(200, 201)) {
    stop(sprintf("API request failed (HTTP %s): %s",
                 httr2::resp_status(resp),
                 httr2::resp_body_string(resp)),
         call. = FALSE)
  }

  result <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  # --- HANDLE ASYNC ---

  if (use_async && !is.null(result$jobID)) {
    message(sprintf("Async job submitted. Job ID: %s", result$jobID))

    job_result <- api_poll_job(
      jobID         = result$jobID,
      wait          = TRUE,
      poll_interval = poll_interval,
      max_wait      = max_wait
    )

    csv_url <- job_result$href

  } else {
    csv_url <- result$href
  }

  # --- DOWNLOAD RESULTS ---

  if (is.null(csv_url)) {
    stop("No download link (href) in API response.", call. = FALSE)
  }

  message("Downloading results...")
  df <- data.table::fread(csv_url)
  message(sprintf("Done: %d rows, %d columns.", nrow(df), ncol(df)))

  if (nrow(df) < nrow(data)) {
    warning(sprintf(
      "%d of %d points could not be snapped and are absent from the result.",
      nrow(data) - nrow(df), nrow(data)
    ))
  }

  return(as.data.frame(df))
}
