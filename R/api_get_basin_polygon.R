#' Get Basin Polygon(s)
#'
#' Retrieves basin polygon boundary/boundaries from the GeoFRESH API.
#' Can accept either basin_id(s) directly or a data.frame of point coordinates
#' (lon, lat, site_id). All requests are sent as JSON to the API.
#'
#' @description
#' This function returns basin polygon(s) as an `sf` object. Two input modes:
#' 1. One or more basin_ids (vector of integers)
#' 2. A data.frame with point coordinates (retrieves basin polygons via lon/lat)
#'
#' @family ocgapi
#' @param basin_id Integer or integer vector. The ID(s) of the basin(s) to retrieve.
#'   If NULL, must provide `points_df`.
#' @param points_df A data.frame with columns for longitude, latitude, and site_id.
#'   Used if `basin_id` is NULL. Each row is sent as a lon/lat point to the API.
#' @param colname_lat Character. Name of latitude column in `points_df`. Default: "latitude".
#' @param colname_lon Character. Name of longitude column in `points_df`. Default: "longitude".
#' @param colname_site_id Character. Name of site ID column in `points_df`. Default: "site_id".
#' @param geometry_only Logical. If `TRUE`, returns only geometry without
#'   attributes. Defaults to `FALSE`.
#' @param comment Character. Optional comment for API logging.
#'
#' @return An `sf` object representing the basin polygon(s). If multiple basins
#'   are retrieved, returns all unique basins.
#'
#' @examples
#' \dontrun{
#' # Method 1: Using a single basin_id
#' basin_sf <- api_get_basin_polygon(
#'   basin_id = 1288419,
#'   geometry_only = FALSE
#' )
#'
#' # Method 1b: Using a vector of basin_ids
#' basins_sf <- api_get_basin_polygon(
#'   basin_id = c(1288419, 1288420, 1288421)
#' )
#'
#' # Method 2: Using a data.frame of points
#' pts <- data.frame(
#'   site_id = c("site_a", "site_b"),
#'   longitude = c(8.6, 9.1),
#'   latitude = c(53.5, 52.3)
#' )
#' basins_sf <- api_get_basin_polygon(points_df = pts)
#'
#' # Visualize
#' library(leaflet)
#' leaflet(basins_sf) |>
#'   addProviderTiles("CartoDB.Positron") |>
#'   addPolygons(color = "blue", fillOpacity = 0.3)
#' }
#'
#' @export
#'
#' @importFrom httr2 request req_headers req_body_json req_perform resp_status resp_status_desc resp_body_json
#' @importFrom jsonlite toJSON
#' @importFrom sf st_read
#' @importFrom dplyr bind_rows
api_get_basin_polygon <- function(basin_id = NULL,
                                  points_df = NULL,
                                  colname_lat = "latitude",
                                  colname_lon = "longitude",
                                  colname_site_id = "site_id",
                                  geometry_only = FALSE,
                                  comment = NULL) {

  # --- Input validation ------------------------------------------------------

  has_basin_id <- !is.null(basin_id)
  has_points_df <- !is.null(points_df)

  if (!has_basin_id && !has_points_df) {
    stop("Must provide one of: 'basin_id' (integer or vector) or 'points_df' (data.frame).",
         call. = FALSE)
  }

  if (has_basin_id && has_points_df) {
    message("Both 'basin_id' and 'points_df' provided. Using 'basin_id'.")
    has_points_df <- FALSE
  }

  # Validate basin_id
  if (has_basin_id) {
    if (!is.numeric(basin_id)) {
      stop("`basin_id` must be numeric (integer or integer vector).", call. = FALSE)
    }
    basin_id <- as.integer(basin_id)
  }

  # Validate points_df
  if (has_points_df) {
    if (!is.data.frame(points_df)) {
      stop("`points_df` must be a data.frame.", call. = FALSE)
    }
    required_cols <- c(colname_lat, colname_lon, colname_site_id)
    missing_cols <- setdiff(required_cols, names(points_df))
    if (length(missing_cols) > 0) {
      stop(sprintf("`points_df` is missing columns: %s", paste(missing_cols, collapse = ", ")),
           call. = FALSE)
    }
    lats <- points_df[[colname_lat]]
    lons <- points_df[[colname_lon]]
    if (!is.numeric(lats) || !is.numeric(lons)) {
      stop("Latitude and longitude columns must be numeric.", call. = FALSE)
    }
    if (any(lats < -90 | lats > 90, na.rm = TRUE)) {
      stop("Latitude values must be between -90 and 90.", call. = FALSE)
    }
    if (any(lons < -180 | lons > 180, na.rm = TRUE)) {
      stop("Longitude values must be between -180 and 180.", call. = FALSE)
    }
  }

  # --- Helper: fetch a single basin polygon via JSON -------------------------

  process_url <- "https://aqua.igb-berlin.de/pygeoapi-dev/processes/get-basin-polygon/execution"

  fetch_polygon <- function(inputs, label = "") {
    body <- list(inputs = inputs)

    resp <- httr2::request(process_url) |>
      httr2::req_headers("Content-Type" = "application/json") |>
      httr2::req_body_json(body) |>
      httr2::req_perform()

    if (httr2::resp_status(resp) >= 400) {
      warning(sprintf("Failed to retrieve basin %s: %s",
                      label, httr2::resp_status_desc(resp)))
      return(NULL)
    }

    result <- httr2::resp_body_json(resp, simplifyVector = TRUE)
    basin_geojson <- jsonlite::toJSON(result, auto_unbox = TRUE)
    sf::st_read(basin_geojson, quiet = TRUE)
  }

  # --- Build request list ----------------------------------------------------

  requests <- list()

  if (has_basin_id) {
    # One request per basin_id
    for (bid in basin_id) {
      inputs <- list(
        basin_id = as.integer(bid),
        geometry_only = geometry_only
      )
      if (!is.null(comment)) inputs$comment <- comment
      requests[[length(requests) + 1]] <- list(inputs = inputs, label = as.character(bid))
    }

  } else {
    # One request per point (lon/lat), sent as JSON coordinates
    for (i in seq_len(nrow(points_df))) {
      lon_i <- points_df[[colname_lon]][i]
      lat_i <- points_df[[colname_lat]][i]
      sid_i <- as.character(points_df[[colname_site_id]][i])

      inputs <- list(
        lon = lon_i,
        lat = lat_i,
        geometry_only = geometry_only
      )
      if (!is.null(comment)) inputs$comment <- comment
      requests[[length(requests) + 1]] <- list(inputs = inputs, label = sid_i)
    }
  }

  # --- Execute requests ------------------------------------------------------

  message(sprintf("Retrieving %d basin polygon(s)...", length(requests)))

  polygons_list <- list()

  for (i in seq_along(requests)) {
    req <- requests[[i]]
    message(sprintf("  [%d/%d] Fetching basin polygon for: %s",
                    i, length(requests), req$label))

    tryCatch({
      sf_obj <- fetch_polygon(req$inputs, label = req$label)
      if (!is.null(sf_obj)) {
        polygons_list[[length(polygons_list) + 1]] <- sf_obj
      }
    }, error = function(e) {
      warning(sprintf("Error retrieving basin for %s: %s", req$label, e$message))
    })
  }

  # --- Combine all polygons --------------------------------------------------

  if (length(polygons_list) == 0) {
    stop("No basin polygons were successfully retrieved.", call. = FALSE)
  }

  combined_sf <- dplyr::bind_rows(polygons_list)

  message(sprintf("\u2713 Retrieved %d basin polygon(s)", nrow(combined_sf)))

  return(combined_sf)
}
