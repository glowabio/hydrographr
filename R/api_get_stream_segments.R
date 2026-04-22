#' Get Stream Segments of a Basin or Upstream of a Point
#'
#' Sends a request to the GeoFRESH API to retrieve stream segments belonging
#' to a given basin, or all upstream segments of a specific location.
#'
#' @description
#' This function returns stream segments as an `sf` object with optional filtering
#' by Strahler order. Two modes of operation:
#'
#' **Basin mode** (default, `upstream = FALSE`):
#' Returns all stream segments *within* a specified basin. The basin can be
#' identified by one of: `basin_id`, `subc_id`, or coordinates (`lon` + `lat`).
#'
#' **Upstream mode** (`upstream = TRUE`):
#' Returns all stream segments *upstream of* a specified location. Only accepts
#' `subc_id` or coordinates (`lon` + `lat`); `basin_id` is not compatible with
#' upstream mode. All requests are sent as JSON to the API.
#'
#' @family ocgapi
#'
#' @param basin_id Integer. The ID of the basin. Only valid in basin mode
#'   (`upstream = FALSE`). One of `basin_id`, `subc_id`, or (`lon` + `lat`)
#'   must be provided. Optional. Default: NULL.
#' @param subc_id Integer. A sub-catchment ID from which the basin is inferred
#'   (basin mode) or from which to trace upstream (upstream mode). Optional. Default: NULL.
#' @param lon Numeric. Longitude of a point (used together with `lat`). Works
#'   in both basin mode and upstream mode. Optional. Default: NULL.
#' @param lat Numeric. Latitude of a point (used together with `lon`). Latitude
#'   must be between -90 and 90. Optional. Default: NULL.
#' @param upstream Logical. If `FALSE` (default), returns segments within the basin.
#'   If `TRUE`, returns segments upstream of the specified location. Requires `subc_id`
#'   or (`lon` + `lat`); `basin_id` is not supported in upstream mode. Default: FALSE.
#' @param geometry_only Logical. If `TRUE`, returns only the geometry of the stream
#'   segments without associated attributes. Defaults to `FALSE`.
#' @param strahler_min Integer (optional). The minimum Strahler stream order to include.
#'   Streams below this order will be excluded. Defaults to `NULL` (no filtering).
#'   Note: parameter name is `strahler_min` in basin mode, `min_strahler` in upstream mode
#'   (the function handles this internally).
#' @param add_segment_ids Logical (optional). If `TRUE`, segment IDs are added to
#'   the output. Only used in basin mode. Defaults to `NULL` (omitted from request).
#' @param add_upstream_ids Logical (optional). If `TRUE`, upstream sub-catchment IDs
#'   are added to the output. Only used in upstream mode. Defaults to `FALSE`.
#' @param comment Character. Optional comment for API logging.
#'
#' @return An `sf` object representing the stream segments. Geometry type: LINESTRING.
#'
#' @examples
#' \dontrun{
#' # === BASIN MODE (upstream = FALSE) ===
#'
#' # Using basin_id
#' segm_sf <- api_get_stream_segments(
#'   basin_id = 1288419,
#'   geometry_only = FALSE,
#'   strahler_min = 4,
#'   add_segment_ids = TRUE
#' )
#'
#' # Using subc_id
#' segm_sf <- api_get_stream_segments(
#'   subc_id = 506586041,
#'   geometry_only = TRUE
#' )
#'
#' # Using lon/lat
#' segm_sf <- api_get_stream_segments(
#'   lon = 8.278198242187502,
#'   lat = 53.54910661890981,
#'   geometry_only = TRUE
#' )
#'
#' # === UPSTREAM MODE (upstream = TRUE) ===
#'
#' # Using subc_id to get upstream segments
#' upstream_sf <- api_get_stream_segments(
#'   subc_id = 506586041,
#'   upstream = TRUE,
#'   geometry_only = FALSE,
#'   strahler_min = 1
#' )
#'
#' # Using lon/lat to get upstream segments (Sarantaporos outlet)
#' upstream_sf <- api_get_stream_segments(
#'   lon = 20.538704,
#'   lat = 40.113735,
#'   upstream = TRUE,
#'   geometry_only = FALSE
#' )
#'
#' # Visualize with leaflet
#' library(leaflet)
#' leaflet(upstream_sf) %>%
#'   addProviderTiles("CartoDB.Positron") %>%
#'   addPolylines(color = "blue")
#' }
#'
#' @export
#'
#' @importFrom httr2 request req_headers req_body_json req_perform resp_status resp_status_desc resp_body_json
#' @importFrom jsonlite toJSON
#' @importFrom sf st_read st_geometry_type st_collection_extract
api_get_stream_segments <- function(basin_id = NULL,
                                    subc_id = NULL,
                                    lon = NULL,
                                    lat = NULL,
                                    upstream = FALSE,
                                    geometry_only = FALSE,
                                    strahler_min = NULL,
                                    add_segment_ids = NULL,
                                    add_upstream_ids = FALSE,
                                    comment = NULL) {

  # --- Input validation ------------------------------------------------------

  has_basin_id <- !is.null(basin_id)
  has_subc_id <- !is.null(subc_id)
  has_lonlat <- !is.null(lon) && !is.null(lat)

  # Validate upstream parameter
  if (!is.logical(upstream)) {
    stop("`upstream` must be logical (TRUE/FALSE).", call. = FALSE)
  }

  # --- UPSTREAM MODE VALIDATION ----------------------------------------------

  if (upstream) {
    # In upstream mode, basin_id is not allowed
    if (has_basin_id) {
      stop("`basin_id` is not compatible with `upstream = TRUE`. Use `subc_id` or `lon` + `lat` instead.",
           call. = FALSE)
    }

    # Must provide either subc_id or lon/lat
    if (!has_subc_id && !has_lonlat) {
      stop("In upstream mode, must provide one of: 'subc_id' or 'lon' + 'lat'.",
           call. = FALSE)
    }

    # Validate subc_id
    if (has_subc_id && !is.numeric(subc_id)) {
      stop("`subc_id` must be a numeric value.", call. = FALSE)
    }

    # Validate lon/lat
    if (has_lonlat) {
      if (!is.numeric(lon) || !is.numeric(lat)) {
        stop("`lon` and `lat` must be numeric values.", call. = FALSE)
      }
      if (lat < -90 || lat > 90) {
        stop("`lat` must be between -90 and 90.", call. = FALSE)
      }
      if (lon < -180 || lon > 180) {
        stop("`lon` must be between -180 and 180.", call. = FALSE)
      }
    }

    # Validate geometry_only
    if (!is.logical(geometry_only)) {
      stop("`geometry_only` must be logical (TRUE/FALSE).", call. = FALSE)
    }

    # Validate add_upstream_ids
    if (!is.logical(add_upstream_ids)) {
      stop("`add_upstream_ids` must be logical (TRUE/FALSE).", call. = FALSE)
    }

    # Validate strahler_min
    if (!is.null(strahler_min) && (!is.numeric(strahler_min) || strahler_min < 1)) {
      stop("`strahler_min` must be a positive numeric value or NULL.", call. = FALSE)
    }

    # --- UPSTREAM MODE: Construct body -----

    process_url <- "https://aqua.igb-berlin.de/pygeoapi/processes/get-upstream-streamsegments/execution"

    inputs <- list(
      geometry_only = geometry_only,
      add_upstream_ids = add_upstream_ids
    )

    if (has_subc_id) {
      inputs$subc_id <- as.integer(subc_id)
    } else {
      inputs$lon <- lon
      inputs$lat <- lat
    }

    if (!is.null(strahler_min)) {
      inputs$min_strahler <- as.integer(strahler_min)  # Note: API uses "min_strahler"
    }

    if (!is.null(comment)) {
      inputs$comment <- comment
    }

    body <- list(inputs = inputs)

  } else {
    # --- BASIN MODE VALIDATION ------------------------------------------------

    # Must provide exactly one input method
    if (!has_basin_id && !has_subc_id && !has_lonlat) {
      stop("Must provide one of: 'basin_id', 'subc_id', or 'lon' + 'lat'.",
           call. = FALSE)
    }

    # Validate basin_id
    if (has_basin_id && !is.numeric(basin_id)) {
      stop("`basin_id` must be a numeric value.", call. = FALSE)
    }

    # Validate subc_id
    if (has_subc_id && !is.numeric(subc_id)) {
      stop("`subc_id` must be a numeric value.", call. = FALSE)
    }

    # Validate lon/lat
    if (has_lonlat) {
      if (!is.numeric(lon) || !is.numeric(lat)) {
        stop("`lon` and `lat` must be numeric values.", call. = FALSE)
      }
      if (lat < -90 || lat > 90) {
        stop("`lat` must be between -90 and 90.", call. = FALSE)
      }
      if (lon < -180 || lon > 180) {
        stop("`lon` must be between -180 and 180.", call. = FALSE)
      }
    }

    # Validate strahler_min
    if (!is.null(strahler_min) && (!is.numeric(strahler_min) || strahler_min < 1)) {
      stop("`strahler_min` must be a positive numeric value or NULL.", call. = FALSE)
    }

    # Validate add_segment_ids
    if (!is.null(add_segment_ids) && !is.logical(add_segment_ids)) {
      stop("`add_segment_ids` must be logical (TRUE/FALSE) or NULL.", call. = FALSE)
    }

    # --- BASIN MODE: Construct body --------

    process_url <- "https://aqua.igb-berlin.de/pygeoapi/processes/get-basin-streamsegments/execution"

    inputs <- list(geometry_only = geometry_only)

    if (has_basin_id) {
      inputs$basin_id <- as.integer(basin_id)
    } else if (has_subc_id) {
      inputs$subc_id <- as.integer(subc_id)
    } else {
      inputs$lon <- lon
      inputs$lat <- lat
    }

    if (!is.null(strahler_min)) {
      inputs$strahler_min <- as.integer(strahler_min)  # Note: API uses "strahler_min"
    }

    if (!is.null(add_segment_ids)) {
      inputs$add_segment_ids <- add_segment_ids
    }

    if (!is.null(comment)) {
      inputs$comment <- comment
    }

    body <- list(inputs = inputs)
  }

  # --- Send request ----------------------------------------------------------

  resp <- httr2::request(process_url) %>%
    httr2::req_headers("Content-Type" = "application/json") %>%
    httr2::req_body_json(body) %>%
    httr2::req_perform()

  # --- Handle response -------------------------------------------------------

  if (httr2::resp_status(resp) >= 400) {
    stop("Server returned an error: ", httr2::resp_status_desc(resp), call. = FALSE)
  }

  result <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  # --- Convert GeoJSON-like list to sf ---------------------------------------

  segm_geojson <- jsonlite::toJSON(result, auto_unbox = TRUE)
  segm_sf <- sf::st_read(segm_geojson, quiet = TRUE)

  # --- Handle GeometryCollection (returned when geometry_only = TRUE) --------
  # Extract individual geometries so leaflet and other tools can render them.

  geom_types <- as.character(unique(sf::st_geometry_type(segm_sf)))

  if ("GEOMETRYCOLLECTION" %in% geom_types) {
    segm_sf <- sf::st_collection_extract(segm_sf, type = "LINESTRING")
  }

  # --- Return sf object ------------------------------------------------------

  return(segm_sf)
}

