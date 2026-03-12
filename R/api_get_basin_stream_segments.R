#' Get Stream Segments for a Basin
#'
#' Sends a request to the GeoFRESH API to retrieve
#' stream segments belonging to a given basin and returns them as an `sf` object.
#'
#' @description
#' This function returns all stream segments within a specified basin as spatial
#' features, optionally filtering by a minimum Strahler order.
#' The basin can be identified by one of: `basin_id`, `subc_id`, or
#' a pair of coordinates (`lon` + `lat`). All inputs are sent as JSON.
#'
#' @family ocgapi
#' @param basin_id Integer. The ID of the basin. One of `basin_id`, `subc_id`,
#'   or (`lon` + `lat`) must be provided.
#' @param subc_id Integer. A sub-catchment ID from which the basin is inferred.
#' @param lon Numeric. Longitude of a point (used together with `lat`).
#' @param lat Numeric. Latitude of a point (used together with `lon`).
#' @param geometry_only Logical. If `TRUE`, returns only the geometry of the stream
#'   segments without associated attributes. Defaults to `FALSE`.
#' @param strahler_min Integer (optional). The minimum Strahler stream order to include.
#'   Streams below this order will be excluded. Defaults to `NULL` (no filtering).
#' @param add_segment_ids Logical (optional). If `TRUE`, segment IDs are added to
#'   the output. Defaults to `NULL` (omitted from request).
#' @param comment Character. Optional comment for API logging.
#'
#' @return An `sf` object representing the stream segments of the given basin.
#'
#' @examples
#' \dontrun{
#' # Using basin_id
#' segm_sf <- api_get_basin_streamsegments(
#'   basin_id = 1288419,
#'   geometry_only = FALSE,
#'   strahler_min = 4,
#'   add_segment_ids = TRUE
#' )
#'
#' # Using subc_id
#' segm_sf <- api_get_basin_streamsegments(
#'   subc_id = 506586041,
#'   geometry_only = TRUE
#' )
#'
#' # Using lon/lat
#' segm_sf <- api_get_basin_streamsegments(
#'   lon = 8.278198242187502,
#'   lat = 53.54910661890981,
#'   geometry_only = TRUE
#' )
#'
#' library(leaflet)
#' leaflet(segm_sf) |>
#'   addProviderTiles("CartoDB.Positron") |>
#'   addPolylines(color = "blue")
#' }
#'
#' @export
#'
#' @importFrom httr2 request req_headers req_body_json req_perform resp_status resp_status_desc resp_body_json
#' @importFrom jsonlite toJSON
#' @importFrom sf st_read
api_get_basin_streamsegments <- function(basin_id = NULL,
                                         subc_id = NULL,
                                         lon = NULL,
                                         lat = NULL,
                                         geometry_only = FALSE,
                                         strahler_min = NULL,
                                         add_segment_ids = NULL,
                                         comment = NULL) {

  # --- Input validation ------------------------------------------------------

  has_basin_id <- !is.null(basin_id)
  has_subc_id <- !is.null(subc_id)
  has_lonlat <- !is.null(lon) && !is.null(lat)

  if (!has_basin_id && !has_subc_id && !has_lonlat) {
    stop("Must provide one of: 'basin_id', 'subc_id', or 'lon' + 'lat'.",
         call. = FALSE)
  }

  if (has_basin_id && !is.numeric(basin_id)) {
    stop("`basin_id` must be a numeric value.", call. = FALSE)
  }

  if (has_subc_id && !is.numeric(subc_id)) {
    stop("`subc_id` must be a numeric value.", call. = FALSE)
  }

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

  if (!is.null(strahler_min) && (!is.numeric(strahler_min) || strahler_min < 1)) {
    stop("`strahler_min` must be a positive numeric value or NULL.", call. = FALSE)
  }

  if (!is.null(add_segment_ids) && !is.logical(add_segment_ids)) {
    stop("`add_segment_ids` must be logical (TRUE/FALSE) or NULL.", call. = FALSE)
  }

  # --- Construct body --------------------------------------------------------

  process_url <- "https://aqua.igb-berlin.de/pygeoapi-dev/processes/get-basin-streamsegments/execution"

  # Assemble inputs: location identifier (priority: basin_id > subc_id > lon/lat)
  inputs <- list(geometry_only = geometry_only)

  if (has_basin_id) {
    inputs$basin_id <- as.integer(basin_id)
  } else if (has_subc_id) {
    inputs$subc_id <- as.integer(subc_id)
  } else {
    inputs$lon <- lon
    inputs$lat <- lat
  }

  # Optional parameters
  if (!is.null(strahler_min)) {
    inputs$strahler_min <- as.integer(strahler_min)
  }

  if (!is.null(add_segment_ids)) {
    inputs$add_segment_ids <- add_segment_ids
  }

  if (!is.null(comment)) {
    inputs$comment <- comment
  }

  body <- list(inputs = inputs)

  # --- Send request ----------------------------------------------------------

  resp <- httr2::request(process_url) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(body) |>
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
