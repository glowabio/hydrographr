#' Get Upstream Stream Segments
#'
#' Sends a request to the GeoFRESH API to retrieve all stream segments
#' upstream of a given point or sub-catchment and returns them as an `sf` object.
#'
#' @description
#' This function returns all stream segments upstream of a specified location.
#' The location can be identified by one of: `subc_id`, or a pair of
#' coordinates (`lon` + `lat`). All inputs are sent as JSON.
#'
#' @family ocgapi
#' @param subc_id Integer. A sub-catchment ID from which to trace upstream.
#'   One of `subc_id` or (`lon` + `lat`) must be provided.
#' @param lon Numeric. Longitude of a point (used together with `lat`).
#' @param lat Numeric. Latitude of a point (used together with `lon`).
#' @param geometry_only Logical. If `TRUE`, returns only the geometry of the stream
#'   segments without associated attributes. Defaults to `FALSE`.
#' @param add_upstream_ids Logical. If `TRUE`, upstream sub-catchment IDs are
#'   added to the output. Defaults to `FALSE`.
#' @param comment Character. Optional comment for API logging.
#'
#' @return An `sf` object representing the upstream stream segments.
#'
#' @examples
#' \dontrun{
#' # Using lon/lat (Sarantaporos outlet)
#' upstream_sf <- api_get_upstream_stream_segments(
#'   lon = 20.538704,
#'   lat = 40.113735,
#'   geometry_only = FALSE,
#'   add_upstream_ids = TRUE
#' )
#'
#' # Using subc_id
#' upstream_sf <- api_get_upstream_stream_segments(
#'   subc_id = 506586041,
#'   geometry_only = FALSE
#' )
#'
#' library(leaflet)
#' leaflet(upstream_sf) |>
#'   addProviderTiles("CartoDB.Positron") |>
#'   addPolylines(color = "blue")
#' }
#'
#' @export
#'
#' @importFrom httr2 request req_headers req_body_json req_perform resp_status resp_status_desc resp_body_json
#' @importFrom jsonlite toJSON
#' @importFrom sf st_read st_geometry_type st_collection_extract
api_get_upstream_stream_segments <- function(subc_id = NULL,
                                             lon = NULL,
                                             lat = NULL,
                                             geometry_only = FALSE,
                                             add_upstream_ids = FALSE,
                                             comment = NULL) {

  # --- Input validation ------------------------------------------------------

  has_subc_id <- !is.null(subc_id)
  has_lonlat <- !is.null(lon) && !is.null(lat)

  if (!has_subc_id && !has_lonlat) {
    stop("Must provide one of: 'subc_id' or 'lon' + 'lat'.",
         call. = FALSE)
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

  if (!is.logical(geometry_only)) {
    stop("`geometry_only` must be logical (TRUE/FALSE).", call. = FALSE)
  }

  if (!is.logical(add_upstream_ids)) {
    stop("`add_upstream_ids` must be logical (TRUE/FALSE).", call. = FALSE)
  }

  # --- Construct body --------------------------------------------------------

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
    stop("Server returned an error: ", httr2::resp_status_desc(resp),
         call. = FALSE)
  }

  result <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  # --- Convert GeoJSON-like list to sf ---------------------------------------

  segm_geojson <- jsonlite::toJSON(result, auto_unbox = TRUE)
  segm_sf <- sf::st_read(segm_geojson, quiet = TRUE)

  # --- Handle GeometryCollection (returned when geometry_only = TRUE) --------

  geom_types <- as.character(unique(sf::st_geometry_type(segm_sf)))

  if ("GEOMETRYCOLLECTION" %in% geom_types) {
    segm_sf <- sf::st_collection_extract(segm_sf, type = "LINESTRING")
  }

  # --- Return sf object ------------------------------------------------------

  return(segm_sf)
}
