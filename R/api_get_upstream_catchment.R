#' Get Upstream Catchment Polygon (Dissolved)
#'
#' Sends a request to the GeoFRESH API to retrieve the dissolved upstream
#' catchment polygon for a given point or sub-catchment and returns it
#' as an `sf` object.
#'
#' @description
#' This function returns a single dissolved polygon representing the merged
#' area of all sub-catchments upstream of a specified location. The location
#' can be identified by one of: `subc_id`, a pair of coordinates
#' (`lon` + `lat`), or a GeoJSON `point`. All inputs are sent as JSON.
#'
#' @family ocgapi
#' @param subc_id Integer. A sub-catchment ID from which to trace upstream.
#'   One of `subc_id`, (`lon` + `lat`), or `point` must be provided.
#' @param lon Numeric. Longitude of a point (used together with `lat`).
#' @param lat Numeric. Latitude of a point (used together with `lon`).
#' @param point List. A GeoJSON Point geometry or Feature. Alternative to
#'   providing `lon` + `lat`. Optional.
#' @param add_upstream_ids Logical. If `TRUE`, upstream sub-catchment IDs are
#'   included in the output. Defaults to `FALSE`.
#' @param comment Character. Optional comment for API logging.
#'
#' @return An `sf` object representing the dissolved upstream catchment polygon.
#'
#' @examples
#' \dontrun{
#' # Using lon/lat (Sarantaporos outlet)
#' catchment_sf <- api_get_upstream_catchment(
#'   lon = 20.538704,
#'   lat = 40.113735
#'   add_upstream_ids = TRUE
#' )
#'
#' # Using subc_id
#' catchment_sf <- api_get_upstream_catchment(
#'   subc_id = 506586041
#' )
#'
#' # Using a GeoJSON point
#' catchment_sf <- api_get_upstream_catchment(
#'   point = list(
#'     type = "Feature",
#'     geometry = list(
#'       type = "Point",
#'       coordinates = c(20.538704, 40.113735)
#'     )
#'   )
#' )
#'
#' library(leaflet)
#' leaflet(catchment_sf) |>
#'   addProviderTiles("CartoDB.Positron") |>
#'   addPolygons(color = "blue")
#' }
#'
#' @export
#'
#' @importFrom httr2 request req_headers req_body_json req_perform resp_status resp_status_desc resp_body_json
#' @importFrom jsonlite toJSON
#' @importFrom sf st_read st_geometry_type st_collection_extract
api_get_upstream_catchment <- function(subc_id = NULL,
                                       lon = NULL,
                                       lat = NULL,
                                       point = NULL,
                                       add_upstream_ids = FALSE,
                                       comment = NULL) {

  # --- Input validation ------------------------------------------------------

  has_subc_id <- !is.null(subc_id)
  has_lonlat <- !is.null(lon) && !is.null(lat)
  has_point <- !is.null(point)

  if (!has_subc_id && !has_lonlat && !has_point) {
    stop("Must provide one of: 'subc_id', 'lon' + 'lat', or 'point'.",
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

  if (has_point && !is.list(point)) {
    stop("`point` must be a list (GeoJSON Point geometry or Feature).",
         call. = FALSE)
  }

  if (!is.logical(add_upstream_ids)) {
    stop("`add_upstream_ids` must be logical (TRUE/FALSE).", call. = FALSE)
  }

  # --- Construct body --------------------------------------------------------

  process_url <- "https://aqua.igb-berlin.de/pygeoapi/processes/get-upstream-dissolved-cont/execution"

  inputs <- list(
    geometry_only = TRUE,
    add_upstream_ids = add_upstream_ids
  )

  # Location identifier (priority: point > subc_id > lon/lat)
  if (has_point) {
    inputs$point <- point
  } else if (has_subc_id) {
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

  catch_geojson <- jsonlite::toJSON(result, auto_unbox = TRUE)
  catch_sf <- sf::st_read(catch_geojson, quiet = TRUE)

  # --- Handle GeometryCollection (if returned) -------------------------------

  geom_types <- as.character(unique(sf::st_geometry_type(catch_sf)))

  if ("GEOMETRYCOLLECTION" %in% geom_types) {
    catch_sf <- sf::st_collection_extract(catch_sf, type = "POLYGON")
  }

  # --- Return sf object ------------------------------------------------------

  return(catch_sf)
}
