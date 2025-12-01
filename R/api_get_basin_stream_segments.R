#' Get Stream Segments for a Basin
#'
#' Sends a request to the GeoFRESH API to retrieve
#' stream segments belonging to a given basin and returns them as an `sf` object.
#'
#' @description
#' This function returns all stream segments within a specified basin as spatial
#' features, optionally filtering by a minimum Strahler order.
#'
#' @family ocgapi
#' @param basin_id Integer. The ID of the basin to retrieve stream segments for.
#' @param geometry_only Logical. If `TRUE`, returns only the geometry of the stream
#' segments without associated attributes. Defaults to `FALSE`.
#' @param strahler_min Integer (optional). The minimum Strahler stream order to include.
#' Streams below this order will be excluded. Defaults to `NULL` (no filtering).
#'
#' @return An `sf` object representing the stream segments of the given basin.
#'
#' @examples
#' \dontrun{
#' segm_sf <- api_get_basin_streamsegments(
#'   basin_id = 1288419,
#'   geometry_only = FALSE,
#'   strahler_min = 3
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
                                     geometry_only = FALSE,
                                     comment = NULL,
                                     strahler_min = NULL) {
  # --- Input validation ------------------------------------------------------

  if (missing(basin_id) || !is.numeric(basin_id)) {
    stop("`basin_id` must be a numeric value.", call. = FALSE)
  }

  if (!is.null(strahler_min) && (!is.numeric(strahler_min) || strahler_min < 1)) {
    stop("`strahler_min` must be a positive numeric value or NULL.", call. = FALSE)
  }


# --- Construct body ------------------------------------------------
# Define process url
process_url <- "https://aqua.igb-berlin.de/pygeoapi-dev/processes/get-basin-streamsegments/execution"

# Assemble inputs dynamically
inputs <- list(
  basin_id = as.integer(basin_id),
  # subc_id = as.integer(subc_id),
  geometry_only = geometry_only,
  comment = comment
)

# Add strahler_min only if provided
if (!is.null(strahler_min)) {
  inputs$strahler_min <- as.integer(strahler_min)
}

# Add subc_id only if provided
if (!is.null(subc_id)) {
  inputs$subc_id <- as.integer(subc_id)
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

# --- Return sf object ------------------------------------------------------
return(segm_sf)
}
#
# api_get_basin_stream_segments <- function(
#     basin_id = NULL,
#     subc_id = NULL,
#     geometry_only = FALSE,
#     strahler_min = NULL
# ) {
#
#   # --- Input validation ---
#   # if (!is.null(basin_id) && !is.null(subc_id)) {
#   #   stop("Provide either basin_id OR subc_id, not both.")
#   # }
#   # if (is.null(basin_id) && is.null(subc_id)) {
#   #   stop("You must provide basin_id or subc_id.")
#   # }
#
#   if (!is.null(strahler_min) && (!is.numeric(strahler_min) || strahler_min < 1)) {
#     stop("`strahler_min` must be a positive numeric value or NULL.", call. = FALSE)
#   }
#
#   # --- Build inputs list dynamically ---
#   if (!is.null(subc_id)) {
#     inputs <- list(
#       subc_id = subc_id,
#       geometry_only = geometry_only
#     )
#   } else {
#     inputs <- list(
#       basin_id = basin_id,
#       geometry_only = geometry_only
#     )
#   }
#
#   #  Add strahler_min only if provided
#     if (!is.null(strahler_min)) {
#       inputs$strahler_min <- as.integer(strahler_min)
#     }
#
#   # --- JSON body ---
#   # body <- jsonlite::toJSON(list(inputs = inputs), auto_unbox = TRUE)
#
#   # --- POST request ---
#   url <- "https://aqua.igb-berlin.de/pygeoapi-dev/processes/get-basin-streamsegments/execution"
#
#   # # Assemble inputs dynamically
#   # inputs <- list(
#   #   basin_id = as.integer(basin_id),
#   #   # subc_id = as.integer(subc_id),
#   #   geometry_only = geometry_only,
#   #   comment = comment
#   # )
#   #
#   # # Add strahler_min only if provided
#   # if (!is.null(strahler_min)) {
#   #   inputs$strahler_min <- as.integer(strahler_min)
#   # }
#   #
#   # # Add subc_id only if provided
#   # if (!is.null(subc_id)) {
#   #   inputs$subc_id <- as.integer(subc_id)
#   # }
#
#
#   body <- list(inputs = inputs)
#
#   # --- Send request ----------------------------------------------------------
#   resp <- httr2::request(process_url) |>
#     httr2::req_headers("Content-Type" = "application/json") |>
#     httr2::req_body_json(body) |>
#     httr2::req_perform()
#
#   # --- Handle response -------------------------------------------------------
#   if (httr2::resp_status(resp) >= 400) {
#     stop("Server returned an error: ", httr2::resp_status_desc(resp), call. = FALSE)
#   }
#
#   result <- httr2::resp_body_json(resp, simplifyVector = TRUE)
#
#   # --- Convert GeoJSON-like list to sf ---------------------------------------
#   segm_geojson <- jsonlite::toJSON(result, auto_unbox = TRUE)
#   segm_sf <- sf::st_read(segm_geojson, quiet = TRUE)
#
#   # --- Return sf object ------------------------------------------------------
#   return(segm_sf)
# }
