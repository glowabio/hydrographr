#' Snap Site Coordinates to the River Network (from Data Frame)
#'
#' Sends site coordinates directly (from an in-memory data frame)
#' to the GeoFRESH API to snap them to the stream of the subcatchment
#' where the point is located.
#' Optionally constrained by Strahler order.
#'
#' @family ocgapi
#' @param df A data.frame containing coordinates and a site ID column.
#' @param colname_lat Name of the latitude column in `df`. Default is `"latitude"`.
#' @param colname_lon Name of the longitude column in `df`. Default is `"longitude"`.
#' @param colname_site_id Name of the site ID column in `df`. Default is `"site_id"`.
#' @param strahler Optional numeric. Minimum Strahler order to snap to.
#' @param geometry_only Logical. If TRUE, return only geometry.
#' @param comment Optional comment string.
#' @return A list with:
#'   \item{data}{A `data.frame` with the snapped coordinates.}
#'   \item{href}{A character string with the URL of the result CSV.}
#'
#' @import httr jsonlite dplyr
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   site_id = c("A1", "A2"),
#'   latitude = c(54.69507, 54.29507),
#'   longitude = c(9.931555, 9.921555)
#' )
#'
#' result <- api_get_snapped_points_df(df)
#' head(result$data)
#' cat("Download link:", result$href)
#' }

api_get_snapped_points_df <- function(df,
                                      colname_lat = "latitude",
                                      colname_lon = "longitude",
                                      colname_site_id = "site_id",
                                      strahler = NULL,
                                      geometry_only = FALSE,
                                      comment = NULL) {
  # ---- Validation ----
  if (!all(c(colname_lat, colname_lon, colname_site_id) %in% names(df))) {
    stop("Input data frame must contain latitude, longitude, and site_id columns.")
  }

  # ---- Build GeoJSON FeatureCollection ----
  features <- apply(df, 1, function(row) {
    list(
      type = "Feature",
      geometry = list(
        type = "Point",
        coordinates = c(
          as.numeric(row[[colname_lon]]),
          as.numeric(row[[colname_lat]])
        )
      ),
      properties = as.list(row)
    )
  })

  geojson <- list(
    type = "FeatureCollection",
    features = features
  )

  # ---- Select API endpoint ----
  process_url <- if (is.null(strahler) || isTRUE(strahler == 1)) {
    "https://aqua.igb-berlin.de/pygeoapi-dev/processes/get-snapped-points-plural/execution"
  } else {
    "https://aqua.igb-berlin.de/pygeoapi-dev/processes/get-snapped-points-strahler/execution"
  }

  # ---- Build request body ----
  inputs <- list(
    colname_site_id = colname_site_id,
    points_geojson = geojson,
    geometry_only = tolower(as.character(geometry_only))
  )

  if (!is.null(strahler)) inputs$strahler <- strahler
  if (!is.null(comment)) inputs$comment <- comment

  body <- list(
    inputs = inputs,
    outputs = list(transmissionMode = "reference")
  )

  # ---- Send POST request ----
  response <- httr::POST(
    url = process_url,
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    httr::add_headers("Content-Type" = "application/json")
  )

  # ---- Error handling ----
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve results. HTTP Status: ", httr::status_code(response))
  }

  result_json <- httr::content(response, "parsed")
  csv_download_url <- result_json$href

  if (is.null(csv_download_url) || !grepl("\\.csv$", csv_download_url)) {
    stop("No valid CSV download link found in API response.")
  }

  # ---- Download and parse results ----
  df_out <- tryCatch({
    read.table(csv_download_url, sep = ",", header = TRUE)
  }, error = function(e) {
    stop("Error downloading CSV: ", e$message)
  })

  # ---- Return result ----
  list(data = df_out, href = csv_download_url)
}
