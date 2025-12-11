#' Snap Many Points to River Network Using Strahler Threshold (CSV Input)
#'
#' Sends a CSV file URL with multiple points to the GeoFRESH API process
#' `get-snapped-points-strahler-plural`, which snaps each point to the nearest
#' stream segment above a given minimum Strahler order.
#'
#' @family ocgapi
#' @param csv_url URL to a CSV file containing the point data.
#' @param colname_lon Name of the longitude column inside the CSV. Default: "longitude".
#' @param colname_lat Name of the latitude column inside the CSV. Default: "latitude".
#' @param colname_site_id Name of the site ID column inside the CSV. Default: "site_id".
#' @param min_strahler Minimum Strahler order to snap to (e.g., 5).
#'
#' @return A list with:
#'   \describe{
#'     \item{data}{A data.frame with the snapped output}
#'     \item{href}{The download link returned by the API}
#'   }
#'
#' @import httr
#' @import jsonlite
#' @import readr
#' @export
#' @author Afroditi Grigoropoulou, Merret Buurman
#'
#' @examples
#' \dontrun{
#' result <- api_get_snapped_points_strahler_plural(
#'   csv_url = "https://aqua.igb-berlin.de/referencedata/aqua90m/spdata_barbus.csv",
#'   min_strahler = 5
#' )
#'
#' head(result$data)
#' cat("Download link:", result$href)
#' }
api_get_snapped_points_strahler_plural <- function(
    csv_url,
    colname_lon = "longitude",
    colname_lat = "latitude",
    colname_site_id = "site_id",
    min_strahler = NULL
) {

  if (is.null(min_strahler)) {
    stop("min_strahler must be provided.")
  }

  process_url <-"https://aqua.igb-berlin.de/pygeoapi-dev/processes/get-snapped-points-strahler-plural/execution"



  # Build POST body
  body <- list(
    inputs = list(
      csv_url = csv_url,
      colname_lon = colname_lon,
      colname_lat = colname_lat,
      colname_site_id = colname_site_id,
      min_strahler = min_strahler
    ),
    outputs = list(
      transmissionMode = "reference"
    )
  )

  # Send request
  response <- httr::POST(
    url = process_url,
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    httr::add_headers("Content-Type" = "application/json")
  )

  status <- httr::status_code(response)

  if (status != 200) {
    stop(sprintf("API request failed (HTTP %s): %s",
                 status,
                 httr::content(response, as = "text")))
  }

  result <- httr::content(response, as = "parsed", simplifyVector = TRUE)

  if (is.null(result$href)) {
    stop("API response does not include a download link (href).")
  }

  # Download CSV
  df <- data.table::fread(result$href)

  return(list(
    data = df,
    href = result$href
  ))
}
