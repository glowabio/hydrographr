#' Snap Site Coordinates to the River Network
#'
#' Sends a request to the GeoFRESH API to snap a set of site coordinates to the nearest flowline
#' in the river network. Returns both the snapped and original coordinates, along with a link
#' to download the results or to use it in another function of the family ocgapi.
#'
#' @family ocgapi
#' @param csv_url A URL to a CSV file hosted online. The CSV must contain latitude, longitude, and site ID columns.
#' @param colname_lat Name of the latitude column in the CSV. Default is "latitude".
#' @param colname_lon Name of the longitude column in the CSV. Default is "longitude".
#' @param colname_site_id Name of the site ID column in the CSV. Default is "site_id".
#' @param process_url URL of the pygeoapi process called by the function.
#' #'   Must be specified explicitly, e.g.,
#'   `"http://localhost:5000/processes/get-snapped-points/execution"`.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{data}{A `data.frame` containing the snapped coordinates and additional information.}
#'   \item{href}{A `character` string with the URL of the output CSV.}
#' }
#'
#' @import httr
#' @import jsonlite
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' # Example usage
#' url <- "https://example.com/sites.csv"
#' snapped_df <- api_get_snapped_points(csv_url = url,
#'  colname_lat = "latitude",
#'  colname_lon = "longitude",
#'  colname_site_id = "site_id",
#' process_url = "http://localhost:5000/processes/get-snapped-points/execution")
#'
#'
#' #' head(result$data)
#' cat("Download link:", result$href)
#'
#' }
#'
#'
#'
#'
#' @export
api_get_snapped_points <- function(csv_url,
                                   colname_lat = "latitude",
                                   colname_lon = "longitude",
                                   colname_site_id = "site_id",
                                   process_url = NULL) {

  if (is.null(csv_url)) {
    stop("A 'csv_url' must be provided.")
  }

  # Ensure that the process url is provided
  if (is.null(process_url)) {
    stop("Please provide the URL of the pygeoapi process")
  }

  message("Using CSV-based API...")

  body <- list(
    inputs = list(
      csv_url = csv_url,
      colname_lat = colname_lat,
      colname_lon = colname_lon,
      colname_site_id = colname_site_id
    ),
    outputs = list(transmissionMode = "reference")
  )

  response <- httr::POST(
    url = process_url,
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    httr::add_headers("Content-Type" = "application/json")
  )

  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve results. HTTP Status: ", httr::status_code(response))
  }

  result_json <- httr::content(response, "parsed")

  csv_download_url <- result_json$href
  if (is.null(csv_download_url) || !grepl("\\.csv$", csv_download_url)) {
    stop("No valid CSV download link found in the results.")
  }

  df <- tryCatch({
    read.table(csv_download_url, sep = ",", header = TRUE)
  }, error = function(e) {
    stop("Error downloading CSV: ", e$message)
  })

  # Return both the data frame and the download link as a list
  return(list(
    data = df,
    href = csv_download_url
  ))
}

