#' Retrieve Local IDs for Sampling Sites
#'
#' Queries the GeoFRESH API to obtain regional unit, basin,
#' and subcatchment IDs for a set of sampling locations.
#' Handles both single-point and multi-point inputs.
#' Multi-point inputs should be provided via a CSV URL.
#'
#' @family ocgapi
#' @param df A data frame with one site location.
#' Must include latitude and longitude columns.
#' Required if `csv_url` is not provided.
#' @param csv_url URL to a CSV file hosted online
#' (required for more than one point).
#' @param colname_lat Name of the latitude column in `df` or CSV file.
#' Default is "latitude".
#' @param colname_lon Name of the longitude column in `df` or CSV file.
#' Default is "longitude".
#' @param colname_site_id Name of the site ID column in `df` or CSV file.
#' Default is "site_id".
#' @param comment Optional comment string to include in the API request,
#' useful for tracking jobs.
#' @param process_url URL of the pygeoapi process called by the function.
#'   Must be specified explicitly, e.g.,
#'   `"http://localhost:5000/processes/get-local-ids/execution"`.
#' @return A data frame with subcatchment information
#' (`reg_id`, `basin_id`, `subc_id`) joined to the input site data.
#'
#' @import httr
#' @import jsonlite
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' # Example for single-point request
#' site_df <- data.frame(
#'   site_id = "s1",
#'   latitude = 52.45,
#'   longitude = 13.31
#' )
#' get_local_ids(df = site_df)
#'
#' # Example for multi-point request using CSV URL
#' get_local_ids(csv_url = "https://example.com/sites.csv")
#' }
#'
#' @export

api_get_local_ids <- function(df = NULL, csv_url = NULL,
                               colname_lat = "latitude",
                               colname_lon = "longitude",
                               colname_site_id = "site_id",
                               comment = NULL,
                               process_url = NULL) {

  # Ensure at least one input is provided
  if (is.null(df) && is.null(csv_url)) {
    stop("Either 'df' or 'csv_url' must be provided.")
  }

  # Ensure that the process url is provided
  if (is.null(process_url)) {
    stop("Please provide the URL of the pygeoapi process")
  }

  # Case 1: Single Point API (for one point only)
  if (!is.null(df) && nrow(df) == 1) {
    message("Using single-point API...")

    # Extract single point coordinates
    lon <- df[[colname_lon]][1]
    lat <- df[[colname_lat]][1]

    # Construct request body
    body <- list(
      inputs = list(
        lon = lon,
        lat = lat
      )
    )

    if (!is.null(comment)) {
      body$inputs$comment <- comment
    }

    # Send POST request
    response <- httr::POST(
      url = process_url,
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      encode = "json",
      httr::add_headers("Content-Type" = "application/json")
    )

    if (status_code(response) != 200) {
      stop("Failed to retrieve results. HTTP Status: ", status_code(response))
    }

    # Parse JSON response
    result_json <- content(response, "parsed")

    # Convert JSON result to a dataframe and merge with input
    result_df <- df %>%
      mutate(
        reg_id = result_json$ids$reg_id,
        basin_id = result_json$ids$basin_id,
        subc_id = result_json$ids$subc_id
      )

    return(result_df)
  }

  # Case 2: Multiple Points API (for more than one point)
  if (!is.null(df) && nrow(df) > 1 && is.null(csv_url)) {
    stop("df too big, please provide a csv_url.")
  }

  # Check that the csv_url is a string ending with .csv
  if (!is.character(csv_url) || !grepl("\\.csv$", csv_url)) {
    stop("A valid 'csv_url' must be provided.")
  }

  message("Using multi-point API...")

  # Construct request body
  body <- list(
    inputs = list(
      csv_url = csv_url,
      colname_lat = colname_lat,
      colname_lon = colname_lon,
      colname_site_id = colname_site_id
    ),
    outputs = list(transmissionMode = "reference")  # Expect CSV download link
  )

  if (!is.null(comment)) {
    body$inputs$comment <- comment
  }


  # Send the POST request
  response <- httr::POST(
    url = process_url,
    body = toJSON(body, auto_unbox = TRUE),
    encode = "json",
    add_headers("Content-Type" = "application/json", "Prefer" = "respond-async")
  )

  if (status_code(response) != 201) {
    stop("Failed to submit request. HTTP Status: ", status_code(response))
  }

  # Poll for job status
  location_url <- response$headers$location
  if (is.null(location_url)) stop("No location header found in the response.")

  status <- "accepted"
  while (status != "successful") {
    Sys.sleep(5)  # Wait 5 seconds before polling

    status_response <- httr::GET(location_url)
    status_json <- content(status_response, "parsed")

    status <- status_json$status
    message(paste("Job status:", status))

    if (status == "failed") stop("Job failed.")
  }

  # Extract JSON results URL
  json_results_url <- NULL
  if (!is.null(status_json$links)) {
    for (link in status_json$links) {
      if (grepl("\\?f=json$", link$href)) {  # Find JSON link
        json_results_url <- link$href
        break
      }
    }
  }

  if (is.null(json_results_url)) {
    stop("No JSON results URL found in the response.")
  }

  # Fetch JSON results
  json_results_response <- httr::GET(json_results_url)
  json_results <- content(json_results_response, "parsed")

  # Extract CSV download URL
  csv_download_url <- json_results$href
  if (is.null(csv_download_url) || !grepl("\\.csv$", csv_download_url)) {
    stop("No valid CSV download link found in the results.")
  }

  # Download CSV and return as dataframe
  df <- tryCatch({
    read.table(csv_download_url, sep = ";", header = TRUE)
  }, error = function(e) {
    stop("Error downloading CSV: ", e$message)
  })

  return(df)
}



