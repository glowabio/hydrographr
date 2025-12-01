#' Retrieve Local IDs for Sampling Sites
#'
#' Queries the GeoFRESH API to obtain regional unit, basin,
#' and subcatchment IDs for a set of sampling locations.
#' Handles both single-point and multi-point inputs.
#'
#' @family ocgapi
#' @param csv_url URL to a CSV file hosted online.
#' @param colname_lat Name of the latitude column in CSV file.
#' @param colname_lon Name of the longitude column in CSV file.
#' @param colname_site_id Name of the site ID column in CSV file.
#' @param colname_subc_id Name of the subcatchment ID column in CSV file (optional).
#' @param which_ids Which ID type(s) to retrieve, e.g. `"reg_id"`, `"basin_id"`,
#' `"subc_id"`.
#' @param comment Optional comment string.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{data}{A `data.frame` with the retrieved IDs.}
#'   \item{href}{A `character` string with the URL of the output CSV.}
#' }
#'
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @export
#' @author Afroditi Grigoropoulou, Merret Buurman
api_get_local_ids <- function(csv_url,
                              colname_lat = "latitude",
                              colname_lon = "longitude",
                              colname_site_id = "site_id",
                              colname_subc_id = NULL,
                              which_ids = "reg_id",
                              comment = NULL) {

  # ---- Validation ----
  if (is.null(csv_url)) stop("A 'csv_url' must be provided.")
  if (!is.character(csv_url) || !grepl("^https?://", csv_url)) {
    stop("csv_url must be a valid HTTP or HTTPS URL.")
  }

  # ---- Define process URL ----
  process_url <- "https://aqua.igb-berlin.de/pygeoapi-dev/processes/get-local-ids-plural/execution"

  # ---- Build request body ----
  inputs <- list(
    csv_url = csv_url,
    colname_lat = colname_lat,
    colname_lon = colname_lon,
    colname_site_id = colname_site_id,
    which_ids = which_ids
  )

  # Add optional arguments only if provided
  if (!is.null(colname_subc_id)) inputs$colname_subc_id <- colname_subc_id
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

  # ---- Handle errors ----
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve results. HTTP Status: ",
         httr::status_code(response),
         "\nMessage: ", httr::content(response, "text", encoding = "UTF-8"))
  }

  # ---- Parse results ----
  result_json <- httr::content(response, "parsed")

  csv_download_url <- result_json$href
  if (is.null(csv_download_url) || !grepl("\\.csv$", csv_download_url)) {
    stop("No valid CSV download link found in the results.")
  }

  # ---- Read CSV ----
  df_out <- tryCatch({
    read.csv(csv_download_url)
  }, error = function(e) {
    stop("Error downloading CSV: ", e$message)
  })

  # ---- Return ----
  return(list(
    data = df_out,
    href = csv_download_url
  ))
}
