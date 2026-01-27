#' Retrieve Local IDs for Sampling Sites
#'
#' @description
#' Queries the GeoFRESH API to obtain regional unit, basin, and subcatchment
#' IDs for sampling locations provided via a CSV file. Useful for linking
#' observational data to the stream network hierarchy.
#'
#' @family ocgapi
#' @param csv_url Character. URL to a CSV file hosted online (must be publicly
#'   accessible via HTTP/HTTPS).
#' @param colname_lat Character. Name of the latitude column in CSV.
#'   Default: `"latitude"`.
#' @param colname_lon Character. Name of the longitude column in CSV.
#'   Default: `"longitude"`.
#' @param colname_site_id Character. Name of the site ID column in CSV.
#'   Default: `"site_id"`.
#' @param colname_subc_id Character (optional). Name of the subcatchment ID
#'   column in CSV if pre-computed IDs should be used. Default: `NULL`.
#' @param which_ids Character. ID type(s) to retrieve. Options: `"reg_id"`,
#'   `"basin_id"`, `"subc_id"`. Default: `"reg_id"`.
#' @param comment Character (optional). Comment for request logging.
#'   Default: `NULL`.
#'
#' @return A list containing:
#'   \describe{
#'     \item{data}{data.frame. Retrieved IDs with one row per site.}
#'     \item{href}{Character. URL of the output CSV file.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Retrieve basin IDs for sampling sites
#' result <- api_get_local_ids(
#'   csv_url = "https://example.com/sites.csv",
#'   which_ids = "basin_id"
#' )
#'
#' # Access the data
#' head(result$data)
#' }
#'
#' @export
#' @importFrom httr POST add_headers status_code content
#' @importFrom jsonlite toJSON
#' @importFrom data.table fread
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
  process_url <- "https://aqua.igb-berlin.de/pygeoapi/processes/get-local-ids-plural/execution"

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
