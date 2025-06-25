#' Retrieve Upstream Subcatchments for Snapped Coordinates
#'
#' This function queries the GeoFRESH API to retrieve all upstream subcatchments for each input site.
#' It is intended for use *after* site coordinates have been snapped to the river network using the
#' [get_snapped_points()] function. Returns a long-format data frame where each row represents an
#' upstream subcatchment associated with a given site.
#'
#' @family pygeoapi
#' @param df A data frame containing snapped site coordinates, including latitude, longitude, and a site ID column.
#' @param colname_lat Name of the latitude column. Default is `"latitude"`.
#' @param colname_lon Name of the longitude column. Default is `"longitude"`.
#' @param colname_site_id Name of the site ID column. Default is `"site_id"`.
#' @param process_url URL of the pygeoapi process called by the function.
#' #'   Must be specified explicitly, e.g.,
#'   `"http://localhost:5000/processes/get-upstream-subcids/execution"`.
#' @param comment Optional character string to tag each API call (e.g., for logging or debugging).

#'
#' @return A data frame in long format with columns: \code{site_id}, \code{subc_id}, and \code{upstream_id}.
#'         Each row represents an upstream subcatchment ID linked to a sampling site.
#'
#' @import httr
#' @import jsonlite
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' # Example with 3 sites (must be snapped to the river network first)
#' df_sites <- data.frame(
#'   site_id = c("C-001841", "C-001842", "C-0101543"),
#'   latitude = c(40.26786, 40.29958, 40.14107),
#'   longitude = c(20.77203, 20.78125, 20.84042),
#'   stringsAsFactors = FALSE
#' )
#'
#' # (Assumes these coordinates are already snapped using get_snapped_points())
#' upstream_df <- get_upstream_subcids(df = df_sites)
#' head(upstream_df)
#' }
#'
#' @export
api_get_upstream_subcids <- function(df,
                                 colname_lat = "latitude",
                                 colname_lon = "longitude",
                                 colname_site_id = "site_id",
                                 process_url = NULL,
                                 comment = NULL) {

  # Ensure at least one input is provided
  if (is.null(df) || nrow(df) == 0) {
    stop("Input dataframe is empty or NULL.")
  }

  # Ensure that the process url is provided
  if (is.null(process_url)) {
    stop("Please provide the URL of the pygeoapi process")
  }

  all_results <- list()

  for (i in seq_len(nrow(df))) {
    lon <- df[[colname_lon]][i]
    lat <- df[[colname_lat]][i]
    site_id <- df[[colname_site_id]][i]

    body <- list(
      inputs = list(
        lon = lon,
        lat = lat
      )
    )

    if (!is.null(comment)) {
      body$inputs$comment <- paste0(comment, "_row", i)
    }


    response <- try(httr::POST(
      url = process_url,
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      encode = "json",
      httr::add_headers("Content-Type" = "application/json")
    ), silent = TRUE)

    if (inherits(response, "try-error") || httr::status_code(response) != 200) {
      warning(sprintf("Site %d failed. Skipping...", i))
      next
    }

    result_json <- httr::content(response, "parsed")

    if (is.null(result_json$upstream_ids) || length(result_json$upstream_ids) == 0) {
      warning(sprintf("No upstream IDs found for site %d. Skipping...", i))
      next
    }

    result_df <- data.frame(
      site_id = rep(site_id, length(result_json$upstream_ids)),
      subc_id = rep(result_json$subc_id, length(result_json$upstream_ids)),
      upstream_id = unlist(result_json$upstream_ids),
      stringsAsFactors = FALSE
    )

    all_results[[length(all_results) + 1]] <- result_df
    message(sprintf("Site %d done. %d upstream segments.", i, nrow(result_df)))
  }

  if (length(all_results) == 0) {
    warning("No valid results returned.")
    return(data.frame(site_id = character(), subc_id = numeric(), upstream_id = numeric()))
  }

  combined_df <- do.call(rbind, all_results)
  return(combined_df)
}


