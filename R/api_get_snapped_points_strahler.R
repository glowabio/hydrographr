#' Snap Points to River Network Using Strahler Threshold
#'
#' Sends individual coordinate points to the GeoFRESH API to snap them
#' to the nearest stream segment above a given Strahler order.
#'
#' @family ocgapi
#' @param df A data.frame containing point data.
#' @param colname_lat Name of the latitude column. Default: "latitude".
#' @param colname_lon Name of the longitude column. Default: "longitude".
#' @param colname_site_id Name of the site ID column. Default: "site_id".
#' @param strahler Minimum Strahler order to snap to (e.g., 3).
#' @param geometry_only Logical. If TRUE, return only geometry.
#' @param comment Optional comment string passed to the API.
#' @param process_url URL of the pygeoapi process (e.g., "http://localhost:5000/processes/get-snapped-points-strahler/execution").
#'
#' @return A data.frame with the snapped point data.
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @export
api_get_snapped_points_strahler <- function(df,
                                            colname_lat = "latitude",
                                            colname_lon = "longitude",
                                            colname_site_id = "site_id",
                                            strahler = 3,
                                            geometry_only = FALSE,
                                            comment = NULL,
                                            process_url = NULL) {
  if (is.null(process_url)) {
    stop("Please provide the URL of the pygeoapi process")
  }

  if (!all(c(colname_lat, colname_lon, colname_site_id) %in% names(df))) {
    stop("Missing required columns in the input data frame.")
  }

  # Initialize empty result list
  results <- list()

  for (i in seq_len(nrow(df))) {
    lat <- df[[colname_lat]][i]
    lon <- df[[colname_lon]][i]
    site_id <- df[[colname_site_id]][i]

    body <- list(
      inputs = list(
        lon = lon,
        lat = lat,
        strahler = strahler,
        geometry_only = tolower(as.character(geometry_only)),
        comment = ifelse(is.null(comment), site_id, paste0(comment, "-", site_id))
      )
    )

    response <- httr::POST(
      url = process_url,
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      encode = "json",
      httr::add_headers("Content-Type" = "application/json")
    )

    if (httr::status_code(response) != 200) {
      warning(sprintf("Failed for site_id %s: HTTP %s", site_id, httr::status_code(response)))
      next
    }

    result <- httr::content(response, as = "parsed", simplifyVector = TRUE)

    result_df <- extract_snapping_info(result)

    # Flatten and store
    results[[i]] <- cbind(site_id = site_id, result_df)
  }

  # Combine all rows
  final_df <- dplyr::bind_rows(results)

  return(final_df)
}

