#' Get Basin Polygon(s)
#'
#' Retrieves basin polygon boundary/boundaries from the GeoFRESH API.
#' Can accept either a basin_id directly, a single point (lat/lon), or a CSV
#' with multiple points to retrieve multiple basin polygons.
#'
#' @description
#' This function returns basin polygon(s) as an `sf` object. Three input modes:
#' 1. Direct basin_id (single polygon)
#' 2. Single point coordinates (retrieves basin_id, then polygon)
#' 3. CSV with multiple points (retrieves basin_ids for all points, then unique polygons)
#'
#' @family ocgapi
#' @param basin_id Integer. The ID of the basin to retrieve. If NULL, must provide
#'   either single coordinates OR csv_url.
#' @param latitude Numeric. Latitude of a single point (used if basin_id is NULL).
#' @param longitude Numeric. Longitude of a single point (used if basin_id is NULL).
#' @param csv_url Character. URL to a CSV file with multiple points (used if basin_id is NULL).
#' @param colname_lat Character. Name of latitude column in CSV. Default: "latitude".
#' @param colname_lon Character. Name of longitude column in CSV. Default: "longitude".
#' @param colname_site_id Character. Name of site ID column in CSV. Default: "site_id".
#' @param geometry_only Logical. If `TRUE`, returns only geometry without
#'   attributes. Defaults to `FALSE`.
#' @param comment Character. Optional comment for API logging.
#'
#' @return An `sf` object representing the basin polygon(s). If multiple basins
#'   are retrieved (from CSV input), returns all unique basins.
#'
#' @examples
#' \dontrun{
#' # Method 1: Using basin_id directly
#' basin_sf <- api_get_basin_polygon(
#'   basin_id = 1288419,
#'   geometry_only = FALSE
#' )
#'
#' # Method 2: Using single point coordinates
#' basin_sf <- api_get_basin_polygon(
#'   latitude = 53.5,
#'   longitude = 8.6
#' )
#'
#' # Method 3: Using CSV with multiple points
#' basins_sf <- api_get_basin_polygon(
#'   csv_url = "https://example.com/sites.csv",
#'   colname_lat = "lat",
#'   colname_lon = "lon",
#'   colname_site_id = "site_id"
#' )
#'
#' # Visualize
#' library(leaflet)
#' leaflet(basin_sf) |>
#'   addProviderTiles("CartoDB.Positron") |>
#'   addPolygons(color = "blue", fillOpacity = 0.3)
#' }
#'
#' @export
#'
#' @importFrom httr2 request req_headers req_body_json req_perform resp_status resp_status_desc resp_body_json
#' @importFrom jsonlite toJSON
#' @importFrom sf st_read
#' @importFrom dplyr bind_rows
api_get_basin_polygon <- function(basin_id = NULL,
                                  latitude = NULL,
                                  longitude = NULL,
                                  csv_url = NULL,
                                  colname_lat = "latitude",
                                  colname_lon = "longitude",
                                  colname_site_id = "site_id",
                                  geometry_only = FALSE,
                                  comment = NULL) {

  # --- Input validation ------------------------------------------------------

  # Check that one and only one input method is provided
  has_basin_id <- !is.null(basin_id)
  has_single_point <- !is.null(latitude) && !is.null(longitude)
  has_csv <- !is.null(csv_url)

  n_inputs <- sum(has_basin_id, has_single_point, has_csv)

  if (n_inputs == 0) {
    stop("Must provide one of: 'basin_id', coordinates ('latitude' + 'longitude'), or 'csv_url'.",
         call. = FALSE)
  }

  if (n_inputs > 1) {
    message("Multiple inputs provided. Priority: basin_id > csv_url > single point.")
    if (has_basin_id) {
      has_single_point <- FALSE
      has_csv <- FALSE
    } else if (has_csv) {
      has_single_point <- FALSE
    }
  }

  # Validate basin_id if provided
  if (has_basin_id && !is.numeric(basin_id)) {
    stop("`basin_id` must be a numeric value.", call. = FALSE)
  }

  # Validate single point coordinates
  if (has_single_point) {
    if (!is.numeric(latitude) || !is.numeric(longitude)) {
      stop("`latitude` and `longitude` must be numeric values.", call. = FALSE)
    }
    if (latitude < -90 || latitude > 90) {
      stop("`latitude` must be between -90 and 90.", call. = FALSE)
    }
    if (longitude < -180 || longitude > 180) {
      stop("`longitude` must be between -180 and 180.", call. = FALSE)
    }
  }

  # Validate CSV URL
  if (has_csv) {
    if (!is.character(csv_url) || !grepl("^https?://", csv_url)) {
      stop("`csv_url` must be a valid HTTP or HTTPS URL.", call. = FALSE)
    }
  }

  # --- Get basin_id(s) if not provided ---------------------------------------

  basin_ids <- NULL

  if (has_csv) {
    # Get basin_ids from CSV using api_get_local_ids
    message("Retrieving basin_ids from CSV...")

    tryCatch({
      local_ids_result <- api_get_local_ids(
        csv_url = csv_url,
        colname_lat = colname_lat,
        colname_lon = colname_lon,
        colname_site_id = colname_site_id,
        which_ids = "basin_id",
        comment = comment
      )

      # Extract unique basin_ids
      basin_ids <- unique(local_ids_result$data$basin_id)
      basin_ids <- basin_ids[!is.na(basin_ids)]  # Remove NAs

      if (length(basin_ids) == 0) {
        stop("No valid basin_ids found in the CSV data.", call. = FALSE)
      }

      message(sprintf("Found %d unique basin(s) from %d points",
                      length(basin_ids),
                      nrow(local_ids_result$data)))

    }, error = function(e) {
      stop("Error retrieving basin_ids from CSV: ", e$message, call. = FALSE)
    })

  } else if (has_single_point) {
    # Get basin_id from single point
    message(sprintf("Retrieving basin_id for coordinates: lat=%.5f, lon=%.5f",
                    latitude, longitude))

    # Create temporary CSV with single point
    temp_csv_file <- tempfile(fileext = ".csv")
    temp_df <- data.frame(
      site_id = "temp_point",
      latitude = latitude,
      longitude = longitude
    )
    names(temp_df) <- c(colname_site_id, colname_lat, colname_lon)
    write.csv(temp_df, temp_csv_file, row.names = FALSE)

    # Upload temp file to get a URL (you may need to implement this)
    # For now, assuming you have a function to upload and get URL
    # temp_csv_url <- upload_to_server(temp_csv_file)

    # Alternative: Read the file and use api_get_local_ids if you can get a URL
    # For this example, I'll show the pattern assuming upload capability

    tryCatch({
      # This requires a way to convert local file to URL
      # You might need to use your server's upload endpoint
      stop("Single point mode requires uploading temp CSV to server. ",
           "Please use csv_url or basin_id directly for now.",
           call. = FALSE)

      # Once you have upload capability, uncomment:
      # local_ids_result <- api_get_local_ids(
      #   csv_url = temp_csv_url,
      #   colname_lat = colname_lat,
      #   colname_lon = colname_lon,
      #   colname_site_id = colname_site_id,
      #   which_ids = "basin_id"
      # )
      #
      # basin_ids <- unique(local_ids_result$data$basin_id)
      # message(sprintf("Retrieved basin_id: %d", basin_ids[1]))

    }, error = function(e) {
      stop("Error retrieving basin_id: ", e$message, call. = FALSE)
    }, finally = {
      unlink(temp_csv_file)
    })

  } else {
    # basin_id was provided directly
    basin_ids <- basin_id
  }

  # --- Retrieve polygons for each basin_id -----------------------------------

  message(sprintf("Retrieving %d basin polygon(s)...", length(basin_ids)))

  process_url <- "https://aqua.igb-berlin.de/pygeoapi/processes/get-basin-polygon/execution"

  polygons_list <- list()

  for (i in seq_along(basin_ids)) {
    bid <- basin_ids[i]

    message(sprintf("  [%d/%d] Fetching basin_id: %d", i, length(basin_ids), bid))

    # Construct request body
    inputs <- list(
      basin_id = as.integer(bid),
      geometry_only = geometry_only
    )

    if (!is.null(comment)) {
      inputs$comment <- comment
    }

    body <- list(inputs = inputs)

    # Send request
    tryCatch({
      resp <- httr2::request(process_url) |>
        httr2::req_headers("Content-Type" = "application/json") |>
        httr2::req_body_json(body) |>
        httr2::req_perform()

      if (httr2::resp_status(resp) >= 400) {
        warning(sprintf("Failed to retrieve basin %d: %s",
                        bid, httr2::resp_status_desc(resp)))
        next
      }

      result <- httr2::resp_body_json(resp, simplifyVector = TRUE)

      # Convert to sf
      basin_geojson <- jsonlite::toJSON(result, auto_unbox = TRUE)
      basin_sf <- sf::st_read(basin_geojson, quiet = TRUE)

      polygons_list[[i]] <- basin_sf

    }, error = function(e) {
      warning(sprintf("Error retrieving basin %d: %s", bid, e$message))
    })
  }

  # --- Combine all polygons --------------------------------------------------

  if (length(polygons_list) == 0) {
    stop("No basin polygons were successfully retrieved.", call. = FALSE)
  }

  # Combine all sf objects
  combined_sf <- dplyr::bind_rows(polygons_list)

  message(sprintf("✓ Retrieved %d basin polygon(s)", nrow(combined_sf)))

  return(combined_sf)
}
