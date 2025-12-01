library(httr2)
library(dplyr)
library(purrr)
library(jsonlite)

api_get_snapped_points_seq <- function(df,
                                       lon_col = "longitude",
                                       lat_col = "latitude",
                                       site_id_col = "site_id",
                                       strahler = 3,
                                       geometry_only = FALSE,
                                       batch_size = 100
) {

  process_url <-"https://aqua.igb-berlin.de/pygeoapi-dev/processes/get-snapped-points-strahler/execution"

  # NSE helper for dynamic column names
  lon_col  <- rlang::sym(lon_col)
  lat_col  <- rlang::sym(lat_col)
  site_id_col <- rlang::sym(site_id_col)


  # Ensure required columns
  stopifnot(all(c("site_id", "longitude", "latitude") %in% names(df)))

  df_split <- split(df, ceiling(seq_len(nrow(df)) / batch_size))

  process_one_row <- function(row) {
    body <- list(
      inputs = list(
        lon = row[[rlang::as_string(lon_col)]],
        lat = row[[rlang::as_string(lat_col)]],
        strahler = strahler,
        geometry_only = tolower(as.character(geometry_only)),
        comment = as.character(row[[rlang::as_string(site_id_col)]])
      )
    )

    resp <- request(process_url) |>
      req_body_json(body) |>
      req_perform()

    # extract JSON
    result <- resp |>
      resp_body_json()

    # snapped point is always first feature
    pt <- result$features[[1]]$geometry$coordinates

    tibble(
      site_id = row$site_id,
      lon = row$longitude,
      lat = row$latitude,
      lon_snap = pt[[1]],
      lat_snap = pt[[2]],
      subc_after_snap = result$subc_id_after_snapping,
      strahler = result$features[[1]]$properties$strahler
    )
  }

  # Each batch is still row-wise requests
  results <- map_dfr(df_split, \(chunk) {
    map_dfr(seq_len(nrow(chunk)), \(i) process_one_row(chunk[i, ]))
  })

  return(results)
}
