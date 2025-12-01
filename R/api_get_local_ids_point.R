api_get_local_ids_point <- function(subc_id, which_ids = "basin_id", comment = NULL) {

  process_url <- "https://aqua.igb-berlin.de/pygeoapi-dev/processes/get-local-ids/execution"

  inputs <- list(
    subc_id = subc_id,
    which_ids = which_ids
  )

  if (!is.null(comment)) inputs$comment <- comment

  body <- list(inputs = inputs)

  response <- httr::POST(
    url = process_url,
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    httr::add_headers("Content-Type" = "application/json")
  )

  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve single-point IDs. Status ",
         httr::status_code(response),
         "\n", httr::content(response, "text"))
  }

  httr::content(response, "parsed")
}
