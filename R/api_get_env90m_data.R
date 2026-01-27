#' Retrieve 90m Environmental Variables for Subcatchments
#'
#' @description
#' Queries the GeoFRESH API to retrieve environmental zonal statistics from the
#' Environment90m dataset (Marquez et al., 2025, in prep.) for specified
#' subcatchments. Handles large requests automatically by chunking.
#'
#' @family ocgapi
#' @param subc_ids Numeric vector. Unique subcatchment IDs to retrieve data for.
#' @param variables Character vector. Variable names to retrieve
#'   (e.g., `"bio5"`, `"slope_grad_dw_cel"`).
#' @param comment Character (optional). Metadata comment for request tracing.
#'   Default: `NULL`.
#' @param chunk_size Integer. Maximum subcatchments per API request.
#'   Default: `3000`.
#'
#' @return A data.frame containing zonal statistics (mean, min, max) of
#'   environmental variables per subcatchment. One row per subcatchment with
#'   columns for each requested variable plus a `subc_id` column.
#'
#' @examples
#' \dontrun{
#' # Retrieve climate and flow variables
#' subc_ids <- c(560230105, 560149764, 560251298)
#' vars <- c("bio1", "flow", "cti")
#' env_df <- api_get_env90m_data(
#'   subc_ids = subc_ids,
#'   variables = vars
#' )
#' }
#'
#' @export
#' @importFrom httr POST add_headers http_type content status_code
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom purrr map_dfr
api_get_env90m_data <- function(subc_ids, variables, comment = NULL,
chunk_size = 3000) {


  # Define process url
  process_url <- "https://aqua.igb-berlin.de/pygeoapi/processes/get-local-subcids-plural/execution"

  subc_ids <- unique(subc_ids)
  n_chunks <- ceiling(length(subc_ids) / chunk_size)
  all_results <- list()

  for (i in seq_len(n_chunks)) {
    cat("Processing chunk", i, "of", n_chunks, "...\n")

    chunk_ids <- subc_ids[((i - 1) * chunk_size + 1):min(i * chunk_size, length(subc_ids))]

    # Manual JSON formatting
    var_str <- paste(sprintf('"%s"', variables), collapse = ", ")
    subc_str <- paste(chunk_ids, collapse = ", ")
    comment_line <- if (!is.null(comment)) paste0(', "comment": "', comment, '"') else ""

    json_payload <- sprintf('{
      "inputs": {
        "subc_ids": [%s],
        "variables": [%s]%s
      }
    }', subc_str, var_str, comment_line)

    # POST request
    response <- httr::POST(
      url = process_url,
      body = json_payload,
      encode = "raw",
      httr::add_headers(`Content-Type` = "application/json")
    )

    # Basic checks
    if (httr::http_type(response) != "application/json") {
      warning(paste("Chunk", i, "did not return JSON. Skipping..."))
      next
    }

    # Parse content
    response_content <- httr::content(response, as = "parsed", type = "application/json")

    # Check for error
    if (!is.null(response_content$type) && response_content$type == "InvalidParameterValue") {
      warning(paste("Chunk", i, "failed:", response_content$description))
      next
    }


      # Remove unwanted entries (e.g., "comment" field if present)
  response_content <- response_content[names(response_content) != "comment"]


    # Convert chunk to data.frame
  chunk_df <- purrr::map2_df(
    names(response_content),
    response_content,
    ~ {
      # Replace NULLs with NAs, otherwise can't shape it into a data.frame
      cleaned <- purrr::map(.y, ~ if (is.null(.x)) NA else .x)

      # Convert to data.frame
      env_data <- as.data.frame(cleaned)

      # Add subc_id
      env_data$subc_id <- as.numeric(.x)

      return(env_data)
    }
  )

    all_results[[i]] <- chunk_df
    Sys.sleep(1)  # polite pause
  }

  final_result <- dplyr::bind_rows(all_results) %>%
    dplyr::relocate(subc_id)

  return(final_result)
}
