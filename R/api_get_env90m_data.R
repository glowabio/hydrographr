#' Retrieve 90m-Resolution Environmental Variables for Subcatchments
#'
#' This function queries the GeoFRESH API to retrieve
#' environmental zonal statistics of the variables included in the
#' Environment90m dataset (Marquez et al., 2025, in prep.).
#'
#' @family ocgapi
#' @param subc_ids A numeric vector of unique subcatchment IDs.
#' @param variables A character vector of variable names to retrieve
#'   (e.g., `"bio5"`, `"slope_grad_dw_cel"`).
#' @param comment Optional character string to include in the request
#'   metadata (can help with tracing requests on the server).
#' @param chunk_size Integer. Maximum number of subcatchments to send per request
#'   (default is 3000).
#' @param process_url Character. The full URL of the pygeoapi process to call.
#'   Must be specified explicitly, e.g.,
#'   `"http://localhost:5000/processes/get-env90m/execution"`.
#'
#' @return A data frame with one row per subcatchment and columns for each
#'   requested environmental variable, plus a `subc_id` column.
#' @return A data frame containing zonal statistics (mean,min,max) of environmental variables
#' per subcatchment. Each row corresponds to one subcatchment.
#'
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import purrr
#'
#' @examples
#' \dontrun{
#' subc_ids <- c(560230105, 560149764, 560251298)
#' vars <- vars <- c("bio1", "flow", "cti")
#' env_df <- api_get_env90m_data(subc_ids = subc_ids, variables = vars)
#' }
#'
#' @export
api_get_env90m_data <- function(subc_ids, variables, comment = NULL,
chunk_size = 3000, process_url = NULL) {


  # Ensure that the process url is provided
  if (is.null(process_url)) {
    stop("Please provide the URL of the pygeoapi process")
  }


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
