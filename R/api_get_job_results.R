#' Download and return results from a completed job
#'
#' @param jobID Character. Job ID or the result object from api_poll_job().
#' @param base_url Character. Base URL for the pygeoapi instance.
#' @param as_dataframe Logical. If TRUE (default), return dataframe. If FALSE, return URL only.
#'
#' @return Data frame with the results, or character URL if as_dataframe = FALSE
#'
#' @export
api_get_job_results <- function(
    jobID,
    base_url = "https://aqua.igb-berlin.de/pygeoapi-dev",
    as_dataframe = TRUE
) {

  # Handle both jobID string and result object
  if (is.list(jobID)) {
    if (!is.null(jobID$href)) {
      csv_url <- jobID$href
    } else if (!is.null(jobID$jobID)) {
      jobID <- jobID$jobID
    } else {
      stop("Invalid input: provide jobID string or result object with $href or $jobID")
    }
  }

  # If we don't have the URL yet, fetch it
  if (!exists("csv_url")) {
    results_url <- sprintf("%s/jobs/%s/results?f=json", base_url, jobID)

    results_resp <- httr::GET(results_url)
    if (httr::http_error(results_resp)) {
      stop(sprintf(
        "Failed to fetch results for job %s. Status: %s",
        jobID, httr::http_status(results_resp)$message
      ))
    }

    results <- jsonlite::fromJSON(httr::content(results_resp, "text", encoding = "UTF-8"))
    csv_url <- results$href  # Adjust based on actual API response structure
  }

  # Return URL only if requested
  if (!as_dataframe) {
    return(csv_url)
  }

  # Download and read CSV
  message(sprintf("Downloading results from: %s", csv_url))
  temp_file <- tempfile(fileext = ".csv")

  tryCatch({
    utils::download.file(csv_url, temp_file, quiet = TRUE)
    df <- utils::read.csv(temp_file)
    unlink(temp_file)

    message(sprintf("✓ Downloaded %d rows, %d columns", nrow(df), ncol(df)))
    return(df)

  }, error = function(e) {
    unlink(temp_file)
    stop(sprintf("Failed to download/read results: %s", e$message))
  })
}
