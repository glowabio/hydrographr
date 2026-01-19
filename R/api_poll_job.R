#' Poll status of an async job
#'
#' @param jobID Character. The job ID returned from an async API call.
#' @param wait Logical. If TRUE, poll until complete. If FALSE, check once.
#' @param poll_interval Numeric. Seconds between checks (default: 5).
#' @param max_wait Numeric. Maximum wait time in seconds (default: 600).
#' @param base_url Character. Base URL for the pygeoapi instance.
#'
#' @return List with job status and href (if complete)
#'
#' @export
api_poll_job <- function(
    jobID,
    wait = TRUE,
    poll_interval = 5,
    max_wait = 72000,
    base_url = "https://aqua.igb-berlin.de/pygeoapi"
) {

  job_url <- sprintf("%s/jobs/%s?f=json", base_url, jobID)
  results_url <- sprintf("%s/jobs/%s/results?f=json", base_url, jobID)

  check_status <- function() {
    status_resp <- httr::GET(job_url)
    if (httr::http_error(status_resp)) {
      stop(sprintf("Failed to fetch job status: %s", httr::http_status(status_resp)$message))
    }
    jsonlite::fromJSON(httr::content(status_resp, "text", encoding = "UTF-8"))
  }

  # Single check mode
  if (!wait) {
    status <- check_status()
    message(sprintf("Job %s: %s (%.0f%% complete)",
                    jobID, status$status, status$progress %||% 0))
    return(structure(
      list(
        jobID = jobID,
        status = status$status,
        progress = status$progress,
        href = if (status$status == "successful") {
          results_resp <- httr::GET(results_url)
          results <- jsonlite::fromJSON(httr::content(results_resp, "text", encoding = "UTF-8"))
          results$href
        } else NULL
      ),
      class = "ocg_job_status"
    ))
  }

  # Polling mode
  # message(sprintf("Polling job %s...", jobID))
  elapsed <- 0

  while (elapsed < max_wait) {
    status <- check_status()

    message(sprintf(
      "[%ds] Status: %s | Progress: %.0f%%",
      elapsed, status$status, status$progress %||% 0
    ))

    if (status$status == "successful") {
      message(sprintf("✓ Job %s completed successfully!", jobID))

      # Get results URL
      results_resp <- httr::GET(results_url)
      results <- jsonlite::fromJSON(httr::content(results_resp, "text", encoding = "UTF-8"))

      return(structure(
        list(
          jobID = jobID,
          status = "successful",
          href = results$href,
          completed = status$finished %||% status$updated
        ),
        class = "ocg_job_status"
      ))
    }

    if (status$status %in% c("failed", "dismissed")) {
      stop(sprintf("Job %s %s: %s", jobID, status$status, status$message %||% "Unknown error"))
    }

    Sys.sleep(poll_interval)
    elapsed <- elapsed + poll_interval
  }

  stop(sprintf(
    "Job %s timed out after %d seconds. Current status: %s\nCheck later with: api_poll_job('%s')",
    jobID, max_wait, status$status, jobID
  ))
}
