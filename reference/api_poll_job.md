# Poll status of an async job

Poll status of an async job

## Usage

``` r
api_poll_job(
  jobID,
  wait = TRUE,
  poll_interval = 5,
  max_wait = 72000,
  base_url = "https://aqua.igb-berlin.de/pygeoapi"
)
```

## Arguments

- jobID:

  Character. The job ID returned from an async API call.

- wait:

  Logical. If TRUE, poll until complete. If FALSE, check once.

- poll_interval:

  Numeric. Seconds between checks (default: 5).

- max_wait:

  Numeric. Maximum wait time in seconds (default: 600).

- base_url:

  Character. Base URL for the pygeoapi instance.

## Value

List with job status and href (if complete)
