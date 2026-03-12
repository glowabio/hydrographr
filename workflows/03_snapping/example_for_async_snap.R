
# ============================================================================
# Example: Running api_get_snapped_points_cascade_plural with async support
# ============================================================================

# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)

# ============================================================================
# EXAMPLE 1: Small dataset (sync mode, auto-detected)
# ============================================================================
result_small <- api_get_snapped_points_strahler_plural_async(
  csv_url = "https://nimbus.igb-berlin.de/index.php/s/QDXMorHrFXHnPbM/download/dams_sarantaporos.csv",
  colname_lon = "longitude",
  colname_lat = "latitude",
  colname_site = "site_id",
  min_strahler = 4,
  add_distance = TRUE,
  force_async = TRUE)

df <- api_get_job_results(result_small$href)



# api_get_snapped_points_strahler_plural_async()
result_small <- api_get_snapped_points_cascade_plural_async(
  csv_url = "https://nimbus.igb-berlin.de/index.php/s/QDXMorHrFXHnPbM/download/dams_sarantaporos.csv",
  col_lon = "longitude",
  col_lat = "latitude",
  col_site = "site_id",
  strahler_seq = c(4, 3, 2),
  distance_threshold = 150,
  comment = "Small dataset test"
)

# Check results
str(result_small)
head(result_small$snapped_points_df)
nrow(result_small$snapped_points_df)

# ============================================================================
# EXAMPLE 2: Large dataset (async mode, auto-detected)
# ============================================================================

result_large <- api_get_snapped_points_cascade_plural(
  csv_url = "https://nimbus.igb-berlin.de/index.php/s/HMNXJgrCiaokGye/download/Data_GBIF_clean.csv",
  col_lon = "decimalLongitude",
  col_lat = "decimalLatitude",
  col_site = "gbifID",
  strahler_seq = c(5, 4, 3),  # Start with higher Strahler for large dataset
  distance_threshold = 200,    # More lenient threshold
  comment = "GBIF large dataset",
  async_threshold = 1000,      # Auto-async if > 1000 points
  poll_interval = 10,          # Check every 10 seconds
  max_wait = 1200              # Wait up to 20 minutes
)

# Inspect results
summary(result_large$snapped_points_df)
table(result_large$snapped_points_df$distance_metres < 150)

# ============================================================================
# EXAMPLE 3: Force async mode (for testing)
# ============================================================================

result_forced <- api_get_snapped_points_cascade_plural(
  csv_url = "https://nimbus.igb-berlin.de/index.php/s/YOUR_FILE.csv",
  col_lon = "lon",
  col_lat = "lat",
  col_site = "id",
  strahler_seq = c(4, 3),
  distance_threshold = 100,
  force_async = TRUE,  # Force async even for small datasets
  poll_interval = 5,
  max_wait = 600
)

# ============================================================================
# EXAMPLE 4: Manual async workflow (step-by-step)
# ============================================================================

# Step 1: Submit just the first snapping job
job <- api_get_snapped_points_strahler_plural(
  csv_url = "https://nimbus.igb-berlin.de/index.php/s/HMNXJgrCiaokGye/download/Data_GBIF_clean.csv",
  colname_lon = "decimalLongitude",
  colname_lat = "decimalLatitude",
  colname_site_id = "gbifID",
  min_strahler = 5,
  add_distance = TRUE,
  force_async = TRUE
)

# Check if async
if (job$async) {
  print(paste("Job submitted:", job$jobID))

  # Step 2: Check status without waiting
  status <- api_poll_job(job$jobID, wait = FALSE)
  print(status)

  # Step 3: Wait for completion
  result <- api_poll_job(job$jobID, wait = TRUE, poll_interval = 10)
  print(paste("Job complete! CSV URL:", result$href))

  # Step 4: Download results
  df <- api_get_job_results(result$href)
  print(paste("Downloaded", nrow(df), "rows"))

  # Step 5: Continue with cascade manually or use the URL
  # ...
}

# ============================================================================
# EXAMPLE 5: Analyzing cascade results
# ============================================================================

result <- api_get_snapped_points_cascade_plural(
  csv_url = "https://nimbus.igb-berlin.de/index.php/s/HMNXJgrCiaokGye/download/Data_GBIF_clean.csv",
  col_lon = "decimalLongitude",
  col_lat = "decimalLatitude",
  col_site = "gbifID",
  strahler_seq = c(5, 4, 3, 2),
  distance_threshold = 150,
  comment = "Full cascade analysis"
)

# Analyze snapping distances
library(ggplot2)

result$snapped_points_df %>%
  ggplot(aes(x = distance_metres)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = 150, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Distribution of Snapping Distances",
    subtitle = paste("Total points snapped:", nrow(result$snapped_points_df)),
    x = "Distance (meters)",
    y = "Count"
  ) +
  theme_minimal()

# Summary statistics
summary(result$snapped_points_df$distance_metres)

# Points by distance category
result$snapped_points_df %>%
  mutate(
    distance_category = case_when(
      distance_metres < 50 ~ "< 50m",
      distance_metres < 100 ~ "50-100m",
      distance_metres < 150 ~ "100-150m",
      TRUE ~ "> 150m"
    )
  ) %>%
  count(distance_category)

# ============================================================================
# EXAMPLE 6: Error handling and recovery
# ============================================================================

# If a job times out or fails, you can recover using the job ID
jobID <- "bf410cd8-ebe1-11f0-9daf-4d7e3f3c94f9"  # From earlier run

# Check current status
status <- api_poll_job(jobID, wait = FALSE)
print(status)

# If successful, retrieve results
if (status$status == "successful") {
  df <- api_get_job_results(jobID)
  print(paste("Recovered", nrow(df), "rows"))
}

# ============================================================================
# EXAMPLE 7: Batch processing multiple regions
# ============================================================================

regions <- list(
  europe = "https://example.com/europe_points.csv",
  asia = "https://example.com/asia_points.csv",
  americas = "https://example.com/americas_points.csv"
)

results_all <- lapply(names(regions), function(region_name) {
  message(sprintf("\n\n========== Processing: %s ==========", region_name))

  result <- api_get_snapped_points_cascade_plural(
    csv_url = regions[[region_name]],
    col_lon = "longitude",
    col_lat = "latitude",
    col_site = "site_id",
    strahler_seq = c(4, 3, 2),
    distance_threshold = 150,
    comment = paste("Region:", region_name)
  )

  result$snapped_points_df$region <- region_name
  return(result)
})

# Combine all regions
all_snapped <- dplyr::bind_rows(lapply(results_all, function(x) x$snapped_points_df))

# Summary by region
all_snapped %>%
  group_by(region) %>%
  summarise(
    n_points = n(),
    mean_distance = mean(distance_metres),
    median_distance = median(distance_metres),
    max_distance = max(distance_metres)
  )
