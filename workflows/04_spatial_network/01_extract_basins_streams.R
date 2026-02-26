# ============================================================================
# GET BASIN IDs AND STREAM NETWORKS
# ============================================================================
# Purpose: Retrieve basin information and download stream networks
# Input: Snapped points from step 03
# Output: Basin IDs, stream network shapefiles
# ============================================================================
library(hydrographr)
library(sf)
library(data.table)
library(dplyr)

# Load helper function
source("~/Documents/Postdoc/code/workflow_paper/helpers/save_to_nimbus.R")

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)

# ============================================================================
# LOAD SNAPPED POINTS
# ============================================================================

message("\n=== Loading Snapped Points ===")

# Load combined fish data
fish_snapped <- fread("points_snapped/fish/all_snapped_fish_points.csv")
message(sprintf("Loaded fish data: %d points", nrow(fish_snapped)))

# Load dams data
dams_snapped <- fread("points_snapped/dams/dams_snapped_points.csv")
message(sprintf("Loaded dams data: %d points", nrow(dams_snapped)))

# Add source identifier to dams if not present
if (!"source" %in% names(dams_snapped)) {
  dams_snapped$source <- "Dams"
}

# Rename site_id column for dams
if ("id1" %in% names(dams_snapped)) {
  dams_snapped <- dams_snapped %>% rename(site_id = id1)
}

# Get common columns
common_cols <- intersect(names(fish_snapped), names(dams_snapped))
message(sprintf("Common columns: %s", paste(common_cols, collapse = ", ")))

# Combine fish and dams
all_snapped <- rbind(
  fish_snapped[, ..common_cols],
  dams_snapped[, ..common_cols]
)

message(sprintf("Combined total: %d snapped points", nrow(all_snapped)))

# write out to get the URL of the file
fwrite(all_snapped, "points_snapped/all_points_snapped.csv")
all_snapped_csv <- "https://nimbus.igb-berlin.de/index.php/s/2aXF9TLqCxZBAwt/download/all_points_snapped.csv"
all_snapped <- fread(all_snapped_csv)

# Summary by source
message("\nPoints by source:")

# ============================================================================
# GET BASIN INFORMATION
# ============================================================================

message("\n=== Getting Basin IDs ===")

basin_ids <- api_get_local_ids(data = all_snapped,
                               colname_lon = "longitude_snapped",
                               colname_lat = "latitude_snapped",
                               colname_site_id = "site_id",
                               colname_subc_id = "subc_id"
)

# Merge with snapped data
all_snapped_with_basins <- left_join(all_snapped, basin_ids)

# Save combined file
# fwrite(all_snapped_with_basins, "points_snapped/all_snapped_with_basins.csv")
message(sprintf("Saved: points_snapped/all_snapped_with_basins.csv"))

all_snapped_with_basins <- fread("points_snapped/all_snapped_with_basins.csv")

# Summary by source
message("\nPoints by source:")
source_summary <- all_snapped_with_basins %>%
  group_by(source) %>%
  summarise(
    n_points = n(),
    n_basins = n_distinct(basin_id),
    .groups = "drop"
  )
print(source_summary)

# ============================================================================
# DOWNLOAD STREAM NETWORKS
# ============================================================================

message("\n=== Downloading Stream Networks ===")

unique_basins <- unique(all_snapped_with_basins$basin_id)
message(sprintf("Processing %d unique basins", length(unique_basins)))

# Create directory for stream networks
dir.create("spatial/stream_networks", showWarnings = FALSE)

stream_networks <- list()

for (i in seq_along(unique_basins)) {
  basin <- unique_basins[i]
  message(sprintf("[%d/%d] Basin %s...", i, length(unique_basins), basin))

  tryCatch({
    stream_sf <- api_get_basin_streamsegments(
      basin_id = basin,
      strahler_min = min(all_snapped$strahler)
    )

    # ADD BASIN_ID COLUMN
    stream_sf$basin_id <- basin

    stream_networks[[as.character(basin)]] <- stream_sf

  }, error = function(e) {
    message(sprintf("  ⚠️  Error downloading basin %s: %s", basin, e$message))
  })
}

# Combine all networks
if (length(stream_networks) > 0) {
  all_streams <- do.call(rbind, stream_networks)

  # Save locally first
  temp_path <- tempfile(fileext = ".gpkg")
  st_write(all_streams, temp_path, delete_dsn = TRUE)

  # Copy to nimbus
  nimbus_dest <- file.path(nimbus_path, "spatial/stream_networks/all_stream_networks_Feb.gpkg")
  system2("cp", args = c(shQuote(temp_path), shQuote(nimbus_dest)))
  unlink(temp_path)

  message(sprintf("\n✓ Saved %d stream segments from %d basins",
                  nrow(all_streams), length(unique_basins)))
  message("  File: stream_networks/all_stream_networks.gpkg")
} else {
  message("\n⚠️  No stream networks downloaded")
}

# ============================================================================
# EXTRACT PARTIAL STREAM NETWORK
# ============================================================================

all_streams_filtered <- extract_partial_stream_network(
  all_streams,
  all_snapped$subc_id,
  strahler_retain_threshold = 4,
  upstream_buffer = 3      # number of upstream segments to include
)

save_to_nimbus(all_streams_filtered, "spatial/stream_networks/partial_stream_network_Feb.gpkg")

# write out points as .gpkg
all_snapped_with_basins_vect <- st_as_sf(
  all_snapped_with_basins,
  coords = c("longitude_snapped", "latitude_snapped"),  # Column names for x, y
  crs = 4326                  # WGS84 (lat/lon)
)

save_to_nimbus(all_snapped_with_basins_vect, "points_snapped/all_snapped_points.gpkg")

# original points
all_original_with_basins_vect <- st_as_sf(
  all_snapped_with_basins,
  coords = c("longitude_original", "latitude_original"),  # Column names for x, y
  crs = 4326                  # WGS84 (lat/lon)
)
save_to_nimbus(all_original_with_basins_vect, "points_cleaned/all_points_before_snap.gpkg")


# ============================================================================
# FINAL SUMMARY
# ============================================================================

message("\n=== Basin Retrieval Complete ===")
message("\nFinal Statistics:")
message(sprintf("  Total basins: %d", length(unique_basins)))
message(sprintf("  Stream segments downloaded: %d",
                if(exists("all_streams")) nrow(all_streams) else 0))

message("\nFiles created:")
message("  - points_snapped/all_snapped_with_basins.csv")
message("  - points_snapped/fish_snapped_with_basins.csv")
message("  - points_snapped/dams_snapped_with_basins.csv")
message("  - stream_networks/all_stream_networks.gpkg")
