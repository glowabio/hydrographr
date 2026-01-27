# ============================================================================
# NETWORK BUFFER ANALYSIS
# ============================================================================
# Purpose: Calculate upstream/downstream network buffers for each point
# Input: Snapped points and stream networks from step 04
# Output: Network buffer segments
# ============================================================================

library(hydrographr)
library(sf)
library(data.table)
library(dplyr)

# Load data
all_snapped <- fread("../results/all_snapped_with_basins.csv")
all_streams <- st_read("../results/all_stream_networks.gpkg")

# Convert snapped points to sf
points_sf <- st_as_sf(
  all_snapped,
  coords = c("longitude_snapped", "latitude_snapped"),
  crs = 4326
)

# ============================================================================
# EXTRACT PARTIAL NETWORK (Optional - for performance)
# ============================================================================

message("\n=== Extracting Partial Networks ===")

partial_network <- extract_partial_stream_network(
  snapped_points = points_sf,
  all_stream = all_streams,
  upstream_distance = 5000,
  downstream_distance = 2000
)

st_write(partial_network, "../results/partial_stream_network.gpkg", 
         delete_dsn = TRUE)

# ============================================================================
# CALCULATE NETWORK BUFFERS
# ============================================================================

message("\n=== Calculating Network Buffers ===")

network_buffers <- get_buffer_along_the_network(
  lines_sf = partial_network,
  start_points = points_sf,
  up_radius = 500,    # meters upstream
  down_radius = 2000  # meters downstream
)

st_write(network_buffers, "../results/network_buffers.gpkg", 
         delete_dsn = TRUE)

message("\n=== Network analysis complete ===")
