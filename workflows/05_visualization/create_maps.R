# ============================================================================
# CREATE VISUALIZATION MAPS
# ============================================================================
# Purpose: Generate interactive maps of snapped points and networks
# Input: Results from previous steps
# Output: Interactive HTML maps
# ============================================================================

library(leaflet)
library(sf)
library(data.table)
library(htmlwidgets)

# Load data
all_snapped <- fread("../results/all_snapped_with_basins.csv")
all_streams <- st_read("../results/all_stream_networks.gpkg")

# Convert to sf
points_sf <- st_as_sf(
  all_snapped,
  coords = c("longitude_snapped", "latitude_snapped"),
  crs = 4326
)

# ============================================================================
# MAP 1: All Snapped Points by Source
# ============================================================================

message("Creating map 1: Snapped points...")

map1 <- leaflet(points_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    color = ~ifelse(source == "HCMR", "red", "blue"),
    radius = 3,
    fillOpacity = 0.7,
    popup = ~paste0(
      "Source: ", source, "<br>",
      "Site: ", site_id, "<br>",
      "Basin: ", basin_id, "<br>",
      "Snap distance: ", round(distance_metres, 1), "m"
    )
  ) %>%
  addLegend(
    colors = c("red", "blue"),
    labels = c("HCMR", "GBIF"),
    title = "Data Source"
  )

saveWidget(map1, "../results/maps/map_all_points.html", selfcontained = TRUE)

# ============================================================================
# MAP 2: Stream Networks with Points
# ============================================================================

message("Creating map 2: Streams and points...")

map2 <- leaflet() %>%
  addTiles() %>%
  addPolylines(data = all_streams, color = "blue", weight = 1, opacity = 0.6) %>%
  addCircleMarkers(
    data = points_sf, 
    color = ~ifelse(source == "HCMR", "red", "orange"),
    radius = 4,
    fillOpacity = 0.8
  )

saveWidget(map2, "../results/maps/map_streams_and_points.html", selfcontained = TRUE)

message("\n=== Maps created in results/maps/ ===")
