#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02 -- dam_impact_zone_analysis.R
# Calculate affected habitat within impact zones around dams
# impact zone = upstream/downstream buffer zones from each dam
#
# CONCEPT:
#   - Each dam affects habitat within a certain radius (up/downstream)
#   - impact zone = river segments within these radii
#   - Quantifies total habitat length affected by dams
#
# INPUT:
#   - river_graph.RDS (from script 01)
#   - partial_stream_network.gpkg (spatial network)
#   - dams_snapped_points.csv
#   - fish_all_species_snapped.csv
#
# OUTPUT:
#   - impact_zone_by_dam.gpkg (spatial impact zones)
#   - impact_zone_summary.csv (affected lengths by dam/scenario)
#   - impact_zone_map.html (interactive map)
#
# LOCATION: workflows/04_network_analysis/02_dam_impact_zone_analysis.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(sf)
library(data.table)
library(lwgeom)
library(leaflet)
library(htmlwidgets)

# Set working directory
wdir <- "/run/user/1000/gvfs/dav:host=nimbus.igb-berlin.de,ssl=true,user=grigoropoulou,prefix=%2Fremote.php%2Fwebdav/workflow_paper/data"
wdir <- "~/Documents/Postdoc/projects/workflow_paper/data"

setwd(wdir)

# Create output directory
dir.create("connectivity", showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

TARGET_BASIN_ID <- 1292502  # Same basin as fragmentation analysis
EXAMPLE_SPECIES <- "Barbus_prespensis"

# impact zone radii (meters)
UPSTREAM_RADIUS <- 500    # Distance upstream affected by dam
DOWNSTREAM_RADIUS <- 2000  # Distance downstream affected by dam

message("=== DAM IMPACT ZONE ANALYSIS ===")
message("Basin: ", TARGET_BASIN_ID)
message("Species: ", EXAMPLE_SPECIES)
message("Upstream radius: ", UPSTREAM_RADIUS, " m")
message("Downstream radius: ", DOWNSTREAM_RADIUS, " m")
message("Date: ", Sys.Date())

# ============================================================
# STEP 1: Load river network (spatial version for impact zones)
# ============================================================

message("\n[1/7] Loading spatial river network...")

stream_network <- st_read("spatial/stream_networks/partial_stream_network.gpkg",
                          quiet = TRUE)

# Filter to target basin
# First need to get basin_id from graph or node data
river_graph <- readRDS("spatial/stream_networks/river_graph.RDS")

node_data <- data.frame(
  name = V(river_graph)$name,
  basin_id = V(river_graph)$basin_id
) %>%
  filter(!is.na(basin_id))

basin_nodes <- node_data %>%
  filter(basin_id == TARGET_BASIN_ID) %>%
  pull(name)

# Filter spatial network to basin
basin_streams <- stream_network %>%
  filter(subc_id %in% basin_nodes)

message("  Basin streams: ", nrow(basin_streams))
message("  Total length: ", round(sum(basin_streams$length) / 1000, 1), " km")

# ============================================================
# STEP 2: Load dams and categorize
# ============================================================

message("\n[2/7] Loading dam data...")

dams_all <- fread("points_snapped/dams/dams_snapped_points.csv")

# Filter to target basin
dams_basin <- dams_all %>%
  filter(as.character(subc_id) %in% basin_nodes)

# Separate by status
dams_existing <- dams_basin %>%
  filter(status == "existing")

dams_planned <- dams_basin %>%
  filter(status == "planned") %>%
  filter(phase != "R")

message("  Existing dams: ", nrow(dams_existing))
message("  Planned dams: ", nrow(dams_planned))

# ============================================================
# STEP 3: Load species occurrences
# ============================================================

message("\n[3/7] Loading species occurrence data...")

fish_all <- fread("points_snapped/fish/fish_all_species_snapped.csv")

# Filter to target basin and species
species_occurrences <- fish_all %>%
  filter(as.character(subc_id) %in% basin_nodes) %>%
  filter(species == EXAMPLE_SPECIES |
           species == gsub("_", " ", EXAMPLE_SPECIES))

message("  Species occurrences: ", nrow(species_occurrences))

# ============================================================
# STEP 4: Calculate impact zones for existing dams
# ============================================================

message("\n[4/7] Calculating impact zones for existing dams...")

# Convert existing dams to sf points
dams_existing_sf <- dams_existing %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"),
           crs = 4326)

# Calculate impact zone using hydrographr function
message("  Running get_buffer_along_the_network()...")

impact_zone_existing <- get_buffer_along_the_network(
  lines_sf = basin_streams,
  start_points = dams_existing_sf,
  up_radius = UPSTREAM_RADIUS,
  down_radius = DOWNSTREAM_RADIUS
)

message("  Impact zone segments: ", nrow(impact_zone_existing))

# Add dam metadata
impact_zone_existing <- impact_zone_existing %>%
  mutate(dam_status = "existing")

# ============================================================
# STEP 5: Calculate impact zones for planned dams
# ============================================================

message("\n[5/7] Calculating impact zones for planned dams...")

if (nrow(dams_planned) > 0) {

  # Convert planned dams to sf points
  dams_planned_sf <- dams_planned %>%
    st_as_sf(coords = c("longitude_snapped", "latitude_snapped"),
             crs = 4326)

  impact_zone_planned <- get_buffer_along_the_network(
    lines_sf = basin_streams,
    start_points = dams_planned_sf,
    up_radius = UPSTREAM_RADIUS,
    down_radius = DOWNSTREAM_RADIUS
  )

  message("  Impact zone segments: ", nrow(impact_zone_planned))

  # Add dam metadata
  impact_zone_planned <- impact_zone_planned %>%
    mutate(dam_status = "planned")

  # Combine both scenarios
  impact_zone_all <- bind_rows(
    impact_zone_existing,
    impact_zone_planned
  )

} else {
  message("  No planned dams - skipping")
  impact_zone_all <- impact_zone_existing
}

# Save impact zones
st_write(impact_zone_all,
         "connectivity/impact_zone_by_dam.gpkg",
         delete_dsn = TRUE)

message("  Saved: connectivity/impact_zone_by_dam.gpkg")

# ============================================================
# STEP 6: Calculate affected habitat statistics
# ============================================================

message("\n[6/7] Calculating affected habitat statistics...")

# Total length of basin
total_basin_length <- sum(basin_streams$length, na.rm = TRUE)

# Affected length by existing dams
affected_length_existing <- impact_zone_existing %>%
  st_drop_geometry() %>%
  summarise(total_length = sum(length, na.rm = TRUE)) %>%
  pull(total_length)

# Affected length by all dams (existing + planned)
affected_length_all <- impact_zone_all %>%
  st_drop_geometry() %>%
  summarise(total_length = sum(length, na.rm = TRUE)) %>%
  pull(total_length)

# Calculate proportions
prop_existing <- affected_length_existing / total_basin_length
prop_all <- affected_length_all / total_basin_length

# Create summary
impact_zone_summary <- data.frame(
  basin_id = TARGET_BASIN_ID,
  species = EXAMPLE_SPECIES,
  upstream_radius_m = UPSTREAM_RADIUS,
  downstream_radius_m = DOWNSTREAM_RADIUS,
  total_basin_length_m = total_basin_length,
  n_dams_existing = nrow(dams_existing),
  n_dams_planned = nrow(dams_planned),
  affected_length_existing_m = affected_length_existing,
  affected_length_all_m = affected_length_all,
  proportion_affected_existing = round(prop_existing, 4),
  proportion_affected_all = round(prop_all, 4),
  additional_affected_m = affected_length_all - affected_length_existing,
  additional_proportion = round(prop_all - prop_existing, 4)
)

print(impact_zone_summary)

write.csv(impact_zone_summary,
          "connectivity/impact_zone_summary.csv",
          row.names = FALSE)

message("\nSummary Statistics:")
message("  Total basin length: ", round(total_basin_length / 1000, 1), " km")
message("  Affected by existing dams: ", round(affected_length_existing / 1000, 1),
        " km (", round(100 * prop_existing, 1), "%)")
message("  Affected by all dams: ", round(affected_length_all / 1000, 1),
        " km (", round(100 * prop_all, 1), "%)")
message("  Additional impact of planned dams: ",
        round((affected_length_all - affected_length_existing) / 1000, 1), " km")


# ============================================================
# FINAL SUMMARY
# ============================================================

message("\n=== IMPACT ZONE ANALYSIS COMPLETE ===")
message("Basin: ", TARGET_BASIN_ID)
message("Species: ", EXAMPLE_SPECIES)
message("\nImpact Zone Parameters:")
message("  Upstream radius: ", UPSTREAM_RADIUS, " m")
message("  Downstream radius: ", DOWNSTREAM_RADIUS, " m")
message("\nHabitat Impact:")
message("  Total basin length: ", round(total_basin_length / 1000, 1), " km")
message("  Affected by existing dams: ", round(affected_length_existing / 1000, 1),
        " km (", round(100 * prop_existing, 1), "%)")
message("  Additional impact if planned dams built: ",
        round((affected_length_all - affected_length_existing) / 1000, 1), " km")
message("  Total affected (existing + planned): ", round(affected_length_all / 1000, 1),
        " km (", round(100 * prop_all, 1), "%)")
message("\nOutput Files:")
message("  • connectivity/impact_zone_by_dam.gpkg")
message("  • connectivity/impact_zone_summary.csv")
message("  • connectivity/impact_zone_map.html")
message("\nAnalysis complete: ", Sys.time())
