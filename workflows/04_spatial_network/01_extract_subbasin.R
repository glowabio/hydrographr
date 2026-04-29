#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_extract_subbasin.R
#
# Extract the target subbasin for connectivity and SDM analysis.
# All analysis is scoped to the subbasin (Sarantaporos + Voidomatis),
# defined as all stream segments upstream of the outlet point.
# No full basin download is required.
#
# Workflow:
#   1. Load Greece-wide snapped fish + dams
#   2. Assign basin IDs via api_get_ids() (combined call)
#   3. Extract subbasin polygon and stream network from outlet point
#   4. Filter fish + dams to subbasin subcatchments
#   5. [VISUALISATION] Inspect full network + points, verify outlet
#   6. Prune stream network to reaches with observations + buffer
#   7. [VISUALISATION] Compare full vs pruned network
#   8. Load species checklist + species coverage summary
#   9. Filter fish to target species → SDM training output
#
# Input:
#   - points_snapped/fish/fish_all_species_snapped.csv   (Greece-wide)
#   - points_snapped/dams/dams_snapped_points.csv        (Greece-wide)
#   - range_maps/vjosa_species_checklist_iucn.csv
#
# Output:
#   - points_snapped/all_snapped_with_basins.csv
#   - spatial/subbasin/subbasin_polygon.gpkg
#   - spatial/subbasin/stream_network.gpkg
#   - spatial/subbasin/stream_network_pruned.gpkg
#   - points_snapped/subbasin/subbasin_subc_ids_pruned.csv  (for predict table)
#   - points_snapped/subbasin/fish_subbasin.csv             (connectivity)
#   - points_snapped/subbasin/dams_subbasin.csv             (connectivity)
#   - points_snapped/subbasin/fish_vjosa_species_subbasin.csv (SDM training)
#   - points_snapped/subbasin/species_coverage_summary.csv
#
# LOCATION: workflows/04_spatial_network/01_extract_subbasin.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(sf)
library(data.table)
library(dplyr)
library(leaflet)

select <- dplyr::select

source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

BASIN_ID <- 1292502

# Subbasin outlet point (confluence of Sarantaporos + Voidomatis → Aoos mainstem)
# All stream segments upstream of this point define the subbasin
# Coordinates obtained from https://aqua.igb-berlin.de/upstream-dev/
OUTLET_LON <- 20.5870613
OUTLET_LAT <- 40.0728991

# ============================================================
# STEP 1: Load Greece-wide snapped points
# ============================================================

message("\n=== Step 1: Loading snapped points ===")

fish_snapped <- fread("points_snapped/fish/fish_all_species_snapped.csv")
message("  Fish points loaded: ", nrow(fish_snapped))

dams_snapped <- fread("points_snapped/dams/dams_snapped_points.csv")
message("  Dam points loaded: ", nrow(dams_snapped))

# Harmonise column names
if (!"source" %in% names(dams_snapped)) dams_snapped$source <- "Dams"
if ("id1" %in% names(dams_snapped)) dams_snapped <- dams_snapped %>% rename(site_id = id1)

# Combine for single API call
common_cols <- intersect(names(fish_snapped), names(dams_snapped))
all_snapped <- rbind(
  fish_snapped[, ..common_cols],
  dams_snapped[, ..common_cols]
)

# Deduplicate to unique site locations before API call
# (fish file has multiple rows per site due to multiple species per location)
all_snapped <- all_snapped %>%
  distinct(site_id, longitude_snapped, latitude_snapped, subc_id, source)

message("  Unique site locations: ", nrow(all_snapped))

# Write out and reload via Nimbus URL
# (required for api_get_ids() which reads from a remote CSV)
fwrite(all_snapped, "points_snapped/all_points_snapped_unique_locations.csv")
all_snapped_csv <- "https://nimbus.igb-berlin.de/index.php/s/7Xx9xANQ4rDLkWk/download/all_points_snapped_unique_locations.csv"
all_snapped <- fread(all_snapped_csv)

# ============================================================
# STEP 2: Assign basin IDs
# ============================================================

message("\n=== Step 2: Assigning basin IDs ===")

basin_ids <- api_get_ids(
  points          = all_snapped,
  colname_lon     = "longitude_snapped",
  colname_lat     = "latitude_snapped",
  colname_site_id = "site_id",
  mode            = "local"
)

all_snapped_with_basins <- left_join(all_snapped, basin_ids, by = "site_id")

# Save full Greece-wide file (useful for reference / future reuse)
fwrite(all_snapped_with_basins, "points_snapped/all_snapped_with_basins.csv")
message("  Saved: points_snapped/all_snapped_with_basins.csv")

cat("  Points missing basin_id:", sum(is.na(all_snapped_with_basins$basin_id)), "\n")

# ============================================================
# STEP 3: Extract subbasin polygon and stream network
# ============================================================

message("\n=== Step 3: Extracting subbasin ===")

dir.create("spatial/subbasin", recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/subbasin", recursive = TRUE, showWarnings = FALSE)

# Subbasin polygon upstream of outlet point
subbasin_polygon <- api_get_upstream_catchment(
  lon = OUTLET_LON,
  lat = OUTLET_LAT
)
st_write(subbasin_polygon, "spatial/subbasin/subbasin_polygon.gpkg",
         delete_dsn = TRUE)
save_to_nimbus(subbasin_polygon, "spatial/subbasin/subbasin_polygon.gpkg")
message("  Subbasin polygon saved")

# Full stream network upstream of outlet point
# geometry_only = FALSE ensures subc_id attributes are returned
# Captures both Sarantaporos and Voidomatis tributaries
subbasin_streams <- api_get_stream_segments(
  lon           = OUTLET_LON,
  lat           = OUTLET_LAT,
  upstream      = TRUE,
  geometry_only = FALSE,
  min_strahler  = 2
)

st_write(subbasin_streams, "spatial/subbasin/stream_network.gpkg",
         delete_dsn = TRUE)
save_to_nimbus(subbasin_streams, "spatial/subbasin/stream_network.gpkg")
message("  Subbasin stream segments: ", nrow(subbasin_streams))

# All subcatchment IDs within subbasin — used to filter points
subbasin_subc_ids <- unique(subbasin_streams$subc_id)
message("  Subcatchments in subbasin: ", length(subbasin_subc_ids))


# ============================================================
# STEP 3b: Download full basin stream network
#
# The full Vjosa basin network is used as the SDM training extent.
# Pseudoabsences will be sampled from basin subcatchments, providing
# a broader environmental background than the subbasin alone.
# The network is pruned to reaches where target species were recorded.
# ============================================================

message("\n=== Step 3b: Downloading full basin stream network ===")

dir.create("spatial/basin", recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/basin", recursive = TRUE, showWarnings = FALSE)

basin_streams <- api_get_stream_segments(
  basin_id      = BASIN_ID,
  geometry_only = FALSE,
  min_strahler  = 2
)
basin_streams$basin_id <- BASIN_ID

st_write(basin_streams, "spatial/basin/stream_network.gpkg", delete_dsn = TRUE)
save_to_nimbus(basin_streams, "spatial/basin/stream_network.gpkg")
message("  Basin stream segments: ", nrow(basin_streams))

# All basin subcatchment IDs
basin_subc_ids <- unique(basin_streams$subc_id)
message("  Subcatchments in basin: ", length(basin_subc_ids))

# Fish points within the full basin (target species only)
# Used to drive pruning — retains reaches where SDM species were recorded
fish_basin_species <- fread("points_snapped/fish/fish_all_species_snapped.csv") %>%
  filter(subc_id %in% basin_subc_ids)

# Load species checklist early to filter to target species
# (full checklist load repeated in Step 8 for coverage summary)
vjosa_species_for_pruning <- fread("range_maps/vjosa_species_checklist_iucn.csv") %>%
  filter(basin_id == BASIN_ID) %>%
  pull(species) %>%
  unique()

fish_basin_sdm <- fish_basin_species %>%
  filter(species %in% vjosa_species_for_pruning)

message("  Fish points for basin pruning: ", nrow(fish_basin_sdm),
        " (", n_distinct(fish_basin_sdm$species), " species)")

# Prune basin network to reaches with target species observations
basin_streams_pruned <- extract_partial_stream_network(
  stream                    = basin_streams,
  snapped_subcs             = fish_basin_sdm$subc_id,
  strahler_retain_threshold = MIN_STRAHLER,
  upstream_buffer           = UPSTREAM_BUFFER
)


st_write(basin_streams_pruned, "spatial/basin/stream_network_pruned.gpkg",
         delete_dsn = TRUE)
save_to_nimbus(basin_streams_pruned, "spatial/basin/stream_network_pruned.gpkg")
message("  Pruned basin stream segments: ", nrow(basin_streams_pruned),
        " (from ", nrow(basin_streams), " full basin segments)")

# Save basin pruned subcatchment IDs
# Used for: environmental variable extraction + pseudoabsence sampling
basin_subc_ids_pruned <- unique(basin_streams_pruned$subc_id)
message("  Basin subcatchments after pruning: ", length(basin_subc_ids_pruned),
        " (from ", length(basin_subc_ids), " full basin subcatchments)")

fwrite(
  data.table(subc_id = basin_subc_ids_pruned),
  "points_snapped/basin/basin_subc_ids_pruned.csv"
)



# ============================================================
# STEP 4: Filter fish + dams to subbasin
# ============================================================

message("\n=== Step 4: Filtering points to subbasin ===")

fish_subbasin <- all_snapped_with_basins %>%
  filter(source %in% c("HCMR", "GBIF"), subc_id %in% subbasin_subc_ids)

dams_subbasin <- all_snapped_with_basins %>%
  filter(source %in% c("RAAY", "AMBER"), subc_id %in% subbasin_subc_ids)

message("  Fish points in subbasin: ", nrow(fish_subbasin))
message("  Dams in subbasin: ", nrow(dams_subbasin))
if (nrow(dams_subbasin) > 0) {
  message("  By phase:")
  print(table(dams_subbasin$phase))
}

fwrite(fish_subbasin, "points_snapped/subbasin/fish_subbasin.csv")
fwrite(dams_subbasin, "points_snapped/subbasin/dams_subbasin.csv")

# ============================================================
# STEP 5: Visualise full network + points, verify outlet location
#
# Inspect all snapped fish and dam occurrences within the subbasin
# on the full stream network. The outlet point is marked — verify
# that all points fall within the subbasin boundary before pruning.
# ============================================================

message("\n=== Step 5: Visualising subbasin points ===")

fish_subbasin_sf <- fish_subbasin %>%
  filter(!is.na(longitude_snapped), !is.na(latitude_snapped)) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

dams_subbasin_sf <- dams_subbasin %>%
  filter(!is.na(longitude_snapped), !is.na(latitude_snapped)) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

map_full <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%

  # Subbasin boundary polygon
  addPolygons(
    data        = subbasin_polygon,
    color       = "#525252",
    weight      = 2,
    fillOpacity = 0.05,
    group       = "Subbasin boundary"
  ) %>%

  # Full stream network
  addPolylines(
    data    = subbasin_streams,
    color   = "#6baed6",
    weight  = 1.5,
    opacity = 0.8,
    group   = "Stream network"
  ) %>%

  # Fish points — blue circles
  addCircleMarkers(
    data        = fish_subbasin_sf,
    radius      = 4,
    color       = "#2166ac",
    fillColor   = "#2166ac",
    fillOpacity = 0.7,
    stroke      = FALSE,
    label       = ~paste0(site_id, " | ", source),
    group       = "Fish"
  ) %>%

  # Dam points — red circles
  addCircleMarkers(
    data        = dams_subbasin_sf,
    radius      = 5,
    color       = "#d73027",
    fillColor   = "#d73027",
    fillOpacity = 0.8,
    stroke      = FALSE,
    label       = ~site_id,
    group       = "Dams"
  ) %>%

  # Outlet point — labelled marker
  addMarkers(
    lng   = OUTLET_LON,
    lat   = OUTLET_LAT,
    label = "Subbasin outlet — all points upstream of this location"
  ) %>%

  addLayersControl(
    overlayGroups = c("Subbasin boundary", "Stream network", "Fish", "Dams"),
    options       = layersControlOptions(collapsed = FALSE)
  ) %>%

  addLegend(
    position = "bottomright",
    colors   = c("#2166ac", "#d73027", "#6baed6"),
    labels   = c("Fish occurrences", "Dams", "Stream network"),
    title    = "Subbasin — full network"
  )

print(map_full)
# Verify that all points fall within the subbasin boundary before proceeding

# ============================================================
# STEP 6: Prune stream network
#
# Retain only reaches where fish or dams were recorded, plus a small
# upstream buffer. This removes uninformative headwater segments and
# reduces the size of the predict table.
# The pruned network subcatchment IDs are saved for use in the
# predict table construction script.
# ============================================================

# Minimum Strahler order for network pruning
MIN_STRAHLER <- 4
UPSTREAM_BUFFER <- 3


message("\n=== Step 6: Pruning stream network ===")

fish_subbasin <- fread("points_snapped/subbasin/fish_subbasin.csv")
dams_subbasin <- fread("points_snapped/subbasin/dams_subbasin.csv")


subbasin_streams_pruned <- extract_partial_stream_network(
  stream                    = subbasin_streams,
  snapped_subcs             = c(fish_subbasin$subc_id, dams_subbasin$subc_id),
  strahler_retain_threshold = MIN_STRAHLER,
  upstream_buffer           = UPSTREAM_BUFFER
)


st_write(subbasin_streams_pruned, "spatial/subbasin/stream_network_pruned.gpkg",
         delete_dsn = TRUE)
save_to_nimbus(subbasin_streams_pruned, "spatial/subbasin/stream_network_pruned.gpkg")
message("  Pruned stream segments: ", nrow(subbasin_streams_pruned),
        " (from ", nrow(subbasin_streams), " full network segments)")

# Save pruned subcatchment IDs — used by the predict table script
subbasin_subc_ids_pruned <- unique(subbasin_streams_pruned$subc_id)
message("  Subcatchments after pruning: ", length(subbasin_subc_ids_pruned),
        " (from ", length(subbasin_subc_ids), " full subbasin subcatchments)")

fwrite(
  data.table(subc_id = subbasin_subc_ids_pruned),
  "points_snapped/subbasin/subbasin_subc_ids_pruned.csv"
)

# ============================================================
# STEP 7: Visualise full vs pruned network
#
# Compare the full upstream network with the pruned network to verify
# that pruning retains all ecologically relevant reaches while removing
# uninformative headwater segments.
# ============================================================

message("\n=== Step 7: Comparing full vs pruned stream network ===")

subbasin_polygon <- st_read("spatial/subbasin/subbasin_polygon.gpkg")

map_pruning <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%

  # Subbasin boundary
  addPolygons(
    data        = subbasin_polygon,
    color       = "#525252",
    weight      = 2,
    fillOpacity = 0.05,
    group       = "Subbasin boundary"
  ) %>%

  # Full network — light grey
  addPolylines(
    data    = subbasin_streams,
    color   = "#bdbdbd",
    weight  = 1,
    opacity = 0.8,
    group   = "Full network"
  ) %>%

  # Pruned network — blue
  addPolylines(
    data    = subbasin_streams_pruned,
    color   = "#2166ac",
    weight  = 2,
    opacity = 0.9,
    group   = "Pruned network"
  ) %>%

  # Fish points — to verify all occurrences fall on pruned network
  addCircleMarkers(
    data        = fish_subbasin_sf,
    radius      = 4,
    color       = "#e6550d",
    fillColor   = "#e6550d",
    fillOpacity = 0.7,
    stroke      = FALSE,
    label       = ~paste0(site_id, " | ", source),
    group       = "Fish"
  ) %>%

  # Outlet point
  addMarkers(
    lng   = OUTLET_LON,
    lat   = OUTLET_LAT,
    label = "Subbasin outlet"
  ) %>%

  addLayersControl(
    overlayGroups = c("Subbasin boundary", "Full network", "Pruned network", "Fish"),
    options       = layersControlOptions(collapsed = FALSE)
  ) %>%

  addLegend(
    position = "bottomright",
    colors   = c("#bdbdbd", "#2166ac", "#e6550d"),
    labels   = c("Full network", "Pruned network", "Fish occurrences"),
    title    = "Stream network — full vs pruned"
  )

print(map_pruning)
# Verify that all fish occurrence points lie on the pruned network —
# if any point falls only on the grey (full) network, pruning has
# incorrectly removed a reach with observations

# ============================================================
# STEP 8: Species checklist and coverage summary
# ============================================================

message("\n=== Step 8: Species checklist and coverage summary ===")

vjosa_species <- fread("range_maps/vjosa_species_checklist_iucn.csv") %>%
  filter(basin_id == BASIN_ID) %>%
  pull(species) %>%
  unique()

message("  Species expected in basin (IUCN): ", length(vjosa_species))

# Join species info back from full fish file for coverage summary
# (fish_subbasin from all_snapped_with_basins has no species column)
fish_subbasin_species <- fread("points_snapped/fish/fish_all_species_snapped.csv") %>%
  filter(subc_id %in% subbasin_subc_ids)

species_summary <- data.frame(species = vjosa_species) %>%
  mutate(
    in_subbasin    = species %in% unique(fish_subbasin_species$species),
    n_occ_subbasin = sapply(species, function(sp)
      sum(fish_subbasin_species$species == sp))
  ) %>%
  arrange(desc(n_occ_subbasin))

fwrite(species_summary, "points_snapped/subbasin/species_coverage_summary.csv")

cat("\nSpecies coverage:\n")
cat("  IUCN expected in basin:        ", length(vjosa_species), "\n")
cat("  With occurrences in subbasin:  ", sum(species_summary$in_subbasin), "\n")
cat("  IUCN-only (no occurrences):    ", sum(!species_summary$in_subbasin), "\n")

unexpected <- unique(fish_subbasin_species$species)
unexpected <- unexpected[!unexpected %in% vjosa_species]
if (length(unexpected) > 0) {
  cat("\n  Unexpected species in subbasin (not in IUCN list):\n")
  cat("  ", paste(unexpected, collapse = ", "), "\n")
}

# ============================================================
# STEP 9: Filter fish to target species → SDM training output
# ============================================================

message("\n=== Step 9: Filtering to target species for SDM ===")

# Retain only IUCN-listed Vjosa species from the full species file
# Note: includes both HCMR and GBIF records within the subbasin
# GBIF records outside the subbasin are excluded — endemic species
# have no reliable records beyond the Aoos/Vjosa basin
fish_sdm <- fish_subbasin_species %>%
  filter(species %in% vjosa_species)

fwrite(fish_sdm, "points_snapped/subbasin/fish_vjosa_species_subbasin.csv")
message("  SDM training occurrences: ", nrow(fish_sdm),
        " (", n_distinct(fish_sdm$species), " species)")

# ============================================================
# SUMMARY
# ============================================================

message("\n=== Subbasin Extraction Complete ===")
message("\nOutputs:")
message("  points_snapped/all_snapped_with_basins.csv")
message("  spatial/subbasin/subbasin_polygon.gpkg")
message("  spatial/subbasin/stream_network.gpkg")
message("  spatial/subbasin/stream_network_pruned.gpkg")
message("  points_snapped/subbasin/subbasin_subc_ids_pruned.csv       (predict table)")
message("  points_snapped/subbasin/fish_vjosa_species_subbasin.csv    (SDM training)")
message("  points_snapped/subbasin/fish_subbasin.csv                  (connectivity)")
message("  points_snapped/subbasin/dams_subbasin.csv                  (connectivity)")
message("  points_snapped/subbasin/species_coverage_summary.csv")
message("\nNext steps:")
message("  SDM:          use fish_vjosa_species_subbasin.csv for training,")
message("                predict on subbasin_subc_ids_pruned subcatchments")
message("  Connectivity: use fish_subbasin.csv + dams_subbasin.csv")
message("                with 02_generate_network_graph.R")
message("  spatial/basin/stream_network.gpkg")
message("  spatial/basin/stream_network_pruned.gpkg")
message("  points_snapped/basin/basin_subc_ids_pruned.csv          (SDM pseudoabsences)")
