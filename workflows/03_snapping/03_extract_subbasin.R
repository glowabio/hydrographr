#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_extract_subbasin.R
#
# Extract the target basin and subbasin networks for connectivity and
# SDM analysis.
#
# Training extent:   full Vjosa/Aoos basin (SDM model fitting + pseudoabsences)
# Prediction extent: subbasin upstream of outlet (Sarantaporos + Voidomatis)
#
# Workflow:
#   1. Load basin-filtered snapped fish + dams
#   2. Download basin polygon + stream network + prune  (SDM training extent)
#   3. Extract subbasin polygon + stream network + prune (SDM prediction extent)
#   4. Filter fish + dams to subbasin subcatchments
#   5. [VISUALISATION] Basin + subbasin networks + points
#   6. [VISUALISATION] Full vs pruned subbasin network
#   7. Filter fish to target species → SDM outputs
#
# Input:
#   - points_snapped/fish/fish_all_species_snapped.csv
#   - points_snapped/dams/dams_snapped_points.csv
#   - config/study_area_params.csv
#   - points_original/fish/species_list_sarantaporos.txt
#
# Output:
#   - spatial/basin/basin_polygon.gpkg
#   - spatial/basin/stream_network.gpkg
#   - spatial/basin/stream_network_pruned.gpkg
#   - spatial/basin/basin_subc_ids_pruned.csv
#   - spatial/subbasin/subbasin_polygon.gpkg
#   - spatial/subbasin/stream_network.gpkg
#   - spatial/subbasin/stream_network_pruned.gpkg
#   - spatial/subbasin/subbasin_subc_ids_pruned.csv
#   - points_snapped/subbasin/fish_subbasin.csv
#   - points_snapped/subbasin/dams_subbasin.csv
#   - points_snapped/subbasin/fish_sdm_basin.csv
#   - points_snapped/subbasin/fish_sdm_subbasin.csv
#
# LOCATION: workflows/03_snapping/03_extract_subbasin.R
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

# Load basin ID derived in 01_clean_hcmr_fish.R
study_params <- fread("config/study_area_params.csv")
BASIN_ID <- study_params[param == "BASIN_ID", as.integer(value)]
message("  Basin ID: ", BASIN_ID)

# Subbasin outlet point (confluence of Sarantaporos + Voidomatis → Aoos mainstem)
# Coordinates obtained from https://aqua.igb-berlin.de/upstream-dev/
OUTLET_LON <- 20.5870613
OUTLET_LAT <- 40.0728991

# Network pruning parameters — applied to both basin and subbasin
MIN_STRAHLER    <- 4
UPSTREAM_BUFFER <- 3

# Target species for SDM
target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>%
  unique()

message("  Target species: ", length(target_species))

# ============================================================
# STEP 1: Load basin-filtered snapped points
# ============================================================

message("\n=== Step 1: Loading snapped points ===")

# Both files are already basin-filtered by the cleaning scripts
fish_snapped <- fread("points_snapped/fish/fish_all_species_snapped.csv")
message("  Fish records loaded: ", nrow(fish_snapped))

dams_snapped <- fread("points_snapped/dams/dams_snapped_points.csv")
if ("id1" %in% names(dams_snapped)) dams_snapped <- dams_snapped %>% rename(site_id = id1)
message("  Dam records loaded: ", nrow(dams_snapped))

# Deduplicate to unique site locations
# fish file has multiple rows per site (one per species)
fish_unique <- fish_snapped %>%
  distinct(site_id, longitude_snapped, latitude_snapped, subc_id, source)

dams_unique <- dams_snapped %>%
  distinct(site_id, longitude_snapped, latitude_snapped, subc_id, source)

message("  Unique fish sites: ", nrow(fish_unique))
message("  Unique dam sites:  ", nrow(dams_unique))

# ============================================================
# STEP 2: Download basin polygon + stream network + prune
# (SDM training extent)
# ============================================================

message("\n=== Step 2: Downloading basin network (SDM training extent) ===")

dir.create("spatial/basin",        recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/basin", recursive = TRUE, showWarnings = FALSE)

# Basin polygon
basin_polygon <- api_get_basin_polygon(basin_id = BASIN_ID)
st_write(basin_polygon, "spatial/basin/basin_polygon.gpkg", delete_dsn = TRUE)
save_to_nimbus(basin_polygon, "spatial/basin/basin_polygon.gpkg")
message("  Basin polygon saved")

# Full basin stream network
basin_streams <- api_get_stream_segments(
  basin_id      = BASIN_ID,
  geometry_only = FALSE,
  min_strahler  = 2
)
basin_streams$basin_id <- BASIN_ID



st_write(basin_streams, "spatial/basin/stream_network.gpkg", delete_dsn = TRUE)
save_to_nimbus(basin_streams, "spatial/basin/stream_network.gpkg")

basin_subc_ids <- unique(basin_streams$subc_id)
message("  Basin stream segments: ", nrow(basin_streams),
        " | subcatchments: ", length(basin_subc_ids))

# Prune basin network based on all fish + dam points in basin
basin_streams_pruned <- extract_partial_stream_network(
  stream                    = basin_streams,
  snapped_subcs             = c(fish_unique$subc_id, dams_unique$subc_id),
  strahler_retain_threshold = MIN_STRAHLER,
  upstream_buffer           = UPSTREAM_BUFFER
)


# Deduplicate by subc_id — keep first occurrence
n_before <- nrow(basin_streams_pruned)
basin_streams_pruned <- basin_streams_pruned %>%
  distinct(subc_id, .keep_all = TRUE)
n_after <- nrow(basin_streams_pruned)

if (n_before > n_after) {
  message("  Removed ", n_before - n_after,
          " duplicate edges from pruned network")
}

st_write(basin_streams_pruned, "spatial/basin/stream_network_pruned.gpkg",
         delete_dsn = TRUE)
save_to_nimbus(basin_streams_pruned, "spatial/basin/stream_network_pruned.gpkg")

basin_subc_ids_pruned <- unique(basin_streams_pruned$subc_id)
message("  Pruned basin segments: ", nrow(basin_streams_pruned),
        " | subcatchments: ", length(basin_subc_ids_pruned))

fwrite(data.table(subc_id = basin_subc_ids_pruned),
       "spatial/basin/basin_subc_ids_pruned.csv")
message("  Saved: spatial/basin/basin_subc_ids_pruned.csv")

# ============================================================
# STEP 3: Extract subbasin polygon + stream network + prune
# (SDM prediction extent + connectivity analysis)
# ============================================================

message("\n=== Step 3: Extracting subbasin (SDM prediction extent) ===")

dir.create("spatial/subbasin",        recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/subbasin", recursive = TRUE, showWarnings = FALSE)

# Subbasin polygon
subbasin_polygon <- api_get_upstream_catchment(lon = OUTLET_LON, lat = OUTLET_LAT)
st_write(subbasin_polygon, "spatial/subbasin/subbasin_polygon.gpkg", delete_dsn = TRUE)
save_to_nimbus(subbasin_polygon, "spatial/subbasin/subbasin_polygon.gpkg")
message("  Subbasin polygon saved")

# Full subbasin stream network
# add_target_streams = TRUE ensures target column is returned (needed for pruning)
subbasin_streams <- api_get_stream_segments(
  lon                = OUTLET_LON,
  lat                = OUTLET_LAT,
  upstream           = TRUE,
  geometry_only      = FALSE,
  min_strahler       = 2
)


st_write(subbasin_streams, "spatial/subbasin/stream_network.gpkg", delete_dsn = TRUE)
save_to_nimbus(subbasin_streams, "spatial/subbasin/stream_network.gpkg")

subbasin_subc_ids <- unique(subbasin_streams$subc_id)
message("  Subbasin stream segments: ", nrow(subbasin_streams),
        " | subcatchments: ", length(subbasin_subc_ids))

# Filter points to subbasin before pruning
# (pruning is based on which reaches have observations)
# fish_subbasin_unique <- fish_unique %>% filter(subc_id %in% subbasin_subc_ids)
# dams_subbasin_unique <- dams_unique %>% filter(subc_id %in% subbasin_subc_ids)

# Prune subbasin network
subbasin_streams_pruned <- extract_partial_stream_network(
  stream                    = subbasin_streams,
  snapped_subcs             = c(fish_unique$subc_id, dams_unique$subc_id),
  strahler_retain_threshold = MIN_STRAHLER,
  upstream_buffer           = UPSTREAM_BUFFER
)

# Deduplicate by subc_id — keep first occurrence
n_before <- nrow(subbasin_streams_pruned)
subbasin_streams_pruned <- subbasin_streams_pruned %>%
  distinct(subc_id, .keep_all = TRUE)
n_after <- nrow(subbasin_streams_pruned)

if (n_before > n_after) {
  message("  Removed ", n_before - n_after,
          " duplicate edges from pruned network")
}

# save the clean version
st_write(subbasin_streams_pruned,
         "spatial/subbasin/stream_network_pruned.gpkg",
         delete_dsn = TRUE)

save_to_nimbus(subbasin_streams_pruned, "spatial/subbasin/stream_network_pruned.gpkg")

subbasin_subc_ids_pruned <- unique(subbasin_streams_pruned$subc_id)
message("  Pruned subbasin segments: ", nrow(subbasin_streams_pruned),
        " | subcatchments: ", length(subbasin_subc_ids_pruned))

fwrite(data.table(subc_id = subbasin_subc_ids_pruned),
       "spatial/subbasin/subbasin_subc_ids_pruned.csv")
message("  Saved: spatial/subbasin/subbasin_subc_ids_pruned.csv")

# ============================================================
# STEP 4: Filter fish + dams to subbasin
# ============================================================

message("\n=== Step 4: Filtering points to subbasin ===")

# Use full species file (with species column) for subbasin fish
fish_subbasin <- fish_snapped %>%
  filter(subc_id %in% subbasin_subc_ids)

dams_subbasin <- dams_snapped %>%
  filter(subc_id %in% subbasin_subc_ids)

message("  Fish records in subbasin: ", nrow(fish_subbasin),
        " (", n_distinct(fish_subbasin$species), " species)")
message("  Dams in subbasin: ", nrow(dams_subbasin))
if (nrow(dams_subbasin) > 0) {
  message("  Dams by phase:")
  print(table(dams_subbasin$phase))
}

fwrite(fish_subbasin, "points_snapped/subbasin/fish_subbasin.csv")
fwrite(dams_subbasin, "points_snapped/subbasin/dams_subbasin.csv")

# sf versions for visualisation
fish_subbasin_sf <- fish_subbasin_unique %>%
  filter(!is.na(longitude_snapped), !is.na(latitude_snapped)) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

dams_subbasin_sf <- dams_subbasin_unique %>%
  filter(!is.na(longitude_snapped), !is.na(latitude_snapped)) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

# ============================================================
# STEP 5: Visualise basin + subbasin networks + points
#
# Grey = full basin network (training extent)
# Blue = subbasin network (prediction extent)
# All fish + dam points should fall on the blue subbasin network.
# ============================================================

message("\n=== Step 5: Visualising basin + subbasin ===")

map_basin_subbasin <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%

  addPolygons(
    data = subbasin_polygon, color = "#2166ac", weight = 2,
    fillOpacity = 0.05, group = "Subbasin boundary"
  ) %>%

  addPolylines(
    data = basin_streams, color = "#bdbdbd", weight = 1,
    opacity = 0.7, group = "Basin network"
  ) %>%

  addPolylines(
    data = subbasin_streams, color = "#2166ac", weight = 2,
    opacity = 0.9, group = "Subbasin network"
  ) %>%

  addCircleMarkers(
    data = fish_subbasin_sf, radius = 4,
    color = "#e6550d", fillColor = "#e6550d", fillOpacity = 0.8,
    stroke = FALSE, label = ~paste0(site_id, " | ", source),
    group = "Fish"
  ) %>%

  addCircleMarkers(
    data = dams_subbasin_sf, radius = 5,
    color = "#d73027", fillColor = "#d73027", fillOpacity = 0.8,
    stroke = FALSE, label = ~site_id, group = "Dams"
  ) %>%

  addMarkers(
    lng = OUTLET_LON, lat = OUTLET_LAT,
    label = "Subbasin outlet"
  ) %>%

  addLayersControl(
    overlayGroups = c("Subbasin boundary", "Basin network",
                      "Subbasin network", "Fish", "Dams"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  addLegend(
    position = "bottomright",
    colors   = c("#bdbdbd", "#2166ac", "#e6550d", "#d73027"),
    labels   = c("Basin network", "Subbasin network",
                 "Fish occurrences", "Dams"),
    title    = "Basin and subbasin"
  )

print(map_basin_subbasin)
# Verify: all points should fall on the blue subbasin network
# Grey-only reaches are basin segments outside the subbasin — expected

# ============================================================
# STEP 6: Visualise full vs pruned subbasin network
#
# Grey = full subbasin network
# Blue = pruned network
# All fish points must fall on the blue pruned network.
# Any point on grey only = pruning error.
# ============================================================

message("\n=== Step 6: Comparing full vs pruned subbasin network ===")

map_pruning <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%

  addPolygons(
    data = subbasin_polygon, color = "#525252", weight = 2,
    fillOpacity = 0.05, group = "Subbasin boundary"
  ) %>%

  addPolylines(
    data = subbasin_streams, color = "#bdbdbd", weight = 1,
    opacity = 0.8, group = "Full subbasin network"
  ) %>%

  addPolylines(
    data = subbasin_streams_pruned, color = "#2166ac", weight = 2,
    opacity = 0.9, group = "Pruned network"
  ) %>%

  addCircleMarkers(
    data = fish_subbasin_sf, radius = 4,
    color = "#e6550d", fillColor = "#e6550d", fillOpacity = 0.8,
    stroke = FALSE, label = ~paste0(site_id, " | ", source),
    group = "Fish"
  ) %>%

  addMarkers(
    lng = OUTLET_LON, lat = OUTLET_LAT,
    label = "Subbasin outlet"
  ) %>%

  addLayersControl(
    overlayGroups = c("Subbasin boundary", "Full subbasin network",
                      "Pruned network", "Fish"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  addLegend(
    position = "bottomright",
    colors   = c("#bdbdbd", "#2166ac", "#e6550d"),
    labels   = c("Full subbasin network", "Pruned network", "Fish occurrences"),
    title    = "Subbasin — full vs pruned"
  )

print(map_pruning)

# ============================================================
# STEP 7: Filter fish to target species → SDM outputs
# ============================================================

message("\n=== Step 7: Filtering to target species for SDM ===")

# SDM training: target species occurrences across full basin
fish_sdm_basin <- fish_snapped %>%
  filter(species %in% target_species) %>%
  select(-genus, -family, -order)

fish_sdm_basin <- fish_sdm_basin %>%
  mutate(year = ifelse(source == "HCMR", 2024, NA))

fwrite(fish_sdm_basin, "points_snapped/basin/fish_sdm_basin.csv")
message("  Basin SDM training records: ", nrow(fish_sdm_basin),
        " (", n_distinct(fish_sdm_basin$species), " species)")

# SDM prediction check: target species occurrences within subbasin
fish_sdm_subbasin <- fish_subbasin %>%
  filter(species %in% target_species)

fwrite(fish_sdm_subbasin, "points_snapped/subbasin/fish_sdm_subbasin.csv")
message("  Subbasin SDM records: ", nrow(fish_sdm_subbasin),
        " (", n_distinct(fish_sdm_subbasin$species), " species)")

# Coverage summary
species_coverage <- data.frame(species = target_species) %>%
  mutate(
    n_basin    = sapply(species, function(sp) sum(fish_sdm_basin$species == sp)),
    n_subbasin = sapply(species, function(sp) sum(fish_sdm_subbasin$species == sp)),
    in_basin    = n_basin > 0,
    in_subbasin = n_subbasin > 0
  ) %>%
  arrange(desc(n_basin))

fwrite(species_coverage, "points_snapped/subbasin/species_coverage_summary.csv")

cat("\nSpecies coverage:\n")
cat("  Target species:               ", length(target_species), "\n")
cat("  With records in basin:        ", sum(species_coverage$in_basin), "\n")
cat("  With records in subbasin:     ", sum(species_coverage$in_subbasin), "\n")
cat("  No records anywhere:          ",
    sum(!species_coverage$in_basin), "\n")
print(species_coverage)

# ============================================================
# SUMMARY
# ============================================================

message("\n=== Extraction Complete ===")
message("\nBasin (training extent):")
message("  spatial/basin/basin_polygon.gpkg")
message("  spatial/basin/stream_network.gpkg")
message("  spatial/basin/stream_network_pruned.gpkg")
message("  spatial/basin/basin_subc_ids_pruned.csv")
message("  points_snapped/basin/fish_sdm_basin.csv")
message("\nSubbasin (prediction extent + connectivity):")
message("  spatial/subbasin/subbasin_polygon.gpkg")
message("  spatial/subbasin/stream_network.gpkg")
message("  spatial/subbasin/stream_network_pruned.gpkg")
message("  spatial/subbasin/subbasin_subc_ids_pruned.csv")
message("  points_snapped/subbasin/fish_subbasin.csv")
message("  points_snapped/subbasin/dams_subbasin.csv")
message("  points_snapped/subbasin/fish_sdm_subbasin.csv")
message("  points_snapped/subbasin/species_coverage_summary.csv")
message("\nNext steps:")
message("  SDM: download env vars for basin_subc_ids_pruned,")
message("       predict on subbasin_subc_ids_pruned")
message("  Connectivity: use fish_subbasin.csv + dams_subbasin.csv")
