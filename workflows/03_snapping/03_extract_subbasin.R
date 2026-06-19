#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03_extract_subbasin.R
#
# Extract the target basin and subbasin networks for connectivity and
# SDM analysis.
#
# Training extent:   full Vjosa/Aoos basin (SDM model fitting + pseudoabsences)
# Prediction extent: Sarantaporos sub-basin only (SDM predictions +
#                    connectivity + fragmentation analysis)
#
# Note on extents:
#   SDM models are trained on the full Vjosa/Aoos basin to provide a broad
#   environmental space for fitting and a large pseudoabsence pool. All
#   predictions, however, are generated for the Sarantaporos sub-basin only,
#   which is the sub-basin with the highest planned hydropower development
#   pressure and where field data are concentrated. Using a single prediction
#   extent (Sarantaporos) throughout keeps the SDM, connectivity and
#   fragmentation analyses spatially consistent. The outlet point is placed at
#   the confluence of Sarantaporos into Voidomatis, so the extracted catchment
#   is the Sarantaporos drainage only.
#
# Workflow:
#   1. Load basin-filtered snapped fish + dams
#   2. Download basin polygon + stream network + prune  (SDM training extent)
#   3. Extract Sarantaporos subbasin polygon + stream network + prune
#      (SDM prediction extent + connectivity analysis)
#   4. Filter fish + dams to subbasin subcatchments
#   5. [VISUALISATION] Basin + subbasin networks + points
#   6. [VISUALISATION] Full vs pruned subbasin network
#   7. Filter fish to target species -> SDM outputs
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
#   - spatial/subbasin_sarantaporos/subbasin_polygon.gpkg
#   - spatial/subbasin_sarantaporos/stream_network.gpkg
#   - spatial/subbasin_sarantaporos/stream_network_pruned.gpkg
#   - spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv
#   - points_snapped/subbasin_sarantaporos/fish_subbasin.csv
#   - points_snapped/subbasin_sarantaporos/dams_subbasin.csv
#   - points_snapped/basin/fish_sdm_basin.csv
#   - points_snapped/subbasin_sarantaporos/fish_sdm_subbasin.csv
#   - points_snapped/subbasin_sarantaporos/species_coverage_summary.csv
#
# LOCATION: workflows/03_snapping/03_extract_subbasin.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(sf)
library(data.table)
library(dplyr)
library(leaflet)
library(ggplot2)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

# Load basin ID derived in 01_clean_hcmr_fish.R
study_params <- fread("config/study_area_params.csv")
BASIN_ID <- study_params[param == "BASIN_ID", as.integer(value)]
message("  Basin ID: ", BASIN_ID)

# Subbasin outlet point = confluence of Sarantaporos into Voidomatis.
# This delineates the Sarantaporos drainage only (Voidomatis excluded).
# Coordinates obtained from https://aqua.igb-berlin.de/upstream-dev/
OUTLET_LON <- 20.592
OUTLET_LAT <- 40.071


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
  distinct(site_id, longitude_snapped, latitude_snapped, subc_id)

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
message("  Basin polygon saved")

# Full basin stream network
basin_streams <- api_get_stream_segments(
  basin_id      = BASIN_ID,
  upstream = FALSE,
  geometry_only = FALSE,
  min_strahler  = 2
)
basin_streams$basin_id <- BASIN_ID

st_write(basin_streams, "spatial/basin/stream_network.gpkg", delete_dsn = TRUE)

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

basin_subc_ids_pruned <- unique(basin_streams_pruned$subc_id)
message("  Pruned basin segments: ", nrow(basin_streams_pruned),
        " | subcatchments: ", length(basin_subc_ids_pruned))

fwrite(data.table(subc_id = basin_subc_ids_pruned),
       "spatial/basin/basin_subc_ids_pruned.csv")
message("  Saved: spatial/basin/basin_subc_ids_pruned.csv")

# ============================================================
# STEP 3: Extract Sarantaporos subbasin polygon + stream network + prune
# (SDM prediction extent + connectivity analysis)
#
# Outlet = confluence of Sarantaporos into Voidomatis, so the extracted
# upstream catchment is the Sarantaporos drainage only.
# ============================================================

message("\n=== Step 3: Extracting Sarantaporos subbasin (prediction extent) ===")
message("  Outlet: lon = ", OUTLET_LON, " | lat = ", OUTLET_LAT)
message("  (Confluence of Sarantaporos into Voidomatis)")

dir.create("spatial/subbasin_sarantaporos",        recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/subbasin_sarantaporos", recursive = TRUE, showWarnings = FALSE)

# Subbasin polygon — Sarantaporos drainage only
subbasin_polygon <- api_get_upstream_catchment(lon = OUTLET_LON, lat = OUTLET_LAT,)
st_write(subbasin_polygon, "spatial/subbasin_sarantaporos/subbasin_polygon.gpkg", delete_dsn = TRUE)
message("  Subbasin polygon saved | Area: ",
        round(as.numeric(st_area(subbasin_polygon)) / 1e6, 1), " km2")

# Full subbasin stream network
subbasin_streams <- api_get_stream_segments(
  lon           = OUTLET_LON,
  lat           = OUTLET_LAT,
  upstream      = TRUE,
  geometry_only = FALSE,
  min_strahler  = 2
)

st_write(subbasin_streams, "spatial/subbasin_sarantaporos/stream_network.gpkg", delete_dsn = TRUE)

subbasin_subc_ids <- unique(subbasin_streams$subc_id)
message("  Subbasin stream segments: ", nrow(subbasin_streams),
        " | subcatchments: ", length(subbasin_subc_ids))

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
         "spatial/subbasin_sarantaporos/stream_network_pruned.gpkg",
         delete_dsn = TRUE)

subbasin_subc_ids_pruned <- unique(subbasin_streams_pruned$subc_id)
message("  Pruned subbasin segments: ", nrow(subbasin_streams_pruned),
        " | subcatchments: ", length(subbasin_subc_ids_pruned))

fwrite(data.table(subc_id = subbasin_subc_ids_pruned),
       "spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv")
message("  Saved: spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv")

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

fwrite(fish_subbasin, "points_snapped/subbasin_sarantaporos/fish_subbasin.csv")
fwrite(dams_subbasin, "points_snapped/subbasin_sarantaporos/dams_subbasin.csv")

# unique-site versions for visualisation
fish_subbasin_unique <- fish_unique %>%
  filter(subc_id %in% subbasin_subc_ids)
dams_subbasin_unique <- dams_unique %>%
  filter(subc_id %in% subbasin_subc_ids)

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

fwrite(fish_sdm_subbasin, "points_snapped/subbasin_sarantaporos/fish_sdm_subbasin.csv")
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

fwrite(species_coverage, "points_snapped/subbasin_sarantaporos/species_coverage_summary.csv")

cat("\nSpecies coverage:\n")
cat("  Target species:               ", length(target_species), "\n")
cat("  With records in basin:        ", sum(species_coverage$in_basin), "\n")
cat("  With records in subbasin:     ", sum(species_coverage$in_subbasin), "\n")
cat("  No records anywhere:          ",
    sum(!species_coverage$in_basin), "\n")
print(species_coverage)






####### Visualisation of pruning



# Outlet as sf point (for consistent CRS handling)
outlet_sf <- st_sf(
  geometry = st_sfc(st_point(c(OUTLET_LON, OUTLET_LAT)), crs = 4326)
)

library(patchwork)

# --- MAIN: Sarantaporos sub-basin only ---
p_main <- ggplot() +
  geom_sf(data = subbasin_polygon, colour = "#525252",
          linewidth = 0.6, fill = "#525252", alpha = 0.05) +
  geom_sf(data = subbasin_streams, colour = "#bdbdbd",
          linewidth = 0.3, alpha = 0.8) +
  geom_sf(data = subbasin_streams_pruned, colour = "#2166ac",
          linewidth = 0.7, alpha = 0.9) +
  geom_sf(data = fish_subbasin_sf, colour = "#e6550d",
          size = 1.8, alpha = 0.8) +
  geom_sf(data = outlet_sf, shape = 17, size = 4, colour = "#2166ac",
          stroke = 0) +
  geom_point(data = fish_subbasin,
             aes(longitude_snapped, latitude_snapped), size = 2, colour = "black") +
  geom_point(data = dams_subbasin,
             aes(longitude_snapped, latitude_snapped), shape = 17, size = 2, colour = "red") +
  geom_point(data = data.frame(lon = OUTLET_LON, lat = OUTLET_LAT),
             aes(lon, lat), shape = 60, size = 6, colour = "#2166ac") +
  # zoom to the subbasin extent
  coord_sf(xlim = st_bbox(subbasin_polygon)[c("xmin", "xmax")],
           ylim = st_bbox(subbasin_polygon)[c("ymin", "ymax")],
           expand = TRUE) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),          # remove the checkered grid
        panel.background = element_rect(fill = "white", colour = NA),
        axis.title = element_blank())+
 # Scale bar + north arrow
  annotation_scale(
    location = "br", width_hint = 0.25,
    text_col = "grey10", line_col = "grey10",
    bar_cols = c("grey10", "white")
  ) +
    annotation_north_arrow(
      location = "br",
      pad_x    = unit(0.05, "in"),
      pad_y    = unit(0.20, "in"),
      style    = north_arrow_fancy_orienteering(
        fill     = c("grey10", "white"),
        line_col = "grey10",
        text_col = "grey10"
      )
    )
p_main

# --- INSET: whole basin, subbasin highlighted ---
p_inset <- ggplot() +
  geom_sf(data = basin_polygon, colour = "#525252",
          linewidth = 0.3, fill = "grey95") +
  geom_sf(data = subbasin_polygon, colour = "#2166ac",
          linewidth = 0.5, fill = "#2166ac", alpha = 0.25) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white", colour = "grey70"),
        panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 0.4))


p_final <- p_main +
  inset_element(p_inset,
                left = 0.0, bottom = 0.68, right = 0.32, top = 1.2,
                align_to = "panel")


p_final

png("figures/sarantaporos_map_prunning.png", width = 9, height = 8, units = "in", res = 200)
print(p_final); dev.off()


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
message("\nSubbasin = Sarantaporos (prediction extent + connectivity):")
message("  spatial/subbasin_sarantaporos/subbasin_polygon.gpkg")
message("  spatial/subbasin_sarantaporos/stream_network.gpkg")
message("  spatial/subbasin_sarantaporos/stream_network_pruned.gpkg")
message("  spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv")
message("  points_snapped/subbasin_sarantaporos/fish_subbasin.csv")
message("  points_snapped/subbasin_sarantaporos/dams_subbasin.csv")
message("  points_snapped/subbasin_sarantaporos/fish_sdm_subbasin.csv")
message("  points_snapped/subbasin_sarantaporos/species_coverage_summary.csv")
message("\nNext steps:")
message("  SDM: download env vars for basin_subc_ids_pruned,")
message("       predict on subbasin_subc_ids_pruned")
message("  Connectivity: use fish_subbasin.csv + dams_subbasin.csv")
