#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 04_spatial_network/03_overlay_with_basin_names.R
#
# Create lookup table linking Hydrography90m basin IDs to
# Greek river basin names from the EGY (Water District) shapefile.
#
# Method: overlay snapped occurrence points with Greek named basin
# polygons, then assign the most frequent Greek basin name to each
# H90M basin_id.
#
# Input:
#   - H90M basin polygons (basin_polygons.gpkg)
#   - Greek named basins (egy_rb_new_2014.shp)
#   - Snapped occurrence points (all_snapped_with_basins_from_sp_list.csv)
#
# Output:
#   - Basin name lookup table (basin_name_lookup.csv)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(sf)
library(dplyr)
library(data.table)

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# READ INPUTS
# ============================================================

# H90M basin polygons
basin_polygons <- read_sf("spatial/stream_networks/basin_polygons.gpkg")

# Greek named basins (EGY river basin districts)
greek_basins <- st_read("spatial/egy_rb_new_2014/egy_rb_new_2014.shp") %>%
  st_transform(st_crs(basin_polygons))

# Snapped occurrence points (with basin_id already assigned)
occurrences <- fread("points_snapped/all_snapped_with_basins_from_sp_list.csv")

# ============================================================
# OVERLAY: Point-in-polygon to get Greek basin name per point
# ============================================================

occ_sf <- st_as_sf(occurrences,
                   coords = c("longitude_snapped", "latitude_snapped"),
                   crs = 4326) %>%
  st_transform(st_crs(greek_basins))

occ_sf$greek_basin_name <- greek_basins$name[
  as.integer(st_intersects(occ_sf, greek_basins))
]

# ============================================================
# BUILD LOOKUP TABLE
# Link H90M basin_id to the most frequent Greek basin name
# ============================================================

basin_lookup <- occ_sf %>%
  st_drop_geometry() %>%
  select(basin_id, greek_basin_name) %>%
  filter(!is.na(basin_id), !is.na(greek_basin_name)) %>%
  group_by(basin_id) %>%
  summarize(
    basin_name = names(sort(table(greek_basin_name), decreasing = TRUE))[1],
    n_points = n(),
    .groups = "drop"
  ) %>%
  arrange(basin_name)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 04_spatial_network/03_overlay_with_basin_names.R
#
# Create lookup table linking Hydrography90m basin IDs to
# Greek river basin names from the EGY (Water District) shapefile.
#
# Method: overlay snapped occurrence points with Greek named basin
# polygons, then assign the most frequent Greek basin name to each
# H90M basin_id.
#
# Input:
#   - H90M basin polygons (basin_polygons.gpkg)
#   - Greek named basins (egy_rb_new_2014.shp)
#   - Snapped occurrence points (all_snapped_with_basins_from_sp_list.csv)
#
# Output:
#   - Basin name lookup table (basin_name_lookup.csv)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(sf)
library(dplyr)
library(data.table)

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# READ INPUTS
# ============================================================

# H90M basin polygons
basin_polygons <- read_sf("spatial/stream_networks/basin_polygons.gpkg")

# Greek named basins (EGY river basin districts)
greek_basins <- st_read("spatial/egy_rb_new_2014/egy_rb_new_2014.shp") %>%
  st_transform(st_crs(basin_polygons))

# Snapped occurrence points (with basin_id already assigned)
occurrences <- fread("points_snapped/all_snapped_with_basins_from_sp_list.csv")

# ============================================================
# OVERLAY: Point-in-polygon to get Greek basin name per point
# ============================================================

occ_sf <- st_as_sf(occurrences,
                   coords = c("longitude_snapped", "latitude_snapped"),
                   crs = 4326) %>%
  st_transform(st_crs(greek_basins))

occ_sf$greek_basin_name <- greek_basins$name[
  as.integer(st_intersects(occ_sf, greek_basins))
]

# ============================================================
# BUILD LOOKUP TABLE
# Link H90M basin_id to the most frequent Greek basin name
# ============================================================

# picks the most frequent Greek basin name among all points in that H90M basin.
# This is more robust for basins near boundaries where some points might fall in
# a neighboring Greek basin. With n_points we can see how many points supported
# each match.
basin_lookup <- occ_sf %>%
  st_drop_geometry() %>%
  select(basin_id, greek_basin_name) %>%
  filter(!is.na(basin_id), !is.na(greek_basin_name)) %>%
  group_by(basin_id) %>%
  summarize(
    basin_name = names(sort(table(greek_basin_name), decreasing = TRUE))[1],
    n_points = n(),
    .groups = "drop"
  ) %>%
  arrange(basin_name)

# ============================================================
# SAVE
# ============================================================

fwrite(basin_lookup, "spatial/basin_name_lookup.csv")

message("Basin name lookup saved: ", nrow(basin_lookup), " basins matched")
cat("\n")
print(basin_lookup)
