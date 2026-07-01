#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03_delineate_lake_catchment.R   (Module 12 -- Lake analysis)
#
# Delineate the catchment draining to the lake, merge it with the lake
# surface, compute its area, and extract the sub-catchment IDs that make up
# the lake catchment. Those IDs feed the land cover analysis in 04_.
#
# Workflow:
#   1. Load intersection points, keep those on the stream network
#   2. get_lake_catchment() from the intersection points
#   3. Merge catchment + lake surface raster, compute area
#   4. Resample onto the sub-catchment grid, keep cells >= 90% covered
#   5. Extract sub-catchment IDs of the lake catchment
#   6. Draw the lake catchment map (+ basin locator inset)
#
# INPUT:
#   - lakes/lake_intersections/coord_lake_<id>.txt   (from 02_)
#   - lakes/lake_intersections/lake_<id>.tif          (from 02_)
#   - lakes/lake_intersections/outlets_<id>.gpkg      (from 02_)
#   - spatial/basin/stream_network_pruned.gpkg        (stream network)
#   - lakes/swot_lakes.gpkg                           (SWOT prior lake DB)
#   - spatial/direction_<tile>.tif                    (flow direction, from 01_)
#   - spatial/sub_catchment_<tile>.tif                (from 01_)
#   - spatial/basin/basin_polygon.gpkg                (if present; else fetched)
#
# OUTPUT:
#   - lakes/lake_intersections/basin_lake_<id>_merged.tif
#   - spatial/subc_id_lake_catchment.tif
#   - spatial/basin/basin_polygon.gpkg                (Vjosa/Aoos basin, if fetched)
#   - lakes/subc_IDs_lake_catchment.txt               (sub-catchment IDs)
#   - figures/lakes/lake_catchment_map.png
#   - figures/lakes/lake_catchment_inset_basin_map.png
#
# LOCATION: workflows/12_lakes/03_delineate_lake_catchment.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(terra)
library(sf)
library(data.table)
library(ggplot2)
library(ggspatial)
library(patchwork)

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("figures/lakes", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

LAKE_ID   <- 2130004632
TILE_ID   <- "h20v04"

# Minimum fraction of a sub-catchment cell that must be covered by the lake
# catchment for that cell to be kept.
COVER_MIN <- 0.9

# SWOT prior lake database (same manual download used in 02_).
SWOT_LAKES     <- "lakes/swot_lakes.gpkg"
SWOT_LAKE_NAME <-
  "swot_lakedatabase_20000101t000000_20991231t235959_20250331t170000_v202_light_eu__lake"

# ============================================================
# STEP 1: Load intersection points, keep network reaches
# ============================================================

message("\n=== Loading lake intersection points ===")

gpkg_data <- st_read("spatial/basin/stream_network_pruned.gpkg")

coord_dat <- fread(sprintf(
  "lakes/lake_intersections/coord_lake_%d.txt", LAKE_ID))
head(coord_dat)

# keep only intersection points whose reach is in the stream network
indx      <- which(coord_dat$subc_id %in% gpkg_data$subc_id)
coord_dat <- coord_dat[indx, ]

# ============================================================
# STEP 2: Delineate lake catchment
# ============================================================

message("\n=== Delineating lake catchment ===")

direction <- sprintf("spatial/direction_%s.tif", TILE_ID)
catch     <- "lakes/lake_intersections/"

get_lake_catchment(coord_dat,
                   direction  = direction,
                   flow       = "flow_accu_mean",
                   n          = 1,
                   lake_basin = catch,
                   quiet      = TRUE)

# ============================================================
# STEP 3: Merge catchment + lake surface, compute area
# ============================================================

message("\n=== Merging catchment with lake surface ===")

lake_catch <- terra::rast(sprintf(
  "lakes/lake_intersections/basin_lake_%d_coord_1.tif", LAKE_ID))
lake_rast  <- terra::rast(sprintf(
  "lakes/lake_intersections/lake_%d.tif", LAKE_ID))

lake_catch <- terra::merge(lake_catch, lake_rast)

# reproject to LAEA Europe (EPSG:3035) for an equal-area km^2 calculation
lake_catch_laea <- terra::project(lake_catch, "EPSG:3035")
lake_catch_area <- terra::expanse(lake_catch_laea, unit = "km",
                                  zones = lake_catch_laea)
print(lake_catch_area)

terra::writeRaster(lake_catch, sprintf(
  "lakes/lake_intersections/basin_lake_%d_merged.tif", LAKE_ID),
  overwrite = TRUE)

# ============================================================
# STEP 4: Resample onto sub-catchment grid, keep covered cells
# ============================================================

message("\n=== Aligning to sub-catchment grid ===")

subc_raster <- terra::rast(sprintf(
  "spatial/sub_catchment_%s.tif", TILE_ID))

# crop to the lake catchment bbox (keeps the sub-catchment grid intact)
lake_catch_crop <- terra::crop(subc_raster, lake_catch)

# NA -> 0 so the averaging below yields true coverage fractions
lake_catch_binary <- terra::subst(lake_catch, NA, 0)

# each sub-catchment cell gets the fraction covered by the lake catchment
lake_catch_cover <- terra::resample(lake_catch_binary, lake_catch_crop,
                                    method = "average")

# keep cells covered >= COVER_MIN, then trim NA margins
lake_catch_crop <- terra::trim(
  terra::mask(lake_catch_crop, lake_catch_cover >= COVER_MIN,
              maskvalues = FALSE))

writeRaster(lake_catch_crop, "spatial/subc_id_lake_catchment.tif",
            overwrite = TRUE)

# ============================================================
# STEP 5: Extract sub-catchment IDs
# ============================================================

lake_catch_ids <- extract_ids(
  subc_layer = "spatial/subc_id_lake_catchment.tif")

fwrite(lake_catch_ids, "lakes/subc_IDs_lake_catchment.txt")

message(sprintf("\nLake catchment: %d sub-catchments", nrow(lake_catch_ids)))

# ============================================================
# STEP 6: Draw lake catchment map
# ============================================================

message("\n=== Drawing lake catchment map ===")

# load lake surface (vector), subset to the target lake, and the lake outlet
# point (both produced alongside the intersection points in 02_)
swot_lakes <- st_read(SWOT_LAKES, quiet = TRUE)
lake_surface_vect <- swot_lakes[swot_lakes[[SWOT_LAKE_NAME]] == LAKE_ID, ]

lake_outlet <- st_read(sprintf(
  "lakes/lake_intersections/outlets_%d.gpkg", LAKE_ID), quiet = TRUE)

# vectorize the lake catchment raster for plotting with geom_sf
lake_catch_vect <- terra::as.polygons(lake_catch, dissolve = TRUE) |>
  st_as_sf()

# get the lake catchment bounding box to restrict the map extent
lake_catch_bbox <- st_bbox(lake_catch_vect)

# crop the stream network to the catchment extent so the strahler order
# legend only reflects streams actually shown on the map
gpkg_data_crop <- st_crop(gpkg_data, lake_catch_bbox)

## load the larger basin polygon (Vjosa/Aoos basin) for the inset map. Use
# the existing file if present (created elsewhere in the workflow); otherwise
# look it up from the lake outlet location.
basin_polygon_path <- "spatial/basin/basin_polygon.gpkg"

if (file.exists(basin_polygon_path)) {
  basin_polygon <- st_read(basin_polygon_path, quiet = TRUE)
} else {
  dir.create("spatial/basin", recursive = TRUE, showWarnings = FALSE)
  outlet_coords <- st_coordinates(st_zm(lake_outlet))
  outlet_df <- data.frame(
    site_id   = paste0("lake_", LAKE_ID, "_outlet"),
    longitude = outlet_coords[1, "X"],
    latitude  = outlet_coords[1, "Y"]
  )
  basin_polygon <- api_get_basin_polygon(points_df = outlet_df)
  st_write(basin_polygon, basin_polygon_path, delete_dsn = TRUE)
}

p <- ggplot() +
  geom_sf(data = gpkg_data_crop, colour = "darkblue", linewidth = 0.8) +
  geom_sf(data = lake_surface_vect, fill = "blue", colour = NA, alpha = 0.6) +
  geom_sf(data = lake_catch_vect, fill = NA, colour = "black", linewidth = 0.8) +
  geom_sf(data = lake_outlet, colour = "magenta", size = 4) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  coord_sf(xlim = c(lake_catch_bbox["xmin"], lake_catch_bbox["xmax"]),
           ylim = c(lake_catch_bbox["ymin"], lake_catch_bbox["ymax"])) +
  theme_minimal() +
  labs(title = "Aoos Springs reservoir lake catchment")

# version without the inset map
png("figures/lakes/lake_catchment_map.png", width = 2700, height = 2400, res = 300)
print(p)
dev.off()
message("  Saved: figures/lakes/lake_catchment_map.png")

# inset (locator) map: the larger Vjosa/Aoos basin polygon, with the lake
# surface highlighted to show where the main map sits within the basin
inset_map <- ggplot() +
  geom_sf(data = basin_polygon, fill = "grey90", colour = "grey40", linewidth = 0.3) +
  geom_sf(data = gpkg_data, colour = "grey60", linewidth = 0.2) +
  geom_sf(data = lake_surface_vect, fill = "blue", colour = "blue", linewidth = 0.5) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", colour = "black", linewidth = 0.4),
    plot.margin = margin(1, 1, 1, 1)
  )

# place the inset as its own panel beside the main map (rather than
# overlaying it on top of the data), so it never overlaps the catchment
# outline regardless of the catchment's shape
p_with_inset <- p + inset_map + plot_layout(widths = c(3, 1.5))

png("figures/lakes/lake_catchment_inset_basin_map.png", width = 2700, height = 2400, res = 300)
print(p_with_inset)
dev.off()
message("  Saved: figures/lakes/lake_catchment_inset_basin_map.png")

message("Next: 12_lakes/04_lake_landcover_analysis.R")
