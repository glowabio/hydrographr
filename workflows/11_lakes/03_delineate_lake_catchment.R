#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03_delineate_lake_catchment.R   (Module X -- Lake analysis)
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
#
# INPUT:
#   - data/lakes/lake_intersections/coord_lake_<id>.txt  (from 02_)
#   - data/lakes/lake_intersections/lake_<id>.tif         (from 02_)
#   - data/spatial/vjosa_partial.gpkg                     (stream network)
#   - data/spatial/direction_<basin>.tif                  (flow direction)
#   - data/spatial/sub_catchment_<tile>.tif
#
# OUTPUT:
#   - data/lakes/lake_intersections/basin_lake_<id>_merged.tif
#   - data/spatial/subc_id_lake_catchment.tif
#   - data/subc_IDs.txt                                   (sub-catchment IDs)
#
# LOCATION: workflows/XX_lakes/03_delineate_lake_catchment.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(terra)
library(sf)
library(data.table)

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

LAKE_ID   <- 2130004632
TILE_ID   <- "h20v04"
BASIN_ID  <- 1292502    # Vjosa basin id (direction raster suffix)

# Minimum fraction of a sub-catchment cell that must be covered by the lake
# catchment for that cell to be kept.
COVER_MIN <- 0.9

# ============================================================
# STEP 1: Load intersection points, keep network reaches
# ============================================================

message("\n=== Loading lake intersection points ===")

gpkg_data <- st_read("data/spatial/vjosa_partial.gpkg")

coord_dat <- fread(sprintf(
  "data/lakes/lake_intersections/coord_lake_%d.txt", LAKE_ID))
head(coord_dat)

# keep only intersection points whose reach is in the stream network
indx      <- which(coord_dat$subc_id %in% gpkg_data$subc_id)
coord_dat <- coord_dat[indx, ]

# ============================================================
# STEP 2: Delineate lake catchment
# ============================================================

message("\n=== Delineating lake catchment ===")

direction <- sprintf("data/spatial/direction_%d.tif", BASIN_ID)
catch     <- "data/lakes/lake_intersections/"

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
  "data/lakes/lake_intersections/basin_lake_%d_coord_1.tif", LAKE_ID))
lake_rast  <- terra::rast(sprintf(
  "data/lakes/lake_intersections/lake_%d.tif", LAKE_ID))

lake_catch <- terra::merge(lake_catch, lake_rast)

# reproject to LAEA Europe (EPSG:3035) for an equal-area km^2 calculation
lake_catch_laea <- terra::project(lake_catch, "EPSG:3035")
lake_catch_area <- terra::expanse(lake_catch_laea, unit = "km",
                                  zones = lake_catch_laea)
print(lake_catch_area)

terra::writeRaster(lake_catch, sprintf(
  "data/lakes/lake_intersections/basin_lake_%d_merged.tif", LAKE_ID),
  overwrite = TRUE)

# ============================================================
# STEP 4: Resample onto sub-catchment grid, keep covered cells
# ============================================================

message("\n=== Aligning to sub-catchment grid ===")

subc_raster <- terra::rast(sprintf(
  "data/spatial/sub_catchment_%s.tif", TILE_ID))

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

writeRaster(lake_catch_crop, "data/spatial/subc_id_lake_catchment.tif",
            overwrite = TRUE)

# ============================================================
# STEP 5: Extract sub-catchment IDs
# ============================================================

lake_catch_ids <- extract_ids(
  subc_layer = "data/spatial/subc_id_lake_catchment.tif")

fwrite(lake_catch_ids, "data/subc_IDs.txt")

message(sprintf("\nLake catchment: %d sub-catchments", nrow(lake_catch_ids)))
message("Next: XX_lakes/04_lake_landcover_analysis.R")
