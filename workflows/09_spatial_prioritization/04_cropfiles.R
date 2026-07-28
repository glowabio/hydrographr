#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 04_cropfiles.R   (Module 9 -- Spatial Prioritization)
#
# One-off setup script: crops the global Human Footprint Index (HFP) raster
# to the Sarantaporos sub-basin and computes per-subcatchment zonal means,
# feeding 01_spatial_prioritization.R's cost layer.
#
# The HFP download itself is commented out (see Step 1) -- the raw global
# raster is large (~13.1 GB, confirmed via the source URL's Content-Length;
# do not trust the ~650 MB figure that may still appear in older notes --
# that turned out to be a truncated/incomplete copy, see README.md Module 09
# section) and assumed already on disk at spatial/hfp_2021_100m_v1-2_cog.tif;
# re-enable that line for a genuinely fresh BASE_DIR with no cached copy, or
# to replace a corrupted one.
#
# Workflow:
#   1. (Optional) download the global HFP raster
#   2. Crop it to the Sarantaporos sub-basin polygon
#   3. Reproject the crop to EPSG:4326
#   4. Crop the sub_catchment raster tile to the same extent (downloads it
#      first only if Module 12 hasn't already -- see Step 4)
#   5. Compute zonal mean HFP per subcatchment
#
# Input:
#   - spatial/hfp_2021_100m_v1-2_cog.tif        (raw, pre-provided; see Step 1)
#   - spatial/subbasin_sarantaporos/subbasin_polygon.gpkg   (Module 3)
#   - spatial/sub_catchment_h20v04.tif   (Module 12's 01_download_lake_data.R,
#     if already run; downloaded here otherwise -- see Step 4)
#
# Output:
#   - spatial/hfp_crop.tif, hfp_wgs.tif, subcatchment_sarantaporos.tif
#   - spatial/hfp_zonal_stats.csv   (read by 01_spatial_prioritization.R)
#
# LOCATION: workflows/09_spatial_prioritization/04_cropfiles.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(terra)

if (!exists("WORKFLOWS_DIR"))
  WORKFLOWS_DIR <- Sys.getenv("WORKFLOWS_CODE", "/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows")
source(file.path(WORKFLOWS_DIR, "helpers", "config.R"))
setwd(BASE_DIR)

dir.create("spatial", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# STEP 1: Download the global Human Footprint Index raster
# ============================================================
# Commented out -- raw raster (~650 MB) already on disk; re-enable for a
# genuinely fresh BASE_DIR.

# download.file("https://data.source.coop/vizzuality/hfp-100/hfp_2021_100m_v1-2_cog.tif",
#               destfile = "spatial/hfp_2021_100m_v1-2_cog.tif")

# ============================================================
# STEP 2: Crop HFP to the Sarantaporos sub-basin
# ============================================================

message("\n=== Step 2: Cropping HFP raster to Sarantaporos sub-basin ===")

crop_to_extent(raster_layer = "spatial/hfp_2021_100m_v1-2_cog.tif",
                vector_layer = "spatial/subbasin_sarantaporos/subbasin_polygon.gpkg",
                out_dir      = "spatial/",
                file_name    = "hfp_crop.tif",
                read         = FALSE,
                quiet        = TRUE)

message("  Saved: spatial/hfp_crop.tif")

# ============================================================
# STEP 3: Reproject to EPSG:4326
# ============================================================

message("\n=== Step 3: Reprojecting to EPSG:4326 ===")

hfp     <- terra::rast("spatial/hfp_crop.tif")
hfp_wgs <- terra::project(hfp, "EPSG:4326")
terra::writeRaster(hfp_wgs, "spatial/hfp_wgs.tif", overwrite = TRUE)

message("  Saved: spatial/hfp_wgs.tif")

# ============================================================
# STEP 4: Crop the sub-catchment raster to the same extent
# ============================================================
# The study area overlaps with two MERIT tiles, but the cropped output here
# is clipped to the small Sarantaporos sub-basin polygon (not the full
# basin), which sits entirely within this one tile -- unlike Module 7's SDM
# prep, which downloads both h18v04 and h20v04 for the full basin extent.
# Worth double-checking against the basin/subbasin geometry if the cropped
# output ever looks incomplete at the tile boundary.

message("\n=== Step 4: Downloading + cropping sub_catchment tile ===")

tile_id <- c("h20v04")

# download_tiles(variable = "sub_catchment", ...) writes to whatever nested
# path the remote file listing currently specifies -- historically
# spatial/r.watershed/sub_catchment_tiles20d/, but Module 12
# (12_lakes/01_download_lake_data.R) makes the identical call today and its
# consumer (03_delineate_lake_catchment.R) reads a flat
# spatial/sub_catchment_<tile>.tif, confirming that's the current real
# output path. Reuse it directly if Module 12 already produced it (a single
# ~90m tile, shared across modules); only re-download here if it doesn't
# exist yet.
tfiles <- file.path("spatial", paste0("sub_catchment_", tile_id, ".tif"))

if (!all(file.exists(tfiles))) {
  download_tiles(variable = "sub_catchment", tile_id = tile_id, file_format = "tif",
                 download_dir = "spatial/")
}

for (rast in tfiles) {
  crop_to_extent(
    raster_layer = rast,
    vector_layer = "spatial/subbasin_sarantaporos/subbasin_polygon.gpkg",
    out_dir      = "spatial/",
    file_name    = "subcatchment_sarantaporos.tif",
    read         = FALSE,
    quiet        = TRUE)
}

message("  Saved: spatial/subcatchment_sarantaporos.tif")

# ============================================================
# STEP 5: Zonal statistics (mean HFP per subcatchment)
# ============================================================

message("\n=== Step 5: Computing zonal statistics ===")

stats_table_zon <- extract_zonal_stat(
  data_dir   = paste0(getwd(), "/spatial/"),
  subc_layer = "spatial/subcatchment_sarantaporos.tif",
  subc_id    = "all",
  var_layer  = "hfp_wgs.tif",
  out_dir    = "spatial/",
  file_name  = "hfp_zonal_stats.csv",
  n_cores    = 1)

message("  Saved: spatial/hfp_zonal_stats.csv")

message("\n", strrep("=", 60), "\nCROPFILES COMPLETE\n", strrep("=", 60),
        "\nNext: 01_spatial_prioritization.R")
