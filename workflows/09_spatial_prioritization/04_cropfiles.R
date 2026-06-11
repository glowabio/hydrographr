library(hydrographr)

# ============================================================================
# DOWNLOAD HUMAN FOOT PRINT
# ============================================================================
# Purpose: Download Human Foot Print to use as penalty layer in the prioritization exercise
# Input: GLobal layer at 100 m resolution
# Output: Cropped raster layer for the study area
# ============================================================================
# Date: 2026-04-29
# ============================================================================

# ============================================================================
# SETUP
# ============================================================================

#source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
#BASE_DIR
#setwd(BASE_DIR)
# setwd("/mnt/shared/workflow_paper/data/spatial/")

# ============================================================================
# DOWNLOAD HUMAN FOOT PRINT FILE
# ============================================================================

# download.file("https://data.source.coop/vizzuality/hfp-100/hfp_2021_100m_v1-2_cog.tif", destfile = "spatial/hfp_2021_100m_v1-2_cog.tif")

# ============================================================================
# CROP FILE TO STUDY AREA REGION
# ============================================================================

crop_to_extent(raster_layer="spatial/hfp_2021_100m_v1-2_cog.tif",
                     vector_layer="spatial/subbasin_sarantaporos/subbasin_polygon.gpkg",
                     out_dir="spatial/",
                     file_name="hfp_crop.tif",
                     read = FALSE,
                     quiet = TRUE)

# ============================================================================
# SPECIFY PROJECTION SYSTEM
# ============================================================================

hfp <- terra::rast("spatial/hfp_crop.tif")
hfp_wgs <- terra::project(hfp, "EPSG:4326")
terra::writeRaster(hfp_wgs, "spatial/hfp_wgs.tif", overwrite = TRUE)

# ============================================================================
# CALCULATE ZONAL STATISTICS
# ============================================================================

# First we need to prepare the raster file of sub-catchments for the study area

# The study area overlaps with two tiles
tile_id <- c("h20v04")

# Download the .tif tiles for the sub-catchment variable
download_tiles(variable = "sub_catchment", tile_id = tile_id, file_format = "tif",
              download_dir = "spatial/")

# Crop orginal raster files to the extent of the study area
tfiles  <- list.files("spatial/r.watershed/sub_catchment_tiles20d",
                        pattern = ".tif$",
                        full.names = TRUE)

for(rast in tfiles) {
      crop_to_extent(
        raster_layer = tfiles,
        vector_layer = "spatial/subbasin_sarantaporos/subbasin_polygon.gpkg",
        out_dir = "spatial/",
        file_name = "subcatchment_sarantaporos.tif",
        read = FALSE,
        quiet = TRUE)
  }

# # Merge the two cropped files
# merge_tiles(tile_dir=".",
#             tile_names = list.files(".", full.names = FALSE,
#                                     pattern = "sub_catchment_.*_crop\\.tif"),
#             out_dir=".",
#             file_name = "subcatchment_vjosa.tif",
#             read = FALSE)

# Calculate the zonal statistics
stats_table_zon <- extract_zonal_stat(
                    data_dir = paste0(getwd(), "/spatial/"),
                    subc_layer = "spatial/subcatchment_sarantaporos.tif",
                    subc_id = "all",
                    var_layer = "hfp_wgs.tif",
                    out_dir = "spatial/",
                    file_name = "hfp_zonal_stats.csv",
                    n_cores = 1)


