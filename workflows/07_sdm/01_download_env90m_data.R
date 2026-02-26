# ============================================================================
# DOWNLOAD ENVIRONMENTAL DATA FROM ENVIRONMENT90M
# ============================================================================
# Purpose: Download environmental predictor variables as TXT files from Env90m
# Input: Previously downloaded stream network (partial_stream_network.gpkg)
# Output: TXT files with environmental data organized by dataset
# ============================================================================
# Date: 2026-02-05
# ============================================================================

library(hydrographr)
library(data.table)
library(dplyr)

# ============================================================================
# SETUP
# ============================================================================

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)

# ============================================================================
# LOAD EXISTING STREAM NETWORK
# ============================================================================

message("\n=== Loading Existing Stream Network ===")

# Read the stream network you already downloaded
stream_network <- read_geopackage(
  "spatial/stream_networks/partial_stream_network.gpkg",
  import_as = "data.table"
)

message(sprintf("Loaded stream network: %d segments", nrow(stream_network)))

# Extract unique subcatchment IDs
subc_ids <- unique(stream_network$subc_id)
subc_ids <- subc_ids[!is.na(subc_ids)]

message(sprintf("Unique subcatchments: %d", length(subc_ids)))

# ============================================================================
# DETERMINE TILE IDs
# ============================================================================

message("\n=== Determining Tile IDs ===")

# Option 1: Manually specify tiles (if you know them)
tile_id <- c("h18v04", "h20v04")

# Option 2: Auto-detect from coordinates (uncomment if needed)
# coords <- st_coordinates(st_centroid(st_as_sf(stream_network)))
# tile_id <- get_tile_id(data = as.data.frame(coords), lon = "X", lat = "Y")
# tile_id <- unique(tile_id$tile_id)

message(sprintf("Tiles needed: %s", paste(tile_id, collapse = ", ")))

# ============================================================================
# CREATE DIRECTORY STRUCTURE
# ============================================================================

message("\n=== Creating Directory Structure ===")

# Create directory for environmental data
dir.create("env90m", showWarnings = FALSE, recursive = TRUE)

message("✓ Created directory: env90m/")

# ============================================================================
# DOWNLOAD ENVIRONMENTAL DATA TABLES
# ============================================================================

message("\n=== Downloading Environmental Data Tables ===")
message("This may take several minutes depending on data size...")
message("Total expected download: ~5-15 GB")

# --------------------------------------------------------------------------
# 1. OBSERVED CLIMATE VARIABLES
# --------------------------------------------------------------------------

message("\n--- Downloading Climate Variables ---")

download_observed_climate_tables(
  subset = c("bio01_1981-2010_observed", "bio02_1981-2010_observed"),
  tile_ids = tile_id,
  download = TRUE,
  download_dir = "env90m",
  file_format = "txt",
  delete_zips = TRUE,
  ignore_missing = FALSE,
  tempdir = NULL,
  quiet = FALSE
)

message("\n✓ Climate tables downloaded to: env90m/chelsa_bioclim_v2_1/")

# Verify download
climate_file <- list.files(
  path = "env90m/chelsa_bioclim_v2_1/1981-2010_observed/bio01",
  pattern = ".txt$",
  full.names = TRUE
)[1]

if (!is.na(climate_file) && file.exists(climate_file)) {
  file_size <- file.info(climate_file)$size / 1024 / 1024  # MB
  message(sprintf("  Sample file: %s (%.2f MB)", basename(climate_file), file_size))
  message("\n  First 10 rows:")
  system(paste("head", climate_file))
} else {
  warning("Climate file not found! Download may have failed.")
}

# --------------------------------------------------------------------------
# 2. LAND COVER VARIABLES
# --------------------------------------------------------------------------

message("\n--- Downloading Land Cover Variables ---")

download_landcover_tables(
  base_vars = c("c60"),
  years = c("2020"),
  tile_ids = tile_id,
  download = TRUE,
  download_dir = "env90m",
  file_format = "txt",
  delete_zips = TRUE,
  ignore_missing = FALSE,
  tempdir = NULL,
  quiet = FALSE
)

message("\n✓ Land cover tables downloaded to: env90m/esa_cci_landcover_v2_1_1/")

# Verify download
landcover_file <- list.files(
  path = "env90m/esa_cci_landcover_v2_1_1/c60",
  pattern = ".txt$",
  full.names = TRUE
)[1]

if (!is.na(landcover_file) && file.exists(landcover_file)) {
  file_size <- file.info(landcover_file)$size / 1024 / 1024  # MB
  message(sprintf("  Sample file: %s (%.2f MB)", basename(landcover_file), file_size))
  message("\n  First 10 rows:")
  system(paste("head", landcover_file))
} else {
  warning("Land cover file not found! Download may have failed.")
}

# --------------------------------------------------------------------------
# 3. HYDROGRAPHY90M VARIABLES
# --------------------------------------------------------------------------

message("\n--- Downloading Hydrography Variables ---")

download_hydrography90m_tables(
  subset = c("accumulation", "length", "slope_grad_dw_cel"),
  tile_ids = tile_id,
  download = TRUE,
  download_dir = "env90m",
  file_format = "txt",
  delete_zips = TRUE,
  ignore_missing = FALSE,
  tempdir = NULL,
  quiet = FALSE
)

message("\n✓ Hydrography tables downloaded to: env90m/hydrography90m_v1_0/")

# --------------------------------------------------------------------------
# 4. ADDITIONAL VARIABLES (OPTIONAL - UNCOMMENT IF NEEDED)
# --------------------------------------------------------------------------

# Soil variables
# download_soil_tables(
#   subset = c("clyppt", "sltppt", "sndppt"),
#   tile_ids = tile_id,
#   download = TRUE,
#   download_dir = "env90m",
#   file_format = "txt",
#   delete_zips = TRUE,
#   quiet = FALSE
# )

# Projected climate (future scenarios)
# download_projected_climate_tables(
#   subset = c("bio01_2041-2070_ssp370", "bio01_2041-2070_ssp585"),
#   tile_ids = tile_id,
#   download = TRUE,
#   download_dir = "env90m",
#   file_format = "txt",
#   delete_zips = TRUE,
#   quiet = FALSE
# )

# ============================================================================
# CREATE SUBCATCHMENT ID FILE
# ============================================================================

message("\n=== Creating Subcatchment ID Reference File ===")

# Save subc_ids to file (needed for get_predict_table function in next script)
subc_ids_dt <- data.table(subc_id = subc_ids)
fwrite(subc_ids_dt,
       file = "env90m/subc_ids.txt",
       col.names = FALSE)

message(sprintf("✓ Saved %d subcatchment IDs to: env90m/subc_ids.txt",
                length(subc_ids)))

# ============================================================================
# VERIFICATION & SUMMARY
# ============================================================================

message("\n=== Download Verification ===")

# Count downloaded files
n_climate <- length(list.files("env90m/chelsa_bioclim_v2_1",
                               pattern = ".txt$", recursive = TRUE))
n_landcover <- length(list.files("env90m/esa_cci_landcover_v2_1_1",
                                 pattern = ".txt$", recursive = TRUE))
n_hydro <- length(list.files("env90m/hydrography90m_v1_0",
                             pattern = ".txt$", recursive = TRUE))

message(sprintf("\nFiles downloaded:"))
message(sprintf("  Climate: %d files", n_climate))
message(sprintf("  Land cover: %d files", n_landcover))
message(sprintf("  Hydrography: %d files", n_hydro))
message(sprintf("  Total: %d files", n_climate + n_landcover + n_hydro))

# Calculate total disk space used
total_size <- sum(
  file.info(list.files("env90m", pattern = ".txt$",
                       recursive = TRUE, full.names = TRUE))$size,
  na.rm = TRUE
) / 1024 / 1024 / 1024  # GB

message(sprintf("\nTotal disk space used: %.2f GB", total_size))

# ============================================================================
# FINAL SUMMARY
# ============================================================================

message("\n========================================")
message("=== DOWNLOAD COMPLETE ===")
message("========================================")

message("\nDirectory structure created:")
message("  env90m/")
message("  ├── chelsa_bioclim_v2_1/")
message("  ├── esa_cci_landcover_v2_1_1/")
message("  ├── hydrography90m_v1_0/")
message("  └── subc_ids.txt")

message("\nVariables downloaded:")
message("  Climate: bio01_1981-2010_observed, bio02_1981-2010_observed")
message("  Land cover: c60_2020")
message("  Hydrography: accumulation, length, slope_grad_dw_cel")

message("\nTiles downloaded:")
message(sprintf("  %s", paste(tile_id, collapse = ", ")))

message("\nNext steps:")
message("  1. Run script: 02_create_prediction_table.R")
message("  2. This will create the pred_tab.csv for SDM modeling")

message("\nNOTE: Raw downloaded tables can be deleted after creating")
message("      the prediction table to save disk space (~",
        sprintf("%.1f", total_size), " GB)")

message("\n========================================\n")
