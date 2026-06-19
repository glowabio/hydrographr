#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_download_env90m_data.R   (Module 7 -- SDM)
#
# Download environmental predictor variables (climate, land cover,
# hydrography) as per-subcatchment TXT tables from Environment90m /
# Hydrography90m, for the study basin's stream network. These tables feed
# the prediction table built in script 02.
#
# Downloads cover the two MERIT tiles that span the basin (h18v04, h20v04).
# Raw tables are large; they can be deleted after script 02 builds the
# prediction table.
#
# Workflow:
#   1. Load pruned stream network, extract subcatchment IDs
#   2. Download climate, land cover, and hydrography tables for the tiles
#   3. Write the subcatchment ID reference file
#
# INPUT:
#   - spatial/basin/stream_network_pruned.gpkg   (from 03_extract_subbasin.R)
#
# OUTPUT:
#   - env90m/chelsa_bioclim_v2_1/...      (climate tables)
#   - env90m/esa_cci_landcover_v2_1_1/... (land cover tables)
#   - env90m/hydrography90m_v1_0/...      (hydrography tables)
#   - env90m/subc_ids_basin.txt           (subcatchment ID reference)
#
# REQUIRES: internet access; substantial disk space for raw tables.
#
# LOCATION: workflows/07_sdm/01_download_env90m_data.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(data.table)
library(dplyr)

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("env90m", showWarnings = FALSE, recursive = TRUE)

# ============================================================
# PARAMETERS
# ============================================================

# MERIT tiles covering the basin. Can also be auto-detected from coordinates
# with get_tile_id() on the stream network centroids if the basin changes.
tile_id <- c("h18v04", "h20v04")

# ============================================================
# STEP 1: Load stream network and extract subcatchment IDs
# ============================================================

message("\n=== Loading stream network ===")

stream_network <- hydrographr::read_geopackage(
  "spatial/basin/stream_network_pruned.gpkg",
  import_as = "data.table"
)

subc_ids <- unique(stream_network$subc_id)
subc_ids <- subc_ids[!is.na(subc_ids)]

message(sprintf("  %d segments, %d unique subcatchments",
                nrow(stream_network), length(subc_ids)))

# ============================================================
# STEP 2: Download environmental tables
# ============================================================
# Each download_*_tables() call writes per-variable TXT tables under env90m/.
# delete_zips = TRUE removes the archives after extraction.

message("\n=== Downloading environmental tables (may take a while) ===")

# --- Climate (CHELSA bioclim, 1981-2010 observed) ---
download_observed_climate_tables(
  subset = c("bio01_1981-2010_observed", "bio04_1981-2010_observed",
             "bio05_1981-2010_observed", "bio06_1981-2010_observed",
             "bio15_1981-2010_observed", "bio17_1981-2010_observed",
             "bio18_1981-2010_observed"),
  tile_ids = tile_id, download = TRUE, download_dir = "env90m",
  file_format = "txt", delete_zips = TRUE, ignore_missing = FALSE,
  tempdir = NULL, quiet = FALSE
)
message("  Climate -> env90m/chelsa_bioclim_v2_1/")

# --- Land cover (ESA CCI, annual 1992-2020) ---
download_landcover_tables(
  base_vars = c("c10", "c20", "c30", "c40", "c50", "c60", "c120", "c130",
                "c150", "c160", "c180", "c190", "c200", "c210"),
  years = as.character(1992:2020),
  tile_ids = tile_id, download = TRUE, download_dir = "env90m",
  file_format = "txt", delete_zips = TRUE, ignore_missing = FALSE,
  tempdir = NULL, quiet = FALSE
)
message("  Land cover -> env90m/esa_cci_landcover_v2_1_1/")

# --- Hydrography90m ---
download_hydrography90m_tables(
  subset = c("cti", "order_strahler", "length", "cum_length", "gradient",
             "elev_drop", "accumulation", "channel_elv_dw_seg",
             "channel_elv_up_seg", "connections", "stream_dist_dw_near",
             "stream_dist_up_near", "slope_grad_dw_cel", "outlet_diff_dw_basin"),
  tile_ids = tile_id, download = TRUE, download_dir = "env90m",
  file_format = "txt", delete_zips = TRUE, ignore_missing = FALSE,
  tempdir = NULL, quiet = FALSE
)
message("  Hydrography -> env90m/hydrography90m_v1_0/")

# ============================================================
# STEP 3: Write subcatchment ID reference file
# ============================================================
# Used by get_predict_table() in script 02 to restrict the prediction table
# to the basin's subcatchments.

fwrite(data.table(subc_id = subc_ids),
       "env90m/subc_ids_basin.txt", col.names = FALSE)

# ============================================================
# SUMMARY
# ============================================================

n_climate   <- length(list.files("env90m/chelsa_bioclim_v2_1",      pattern = ".txt$", recursive = TRUE))
n_landcover <- length(list.files("env90m/esa_cci_landcover_v2_1_1", pattern = ".txt$", recursive = TRUE))
n_hydro     <- length(list.files("env90m/hydrography90m_v1_0",      pattern = ".txt$", recursive = TRUE))

message("\n", paste(rep("=", 60), collapse = ""))
message("DOWNLOAD COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message(sprintf("  Climate: %d  Land cover: %d  Hydrography: %d files",
                n_climate, n_landcover, n_hydro))
message(sprintf("  Subcatchment IDs: %d -> env90m/subc_ids_basin.txt", length(subc_ids)))
message("\nNext: 02_create_prediction_table.R")
