#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_download_lake_data.R   (Module 12 -- Lake analysis)
#
# Download the Hydrography90m and Env90m layers the lake workflow needs.
# Isolated from the analysis steps because these are large, slow downloads
# that only need to run once; later scripts read the files from disk.
#
# Each downstream function needs a different subset of layers:
#   get_lake_intersection(): basin, segment (stream network), accumulation
#   get_lake_catchment():    direction (flow direction)
#   land cover analysis:     sub_catchment
#
# Workflow:
#   1. Set tile ID and variable list
#   2. Download Hydrography90m tif tiles
#   3. Download Env90m ESA CCI land cover tables (1992-2020)
#
# OUTPUT:
#   - spatial/{basin,sub_catchment,segment,accumulation,direction}*.tif
#   - env90m/esa_cci_landcover_v2_1_1/*.txt
#
# REQUIRES: internet access (Hydrography90m + Env90m download endpoints).
#
# LOCATION: workflows/12_lakes/01_download_lake_data.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("spatial", recursive = TRUE, showWarnings = FALSE)
dir.create("env90m", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Tile covering the study area. Set manually or look up with get_tile_id().
TILE_ID  <- "h20v04"

# Hydrography90m layers needed across the lake functions (see header).
VARS_TIF <- c("basin", "sub_catchment", "segment", "accumulation", "direction")

# Longer timeout so the large tile downloads are not interrupted.
options(timeout = 1000)

# ============================================================
# STEP 1: Download Hydrography90m tiles
# ============================================================

message("\n=== Downloading Hydrography90m tiles ===")

download_tiles(variable     = VARS_TIF,
               tile_id      = TILE_ID,
               file_format  = "tif",
               download_dir = "spatial")

# ============================================================
# STEP 2: Download Env90m land cover tables (1992-2020)
# ============================================================

message("\n=== Downloading Env90m land cover tables ===")

download_landcover_tables(
  base_vars    = "ALL",
  years        = "ALL",
  tile_ids     = TILE_ID,
  download     = TRUE,
  download_dir = "env90m",
  file_format  = "txt",
  delete_zips  = TRUE,
  quiet        = TRUE
)

message("\nDownloads complete.")
message("Next: 12_lakes/02_extract_lake_intersection.R")
