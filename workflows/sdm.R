# ============================================================================
# DOWNLOAD ENVIRONMENTAL DATA FROM ENVIRONMENT90M
# ============================================================================
# Purpose: Download environmental predictor variables as TXT files from Env90m
# Input: Previously downloaded stream network (partial_stream_network.gpkg)
# Output: TXT files with environmental data organized by dataset
# ============================================================================
# Date: 2026-03-23
# ============================================================================

library(hydrographr)
library(data.table)
library(dplyr)

# ============================================================================
# SETUP
# ============================================================================

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)

# ============================================================================
# LOAD EXISTING STREAM NETWORK
# ============================================================================

message("\n=== Loading Existing Stream Network ===")

stream_network <- hydrographr::read_geopackage(
    "spatial/basin/stream_network_pruned.gpkg",
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

dir.create("env90m", showWarnings = FALSE, recursive = TRUE)

message("✓ Created directory: env90m/")

# ============================================================================
# DOWNLOAD ENVIRONMENTAL DATA TABLES
# ============================================================================

message("\n=== Downloading Environmental Data Tables ===")
message("This may take several minutes depending on data size...")
# message("Total expected download: ~ 72.3 GB")

# --------------------------------------------------------------------------
# 1. OBSERVED CLIMATE VARIABLES
# --------------------------------------------------------------------------

message("\n--- Downloading Climate Variables ---")

download_observed_climate_tables(
  subset = c("bio01_1981-2010_observed", "bio04_1981-2010_observed",
             "bio05_1981-2010_observed", "bio06_1981-2010_observed",
             "bio15_1981-2010_observed", "bio17_1981-2010_observed",
             "bio18_1981-2010_observed"),
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
  file_size <- file.info(climate_file)$size / 1024 / 1024
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
  base_vars = c("c10", "c20", "c30", "c40", "c50", "c60", "c120", "c130",
                "c150", "c160", "c180", "c190", "c200", "c210"),
  years = c("1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000",
            "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010",
            "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"),
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
  file_size <- file.info(landcover_file)$size / 1024 / 1024
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
  subset = c("cti", "order_strahler", "length", "cum_length", "gradient", "elev_drop",
             "accumulation", #"channel_grad_dw_seg", "channel_grad_up_seg",
             "channel_elv_dw_seg", "channel_elv_up_seg", "connections",
             "stream_dist_dw_near", "stream_dist_up_near", "slope_grad_dw_cel",
             "outlet_diff_dw_basin"),
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

subc_ids_dt <- data.table(subc_id = subc_ids)
fwrite(subc_ids_dt,
       file = "env90m/subc_ids_basin.txt",
       col.names = FALSE)

message(sprintf("✓ Saved %d subcatchment IDs to: env90m/subc_ids.txt",
                length(subc_ids)))

# ============================================================================
# VERIFICATION & SUMMARY
# ============================================================================

message("\n=== Download Verification ===")

n_climate  <- length(list.files("env90m/chelsa_bioclim_v2_1",      pattern = ".txt$", recursive = TRUE))
n_landcover <- length(list.files("env90m/esa_cci_landcover_v2_1_1", pattern = ".txt$", recursive = TRUE))
n_hydro    <- length(list.files("env90m/hydrography90m_v1_0",       pattern = ".txt$", recursive = TRUE))

message(sprintf("\nFiles downloaded:"))
message(sprintf("  Climate:    %d files", n_climate))
message(sprintf("  Land cover: %d files", n_landcover))
message(sprintf("  Hydrography:%d files", n_hydro))
message(sprintf("  Total:      %d files", n_climate + n_landcover + n_hydro))

total_size <- sum(
  file.info(list.files("env90m", pattern = ".txt$",
                       recursive = TRUE, full.names = TRUE))$size,
  na.rm = TRUE
) / 1024 / 1024 / 1024

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
message("  Climate:     bio01, bio04, bio05, bio06, bio15, bio17, bio18 (1981-2010_observed)")
message("  Land cover:  c10, c20, c30, c40, c50, c60, c120, c130, c150, c160, c180, c190, c200, c210 (1992-2020)")
message("  Hydrography: order_strahler, length, cum_length, gradient, elev_drop, accumulation,")
message("               channel_grad_dw/up_seg, channel_elv_dw/up_seg, connections,")
message("               stream_dist_dw/up_near, slope_grad_dw_cel")

message("\nTiles downloaded:")
message(sprintf("  %s", paste(tile_id, collapse = ", ")))

message("\nNext steps:")
message("  1. Run script: 02_create_prediction_table.R")
message("  2. This will create the pred_tab.csv for SDM modeling")

message(sprintf("\nNOTE: Raw downloaded tables can be deleted after creating"))
message(sprintf("      the prediction table to save disk space (~%.1f GB)", total_size))

message("\n========================================\n")
# ============================================================================
# CREATE PREDICTION TABLE FOR SPECIES DISTRIBUTION MODELING
# ============================================================================
# Purpose: Create prediction table from downloaded Environment90m data
# Input: Downloaded environmental tables from script 01
# Output: predict_table.csv - ready for SDM modeling
# ============================================================================
# Date: 2026-04-23
# ============================================================================

library(hydrographr)
library(data.table)
library(dplyr)

# ============================================================================
# SETUP
# ============================================================================

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)

# ============================================================================
# VERIFY INPUT FILES EXIST
# ============================================================================

message("\n=== Verifying Input Files ===")

if (!file.exists("env90m/subc_ids_basin.txt")) {
  stop("ERROR: env90m/subc_ids_basin.txt not found!",
       "\n  Please run script 01_download_environmental_variables.R first.")
}

required_dirs <- c(
  "env90m/chelsa_bioclim_v2_1",
  "env90m/esa_cci_landcover_v2_1_1",
  "env90m/hydrography90m_v1_0"
)

for (dir in required_dirs) {
  if (!dir.exists(dir)) {
    stop("ERROR: Required directory not found: ", dir,
         "\n  Please run script 01_download_environmental_variables.R first.")
  }
}

message("✓ All required input files found")

# ============================================================================
# CONFIGURATION
# ============================================================================

message("\n=== Configuration ===")

# Define variables to include in prediction table
variables <- c(
  "bio01_1981-2010_observed", "bio04_1981-2010_observed", "bio05_1981-2010_observed",
  "bio06_1981-2010_observed", "bio15_1981-2010_observed", "bio17_1981-2010_observed",
  "bio18_1981-2010_observed",

  # Land cover 2020 (most recent; species observations up to 2024)
  "c10_2020", "c20_2020", "c30_2020", "c40_2020", "c50_2020", "c60_2020",
  "c120_2020", "c130_2020", "c150_2020", "c160_2020", "c180_2020",
  "c190_2020", "c200_2020", "c210_2020",

  "cti", "order_strahler", "length", "cum_length", "gradient", "elev_drop", "accumulation",
   "channel_elv_dw_seg", "channel_elv_up_seg", "outlet_diff_dw_basin",
   "stream_dist_dw_near", "stream_dist_up_near", "slope_grad_dw_cel"
)

# Define statistics to calculate
statistics <- c("mean")

# Define tile IDs (same as in script 01)
tile_id <- c("h18v04", "h20v04")

# Detect available cores for parallel processing
n_cores <- parallel::detectCores() - 1

message(sprintf("Variables: %d total", length(variables)))
message(sprintf("Statistics: %s", paste(statistics, collapse = ", ")))
message(sprintf("Tiles: %s", paste(tile_id, collapse = ", ")))
message(sprintf("CPU cores to use: %d", n_cores))

# ============================================================================
# CREATE PREDICTION TABLE
# ============================================================================

message("\n=== Creating Prediction Table ===")
message("This may take several minutes depending on data size...")

# Define output path
output_file <- "env90m/predict_table.csv"

# Run get_predict_table
predict_table <- get_predict_table(
  variable = variables,
  statistics = statistics,
  tile_id = tile_id,
  input_var_path = "env90m",
  subcatch_id = "env90m/subc_ids_basin.txt",
  out_file_path = output_file,
  read = TRUE,
  quiet = FALSE,
  n_cores = n_cores,
  overwrite = TRUE
)

message(sprintf("\n✓ Prediction table created: %d rows, %d columns",
                nrow(predict_table),
                ncol(predict_table)))

# Simplify bioclimatic variable names — remove temporal suffix
# bio01_1981-2010_observed_mean → bio01_mean
colnames(predict_table) <- gsub("_1981-2010_observed", "", colnames(predict_table))

message("  Column names after simplification:")
print(names(predict_table))

# ============================================================
# RESCALE VARIABLES — apply scale factors from Hydrography90m paper
# ============================================================

message("\n=== Rescaling variables ===")
# predict_table <- fread(output_file)

# CTI: raw values have scale factor of 10^8
# divide to get dimensionless wetness index (typical range 0-30)
predict_table <- predict_table %>%
  mutate(cti_mean = cti_mean / 1e8,
         slope_grad_dw_cel_mean   = slope_grad_dw_cel_mean / 1e6)


cat("CTI range after rescaling:",
    round(range(predict_table$cti_mean, na.rm=TRUE), 3), "\n")

# Bioclimatic variables: all raw values are × 10
# bio01, 05, 06, 08, 09, 10, 11: temperature variables in Kelvin × 10
#   → divide by 10 then subtract 273.15 to convert to °C
# bio04: temperature seasonality (SD × 100) × 10 → divide by 10 only
# bio02, 03, 07: temperature range/isothermality × 10 → divide by 10 only
# bio12-19: precipitation variables × 10 → divide by 10 only (mm)

# Variables that need /10 AND -273.15 (temperature in Kelvin)
bio_kelvin <- c("bio01", "bio05", "bio06", "bio08", "bio09", "bio10", "bio11")

# Variables that need /10 only
bio_scale_only <- c("bio02", "bio03", "bio04", "bio07",
                    "bio12", "bio13", "bio14", "bio15",
                    "bio16", "bio17", "bio18", "bio19")

# Apply to columns present in predict table
# Column names follow pattern bio01_mean, bio05_mean etc.
for (bio in bio_kelvin) {
  col <- paste0(bio, "_mean")
  if (col %in% names(predict_table)) {
    predict_table <- predict_table %>%
      mutate(!!col := (.data[[col]] / 10) - 273.15)
    cat(col, "range after rescaling:",
        round(range(predict_table[[col]], na.rm=TRUE), 2), "°C\n")
  }
}

for (bio in bio_scale_only) {
  col <- paste0(bio, "_mean")
  if (col %in% names(predict_table)) {
    predict_table <- predict_table %>%
      mutate(!!col := .data[[col]] / 10)
    cat(col, "range after rescaling:",
        round(range(predict_table[[col]], na.rm=TRUE), 2), "\n")
  }
}

# Save corrected predict table
fwrite(predict_table, "env90m/predict_table.csv")
message("Saved rescaled predict table: env90m/predict_table.csv")

# ============================================================================
# HANDLE MISSING DATA (IF ANY)
# ============================================================================

message("\n=== Checking for Missing Data ===")

# predict_table <- fread(output_file)

missing_counts <- predict_table[, lapply(.SD, function(x) sum(is.na(x)))]
has_missing <- any(missing_counts > 0)

if (has_missing) {
  message("\n⚠ Missing values detected:")

  for (col in names(missing_counts)) {
    if (col != "subc_id") {
      n_missing <- missing_counts[[col]]
      if (n_missing > 0) {
        pct_missing <- 100 * n_missing / nrow(predict_table)
        message(sprintf("  %s: %d (%.1f%%)", col, n_missing, pct_missing))
      }
    }
  }

  message("\nOptions for handling missing data:")
  message("  1. Keep all rows (NAs will need to be handled in modeling)")
  message("  2. Remove rows with any missing values")
  message("  3. Remove only specific problematic variables")

  message("\nSaving two versions:")

  fwrite(predict_table, "env90m/predict_table_full.csv")
  message("  ✓ Saved: env90m/predict_table_full.csv (all rows, including NAs)")

  predict_table_complete <- na.omit(predict_table)
  fwrite(predict_table_complete, "env90m/predict_table_complete.csv")
  message(sprintf("  ✓ Saved: env90m/predict_table_complete.csv (%d rows, NAs removed)",
                  nrow(predict_table_complete)))

  message(sprintf("\nRemoved %d rows (%.1f%%) with missing values",
                  nrow(predict_table) - nrow(predict_table_complete),
                  100 * (nrow(predict_table) - nrow(predict_table_complete)) / nrow(predict_table)))

  predict_table <- predict_table_complete

} else {
  message("✓ No missing values detected")
}

# ============================================================================
# DATA QUALITY CHECKS
# ============================================================================

message("\n=== Data Quality Summary ===")

message(sprintf("\nDimensions: %d rows × %d columns", nrow(predict_table), ncol(predict_table)))

message("\nColumn names:")
print(names(predict_table))

message("\nBasic statistics:")
print(summary(predict_table))

message("\nChecking for extreme values...")
for (col in names(predict_table)) {
  if (col != "subc_id" && is.numeric(predict_table[[col]])) {
    q <- quantile(predict_table[[col]], probs = c(0.01, 0.99), na.rm = TRUE)
    n_outliers <- sum(predict_table[[col]] < q[1] | predict_table[[col]] > q[2], na.rm = TRUE)
    if (n_outliers > 0) {
      pct_outliers <- 100 * n_outliers / nrow(predict_table)
      message(sprintf("  %s: %d values (%.1f%%) outside 1st-99th percentile",
                      col, n_outliers, pct_outliers))
    }
  }
}

# ============================================================================
# OPTIONAL: CLEAN UP DOWNLOADED TABLES
# ============================================================================

message("\n=== Disk Space Management ===")

downloaded_size <- sum(
  file.info(list.files("env90m", pattern = ".txt$",
                       recursive = TRUE, full.names = TRUE))$size,
  na.rm = TRUE
) / 1024 / 1024 / 1024

predict_table_size <- file.info("env90m/predict_table.csv")$size / 1024 / 1024

message(sprintf("Downloaded tables: %.2f GB", downloaded_size))
message(sprintf("Prediction table: %.2f MB", predict_table_size))
message(sprintf("Space savings if deleted: %.2f GB", downloaded_size))

message("\nTo free up disk space, you can delete the downloaded tables:")
message("  env90m/chelsa_bioclim_v2_1/")
message("  env90m/esa_cci_landcover_v2_1_1/")
message("  env90m/hydrography90m_v1_0/")

# Uncomment to automatically delete
# unlink("env90m/chelsa_bioclim_v2_1", recursive = TRUE)
# unlink("env90m/esa_cci_landcover_v2_1_1", recursive = TRUE)
# unlink("env90m/hydrography90m_v1_0", recursive = TRUE)
# message("\n✓ Deleted downloaded tables")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

message("\n========================================")
message("=== PREDICTION TABLE CREATION COMPLETE ===")
message("========================================")

message("\nFiles created:")
message("  ✓ env90m/predict_table.csv (main file)")
if (has_missing) {
  message("  ✓ env90m/predict_table_full.csv (with NAs)")
  message("  ✓ env90m/predict_table_complete.csv (NAs removed)")
}

message("\nPrediction table summary:")
message(sprintf("  Rows:      %d", nrow(predict_table)))
message(sprintf("  Columns:   %d", ncol(predict_table)))
message(sprintf("  Variables: %d", length(variables)))
message(sprintf("  File size: %.2f MB", predict_table_size))

message("\nNext steps:")
message("  1. Load predict_table.csv in your SDM workflow")
message("  2. Join with species occurrence data")
message("  3. Run species distribution models")
message("  4. Predict habitat suitability")

message("\nExample usage:")
message("  predict_table <- fread('env90m/predict_table.csv')")

message("\n========================================\n")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03b_prepare_sdm_data.R
#
# Prepare species-level presence/absence datasets for SSN and SDM modeling.
#
# Training + prediction extent: full Vjosa/Aoos basin
#
# For each target species:
#   - Presences: HCMR confirmed presences + GBIF records within basin
#   - True absences: HCMR surveyed sites where species was NOT recorded
#   - Pseudoabsences: randomly sampled from basin_subc_ids_pruned,
#     excluding subcatchments already covered by HCMR surveys,
#     number fixed to maintain prevalence = 0.1
#     (n_pseudoabs = n_presences * 9 - n_true_absences, min 0)
#
# Coordinates for SSN snapping:
#   - Presences/true absences: longitude_snapped/latitude_snapped from HCMR
#   - Pseudoabsences: stream reach centroids from basin network
#
# Input:
#   - points_snapped/fish/fish_all_species_snapped.csv
#   - spatial/basin/basin_subc_ids_pruned.csv
#   - spatial/basin/stream_network_pruned.gpkg
#   - env90m/predict_table.csv
#   - points_original/fish/species_list_sarantaporos.txt
#
# Output:
#   - sdm/input/occurr/{species}/occurr_{species}.csv  (per species)
#   - sdm/input/occurr/species_data_summary.csv
#
# LOCATION: workflows/07_sdm/03b_prepare_sdm_data.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(data.table)
library(dplyr)
library(sf)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

TARGET_PREVALENCE <- 0.1
set.seed(42)

# ============================================================
# SETUP
# ============================================================

dir.create("sdm/input/occurr", recursive = TRUE, showWarnings = FALSE)

target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>% unique()

message("Target species: ", length(target_species))

# ============================================================
# STEP 1: Load occurrence data
# ============================================================

message("\n=== Step 1: Loading occurrence data ===")

fish_all <- fread("points_snapped/fish/fish_all_species_snapped.csv") %>%
  filter(species %in% target_species)

message("  Total records for target species: ", nrow(fish_all))
message("  Sources: ", paste(unique(fish_all$source), collapse = ", "))

# ============================================================
# STEP 2: Load environmental predictors
# ============================================================

message("\n=== Step 2: Loading environmental predictors ===")

predict_table <- fread("env90m/predict_table.csv")
message("  Predict table: ", nrow(predict_table), " subcatchments, ",
        ncol(predict_table), " columns")

predictor_cols <- names(predict_table) %>% setdiff("subc_id")
message("  Predictor columns: ", length(predictor_cols))

# ============================================================
# STEP 3: Load basin subcatchment IDs + stream network centroids
# ============================================================

message("\n=== Step 3: Loading basin subcatchments + centroids ===")

basin_subc_ids_pruned <- fread("spatial/basin/basin_subc_ids_pruned.csv") %>%
  pull(subc_id)

message("  Basin pruned subcatchments: ", length(basin_subc_ids_pruned))

# Stream reach centroids for pseudoabsence coordinates
basin_centroids <- st_read("spatial/basin/stream_network_pruned.gpkg",
                           quiet = TRUE) %>%
  distinct(subc_id, .keep_all = TRUE) %>%
  st_centroid() %>%
  st_transform(crs = 4326) %>%
  mutate(
    longitude_snapped = st_coordinates(.)[, 1],
    latitude_snapped  = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(subc_id, longitude_snapped, latitude_snapped)

message("  Basin centroids available: ", nrow(basin_centroids))

# HCMR-surveyed subcatchments — excluded from pseudoabsence pool
hcmr_surveyed_subcs <- fish_all %>%
  filter(source == "HCMR") %>%
  pull(subc_id) %>%
  unique()

message("  HCMR surveyed subcatchments: ", length(hcmr_surveyed_subcs))

# Pseudoabsence pool: basin pruned network minus HCMR surveyed subcatchments
pseudoabs_pool <- setdiff(basin_subc_ids_pruned, hcmr_surveyed_subcs)
message("  Pseudoabsence pool size: ", length(pseudoabs_pool))

# ============================================================
# STEP 4: Build per-species datasets
# ============================================================

message("\n=== Step 4: Building per-species datasets ===")

species_summary <- list()

for (sp in target_species) {

  message("\n  --- ", sp, " ---")

  sp_dir <- file.path("sdm/input/occurr", sp)
  dir.create(sp_dir, recursive = TRUE, showWarnings = FALSE)

  # ---- Presences ----
  presences <- fish_all %>%
    filter(species == sp) %>%
    distinct(subc_id, source, longitude_snapped, latitude_snapped) %>%
    mutate(pres_abs = 1L)

  n_pres <- nrow(presences)
  message("    Presences: ", n_pres,
          " (HCMR: ", sum(presences$source == "HCMR"),
          ", GBIF: ", sum(presences$source == "GBIF"), ")")

  if (n_pres == 0) {
    message("    Skipping — no presences")
    next
  }

  # ---- True absences ----
  true_abs_subcs <- setdiff(hcmr_surveyed_subcs, presences$subc_id)

  true_abs_coords <- fish_all %>%
    filter(subc_id %in% true_abs_subcs) %>%
    distinct(subc_id, longitude_snapped, latitude_snapped)

  true_absences <- data.frame(
    subc_id  = true_abs_subcs,
    source   = "HCMR_true_absence",
    pres_abs = 0L
  ) %>%
    left_join(true_abs_coords, by = "subc_id")

  n_true_abs <- nrow(true_absences)
  message("    True absences: ", n_true_abs)

  # ---- Pseudoabsences ----
  n_abs_needed       <- round(n_pres * (1 - TARGET_PREVALENCE) / TARGET_PREVALENCE)
  n_pseudoabs_needed <- max(0L, n_abs_needed - n_true_abs)

  message("    Total absences needed (prevalence=", TARGET_PREVALENCE, "): ",
          n_abs_needed)
  message("    Pseudoabsences to sample: ", n_pseudoabs_needed)

  sp_pseudoabs_pool <- setdiff(pseudoabs_pool, presences$subc_id)

  if (n_pseudoabs_needed > length(sp_pseudoabs_pool)) {
    message("    WARNING: pool (", length(sp_pseudoabs_pool),
            ") smaller than needed (", n_pseudoabs_needed,
            ") — using all available")
    n_pseudoabs_needed <- length(sp_pseudoabs_pool)
  }

  if (n_pseudoabs_needed > 0) {
    pseudoabs_subcs <- sample(sp_pseudoabs_pool, n_pseudoabs_needed,
                              replace = FALSE)

    pseudoabsences <- data.frame(
      subc_id  = pseudoabs_subcs,
      source   = "pseudoabsence",
      pres_abs = 0L
    ) %>%
      left_join(basin_centroids, by = "subc_id")
  } else {
    pseudoabsences <- data.frame(
      subc_id           = integer(0),
      source            = character(0),
      pres_abs          = integer(0),
      longitude_snapped = numeric(0),
      latitude_snapped  = numeric(0)
    )
  }

  message("    Pseudoabsences sampled: ", nrow(pseudoabsences))

  # ---- Combine ----
  sp_data <- bind_rows(presences, true_absences, pseudoabsences)

  actual_prevalence <- mean(sp_data$pres_abs)
  message("    Actual prevalence: ", round(actual_prevalence, 3),
          " (target: ", TARGET_PREVALENCE, ")")

  # ---- Check coordinates ----
  n_missing_coords <- sum(is.na(sp_data$longitude_snapped) |
                            is.na(sp_data$latitude_snapped))
  if (n_missing_coords > 0) {
    message("    WARNING: ", n_missing_coords,
            " rows missing coordinates — removing")
    sp_data <- sp_data %>%
      filter(!is.na(longitude_snapped), !is.na(latitude_snapped))
  }

  # ---- Join environmental predictors ----
  sp_data <- sp_data %>%
    left_join(
      predict_table %>% select(subc_id, all_of(predictor_cols)),
      by = "subc_id"
    )

  n_missing_env <- sum(is.na(sp_data[[predictor_cols[1]]]))
  if (n_missing_env > 0) {
    message("    WARNING: ", n_missing_env,
            " rows missing environmental data — removing")
    sp_data <- sp_data %>%
      filter(if_all(all_of(predictor_cols), ~ !is.na(.)))
  }

  message("    Final dataset: ", nrow(sp_data), " rows (",
          sum(sp_data$pres_abs), " presences, ",
          sum(sp_data$pres_abs == 0), " absences)")

  # ---- Save ----
  fwrite(sp_data, file.path(sp_dir, paste0("occurr_", sp, ".csv")))
  message("    Saved: sdm/input/occurr/", sp, "/occurr_", sp, ".csv")

  species_summary[[sp]] <- data.frame(
    species          = sp,
    n_presences      = sum(sp_data$pres_abs == 1),
    n_hcmr_pres      = sum(sp_data$pres_abs == 1 & sp_data$source == "HCMR"),
    n_gbif_pres      = sum(sp_data$pres_abs == 1 & sp_data$source == "GBIF"),
    n_true_absences  = sum(sp_data$source == "HCMR_true_absence"),
    n_pseudoabsences = sum(sp_data$source == "pseudoabsence"),
    prevalence       = round(mean(sp_data$pres_abs), 3),
    n_env_predictors = length(predictor_cols),
    has_coords       = TRUE
  )
}

# ============================================================
# STEP 5: Save summary
# ============================================================

message("\n=== Step 5: Saving summary ===")

summary_df <- rbindlist(species_summary)
fwrite(summary_df, "sdm/input/occurr/species_data_summary.csv")

cat("\n")
print(summary_df)

message("\n", paste(rep("=", 80), collapse = ""))
message("SDM DATA PREPARATION COMPLETE")
message(paste(rep("=", 80), collapse = ""))
message("\nTraining extent:   full Vjosa/Aoos basin")
message("Pseudoabsence pool: basin_subc_ids_pruned (", length(basin_subc_ids_pruned), " subcatchments)")
message("Target prevalence: ", TARGET_PREVALENCE)
message("Species processed: ", nrow(summary_df))
message("\nOutputs:")
message("  sdm/input/occurr/{species}/occurr_{species}.csv")
message("  sdm/input/occurr/species_data_summary.csv")
message("\nNext: 04_ssn_models.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03_check_multicollinearity.R
#
# Quality check and collinearity assessment of the basin prediction table.
# Retains VIF-filtered predictor variables for use in SDM fitting.
#
# Workflow:
#   1. Load predict table + quality checks
#   2. Drop near-zero variance land cover classes
#   3. Correlation matrix visualisation
#   4. VIF-based variable selection
#   5. Save retained variables + filtered predict table
#
# Input:
#   - env90m/predict_table.csv
#   - spatial/basin/basin_subc_ids_pruned.csv
#
# Output:
#   - env90m/predict_table_vif.csv   (filtered predict table)
#   - env90m/selected_vars.csv             (retained variable names)
#
# LOCATION: workflows/07_sdm/03_check_multicollinearity.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(data.table)
library(dplyr)
library(corrplot)
library(usdm)
library(tidyr)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# STEP 1: Load predict table + quality checks
# ============================================================

message("\n=== Step 1: Loading and checking predict table ===")

predict_table <- fread("env90m/predict_table.csv")
message("  Rows: ", nrow(predict_table))
message("  Columns: ", ncol(predict_table))

basin_subc_ids_pruned <- fread("spatial/basin/basin_subc_ids_pruned.csv") %>%
  pull(subc_id)

# Check row count matches expected subcatchments
ifelse(
  nrow(predict_table) == length(basin_subc_ids_pruned),
  "Row count matches expected subcatchments.",
  paste0("MISMATCH: predict table has ", nrow(predict_table),
         " rows but expected ", length(basin_subc_ids_pruned))
)

# Check for missing values
missing_counts <- colSums(is.na(predict_table))
if (any(missing_counts > 0)) {
  message("  Columns with missing values:")
  print(missing_counts[missing_counts > 0])
} else {
  message("  No missing values")
}

# Simplify climate variable names for readability
colnames(predict_table) <- gsub("_1981-2010_observed", "", colnames(predict_table))
message("  Column names after simplification:")
print(names(predict_table))

# ============================================================
# STEP 2: Drop near-zero variance land cover classes
# ============================================================

message("\n=== Step 2: Dropping near-zero variance land cover classes ===")

# Check which land cover classes have very few non-zero subcatchments
lc_cols <- names(predict_table)[grepl("_y2020$", names(predict_table))]
lc_nonzero <- predict_table %>%
  select(all_of(lc_cols)) %>%
  summarise(across(everything(), ~ sum(. > 0, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "class", values_to = "n_nonzero") %>%
  arrange(n_nonzero)

message("  Land cover classes by non-zero subcatchment count:")
print(lc_nonzero)

# Drop land cover classes with near-zero variance
# Threshold: fewer than 100 non-zero subcatchments (<2% of network)
# Also drop ecologically irrelevant classes for a mountain river basin
lc_drop <- lc_nonzero %>%
  filter(n_nonzero < 100) %>%
  pull(class)

# Drop classes not expected in Greek freshwater subcatchments
# lc_drop <- c("c130_y2020", "c150_y2020", "c190_y2020", "c200_y2020")
predict_table <- predict_table %>%
  dplyr::select(-any_of(lc_drop))

message("  Dropped: ", paste(lc_drop, collapse = ", "))
message("  Remaining columns: ", ncol(predict_table))

# ============================================================
# STEP 3: Correlation matrix
# ============================================================

message("\n=== Step 3: Correlation matrix ===")

# Select numeric predictor variables (mean values + land cover)
numeric_vars <- predict_table %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(ends_with("_mean") | ends_with("2020"))

message("  Predictor columns for collinearity check: ", ncol(numeric_vars))

cor_matrix <- cor(numeric_vars, use = "complete.obs")

corrplot(cor_matrix,
         method  = "color",
         type    = "lower",
         tl.cex  = 0.6,
         diag    = FALSE,
         col     = COL2("RdBu", 200))

# ============================================================
# STEP 4: VIF-based variable selection
# ============================================================

message("\n=== Step 4: VIF-based variable selection (th = 10) ===")

vif_results <- vifstep(numeric_vars, th = 10)
vif_results@results

selected_vars <- vif_results@results$Variables
message("  Variables retained after VIF: ", length(selected_vars))
print(selected_vars)

# replace channel_elv_up_seg_mean with channel_elv_up_seg_mean for interpretability
selected_vars <- gsub("channel_elv_up_seg_mean",
                      "channel_elv_dw_seg_mean",
                      selected_vars)

# ============================================================
# STEP 5: Save outputs
# ============================================================

message("\n=== Step 5: Saving outputs ===")

# Filtered predict table — subc_id + retained predictors only
predict_table_vif <- predict_table %>%
  dplyr::select(any_of(c("subc_id", selected_vars)))

fwrite(predict_table_vif, "env90m/predict_table_vif.csv")
message("  Saved: env90m/predict_table_vif.csv")
message("  Dimensions: ", nrow(predict_table_vif), " x ", ncol(predict_table_vif))

# Retained variable names — used by 03b_prepare_sdm_data.R and 04_ssn_models.R
fwrite(
  data.table(variable = selected_vars),
  "env90m/selected_vars.csv"
)
message("  Saved: env90m/selected_vars.csv")

# ============================================================
# SUMMARY
# ============================================================

message("\n=== Collinearity Check Complete ===")
message("  Input predictors:    ", ncol(numeric_vars))
message("  Retained after VIF:  ", length(selected_vars))
message("  Removed:             ", ncol(numeric_vars) - length(selected_vars))
message("\nOutputs:")
message("  env90m/predict_table_vif.csv")
message("  env90m/selected_vars.csv")
message("\nNext: 03b_prepare_sdm_data.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 04_ssn_models.R
#
# Fit Spatial Stream Network (SSN) models for freshwater fish species
# distribution in the Vjosa/Aoos basin.
#
# Training + prediction extent: full basin (basin_subc_ids_pruned)
# This provides a larger environmental space for model fitting and
# more pseudoabsences distributed across the basin.
#
# Each species gets its own SSN object because observation sites differ
# (presences + true absences + pseudoabsences are species-specific).
# Prediction sites are shared across species and built once.
#
# Workflow (outside loop — once):
#   1. Load basin stream network + predict table
#   2. Build prediction sites sf object (basin centroids)
#   3. Build LSN edges
#   4. Calculate upstream distances for edges
#   5. Calculate AFVs for edges
#
# Workflow (inside loop — per species):
#   6.  Load species occurrence file from 03b
#   7.  Build obs_sites sf for this species
#   8.  Snap obs + pred sites to LSN
#   9.  Calculate upstream distances for obs + pred sites
#   10. Calculate AFVs for obs + pred sites
#   11. Assemble SSN object + distance matrices
#   12. Join + scale predictors
#   13. Torgegram
#   14. Fit covariance structure candidates, select best by AICc
#   15. Refit with REML, predict across basin, save
#   16. Join subbasin predictions to stream network gpkg + save summary
#
# Input:
#   - spatial/basin/stream_network_pruned.gpkg
#   - sdm/input/occurr/{species}/occurr_{species}.csv  (from 03b)
#   - spatial/basin/basin_subc_ids_pruned.csv
#   - points_snapped/subbasin/subbasin_subc_ids_pruned.csv
#   - env90m/predict_table.csv
#   - points_original/fish/species_list_sarantaporos.txt
#
# Output:
#   - spatial/basin/ssn/{species}.ssn
#   - sdm/ssn_models/ssn_{species}.rds
#   - sdm/ssn_models/torgegram_{species}.png
#   - sdm/ssn_models/model_summary.csv
#   - sdm/predictions/pred_{species}.csv       (full basin)
#   - spatial/subbasin/stream_network_predictions.gpkg  (subbasin only)
#
# LOCATION: workflows/07_sdm/04_ssn_models.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(SSNbler)
library(SSN2)
library(hydrographr)
library(sf)
library(data.table)
library(dplyr)
library(ggplot2)
library(broom)
library(dismo)

# Compute MCC across thresholds (same as 05_maxent.R and 06_random_forest.R)
compute_mcc_threshold <- function(pres_preds, abs_preds,
                                  thresh_seq = seq(0, 1, by = 0.01)) {
  mcc_vals <- sapply(thresh_seq, function(t) {
    TP <- as.numeric(sum(pres_preds >= t, na.rm = TRUE))
    FN <- as.numeric(sum(pres_preds <  t, na.rm = TRUE))
    FP <- as.numeric(sum(abs_preds  >= t, na.rm = TRUE))
    TN <- as.numeric(sum(abs_preds  <  t, na.rm = TRUE))
    denom <- sqrt((TP+FP) * (TP+FN) * (TN+FP) * (TN+FN))
    if (is.na(denom) || denom == 0) return(NA)
    (TP * TN - FP * FN) / denom
  })
  best_thresh <- thresh_seq[which.max(mcc_vals)]
  best_mcc    <- max(mcc_vals, na.rm = TRUE)
  return(list(threshold = best_thresh, mcc = best_mcc))
}

select <- dplyr::select

source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

dir.create("spatial/basin/ssn", recursive = TRUE, showWarnings = FALSE)
dir.create("sdm/ssn_models",    recursive = TRUE, showWarnings = FALSE)
dir.create("sdm/predictions",   recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Target CRS: EPSG 27704 WGS 84 / Equi7 Europe (metres)
# Pan-European projected CRS chosen for workflow generalisability
PROJ_CRS <- 27704

# Snap tolerance in metres for sites_to_lsn()
SNAP_TOL <- 100

# Fixed-effect predictors
# cum_length: longitudinal position in network
# bio01_mean: mean annual temperature (most interpretable bioclimatic variable)
# cti_mean: compound topographic index (habitat wetness proxy)
FORMULA_PREDICTORS <- c("cum_length", "bio01_mean", "cti_mean")

# Occurrence files directory (output of 03b_prepare_sdm_data.R)
OCCURR_DIR <- "sdm/input/occurr"

# Target species
target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>% unique()

message("Target species (", length(target_species), "):")
for (sp in target_species) message("  ", sp)

# ============================================================
# STEP 1: Load basin stream network + predict table
# ============================================================

message("\n=== Step 1: Loading inputs ===")

basin_streams <- st_read("spatial/basin/stream_network_pruned.gpkg") %>%
  st_transform(crs = PROJ_CRS)

# Rename existing length column to avoid conflict with SSNbler's Length
basin_streams <- basin_streams %>%
  rename(length_reach = length)

message("  Basin stream segments: ", nrow(basin_streams))

geom_types <- unique(as.character(st_geometry_type(basin_streams)))
if (any(geom_types == "MULTILINESTRING")) {
  message("  Converting MULTILINESTRING to LINESTRING...")
  basin_streams <- st_cast(basin_streams, "LINESTRING")
}
message("  Geometry type: ", paste(unique(as.character(
  st_geometry_type(basin_streams))), collapse = ", "))

predict_table <- fread("env90m/predict_table.csv")
message("  Predict table: ", nrow(predict_table), " subcatchments, ",
        ncol(predict_table), " columns")

missing_preds <- setdiff(FORMULA_PREDICTORS, names(predict_table))
if (length(missing_preds) > 0)
  stop("Missing predictor columns: ", paste(missing_preds, collapse = ", "))

# Basin subcatchment IDs — used for prediction sites
basin_subc_ids <- fread("spatial/basin/basin_subc_ids_pruned.csv") %>%
  pull(subc_id)

# Subbasin subcatchment IDs — for saving subbasin predictions to gpkg
subbasin_subc_ids <- fread("spatial/subbasin/subbasin_subc_ids_pruned.csv") %>%
  pull(subc_id)

# ============================================================
# STEP 2: Build prediction sites sf object (shared across species)
# ============================================================

message("\n=== Step 2: Building prediction sites ===")

pred_sites <- basin_streams %>%
  filter(subc_id %in% basin_subc_ids) %>%
  distinct(subc_id, .keep_all = TRUE) %>%
  st_point_on_surface() %>%
  select(subc_id)

message("  Prediction sites (full basin): ", nrow(pred_sites))

# ============================================================
# STEP 3: Build LSN edges (shared across species)
# ============================================================

message("\n=== Step 3: Building LSN edges ===")

lsn_path <- "spatial/basin/ssn/lsn"

edges <- lines_to_lsn(
  streams        = basin_streams,
  lsn_path       = lsn_path,
  check_topology = TRUE,
  snap_tolerance = 1,
  topo_tolerance = 20,
  overwrite      = TRUE
)

message("  LSN edges: ", nrow(edges))

# ============================================================
# STEP 4: Calculate upstream distances for edges (shared)
# ============================================================

message("\n=== Step 4: Calculating upstream distances for edges ===")

edges <- updist_edges(
  edges       = edges,
  save_local  = TRUE,
  lsn_path    = lsn_path,
  calc_length = TRUE
)

message("  Edge upstream distances calculated")

# ============================================================
# STEP 5: Calculate AFVs for edges (shared)
# ============================================================

message("\n=== Step 5: Calculating AFVs for edges ===")

if (!"accumulation_mean" %in% names(edges)) {
  edges <- edges %>%
    left_join(
      predict_table %>% select(subc_id, accumulation_mean),
      by = "subc_id"
    )
}

n_missing <- sum(is.na(edges$accumulation_mean))
n_zeros   <- sum(edges$accumulation_mean == 0, na.rm = TRUE)
if (n_missing > 0) {
  message("  WARNING: ", n_missing, " edges missing accumulation — replacing with 1")
  edges$accumulation_mean[is.na(edges$accumulation_mean)] <- 1
}
if (n_zeros > 0) {
  message("  WARNING: ", n_zeros, " edges with zero accumulation — replacing with 1")
  edges$accumulation_mean[edges$accumulation_mean == 0] <- 1
}

edges <- afv_edges(
  edges     = edges,
  infl_col  = "accumulation_mean",
  segpi_col = "areaPI",
  afv_col   = "afvArea",
  lsn_path  = lsn_path
)

message("  AFV range: ", round(min(edges$afvArea), 4),
        " — ", round(max(edges$afvArea), 4))
message("  Most downstream edge AFV (should be ~1.0): ",
        round(max(edges$afvArea), 4))

# ============================================================
# SPECIES LOOP
# ============================================================

ssn_results <- list()

for (sp in target_species) {

  message("\n", paste(rep("=", 60), collapse = ""))
  message("  SPECIES: ", sp)
  message(paste(rep("=", 60), collapse = ""))

  # ---- Step 6: Load species occurrence file from 03b ----

  occurr_file <- file.path(OCCURR_DIR, sp, paste0("occurr_", sp, ".csv"))

  if (!file.exists(occurr_file)) {
    message("  Skipping — occurrence file not found: ", occurr_file)
    next
  }

  sp_data <- fread(occurr_file)

  n_pres <- sum(sp_data$pres_abs == 1)
  n_abs  <- sum(sp_data$pres_abs == 0)

  message("  Presences: ", n_pres,
          " | True absences: ", sum(sp_data$source == "HCMR_true_absence"),
          " | Pseudoabsences: ", sum(sp_data$source == "pseudoabsence"))

  if (n_pres < 3) {
    message("  Skipping — fewer than 3 presences")
    next
  }

  # ---- Step 7: Build obs_sites sf for this species ----

  obs_sites_sp <- sp_data %>%
    filter(!is.na(longitude_snapped), !is.na(latitude_snapped)) %>%
    select(subc_id, pres_abs, source, longitude_snapped, latitude_snapped) %>%
    st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326) %>%
    st_transform(crs = PROJ_CRS)

  message("  Obs sites as sf: ", nrow(obs_sites_sp))

  # ---- Step 8: Snap obs + pred sites to LSN ----

  message("  Snapping sites to LSN...")

  sp_lsn_obs <- paste0("obs_", gsub(" ", "_", sp))

  obs_snapped_sp <- sites_to_lsn(
    sites          = obs_sites_sp,
    edges          = edges,
    lsn_path       = lsn_path,
    file_name      = sp_lsn_obs,
    snap_tolerance = SNAP_TOL,
    save_local     = TRUE,
    overwrite      = TRUE
  )

  message("  Obs snapped: ", nrow(obs_snapped_sp), " / ", nrow(obs_sites_sp))
  cat("  Max snap distance (m):", max(obs_snapped_sp$snapdist, na.rm = TRUE), "\n")

  pred_snapped_sp <- sites_to_lsn(
    sites          = pred_sites,
    edges          = edges,
    lsn_path       = lsn_path,
    file_name      = "pred_basin",
    snap_tolerance = SNAP_TOL,
    save_local     = TRUE,
    overwrite      = TRUE
  )

  message("  Pred snapped: ", nrow(pred_snapped_sp))

  # ---- Step 9: Calculate upstream distances for obs + pred ----

  message("  Calculating upstream distances for sites...")

  site_list_sp <- updist_sites(
    sites      = list(obs = obs_snapped_sp,
                      pred_basin = pred_snapped_sp),
    edges      = edges,
    length_col = "Length",
    save_local = TRUE,
    lsn_path   = lsn_path
  )

  # ---- Step 10: Calculate AFVs for obs + pred sites ----

  message("  Calculating AFVs for sites...")

  site_list_sp <- afv_sites(
    sites      = site_list_sp,
    edges      = edges,
    afv_col    = "afvArea",
    save_local = TRUE,
    lsn_path   = lsn_path
  )

  cat("  afvArea in obs:", "afvArea" %in% names(site_list_sp$obs), "\n")

  # ---- Step 11: Assemble SSN object + distance matrices ----

  message("  Assembling SSN object...")

  ssn_path_sp <- file.path("spatial/basin/ssn",
                           paste0(gsub(" ", "_", sp), ".ssn"))

  ssn_obj_sp <- ssn_assemble(
    edges      = edges,
    lsn_path   = lsn_path,
    obs_sites  = site_list_sp$obs,
    preds_list = list(pred_basin = site_list_sp$pred_basin),
    ssn_path   = ssn_path_sp,
    import     = TRUE,
    check      = TRUE,
    afv_col    = "afvArea",
    overwrite  = TRUE
  )

  message("  SSN object assembled")

  message("  Creating distance matrices...")
  ssn_create_distmat(
    ssn.object    = ssn_obj_sp,
    predpts       = "pred_basin",
    among_predpts = TRUE,
    overwrite     = TRUE
  )
  message("  Distance matrices created")

  # ---- Step 12: Join + scale predictors ----

  message("  Joining and scaling predictors...")

  predict_table_model <- predict_table %>%
    select(subc_id, all_of(FORMULA_PREDICTORS))

  # Join to obs
  obs_data <- ssn_get_data(ssn_obj_sp, "obs")

  if (!"subc_id" %in% names(obs_data))
    stop("subc_id not in obs sites for ", sp)

  obs_with_env <- obs_data %>%
    left_join(predict_table_model, by = "subc_id")

  n_na <- sum(is.na(obs_with_env[[FORMULA_PREDICTORS[1]]]))
  if (n_na > 0)
    message("  WARNING: ", n_na, " obs sites missing predictor values")

  # Scale using obs mean/SD
  obs_scaled <- obs_with_env %>%
    mutate(across(all_of(FORMULA_PREDICTORS),
                  ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))

  ssn_obj_sp <- ssn_put_data(obs_scaled, ssn_obj_sp, "obs")

  # Join + scale pred sites using obs scaling params
  pred_data <- ssn_get_data(ssn_obj_sp, "pred_basin") %>%
    left_join(predict_table_model, by = "subc_id")

  for (pred in FORMULA_PREDICTORS) {
    obs_vals <- obs_with_env %>% st_drop_geometry() %>% pull(pred)
    obs_mean <- mean(obs_vals, na.rm = TRUE)
    obs_sd   <- sd(obs_vals, na.rm = TRUE)
    if (obs_sd > 0)
      pred_data[[pred]] <- (pred_data[[pred]] - obs_mean) / obs_sd
  }

  ssn_obj_sp <- ssn_put_data(pred_data, ssn_obj_sp, "pred_basin")

  # Verify condition number
  obs_env_check <- ssn_get_data(ssn_obj_sp, "obs") %>%
    st_drop_geometry() %>%
    select(all_of(FORMULA_PREDICTORS))
  X_check <- model.matrix(~ ., data = obs_env_check)
  cat("  Condition number:", round(kappa(X_check), 2), "\n")

  # ---- Step 13: Torgegram ----

  formula_rhs <- paste(FORMULA_PREDICTORS, collapse = " + ")
  formula_mod <- as.formula(paste("pres_abs ~", formula_rhs))

  tg <- tryCatch(
    Torgegram(
      formula    = formula_mod,
      ssn.object = ssn_obj_sp,
      type       = c("flowcon", "flowuncon", "euclid")
    ),
    error = function(e) { message("  Torgegram failed: ", e$message); NULL }
  )

  if (!is.null(tg)) {
    png(paste0("sdm/ssn_models/torgegram_", sp, ".png"),
        width = 1200, height = 400)
    plot(tg)
    dev.off()
    message("  Torgegram saved")
  }

  # ---- Step 14: Fit covariance structure candidates ----

  message("  Fitting models...")

  model_specs <- list(
    td_only  = list(taildown_type = "spherical"),
    tu_td    = list(tailup_type   = "exponential",
                    taildown_type = "spherical"),
    tu_td_eu = list(tailup_type   = "exponential",
                    taildown_type = "spherical",
                    euclid_type   = "gaussian")
  )

  fitted_models <- list()

  for (mod_name in names(model_specs)) {
    spec <- model_specs[[mod_name]]
    message("    Fitting: ", mod_name)

    fitted_models[[mod_name]] <- tryCatch(
      do.call(ssn_glm, c(
        list(formula    = formula_mod,
             family     = "binomial",
             ssn.object = ssn_obj_sp,
             additive   = "afvArea",
             estmethod  = "ml"),
        spec
      )),
      error = function(e) {
        message("      Failed: ", e$message)
        NULL
      }
    )
  }

  fitted_models <- Filter(Negate(is.null), fitted_models)

  if (length(fitted_models) == 0) {
    message("  All models failed for ", sp)
    next
  }

  # Model comparison
  if (length(fitted_models) == 1) {
    model_comparison <- glance(fitted_models[[1]])
  } else {
    model_comparison <- tryCatch(
      do.call(glances, fitted_models),
      error = function(e) {
        data.frame(
          model = names(fitted_models),
          AICc  = sapply(fitted_models, function(m) glance(m)$AICc)
        )
      }
    )
  }
  print(model_comparison)

  # Select best covariance structure by AICc
  best_name <- names(fitted_models)[which.min(
    sapply(fitted_models, function(m) glance(m)$AICc)
  )]
  best_model <- fitted_models[[best_name]]
  message("  Best covariance: ", best_name)

  # Check for separation
  se_max <- max(abs(tidy(best_model)$std.error), na.rm = TRUE)
  if (se_max > 1e6)
    message("  WARNING: Very large standard errors — possible separation")

  # loocv RMSPE
  rmspe <- tryCatch(loocv(best_model)$RMSPE, error = function(e) NA)
  message("  loocv RMSPE: ", round(rmspe, 4))

  # ---- Step 15: Refit with REML, predict, save ----

  best_spec <- model_specs[[best_name]]

  best_model_reml <- tryCatch(
    do.call(ssn_glm, c(
      list(formula    = formula_mod,
           family     = "binomial",
           ssn.object = ssn_obj_sp,
           additive   = "afvArea",
           estmethod  = "reml"),
      best_spec
    )),
    error = function(e) {
      message("  REML refit failed — keeping ML model: ", e$message)
      best_model
    }
  )

  saveRDS(best_model_reml,
          paste0("sdm/ssn_models/ssn_", sp, ".rds"))
  message("  Model saved")

  # Predict across full basin
  preds_out <- tryCatch(
    augment(best_model_reml, newdata = "pred_basin"),
    error = function(e) { message("  Prediction failed: ", e$message); NULL }
  )

  if (!is.null(preds_out)) {
    pred_df <- preds_out %>%
      st_drop_geometry() %>%
      select(subc_id, .fitted) %>%
      rename(suitability = .fitted) %>%
      mutate(
        probability = round(1 / (1 + exp(-suitability)), 4),
        species     = sp,
        best_cov    = best_name
      )

    fwrite(pred_df, paste0("sdm/predictions/pred_", sp, ".csv"))
    message("  Predictions saved: ", nrow(pred_df), " basin subcatchments")
    message("  Probability range: ", round(min(pred_df$probability), 3),
            " — ", round(max(pred_df$probability), 3))
  }


  # ---- Evaluate SSN: AUC + TSS + MCC (in-sample fitted values) ----
  # Note: in-sample evaluation — optimistic upper bound on performance
  # Cross-validation not feasible due to computational cost of SSN fitting
  fitted_probs <- 1 / (1 + exp(-fitted(best_model_reml)))
  obs_data_eval <- ssn_get_data(ssn_obj_sp, "obs") %>%
    st_drop_geometry()

  pres_probs <- fitted_probs[obs_data_eval$pres_abs == 1]
  abs_probs  <- fitted_probs[obs_data_eval$pres_abs == 0]

  # AUC + TSS threshold using dismo
  eval_obj       <- dismo::evaluate(p = pres_probs, a = abs_probs)
  auc_ssn        <- eval_obj@auc
  thresh_ssn_tss <- dismo::threshold(eval_obj)$spec_sens

  sensitivity_ssn <- mean(pres_probs >= thresh_ssn_tss)
  specificity_ssn <- mean(abs_probs  <  thresh_ssn_tss)
  tss_ssn         <- sensitivity_ssn + specificity_ssn - 1

  # MCC threshold
  mcc_result_ssn  <- compute_mcc_threshold(pres_probs, abs_probs)
  thresh_ssn_mcc  <- mcc_result_ssn$threshold
  mcc_ssn         <- mcc_result_ssn$mcc

  message("  AUC (in-sample):           ", round(auc_ssn, 3))
  message("  TSS (in-sample):           ", round(tss_ssn, 3),
          " | threshold: ",               round(thresh_ssn_tss, 3))
  message("  MCC threshold (in-sample): ", round(thresh_ssn_mcc, 3),
          " | MCC: ",                     round(mcc_ssn, 3))
  message("  loocv RMSPE:               ", round(rmspe, 4))


  ssn_results[[sp]] <- list(
    best_cov         = best_name,
    n_presences      = n_pres,
    n_true_absences  = sum(sp_data$source == "HCMR_true_absence"),
    n_pseudoabsences = sum(sp_data$source == "pseudoabsence"),
    AICc             = round(glance(best_model_reml)$AICc, 2),
    loocv_RMSPE      = round(rmspe, 4),
    AUC              = round(auc_ssn, 3),
    TSS              = round(tss_ssn, 3),
    best_threshold_tss = round(thresh_ssn_tss, 3),
    MCC              = round(mcc_ssn, 3),
    best_threshold_mcc = round(thresh_ssn_mcc, 3)
  )


  rm(ssn_obj_sp, fitted_models, best_model, best_model_reml,
     obs_sites_sp, obs_snapped_sp, site_list_sp)
  gc()
}

# ============================================================
# STEP 16: Join subbasin predictions to network gpkg + save summary
# and metrics
# ============================================================

message("\n=== Step 16: Saving outputs ===")

# Join predictions to subbasin stream network for QGIS visualisation
# Predictions cover full basin but we display subbasin only
network <- st_read("spatial/subbasin/stream_network_pruned.gpkg")

pred_files <- list.files("sdm/predictions",
                         pattern = "pred_.*\\.csv$",
                         full.names = TRUE)

for (f in pred_files) {
  pred   <- fread(f)
  sp     <- unique(pred$species)
  col    <- paste0("prob_", sp)

  pred_clean <- pred %>%
    filter(subc_id %in% subbasin_subc_ids) %>%
    distinct(subc_id, .keep_all = TRUE) %>%
    select(subc_id, probability) %>%
    rename(!!col := probability)

  network <- network %>%
    left_join(pred_clean, by = "subc_id")

  message("  Joined: ", col)
}

# Join predictor values for QGIS visualisation
network <- network %>%
  left_join(
    predict_table %>% select(subc_id, all_of(FORMULA_PREDICTORS)),
    by = "subc_id"
  )

st_write(network,
         "spatial/subbasin/stream_network_predictions.gpkg",
         delete_dsn = TRUE)
message("  Saved: spatial/subbasin/stream_network_predictions.gpkg")

# Model summary
results_summary <- lapply(names(ssn_results), function(sp) {
  r <- ssn_results[[sp]]
  data.frame(
    species            = sp,
    n_presences        = r$n_presences,
    n_true_absences    = r$n_true_absences,
    n_pseudoabsences   = r$n_pseudoabsences,
    best_cov           = r$best_cov,
    AICc               = r$AICc,
    loocv_RMSPE        = r$loocv_RMSPE,
    AUC                = r$AUC,
    TSS                = r$TSS,
    best_threshold_tss = r$best_threshold_tss,
    MCC                = r$MCC,
    best_threshold_mcc = r$best_threshold_mcc
  )
}) %>% rbindlist()

print(results_summary)
fwrite(results_summary, "sdm/ssn_models/model_summary.csv")
message("  Saved: sdm/ssn_models/model_summary.csv")

target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>% unique()

# Exclude Chondrostoma — SSN not fitted
ssn_species <- target_species[target_species != "Chondrostoma_ohridanum" &
                                target_species != "Chondrostoma_ohridana"]

coef_list  <- list()
fit_list   <- list()
cov_list   <- list()
cv_list    <- list()

for (sp in ssn_species) {

  message("Processing: ", sp)

  mod_file <- paste0("sdm/ssn_models/ssn_", sp, ".rds")
  if (!file.exists(mod_file)) {
    message("  Model file not found — skipping")
    next
  }

  mod <- readRDS(mod_file)

  # 1. Fixed-effect coefficients + SEs
  coef_list[[sp]] <- tidy(mod) %>%
    mutate(species = sp) %>%
    select(species, term, estimate, std.error, statistic, p.value)

  # 2. Model fit statistics
  fit_list[[sp]] <- glance(mod) %>%
    mutate(species = sp) %>%
    select(species, n, AIC, AICc, BIC, logLik, deviance, pseudo.r.squared)

  # 3. Covariance parameters
  cov_raw <- coef(mod, type = "ssn")

  cov_params <- rbindlist(lapply(names(cov_raw), function(component) {
    vals <- cov_raw[[component]]
    data.frame(
      species   = sp,
      component = component,
      parameter = names(vals),
      estimate  = as.numeric(vals)
    )
  }))

  cov_list[[sp]] <- cov_params

  # 4. loocv RMSPE
  cv <- tryCatch(
    loocv(mod),
    error = function(e) { message("  loocv failed: ", e$message); NULL }
  )

  if (!is.null(cv)) {
    cv_list[[sp]] <- data.frame(
      species     = sp,
      loocv_RMSPE = round(cv$RMSPE, 4)
    )
    message("  loocv RMSPE: ", round(cv$RMSPE, 4))
  }
}

# Combine and save
coef_df <- rbindlist(coef_list)
fit_df  <- rbindlist(fit_list)
cov_df  <- rbindlist(cov_list)
cv_df   <- rbindlist(cv_list)

fwrite(coef_df, "sdm/ssn_models/ssn_coefficients.csv")
fwrite(fit_df,  "sdm/ssn_models/ssn_model_fit.csv")
fwrite(cov_df,  "sdm/ssn_models/ssn_covariance_params.csv")
fwrite(cv_df,   "sdm/ssn_models/ssn_loocv.csv")



message("\n", paste(rep("=", 60), collapse = ""))
message("SSN MODELLING COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("\nTraining extent:   full Vjosa/Aoos basin")
message("Prediction extent: full basin (pred saved) + subbasin (gpkg)")
message("\nOutputs:")
message("  spatial/basin/ssn/{species}.ssn")
message("  sdm/ssn_models/ssn_{species}.rds")
message("  sdm/ssn_models/torgegram_{species}.png")
message("  sdm/ssn_models/model_summary.csv")
message("  sdm/predictions/pred_{species}.csv  (full basin)")
message("  spatial/subbasin/stream_network_predictions.gpkg  (subbasin)")
message("\nNext: 05_maxent.R, 06_random_forest.R, 07_ensemble.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 05_maxent.R
#
# Fit MaxEnt species distribution models for freshwater fish species
# in the Vjosa/Aoos basin using the maxnet package.
#
# Approach:
#   - Presence-only modeling (HCMR + GBIF presences from full basin)
#   - Background: all basin subcatchments
#   - Predictors: all VIF-filtered environmental variables
#   - Default regularization (beta = 1.0) and feature classes
#   - Evaluation: AUC + TSS + MCC
#     * Jackknife (leave-one-out) for species with < 10 presences
#     * Random 20% hold-out for species with >= 10 presences
#   - Variable importance: permutation-based AUC drop
#   - Prediction: full basin → subbasin extracted for gpkg
#
# Input:
#   - points_snapped/basin/fish_sdm_basin.csv
#   - spatial/basin/basin_subc_ids_pruned.csv  (background)
#   - env90m/predict_table_vif.csv
#   - points_original/fish/species_list_sarantaporos.txt
#   - spatial/subbasin/stream_network_pruned.gpkg  (for output gpkg)
#   - spatial/subbasin/subbasin_subc_ids_pruned.csv
#
# Output:
#   - sdm/maxent_models/maxent_{species}.rds
#   - sdm/maxent_models/maxent_evaluation.csv
#   - sdm/maxent_models/maxent_variable_importance.csv
#   - sdm/predictions/pred_maxent_{species}.csv  (full basin)
#   - spatial/subbasin/stream_network_predictions_maxent.gpkg
#
# LOCATION: workflows/07_sdm/05_maxent.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(maxnet)
library(data.table)
library(dplyr)
library(sf)

select <- dplyr::select

# ============================================================
# HELPER FUNCTIONS
# ============================================================

# Compute MCC across thresholds
# pres_preds: predicted probabilities at presence/test sites
# abs_preds:  predicted probabilities at absence/background sites
compute_mcc_threshold <- function(pres_preds, abs_preds,
                                  thresh_seq = seq(0, 1, by = 0.01)) {
  mcc_vals <- sapply(thresh_seq, function(t) {
    TP <- as.numeric(sum(pres_preds >= t, na.rm = TRUE))
    FN <- as.numeric(sum(pres_preds <  t, na.rm = TRUE))
    FP <- as.numeric(sum(abs_preds  >= t, na.rm = TRUE))
    TN <- as.numeric(sum(abs_preds  <  t, na.rm = TRUE))
    denom <- sqrt((TP+FP) * (TP+FN) * (TN+FP) * (TN+FN))
    if (is.na(denom) || denom == 0) return(NA)
    (TP * TN - FP * FN) / denom
  })
  best_thresh <- thresh_seq[which.max(mcc_vals)]
  best_mcc    <- max(mcc_vals, na.rm = TRUE)
  return(list(threshold = best_thresh, mcc = best_mcc))
}

# Compute MaxEnt variable importance via permutation
# Permutes each predictor in background data and measures AUC drop
# Positive importance: predictor improves discrimination
# Negative importance: permuting predictor improves AUC (predictor adds noise)
compute_maxent_varimp <- function(mod, pres_data, bg_data,
                                  predictor_cols, n_permut = 10) {
  pres_preds <- predict(mod,
                        newdata = as.data.frame(pres_data),
                        type    = "cloglog")
  bg_preds   <- predict(mod,
                        newdata = as.data.frame(bg_data),
                        type    = "cloglog")
  auc_base   <- mean(sapply(pres_preds, function(p) mean(p > bg_preds)))

  imp_list <- list()
  for (pred in predictor_cols) {
    auc_permut <- numeric(n_permut)
    for (k in seq_len(n_permut)) {
      bg_permut          <- as.data.frame(bg_data)
      bg_permut[[pred]]  <- sample(bg_permut[[pred]])
      bg_preds_p         <- predict(mod,
                                    newdata = bg_permut,
                                    type    = "cloglog")
      auc_permut[k]      <- mean(sapply(pres_preds,
                                        function(p) mean(p > bg_preds_p)))
    }
    imp_list[[pred]] <- data.frame(
      predictor  = pred,
      importance = round(auc_base - mean(auc_permut), 4)
    )
  }
  rbindlist(imp_list) %>% arrange(desc(importance))
}

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

dir.create("sdm/maxent_models", recursive = TRUE, showWarnings = FALSE)
dir.create("sdm/predictions",   recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Minimum presences threshold for hold-out vs jackknife evaluation
# Species with < MIN_PRES_HOLDOUT presences use jackknife (leave-one-out)
# Species with >= MIN_PRES_HOLDOUT presences use 20% random hold-out
MIN_PRES_HOLDOUT <- 10

# Hold-out proportion for species with sufficient presences
HOLDOUT_PROP <- 0.2

# Regularization multiplier (beta) — default 1.0
# Higher values = smoother, less complex response curves
BETA <- 1.0

# Number of permutations for variable importance
# Higher = more stable estimates but slower
N_PERMUT <- 10

set.seed(42)

# Target species
target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>% unique()

message("Target species (", length(target_species), "):")
for (sp in target_species) message("  ", sp)

# ============================================================
# STEP 1: Load data
# ============================================================

message("\n=== Step 1: Loading data ===")

# Presences — HCMR + GBIF from full basin
fish_basin <- fread("points_snapped/basin/fish_sdm_basin.csv") %>%
  filter(species %in% target_species)

message("  Fish records (full basin): ", nrow(fish_basin),
        " (", n_distinct(fish_basin$species), " species)")

# Environmental predictors
predict_table <- fread("env90m/predict_table_vif.csv")
message("  Predict table: ", nrow(predict_table), " subcatchments, ",
        ncol(predict_table), " columns")

# Predictor columns — all VIF-filtered variables
predictor_cols <- names(predict_table) %>% setdiff("subc_id")
message("  Predictor columns: ", length(predictor_cols))

# Basin subcatchment IDs — used as background
basin_subc_ids <- fread("spatial/basin/basin_subc_ids_pruned.csv") %>%
  pull(subc_id)

message("  Background subcatchments (full basin): ", length(basin_subc_ids))

# Subbasin subcatchment IDs — for output gpkg
subbasin_subc_ids <- fread("spatial/subbasin/subbasin_subc_ids_pruned.csv") %>%
  pull(subc_id)

# ============================================================
# STEP 2: Prepare background environmental data
# ============================================================

message("\n=== Step 2: Preparing background data ===")

# Background = all basin subcatchments with environmental data
background_env <- predict_table %>%
  filter(subc_id %in% basin_subc_ids) %>%
  select(subc_id, all_of(predictor_cols))

# Remove rows with NAs
background_env <- background_env %>%
  filter(if_all(all_of(predictor_cols), ~ !is.na(.)))

message("  Background rows after NA removal: ", nrow(background_env))

# ============================================================
# STEP 3: Species loop
# ============================================================

message("\n=== Step 3: Fitting MaxEnt models per species ===")

evaluation_list <- list()
importance_list <- list()

for (sp in target_species) {

  message("\n", paste(rep("=", 60), collapse = ""))
  message("  SPECIES: ", sp)
  message(paste(rep("=", 60), collapse = ""))

  # ---- Get presences ----
  sp_pres <- fish_basin %>%
    filter(species == sp) %>%
    distinct(subc_id) %>%
    pull(subc_id)

  n_pres <- length(sp_pres)
  message("  Presences: ", n_pres)

  if (n_pres < 3) {
    message("  Skipping — fewer than 3 presences")
    next
  }

  # ---- Join environmental data to presences ----
  pres_env <- predict_table %>%
    filter(subc_id %in% sp_pres) %>%
    select(subc_id, all_of(predictor_cols)) %>%
    filter(if_all(all_of(predictor_cols), ~ !is.na(.)))

  n_pres_env <- nrow(pres_env)
  message("  Presences with env data: ", n_pres_env)

  if (n_pres_env < 3) {
    message("  Skipping — fewer than 3 presences with environmental data")
    next
  }

  # ---- Evaluation strategy ----
  # Jackknife for species with < MIN_PRES_HOLDOUT presences
  # 20% hold-out for species with >= MIN_PRES_HOLDOUT presences
  use_jackknife <- n_pres_env < MIN_PRES_HOLDOUT
  message("  Evaluation strategy: ",
          ifelse(use_jackknife, "jackknife (leave-one-out)", "20% hold-out"))

  if (use_jackknife) {

    # ---- Jackknife evaluation ----
    # For each presence, leave it out, fit model on rest, predict on held-out
    jack_preds <- numeric(n_pres_env)

    for (i in seq_len(n_pres_env)) {

      train_pres <- pres_env[-i, ] %>% select(all_of(predictor_cols))
      test_pres  <- pres_env[i, ]  %>% select(all_of(predictor_cols))
      train_bg   <- background_env %>% select(all_of(predictor_cols))

      p_vec <- c(rep(1, nrow(train_pres)), rep(0, nrow(train_bg)))
      x_mat <- bind_rows(train_pres, train_bg) %>% as.data.frame()

      mod_i <- tryCatch(
        maxnet(p = p_vec, data = x_mat, regmult = BETA),
        error = function(e) { NULL }
      )

      if (!is.null(mod_i)) {
        jack_preds[i] <- predict(mod_i,
                                 newdata = as.data.frame(test_pres),
                                 type    = "cloglog")
      } else {
        jack_preds[i] <- NA
      }
    }

    # Full model fitted on all presences (for background predictions + importance)
    p_full <- c(rep(1, n_pres_env), rep(0, nrow(background_env)))
    x_full <- bind_rows(
      pres_env %>% select(all_of(predictor_cols)),
      background_env %>% select(all_of(predictor_cols))
    ) %>% as.data.frame()

    mod_full <- tryCatch(
      maxnet(p = p_full, data = x_full, regmult = BETA),
      error = function(e) { message("  Model fitting failed"); NULL }
    )

    if (is.null(mod_full)) next

    bg_preds <- predict(mod_full,
                        newdata = as.data.frame(
                          background_env %>% select(all_of(predictor_cols))),
                        type = "cloglog")

    # AUC — compare jackknife presence predictions vs background predictions
    auc <- mean(sapply(jack_preds[!is.na(jack_preds)], function(p) {
      mean(p > bg_preds)
    }))

    # TSS at threshold maximising sensitivity + specificity
    thresholds  <- seq(0, 1, by = 0.01)
    sensitivity <- sapply(thresholds, function(t) mean(jack_preds >= t, na.rm=TRUE))
    specificity <- sapply(thresholds, function(t) mean(bg_preds < t))
    tss_vals    <- sensitivity + specificity - 1
    best_thresh <- thresholds[which.max(tss_vals)]
    tss         <- max(tss_vals)

    message("  AUC (jackknife): ", round(auc, 3))
    message("  TSS (jackknife): ", round(tss, 3),
            " at threshold: ", round(best_thresh, 3))

    # MCC threshold — using jackknife preds vs background preds
    mcc_result <- compute_mcc_threshold(jack_preds[!is.na(jack_preds)], bg_preds)
    mcc_thresh <- mcc_result$threshold
    mcc_val    <- mcc_result$mcc
    message("  MCC threshold: ", round(mcc_thresh, 3),
            " | MCC: ", round(mcc_val, 3))

  } else {

    # ---- 20% hold-out evaluation ----
    hold_idx  <- sample(seq_len(n_pres_env),
                        size    = floor(n_pres_env * HOLDOUT_PROP),
                        replace = FALSE)
    train_idx <- setdiff(seq_len(n_pres_env), hold_idx)

    train_pres <- pres_env[train_idx, ] %>% select(all_of(predictor_cols))
    test_pres  <- pres_env[hold_idx, ]  %>% select(all_of(predictor_cols))
    train_bg   <- background_env %>% select(all_of(predictor_cols))

    p_vec <- c(rep(1, nrow(train_pres)), rep(0, nrow(train_bg)))
    x_mat <- bind_rows(train_pres, train_bg) %>% as.data.frame()

    mod_full <- tryCatch(
      maxnet(p = p_vec, data = x_mat, regmult = BETA),
      error = function(e) { message("  Model fitting failed"); NULL }
    )

    if (is.null(mod_full)) next

    # Predict on test presences + background
    test_preds <- predict(mod_full,
                          newdata = as.data.frame(test_pres),
                          type    = "cloglog")

    bg_preds <- predict(mod_full,
                        newdata = as.data.frame(train_bg),
                        type    = "cloglog")

    # AUC
    auc <- mean(sapply(test_preds, function(p) mean(p > bg_preds)))

    # TSS at threshold maximising sensitivity + specificity
    thresholds  <- seq(0, 1, by = 0.01)
    sensitivity <- sapply(thresholds, function(t) mean(test_preds >= t))
    specificity <- sapply(thresholds, function(t) mean(bg_preds < t))
    tss_vals    <- sensitivity + specificity - 1
    best_thresh <- thresholds[which.max(tss_vals)]
    tss         <- max(tss_vals)

    message("  AUC (20% hold-out): ", round(auc, 3))
    message("  TSS (20% hold-out): ", round(tss, 3),
            " at threshold: ", round(best_thresh, 3))

    # MCC threshold
    mcc_result <- compute_mcc_threshold(test_preds, bg_preds)
    mcc_thresh <- mcc_result$threshold
    mcc_val    <- mcc_result$mcc
    message("  MCC threshold: ", round(mcc_thresh, 3),
            " | MCC: ", round(mcc_val, 3))

    # Refit on all presences for final prediction + importance
    p_full <- c(rep(1, n_pres_env), rep(0, nrow(background_env)))
    x_full <- bind_rows(
      pres_env %>% select(all_of(predictor_cols)),
      background_env %>% select(all_of(predictor_cols))
    ) %>% as.data.frame()

    mod_full <- tryCatch(
      maxnet(p = p_full, data = x_full, regmult = BETA),
      error = function(e) { message("  Full model refit failed"); NULL }
    )

    if (is.null(mod_full)) next
  }

  # ---- Save model ----
  saveRDS(mod_full, paste0("sdm/maxent_models/maxent_", sp, ".rds"))
  message("  Model saved")

  # ---- Variable importance via permutation ----
  message("  Computing variable importance (", N_PERMUT, " permutations)...")

  imp_sp <- tryCatch(
    compute_maxent_varimp(
      mod            = mod_full,
      pres_data      = pres_env %>% select(all_of(predictor_cols)),
      bg_data        = background_env %>% select(all_of(predictor_cols)),
      predictor_cols = predictor_cols,
      n_permut       = N_PERMUT
    ),
    error = function(e) {
      message("  Variable importance failed: ", e$message)
      NULL
    }
  )

  if (!is.null(imp_sp)) {
    imp_sp$species <- sp
    importance_list[[sp]] <- imp_sp %>%
      select(species, predictor, importance)
    message("  Top predictor: ", imp_sp$predictor[1],
            " (", round(imp_sp$importance[1], 4), ")")
  }

  # ---- Predict across full basin ----
  basin_pred_data <- background_env %>%
    select(subc_id, all_of(predictor_cols))

  basin_preds <- predict(mod_full,
                         newdata = as.data.frame(
                           basin_pred_data %>% select(all_of(predictor_cols))),
                         type = "cloglog")

  pred_df <- data.frame(
    subc_id     = basin_pred_data$subc_id,
    probability = round(as.numeric(basin_preds), 4),
    species     = sp
  )

  fwrite(pred_df, paste0("sdm/predictions/pred_maxent_", sp, ".csv"))
  message("  Predictions saved: ", nrow(pred_df), " basin subcatchments")
  message("  Probability range: ", round(min(pred_df$probability), 3),
          " — ", round(max(pred_df$probability), 3))

  # ---- Store evaluation ----
  evaluation_list[[sp]] <- data.frame(
    species            = sp,
    n_presences        = n_pres_env,
    eval_strategy      = ifelse(use_jackknife, "jackknife", "20pct_holdout"),
    AUC                = round(auc, 3),
    TSS                = round(tss, 3),
    best_threshold_tss = round(best_thresh, 3),
    MCC                = round(mcc_val, 3),
    best_threshold_mcc = round(mcc_thresh, 3)
  )
}

# ============================================================
# STEP 4: Save evaluation table + variable importance
# ============================================================

message("\n=== Step 4: Saving evaluation table and variable importance ===")

eval_df <- rbindlist(evaluation_list)
print(eval_df)
fwrite(eval_df, "sdm/maxent_models/maxent_evaluation.csv")
message("  Saved: sdm/maxent_models/maxent_evaluation.csv")

if (length(importance_list) > 0) {
  imp_df_all <- rbindlist(importance_list)
  fwrite(imp_df_all, "sdm/maxent_models/maxent_variable_importance.csv")
  message("  Saved: sdm/maxent_models/maxent_variable_importance.csv")
}

# ============================================================
# STEP 5: Join subbasin predictions to network gpkg
# ============================================================

message("\n=== Step 5: Joining predictions to subbasin network ===")

network <- st_read("spatial/subbasin/stream_network_pruned.gpkg")

pred_files <- list.files("sdm/predictions",
                         pattern = "pred_maxent_.*\\.csv$",
                         full.names = TRUE)

for (f in pred_files) {
  pred <- fread(f)
  sp   <- unique(pred$species)
  col  <- paste0("prob_", sp)

  pred_clean <- pred %>%
    filter(subc_id %in% subbasin_subc_ids) %>%
    distinct(subc_id, .keep_all = TRUE) %>%
    select(subc_id, probability) %>%
    rename(!!col := probability)

  network <- network %>%
    left_join(pred_clean, by = "subc_id")

  message("  Joined: ", col)
}

st_write(network,
         "spatial/subbasin/stream_network_predictions_maxent.gpkg",
         delete_dsn = TRUE)
message("  Saved: spatial/subbasin/stream_network_predictions_maxent.gpkg")

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("MAXENT MODELLING COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("\nOutputs:")
message("  sdm/maxent_models/maxent_{species}.rds")
message("  sdm/maxent_models/maxent_evaluation.csv")
message("  sdm/maxent_models/maxent_variable_importance.csv")
message("  sdm/predictions/pred_maxent_{species}.csv  (full basin)")
message("  spatial/subbasin/stream_network_predictions_maxent.gpkg")
message("\nNext: 06_random_forest.R, 07_ensemble.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 06_random_forest.R
#
# Fit Random Forest species distribution models for freshwater fish species
# in the Vjosa/Aoos basin using the ranger package.
#
# Approach:
#   - Presence/absence modeling (HCMR + GBIF presences + pseudoabsences)
#   - Training extent: full basin
#   - Predictors: all VIF-filtered environmental variables
#   - Class balancing: bootstrap sample with equal presences/absences per tree
#     following Valavi et al. (2021) for low prevalence data
#   - Evaluation: AUC + TSS
#     * Jackknife (leave-one-out) for species with < 10 presences
#     * Random 20% hold-out for species with >= 10 presences
#   - Prediction: full basin → subbasin extracted for gpkg
#
# Input:
#   - sdm/input/occurr/{species}/occurr_{species}.csv  (from 03b)
#   - spatial/basin/basin_subc_ids_pruned.csv
#   - env90m/predict_table_vif.csv
#   - points_original/fish/species_list_sarantaporos.txt
#   - spatial/subbasin/stream_network_pruned.gpkg
#   - spatial/subbasin/subbasin_subc_ids_pruned.csv
#
# Output:
#   - sdm/rf_models/rf_{species}.rds
#   - sdm/rf_models/rf_evaluation.csv
#   - sdm/rf_models/rf_variable_importance.csv
#   - sdm/predictions/pred_rf_{species}.csv  (full basin)
#   - spatial/subbasin/stream_network_predictions_rf.gpkg
#
# References:
#   Valavi et al. (2021) Modelling species presence-only data with random forests
#   Ecography 44: 1731-1742
#
# LOCATION: workflows/07_sdm/06_random_forest.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(ranger)
library(data.table)
library(dplyr)
library(sf)
library(parallel)

select <- dplyr::select

# Compute MCC across thresholds
# pres_preds: predicted probabilities at presence/test sites
# abs_preds:  predicted probabilities at absence/background sites
compute_mcc_threshold <- function(pres_preds, abs_preds,
                                  thresh_seq = seq(0, 1, by = 0.01)) {
  mcc_vals <- sapply(thresh_seq, function(t) {
    TP <- as.numeric(sum(pres_preds >= t, na.rm = TRUE))
    FN <- as.numeric(sum(pres_preds <  t, na.rm = TRUE))
    FP <- as.numeric(sum(abs_preds  >= t, na.rm = TRUE))
    TN <- as.numeric(sum(abs_preds  <  t, na.rm = TRUE))
    denom <- sqrt((TP+FP) * (TP+FN) * (TN+FP) * (TN+FN))
    if (is.na(denom) || denom == 0) return(NA)
    (TP * TN - FP * FN) / denom
  })
  best_thresh <- thresh_seq[which.max(mcc_vals)]
  best_mcc    <- max(mcc_vals, na.rm = TRUE)
  return(list(threshold = best_thresh, mcc = best_mcc))
}


source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

dir.create("sdm/rf_models",  recursive = TRUE, showWarnings = FALSE)
dir.create("sdm/predictions", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Minimum presences threshold for hold-out vs jackknife evaluation
MIN_PRES_HOLDOUT <- 10

# Hold-out proportion
HOLDOUT_PROP <- 0.2

# Number of trees
N_TREES <- 1000

# Number of cores
N_CORES <- detectCores() - 6

# TSS threshold sequence
THRESH_SEQ <- seq(0, 1, by = 0.01)

set.seed(42)

# Occurrence files directory
OCCURR_DIR <- "sdm/input/occurr"

# Target species
target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>% unique()

message("Target species (", length(target_species), "):")
for (sp in target_species) message("  ", sp)

# ============================================================
# STEP 1: Load data
# ============================================================

message("\n=== Step 1: Loading data ===")

# Environmental predictors — VIF-filtered + rescaled
predict_table <- fread("env90m/predict_table_vif.csv")
message("  Predict table: ", nrow(predict_table), " subcatchments, ",
        ncol(predict_table), " columns")

predictor_cols <- names(predict_table) %>% setdiff("subc_id")
message("  Predictor columns: ", length(predictor_cols))

# Basin subcatchment IDs — for prediction
basin_subc_ids <- fread("spatial/basin/basin_subc_ids_pruned.csv") %>%
  pull(subc_id)

# Subbasin subcatchment IDs — for output gpkg
subbasin_subc_ids <- fread("spatial/subbasin/subbasin_subc_ids_pruned.csv") %>%
  pull(subc_id)

# Full basin prediction data
basin_pred_data <- predict_table %>%
  filter(subc_id %in% basin_subc_ids) %>%
  filter(if_all(all_of(predictor_cols), ~ !is.na(.)))

message("  Basin prediction rows: ", nrow(basin_pred_data))

# ============================================================
# STEP 2: Species loop
# ============================================================

message("\n=== Step 2: Fitting Random Forest models per species ===")

evaluation_list   <- list()
importance_list   <- list()

for (sp in target_species) {

  message("\n", paste(rep("=", 60), collapse = ""))
  message("  SPECIES: ", sp)
  message(paste(rep("=", 60), collapse = ""))

  # ---- Load occurrence data from 03b ----
  occurr_file <- file.path(OCCURR_DIR, sp, paste0("occurr_", sp, ".csv"))

  if (!file.exists(occurr_file)) {
    message("  Skipping — occurrence file not found: ", occurr_file)
    next
  }

  sp_data <- fread(occurr_file) %>%
    filter(if_all(all_of(predictor_cols), ~ !is.na(.))) %>%
    mutate(pres_abs = as.factor(pres_abs))

  n_pres <- sum(sp_data$pres_abs == 1)
  n_abs  <- sum(sp_data$pres_abs == 0)

  message("  Presences: ", n_pres, " | Absences: ", n_abs)

  if (n_pres < 3) {
    message("  Skipping — fewer than 3 presences")
    next
  }

  # ---- Evaluation strategy ----
  use_jackknife <- n_pres < MIN_PRES_HOLDOUT
  message("  Evaluation strategy: ",
          ifelse(use_jackknife, "jackknife (leave-one-out)", "20% hold-out"))

  # ---- Formula ----
  formula_rf <- as.formula(paste("pres_abs ~",
                                 paste(predictor_cols, collapse = " + ")))

  if (use_jackknife) {

    # ---- Jackknife evaluation ----
    # Leave one presence out, train on rest + all absences, predict on held-out
    pres_rows <- which(sp_data$pres_abs == 1)
    abs_rows  <- which(sp_data$pres_abs == 0)
    jack_preds <- numeric(length(pres_rows))

    for (i in seq_along(pres_rows)) {

      train_idx <- c(pres_rows[-i], abs_rows)
      test_row  <- pres_rows[i]

      train_data <- sp_data[train_idx, ]
      test_data  <- sp_data[test_row, ]

      n_pres_train <- sum(train_data$pres_abs == 1)

      mod_i <- tryCatch(
        ranger(
          formula        = formula_rf,
          data           = train_data %>% select(pres_abs, all_of(predictor_cols)),
          num.trees      = N_TREES,
          probability    = TRUE,
          num.threads    = N_CORES,
          case.weights   = ifelse(train_data$pres_abs == 1,
                                  1,
                                  n_pres_train / sum(train_data$pres_abs == 0)),
          seed           = 42
        ),
        error = function(e) { message("    Fold ", i, " failed: ", e$message); NULL }
      )

      if (!is.null(mod_i)) {
        jack_preds[i] <- predict(mod_i,
                                 data = test_data %>%
                                   select(all_of(predictor_cols)))$predictions[, "1"]
      } else {
        jack_preds[i] <- NA
      }
    }

    # Fit full model on all data for predictions + variable importance
    mod_full <- tryCatch(
      ranger(
        formula      = formula_rf,
        data         = sp_data %>% select(pres_abs, all_of(predictor_cols)),
        num.trees    = N_TREES,
        probability  = TRUE,
        importance   = "permutation",
        num.threads  = N_CORES,
        case.weights = ifelse(sp_data$pres_abs == 1,
                              1,
                              n_pres / n_abs),
        seed         = 42
      ),
      error = function(e) { message("  Full model failed: ", e$message); NULL }
    )

    if (is.null(mod_full)) next

    # Predictions on all absences for AUC/TSS calculation
    abs_preds <- predict(mod_full,
                         data = sp_data[abs_rows, ] %>%
                           select(all_of(predictor_cols)))$predictions[, "1"]

    # AUC
    auc <- mean(sapply(jack_preds[!is.na(jack_preds)], function(p) {
      mean(p > abs_preds)
    }))

    # TSS
    sensitivity <- sapply(THRESH_SEQ,
                          function(t) mean(jack_preds >= t, na.rm = TRUE))
    specificity <- sapply(THRESH_SEQ,
                          function(t) mean(abs_preds < t))
    tss_vals    <- sensitivity + specificity - 1
    best_thresh <- THRESH_SEQ[which.max(tss_vals)]
    tss         <- max(tss_vals)

    message("  AUC (jackknife): ", round(auc, 3))
    message("  TSS (jackknife): ", round(tss, 3),
            " at threshold: ", round(best_thresh, 3))

    # MCC threshold — using jackknife preds vs background preds
    mcc_result <- compute_mcc_threshold(
      jack_preds[!is.na(jack_preds)], bg_preds
    )
    mcc_thresh <- mcc_result$threshold
    mcc_val    <- mcc_result$mcc
    message("  MCC threshold: ", round(mcc_thresh, 3),
            " | MCC: ", round(mcc_val, 3))
    mcc_thresh  <- mcc_result$threshold
    mcc_val     <- mcc_result$mcc
    message("  MCC threshold: ", round(mcc_thresh, 3),
            " | MCC: ", round(mcc_val, 3))


  } else {

    # ---- 20% hold-out evaluation ----
    pres_rows  <- which(sp_data$pres_abs == 1)
    abs_rows   <- which(sp_data$pres_abs == 0)

    hold_pres  <- sample(pres_rows,
                         size    = floor(length(pres_rows) * HOLDOUT_PROP),
                         replace = FALSE)
    train_pres <- setdiff(pres_rows, hold_pres)
    train_idx  <- c(train_pres, abs_rows)

    train_data <- sp_data[train_idx, ]
    test_data  <- sp_data[hold_pres, ]

    n_pres_train <- sum(train_data$pres_abs == 1)
    n_abs_train  <- sum(train_data$pres_abs == 0)

    mod_eval <- tryCatch(
      ranger(
        formula      = formula_rf,
        data         = train_data %>% select(pres_abs, all_of(predictor_cols)),
        num.trees    = N_TREES,
        probability  = TRUE,
        num.threads  = N_CORES,
        case.weights = ifelse(train_data$pres_abs == 1,
                              1,
                              n_pres_train / n_abs_train),
        seed         = 42
      ),
      error = function(e) { message("  Eval model failed: ", e$message); NULL }
    )

    if (is.null(mod_eval)) next

    test_preds <- predict(mod_eval,
                          data = test_data %>%
                            select(all_of(predictor_cols)))$predictions[, "1"]

    abs_preds  <- predict(mod_eval,
                          data = sp_data[abs_rows, ] %>%
                            select(all_of(predictor_cols)))$predictions[, "1"]

    # AUC
    auc <- mean(sapply(test_preds, function(p) mean(p > abs_preds)))

    # TSS
    sensitivity <- sapply(THRESH_SEQ, function(t) mean(test_preds >= t))
    specificity <- sapply(THRESH_SEQ, function(t) mean(abs_preds < t))
    tss_vals    <- sensitivity + specificity - 1
    best_thresh <- THRESH_SEQ[which.max(tss_vals)]
    tss         <- max(tss_vals)

    message("  AUC (20% hold-out): ", round(auc, 3))
    message("  TSS (20% hold-out): ", round(tss, 3),
            " at threshold: ", round(best_thresh, 3))

    # MCC threshold
    mcc_result <- compute_mcc_threshold(test_preds, bg_preds)
    mcc_thresh <- mcc_result$threshold
    mcc_val    <- mcc_result$mcc
    message("  MCC threshold: ", round(mcc_thresh, 3),
            " | MCC: ", round(mcc_val, 3))



    # Refit on all data for final predictions + variable importance
    mod_full <- tryCatch(
      ranger(
        formula      = formula_rf,
        data         = sp_data %>% select(pres_abs, all_of(predictor_cols)),
        num.trees    = N_TREES,
        probability  = TRUE,
        importance   = "permutation",
        num.threads  = N_CORES,
        case.weights = ifelse(sp_data$pres_abs == 1,
                              1,
                              n_pres / n_abs),
        seed         = 42
      ),
      error = function(e) { message("  Full model failed: ", e$message); NULL }
    )

    if (is.null(mod_full)) next
  }

  # ---- Save model ----
  saveRDS(mod_full, paste0("sdm/rf_models/rf_", sp, ".rds"))
  message("  Model saved")

  # ---- Variable importance ----
  imp <- data.frame(
    species   = sp,
    predictor = names(mod_full$variable.importance),
    importance = as.numeric(mod_full$variable.importance)
  ) %>% arrange(desc(importance))

  importance_list[[sp]] <- imp

  message("  Top 5 predictors:")
  print(head(imp %>% select(predictor, importance), 5))

  # ---- Predict across full basin ----
  basin_preds <- predict(mod_full,
                         data = basin_pred_data %>%
                           select(all_of(predictor_cols)))$predictions[, "1"]

  pred_df <- data.frame(
    subc_id     = basin_pred_data$subc_id,
    probability = round(as.numeric(basin_preds), 4),
    species     = sp
  )

  fwrite(pred_df, paste0("sdm/predictions/pred_rf_", sp, ".csv"))
  message("  Predictions saved: ", nrow(pred_df), " basin subcatchments")
  message("  Probability range: ", round(min(pred_df$probability), 3),
          " — ", round(max(pred_df$probability), 3))

  # ---- Store evaluation ----
  evaluation_list[[sp]] <- data.frame(
    species          = sp,
    n_presences      = n_pres,
    eval_strategy    = ifelse(use_jackknife, "jackknife", "20pct_holdout"),
    AUC              = round(auc, 3),
    TSS              = round(tss, 3),
    best_threshold_tss = round(best_thresh, 3),  # rename from best_threshold
    MCC              = round(mcc_val, 3),
    best_threshold_mcc = round(mcc_thresh, 3)
  )
}

# ============================================================
# STEP 3: Save evaluation + variable importance
# ============================================================

message("\n=== Step 3: Saving outputs ===")

eval_df <- rbindlist(evaluation_list)
print(eval_df)
fwrite(eval_df, "sdm/rf_models/rf_evaluation.csv")
message("  Saved: sdm/rf_models/rf_evaluation.csv")

imp_df <- rbindlist(importance_list)
fwrite(imp_df, "sdm/rf_models/rf_variable_importance.csv")
message("  Saved: sdm/rf_models/rf_variable_importance.csv")

# ============================================================
# STEP 4: Join subbasin predictions to network gpkg
# ============================================================

message("\n=== Step 4: Joining predictions to subbasin network ===")

network <- st_read("spatial/subbasin/stream_network_pruned.gpkg")

pred_files <- list.files("sdm/predictions",
                         pattern = "pred_rf_.*\\.csv$",
                         full.names = TRUE)

for (f in pred_files) {
  pred <- fread(f)
  sp   <- unique(pred$species)
  col  <- paste0("prob_", sp)

  pred_clean <- pred %>%
    filter(subc_id %in% subbasin_subc_ids) %>%
    distinct(subc_id, .keep_all = TRUE) %>%
    select(subc_id, probability) %>%
    rename(!!col := probability)

  network <- network %>%
    left_join(pred_clean, by = "subc_id")

  message("  Joined: ", col)
}

st_write(network,
         "spatial/subbasin/stream_network_predictions_rf.gpkg",
         delete_dsn = TRUE)
message("  Saved: spatial/subbasin/stream_network_predictions_rf.gpkg")

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("RANDOM FOREST MODELLING COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("\nOutputs:")
message("  sdm/rf_models/rf_{species}.rds")
message("  sdm/rf_models/rf_evaluation.csv")
message("  sdm/rf_models/rf_variable_importance.csv")
message("  sdm/predictions/pred_rf_{species}.csv  (full basin)")
message("  spatial/subbasin/stream_network_predictions_rf.gpkg")
message("\nNext: 07_ensemble.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 07_ensemble.R
#
# Combine SSN, MaxEnt and Random Forest predictions into an ensemble
# species distribution model for freshwater fish in the Sarantaporos subbasin.
#
# Ensemble method: mean probability averaging across contributing models
#   - Alburnoides_prespensis: SSN + MaxEnt + RF (3 models)
#   - Barbus_prespensis:      SSN + MaxEnt + RF (3 models)
#   - All other species:      MaxEnt + RF (2 models)
#
# SSN excluded for most species due to perfect separation with small samples.
# Continuous probability averaging preserves gradient information for
# connectivity analyses downstream.
#
# Input:
#   - sdm/predictions/pred_{species}.csv           (SSN, full basin)
#   - sdm/predictions/pred_maxent_{species}.csv    (MaxEnt, full basin)
#   - sdm/predictions/pred_rf_{species}.csv        (RF, full basin)
#   - spatial/subbasin/subbasin_subc_ids_pruned.csv
#   - spatial/subbasin/stream_network_pruned.gpkg
#   - points_original/fish/species_list_sarantaporos.txt
#
# Output:
#   - sdm/ensemble/ensemble_{species}.csv          (full basin)
#   - sdm/ensemble/ensemble_summary.csv
#   - spatial/subbasin/stream_network_ensemble.gpkg
#
# LOCATION: workflows/07_sdm/07_ensemble.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(data.table)
library(dplyr)
library(sf)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

dir.create("sdm/ensemble", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Species where SSN is included in ensemble
# (only species without perfect separation)
SSN_SPECIES <- c("Alburnoides_prespensis", "Barbus_prespensis")

# Target species
target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>% unique()

# ============================================================
# STEP 1: Load subbasin IDs
# ============================================================

message("\n=== Step 1: Loading subbasin IDs ===")

subbasin_subc_ids <- fread("spatial/subbasin/subbasin_subc_ids_pruned.csv") %>%
  pull(subc_id)

message("  Subbasin subcatchments: ", length(subbasin_subc_ids))

# ============================================================
# STEP 2: Build ensemble per species
# ============================================================

message("\n=== Step 2: Building ensemble predictions ===")

ensemble_summary <- list()

for (sp in target_species) {

  message("\n", paste(rep("=", 60), collapse = ""))
  message("  SPECIES: ", sp)
  message(paste(rep("=", 60), collapse = ""))

  # ---- Load model predictions ----
  models_used <- character(0)
  pred_list   <- list()

  # MaxEnt
  maxent_file <- paste0("sdm/predictions/pred_maxent_", sp, ".csv")
  if (file.exists(maxent_file)) {
    pred_maxent <- fread(maxent_file) %>%
      select(subc_id, probability) %>%
      rename(prob_maxent = probability)
    pred_list[["maxent"]] <- pred_maxent
    models_used <- c(models_used, "maxent")
    message("  MaxEnt loaded: ", nrow(pred_maxent), " subcatchments")
  } else {
    message("  MaxEnt file not found — skipping")
  }

  # Random Forest
  rf_file <- paste0("sdm/predictions/pred_rf_", sp, ".csv")
  if (file.exists(rf_file)) {
    pred_rf <- fread(rf_file) %>%
      select(subc_id, probability) %>%
      rename(prob_rf = probability)
    pred_list[["rf"]] <- pred_rf
    models_used <- c(models_used, "rf")
    message("  RF loaded: ", nrow(pred_rf), " subcatchments")
  } else {
    message("  RF file not found — skipping")
  }

  # SSN — only for reliable species
  if (sp %in% SSN_SPECIES) {
    ssn_file <- paste0("sdm/predictions/pred_", sp, ".csv")
    if (file.exists(ssn_file)) {
      pred_ssn <- fread(ssn_file) %>%
        select(subc_id, probability) %>%
        rename(prob_ssn = probability)
      pred_list[["ssn"]] <- pred_ssn
      models_used <- c(models_used, "ssn")
      message("  SSN loaded: ", nrow(pred_ssn), " subcatchments")
    } else {
      message("  SSN file not found — skipping")
    }
  }

  if (length(models_used) == 0) {
    message("  No model predictions found — skipping")
    next
  }

  message("  Models in ensemble: ", paste(models_used, collapse = " + "))

  # ---- Join all model predictions by subc_id ----
  ensemble_df <- pred_list[[1]]

  for (i in seq_along(pred_list)[-1]) {
    ensemble_df <- ensemble_df %>%
      full_join(pred_list[[i]], by = "subc_id")
  }

  # ---- Compute ensemble mean probability ----
  prob_cols <- paste0("prob_", models_used)

  ensemble_df <- ensemble_df %>%
    mutate(
      ensemble_mean = rowMeans(across(all_of(prob_cols)), na.rm = TRUE),
      n_models      = rowSums(!is.na(across(all_of(prob_cols)))),
      species       = sp,
      models_used   = paste(models_used, collapse = "+")
    )

  # ---- Check coverage ----
  n_subbasin_covered <- sum(subbasin_subc_ids %in% ensemble_df$subc_id)
  message("  Subbasin coverage: ", n_subbasin_covered, " / ",
          length(subbasin_subc_ids))

  # ---- Summary stats ----
  subbasin_ens <- ensemble_df %>%
    filter(subc_id %in% subbasin_subc_ids) %>%
    pull(ensemble_mean)

  message("  Ensemble probability range (subbasin): ",
          round(min(subbasin_ens, na.rm=TRUE), 3),
          " — ", round(max(subbasin_ens, na.rm=TRUE), 3))
  message("  Ensemble median (subbasin): ",
          round(median(subbasin_ens, na.rm=TRUE), 3))

  # ---- Save full basin ensemble ----
  fwrite(ensemble_df,
         paste0("sdm/ensemble/ensemble_", sp, ".csv"))
  message("  Saved: sdm/ensemble/ensemble_", sp, ".csv")

  ensemble_summary[[sp]] <- data.frame(
    species         = sp,
    models_used     = paste(models_used, collapse = "+"),
    n_models        = length(models_used),
    n_subcs_basin   = nrow(ensemble_df),
    n_subcs_subbasin = n_subbasin_covered,
    ens_min         = round(min(subbasin_ens, na.rm=TRUE), 3),
    ens_median      = round(median(subbasin_ens, na.rm=TRUE), 3),
    ens_max         = round(max(subbasin_ens, na.rm=TRUE), 3)
  )
}

# ============================================================
# STEP 3: Save ensemble summary
# ============================================================

message("\n=== Step 3: Saving ensemble summary ===")

summary_df <- rbindlist(ensemble_summary)
print(summary_df)
fwrite(summary_df, "sdm/ensemble/ensemble_summary.csv")
message("  Saved: sdm/ensemble/ensemble_summary.csv")

# ============================================================
# STEP 4: Join ensemble predictions to subbasin network gpkg
# ============================================================

message("\n=== Step 4: Joining ensemble to subbasin network ===")

network <- st_read("spatial/subbasin/stream_network_pruned.gpkg")

ensemble_files <- list.files("sdm/ensemble",
                             pattern = "ensemble_.*\\.csv$",
                             full.names = TRUE) %>%
  .[!grepl("summary", .)] %>%
  .[!grepl("thresholds", .)]

for (f in ensemble_files) {

  ens <- fread(f)
  sp  <- unique(ens$species)
  col <- paste0("ens_", sp)

  ens_clean <- ens %>%
    filter(subc_id %in% subbasin_subc_ids) %>%
    distinct(subc_id, .keep_all = TRUE) %>%
    select(subc_id, ensemble_mean) %>%
    rename(!!col := ensemble_mean)

  network <- network %>%
    left_join(ens_clean, by = "subc_id")

  message("  Joined: ", col)
}

# Also join individual model predictions for comparison in QGIS
for (sp in target_species) {

  # MaxEnt
  f_me <- paste0("sdm/predictions/pred_maxent_", sp, ".csv")
  if (file.exists(f_me)) {
    pred <- fread(f_me) %>%
      filter(subc_id %in% subbasin_subc_ids) %>%
      distinct(subc_id, .keep_all = TRUE) %>%
      select(subc_id, probability) %>%
      rename(!!paste0("me_", sp) := probability)
    network <- network %>% left_join(pred, by = "subc_id")
  }

  # RF
  f_rf <- paste0("sdm/predictions/pred_rf_", sp, ".csv")
  if (file.exists(f_rf)) {
    pred <- fread(f_rf) %>%
      filter(subc_id %in% subbasin_subc_ids) %>%
      distinct(subc_id, .keep_all = TRUE) %>%
      select(subc_id, probability) %>%
      rename(!!paste0("rf_", sp) := probability)
    network <- network %>% left_join(pred, by = "subc_id")
  }

  # SSN (only for reliable species)
  if (sp %in% SSN_SPECIES) {
    f_ssn <- paste0("sdm/predictions/pred_", sp, ".csv")
    if (file.exists(f_ssn)) {
      pred <- fread(f_ssn) %>%
        filter(subc_id %in% subbasin_subc_ids) %>%
        distinct(subc_id, .keep_all = TRUE) %>%
        select(subc_id, probability) %>%
        rename(!!paste0("ssn_", sp) := probability)
      network <- network %>% left_join(pred, by = "subc_id")
    }
  }
}

st_write(network,
         "spatial/subbasin/stream_network_ensemble.gpkg",
         delete_dsn = TRUE)
message("  Saved: spatial/subbasin/stream_network_ensemble.gpkg")


## Ensemble threshold
ssn_eval <- fread("sdm/ssn_models/model_summary.csv") %>%
  filter(species %in% SSN_SPECIES) %>%
  select(species,
         thresh_ssn_tss = best_threshold_tss,
         thresh_ssn_mcc = best_threshold_mcc)

maxent_eval <- fread("sdm/maxent_models/maxent_evaluation.csv") %>%
  select(species,
         thresh_maxent_tss = best_threshold_tss,
         thresh_maxent_mcc = best_threshold_mcc)

rf_eval <- fread("sdm/rf_models/rf_evaluation.csv") %>%
  select(species,
         thresh_rf_tss = best_threshold_tss,
         thresh_rf_mcc = best_threshold_mcc)

# For SSN species: mean of 3 models; for others: mean of 2
ensemble_thresholds <- maxent_eval %>%
  left_join(rf_eval, by = "species") %>%
  left_join(ssn_eval, by = "species") %>%
  mutate(
    threshold_tss = case_when(
      species %in% SSN_SPECIES ~
        round((thresh_maxent_tss + thresh_rf_tss + thresh_ssn_tss) / 3, 3),
      TRUE ~
        round((thresh_maxent_tss + thresh_rf_tss) / 2, 3)
    ),
    threshold_mcc = case_when(
      species %in% SSN_SPECIES ~
        round((thresh_maxent_mcc + thresh_rf_mcc + thresh_ssn_mcc) / 3, 3),
      TRUE ~
        round((thresh_maxent_mcc + thresh_rf_mcc) / 2, 3)
    )
  )

fwrite(ensemble_thresholds, "sdm/ensemble/ensemble_thresholds.csv")


# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("ENSEMBLE COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("\nEnsemble composition:")
message("  Alburnoides_prespensis: SSN + MaxEnt + RF")
message("  Barbus_prespensis:      SSN + MaxEnt + RF")
message("  All other species:      MaxEnt + RF")
message("\nOutputs:")
message("  sdm/ensemble/ensemble_{species}.csv  (full basin)")
message("  sdm/ensemble/ensemble_summary.csv")
message("  spatial/subbasin/stream_network_ensemble.gpkg")
message("    — columns: ens_{species}, me_{species}, rf_{species},")
message("      ssn_{species} (Alburnoides + Barbus only)")
message("\nNext: check fragmentation in QGIS,")
message("  then proceed to connectivity integration")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 08_habitat_classification.R
#
# Classify continuous ensemble SDM predictions into suitable/unsuitable
# habitat for freshwater fish species in the Sarantaporos + Voidomatis
# subbasin.
#
# Approach:
#   TSS threshold applied directly to ENSEMBLE predictions (primary analysis).
#   MCC threshold applied as sensitivity analysis (Hellegers et al. 2025).
#   No IDW applied at this stage — if a specific species requires
#   spatial constraining after visual inspection, IDW can be applied
#   selectively in a separate step.
#
# Workflow per species (run twice: once for TSS, once for MCC):
#   1. Apply threshold to ensemble predictions
#   2. Fill short unsuitable gaps using fill_habitat_gaps() with sigma_mob
#   3. Remove isolated single suitable reaches (no suitable neighbours)
#   4. Output binary (0/1) and semi-binary (0 or ensemble prob) versions
#
# Threshold:
#   Two methods following Hellegers et al. (2025):
#   - TSS: species-specific mean of MaxEnt + RF + SSN thresholds
#          (SSN included for Alburnoides and Barbus only, where models
#           converged without separation; MaxEnt + RF only for all other species)
#   - MCC: same approach
#   Note: SSN thresholds are in-sample (fitted values), while MaxEnt and RF
#         thresholds are out-of-sample (held-out test data)
#
# Output gpkgs:
#   - stream_network_habitat_tss.gpkg: primary analysis (TSS threshold)
#   - stream_network_habitat_mcc.gpkg: sensitivity analysis (MCC threshold)
#   Each has 4 columns per species: bin_, semi_, gap_, isol_
#
# Gap filling:
#   Uses fill_habitat_gaps() with sigma_mob as gap threshold.
#   Optional MAX_GAP_M caps the maximum gap filled regardless of sigma_mob.
#
# Isolation filter:
#   Single suitable reaches with no suitable immediate neighbours removed.
#
# Input:
#   - spatial/subbasin/stream_network_ensemble.gpkg
#   - sdm/maxent_models/maxent_evaluation.csv
#   - sdm/rf_models/rf_evaluation.csv
#   - sdm/ssn_models/model_summary.csv  (SSN thresholds for Alburnoides + Barbus)
#   - traits/fish_dis_class.txt
#   - points_original/fish/species_list_sarantaporos.txt
#   - spatial/basin/stream_network_pruned.gpkg  (for reach length)
#
# Output:
#   - sdm/habitat/habitat_{species}.csv
#     columns: subc_id, ens_prob,
#              binary_tss, semibinary_tss, gap_filled_tss, isolated_removed_tss,
#              binary_mcc, semibinary_mcc, gap_filled_mcc, isolated_removed_mcc
#   - sdm/habitat/habitat_summary.csv
#   - spatial/subbasin/stream_network_habitat_tss.gpkg  (primary)
#   - spatial/subbasin/stream_network_habitat_mcc.gpkg  (sensitivity)
#
# LOCATION: workflows/07_sdm/08_habitat_classification.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(data.table)
library(dplyr)
library(sf)
library(igraph)
library(hydrographr)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# Source fill_habitat_gaps function
# TODO: move to hydrographr package
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/fill_habitat_gaps.R")

dir.create("sdm/habitat", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Maximum gap length to fill regardless of sigma_mob
# NULL = use only sigma_mob per species
MAX_GAP_M <- NULL

target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>% unique()

# ============================================================
# STEP 1: Load data
# ============================================================

message("\n=== Step 1: Loading data ===")

# Ensemble network
network_sf <- st_read("spatial/subbasin/stream_network_ensemble.gpkg")
network_dt <- network_sf %>% st_drop_geometry()

message("  Network reaches: ", nrow(network_dt))

ens_cols <- names(network_dt)[grepl("^ens_", names(network_dt))]
message("  Ensemble columns: ", paste(ens_cols, collapse = ", "))

# Build graph for gap filling and isolation filter
# Rebuild with correct subc_id → target topology
ensemble_dt <- read_geopackage(
  "spatial/subbasin/stream_network_ensemble.gpkg",
  import_as = "data.table"
)

edges_df <- ensemble_dt %>%
  select(from = subc_id, to = target) %>%
  filter(!is.na(to)) %>%
  mutate(from = as.character(from),
         to   = as.character(to))

network_g <- graph_from_data_frame(edges_df, directed = TRUE)

message("  Graph: ", vcount(network_g), " nodes, ",
        ecount(network_g), " edges")

# Add reach length as edge attribute
############ TODO: REPLACE WHEN get_stream_segments IS FIXED
basin_network_dt <- read_geopackage(
  "spatial/basin/stream_network_pruned.gpkg",
  import_as = "data.table"
) %>% select(subc_id, length)

E(network_g)$weight <- basin_network_dt$length[
  match(edges_df$from, as.character(basin_network_dt$subc_id))
]
############

# Named vector of reach lengths for fill_habitat_gaps()
edge_length <- setNames(E(network_g)$weight, edges_df$from)
cat("Edge length range (m):", range(edge_length, na.rm = TRUE), "\n")
cat("NAs in edge length:", sum(is.na(edge_length)), "\n")

# TSS + MCC thresholds — mean of MaxEnt + RF per species
# SSN excluded (RMSPE not comparable to AUC/TSS/MCC)
maxent_eval <- fread("sdm/maxent_models/maxent_evaluation.csv") %>%
  select(species,
         thresh_maxent_tss = best_threshold_tss,
         thresh_maxent_mcc = best_threshold_mcc)

rf_eval <- fread("sdm/rf_models/rf_evaluation.csv") %>%
  select(species,
         thresh_rf_tss = best_threshold_tss,
         thresh_rf_mcc = best_threshold_mcc)

ssn_eval <- fread("sdm/ssn_models/model_summary.csv") %>%
  filter(species %in% c("Alburnoides_prespensis", "Barbus_prespensis")) %>%
  select(species,
         thresh_ssn_tss = best_threshold_tss,
         thresh_ssn_mcc = best_threshold_mcc)

tss_thresholds <- maxent_eval %>%
  left_join(rf_eval, by = "species") %>%
  left_join(ssn_eval, by = "species") %>%
  mutate(
    threshold_tss = case_when(
      !is.na(thresh_ssn_tss) ~
        round((thresh_maxent_tss + thresh_rf_tss + thresh_ssn_tss) / 3, 3),
      TRUE ~
        round((thresh_maxent_tss + thresh_rf_tss) / 2, 3)
    ),
    threshold_mcc = case_when(
      !is.na(thresh_ssn_mcc) ~
        round((thresh_maxent_mcc + thresh_rf_mcc + thresh_ssn_mcc) / 3, 3),
      TRUE ~
        round((thresh_maxent_mcc + thresh_rf_mcc) / 2, 3)
    )
  )
# tss_thresholds <- maxent_eval %>%
#   left_join(rf_eval, by = "species") %>%
#   mutate(
#     threshold_tss = round((thresh_maxent_tss + thresh_rf_tss) / 2, 3),
#     threshold_mcc = round((thresh_maxent_mcc + thresh_rf_mcc) / 2, 3)
#   )

cat("\nSpecies-specific thresholds:\n")
print(tss_thresholds %>%
        select(species, thresh_maxent_tss, thresh_rf_tss, threshold_tss,
               thresh_maxent_mcc, thresh_rf_mcc, threshold_mcc))

# Dispersal distances (sigma_mob) per species
fish_dis_class <- fread("traits/fish_dis_class.txt", sep = "\t") %>%
  rename(species = species_name) %>%
  filter(species %in% target_species) %>%
  select(species, sigma_mob = distance)

cat("\nSigma_mob per species (m):\n")
print(fish_dis_class)

# ============================================================
# STEP 2: Classify habitat per species
# ============================================================

message("\n=== Step 2: Classifying habitat per species ===")

habitat_summary <- list()

for (sp in target_species) {

  ens_col <- paste0("ens_", sp)

  if (!ens_col %in% names(network_dt)) {
    message("\nSkipping ", sp, " — ensemble column not found")
    next
  }

  message("\n", paste(rep("=", 50), collapse = ""))
  message("  SPECIES: ", sp)
  message(paste(rep("=", 50), collapse = ""))

  # Get species-specific thresholds
  sp_thresh <- tss_thresholds %>% filter(species == sp)

  if (nrow(sp_thresh) == 0 || is.na(sp_thresh$threshold_tss)) {
    message("  WARNING: No thresholds found — using 0.5 for both")
    sp_threshold_tss <- 0.5
    sp_threshold_mcc <- 0.5
  } else {
    sp_threshold_tss <- sp_thresh$threshold_tss
    sp_threshold_mcc <- sp_thresh$threshold_mcc
  }

  message("  Threshold TSS: ", sp_threshold_tss)
  message("  Threshold MCC: ", sp_threshold_mcc)

  # Get sigma_mob
  sigma_mob <- fish_dis_class %>%
    filter(species == sp) %>%
    pull(sigma_mob)

  if (length(sigma_mob) == 0) {
    message("  WARNING: No sigma_mob — using 5000m default")
    sigma_mob <- 5000
  }
  message("  sigma_mob: ", round(sigma_mob), "m")

  ens_probs <- network_dt[[ens_col]]

  # ---- Inner function: run classification for one threshold ----
  run_classification <- function(threshold, label) {

    suitable_before <- as.character(
      network_dt$subc_id[ens_probs >= threshold]
    )
    message("  [", label, "] Suitable above threshold: ",
            length(suitable_before),
            " (", round(100 * length(suitable_before) / nrow(network_dt), 1),
            "%)")

    # Gap filling
    suitable_after <- fill_habitat_gaps(
      g            = network_g,
      suitable_ids = suitable_before,
      edge_length  = edge_length,
      sigma_mob    = sigma_mob,
      max_gap_m    = MAX_GAP_M
    )
    gap_filled_ids <- setdiff(as.character(suitable_after),
                              as.character(suitable_before))
    message("  [", label, "] After gap filling: ", length(suitable_after),
            " (", length(gap_filled_ids), " filled)")

    # Remove isolated single suitable reaches
    isolated <- c()
    for (node in suitable_after) {
      up_n  <- neighbors(network_g, node, mode = "in")$name
      dn_n  <- neighbors(network_g, node, mode = "out")$name
      all_n <- c(up_n, dn_n)
      if (!any(all_n %in% suitable_after)) {
        isolated <- c(isolated, node)
      }
    }
    suitable_final <- setdiff(suitable_after, isolated)
    message("  [", label, "] Isolated removed: ", length(isolated),
            " | Final: ", length(suitable_final))

    total_length_km <- sum(edge_length[suitable_final], na.rm = TRUE) / 1000
    message("  [", label, "] Final suitable length: ",
            round(total_length_km, 1), " km")

    list(
      suitable_final = suitable_final,
      gap_filled_ids = gap_filled_ids,
      isolated       = isolated,
      n_raw          = length(suitable_before),
      n_gap_filled   = length(gap_filled_ids),
      n_isolated     = length(isolated),
      n_final        = length(suitable_final),
      length_km      = round(total_length_km, 1)
    )
  }

  message("  --- TSS ---")
  res_tss <- run_classification(sp_threshold_tss, "TSS")

  message("  --- MCC ---")
  res_mcc <- run_classification(sp_threshold_mcc, "MCC")

  # ---- Build classification columns ----
  sp_habitat <- network_dt %>%
    select(subc_id, ens_prob = all_of(ens_col)) %>%
    mutate(
      # TSS threshold
      binary_tss           = as.integer(
        as.character(subc_id) %in% res_tss$suitable_final),
      semibinary_tss        = ifelse(binary_tss == 1, ens_prob, 0),
      gap_filled_tss        = as.integer(
        as.character(subc_id) %in% res_tss$gap_filled_ids),
      isolated_removed_tss  = as.integer(
        as.character(subc_id) %in% res_tss$isolated),

      # MCC threshold
      binary_mcc           = as.integer(
        as.character(subc_id) %in% res_mcc$suitable_final),
      semibinary_mcc        = ifelse(binary_mcc == 1, ens_prob, 0),
      gap_filled_mcc        = as.integer(
        as.character(subc_id) %in% res_mcc$gap_filled_ids),
      isolated_removed_mcc  = as.integer(
        as.character(subc_id) %in% res_mcc$isolated),

      species = sp
    )

  fwrite(sp_habitat, paste0("sdm/habitat/habitat_", sp, ".csv"))
  message("  Saved: sdm/habitat/habitat_", sp, ".csv")

  habitat_summary[[sp]] <- data.frame(
    species          = sp,
    threshold_tss    = sp_threshold_tss,
    threshold_mcc    = sp_threshold_mcc,
    sigma_mob_m      = round(sigma_mob),
    n_suitable_tss   = res_tss$n_final,
    n_gap_tss        = res_tss$n_gap_filled,
    n_isol_tss       = res_tss$n_isolated,
    length_km_tss    = res_tss$length_km,
    n_suitable_mcc   = res_mcc$n_final,
    n_gap_mcc        = res_mcc$n_gap_filled,
    n_isol_mcc       = res_mcc$n_isolated,
    length_km_mcc    = res_mcc$length_km
  )
}

# ============================================================
# STEP 3: Save summary
# ============================================================

message("\n=== Step 3: Saving summary ===")

summary_df <- rbindlist(habitat_summary)
print(summary_df)
fwrite(summary_df, "sdm/habitat/habitat_summary.csv")
message("  Saved: sdm/habitat/habitat_summary.csv")

# ============================================================
# STEP 4: Join to network gpkgs — one per threshold method
# ============================================================

message("\n=== Step 4: Joining to network gpkgs ===")

network_tss <- network_sf
network_mcc <- network_sf

for (sp in target_species) {

  hab_file <- paste0("sdm/habitat/habitat_", sp, ".csv")
  if (!file.exists(hab_file)) next

  hab <- fread(hab_file)

  # TSS columns
  hab_tss <- hab %>%
    select(
      subc_id,
      !!paste0("bin_",  sp) := binary_tss,
      !!paste0("semi_", sp) := semibinary_tss,
      !!paste0("gap_",  sp) := gap_filled_tss,
      !!paste0("isol_", sp) := isolated_removed_tss
    )
  network_tss <- network_tss %>% left_join(hab_tss, by = "subc_id")

  # MCC columns
  hab_mcc <- hab %>%
    select(
      subc_id,
      !!paste0("bin_",  sp) := binary_mcc,
      !!paste0("semi_", sp) := semibinary_mcc,
      !!paste0("gap_",  sp) := gap_filled_mcc,
      !!paste0("isol_", sp) := isolated_removed_mcc
    )
  network_mcc <- network_mcc %>% left_join(hab_mcc, by = "subc_id")

  message("  Joined: ", sp)
}

# Save TSS gpkg — primary analysis
st_write(network_tss,
         "spatial/subbasin/stream_network_habitat_tss.gpkg",
         delete_dsn = TRUE)
message("  Saved: spatial/subbasin/stream_network_habitat_tss.gpkg (primary)")

# Save MCC gpkg — sensitivity analysis
st_write(network_mcc,
         "spatial/subbasin/stream_network_habitat_mcc.gpkg",
         delete_dsn = TRUE)
message("  Saved: spatial/subbasin/stream_network_habitat_mcc.gpkg (sensitivity)")

message("  Columns per species in each gpkg: bin_, semi_, gap_, isol_")

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("HABITAT CLASSIFICATION COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("\nTwo threshold methods (Hellegers et al. 2025):")
message("  TSS — primary analysis: stream_network_habitat_tss.gpkg")
message("  MCC — sensitivity analysis: stream_network_habitat_mcc.gpkg")
message("\nGap filling: sigma_mob per species",
        ifelse(!is.null(MAX_GAP_M),
               paste0(" (capped at ", MAX_GAP_M, "m)"),
               " (no cap)"))
message("Isolation filter: single reaches with no suitable neighbours removed")
message("Note: IDW not applied — apply selectively per species if needed")
message("\nOutputs:")
message("  sdm/habitat/habitat_{species}.csv")
message("  sdm/habitat/habitat_summary.csv")
message("  spatial/subbasin/stream_network_habitat_tss.gpkg  (primary)")
message("  spatial/subbasin/stream_network_habitat_mcc.gpkg  (sensitivity)")
message("\nNext: 10_patch_metrics.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 07c_ensemble_prediction_map.R
# Workflow paper (Paper 1) — Module 7 figure
#
# Produces two figures:
#   Figure main  — 7-panel grid, one per species, reaches coloured by
#                  ensemble_mean probability (continuous, viridis)
#                  Grey reaches = below TSS threshold (unsuitable)
#   Figure supp  — same but binary (suitable/unsuitable) using TSS threshold
#
# READS:
#   sdm/ensemble/ensemble_<species>.csv   (subc_id, ensemble_mean, ...)
#   sdm/ensemble/ensemble_thresholds.csv  (species, threshold_tss)
#   spatial/subbasin/stream_network_pruned.gpkg
#
# WRITES:
#   figures/sdm/fig_ensemble_continuous.png   (main, 173 mm wide)
#   figures/sdm/fig_ensemble_binary.png       (supplement)
#
# Species (7): Alburnoides_prespensis, Barbus_prespensis,
#              Chondrostoma_ohridanum, Anguilla_anguilla,
#              Salmo_farioides, Squalius_platyceps, Oxynoemacheilus_pindus
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(sf)
library(dplyr)
library(data.table)
library(ggplot2)
library(patchwork)
library(scales)      # label_number for legend

source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("figures/sdm", recursive = TRUE, showWarnings = FALSE)

# ── parameters ───────────────────────────────────────────────────────────────
TARGET_SPECIES <- c(
  "Alburnoides_prespensis",
  "Barbus_prespensis",
  "Chondrostoma_ohridanum",
  "Anguilla_anguilla",
  "Salmo_farioides",
  "Squalius_platyceps",
  "Oxynoemacheilus_pindus"
)

# clean labels for panel titles (italics via expression() in ggplot)
SPECIES_LABELS <- c(
  "Alburnoides_prespensis"   = "A. prespensis",
  "Barbus_prespensis"        = "B. prespensis",
  "Chondrostoma_ohridanum"   = "C. ohridanum",
  "Anguilla_anguilla"        = "A. anguilla",
  "Salmo_farioides"          = "S. farioides",
  "Squalius_platyceps"       = "S. platyceps",
  "Oxynoemacheilus_pindus"   = "O. pindus"
)

FIG_W_MM <- 173
FIG_DPI  <- 300

COL_UNSUITABLE <- "grey88"
COL_NETWORK_BG <- "grey75"   # reaches not in prediction table (outside basin)

# ── load network geometry ────────────────────────────────────────────────────
message("Loading stream network...")
network_sf <- st_read("spatial/subbasin_sarantaporos/stream_network_pruned.gpkg",
                      quiet = TRUE) %>%
  st_transform(4326)
message("  ", nrow(network_sf), " reaches loaded.")

# ── load TSS thresholds ──────────────────────────────────────────────────────
thresholds <- fread("sdm/ensemble/ensemble_thresholds.csv")
# expects columns: species, threshold_tss

# ── shared map theme ─────────────────────────────────────────────────────────
theme_map_sdm <- function(base_size = 8) {
  theme_void(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      plot.title       = element_text(size = 7, face = "italic", hjust = 0.5,
                                      margin = margin(b = 2)),
      plot.margin      = margin(3, 3, 3, 3),
      legend.position  = "none"   # shared legend added via patchwork
    )
}

# ── build one panel per species ──────────────────────────────────────────────
make_panel_continuous <- function(sp) {
  f <- file.path("sdm/ensemble", paste0("ensemble_", sp, ".csv"))
  if (!file.exists(f)) {
    message("  WARNING: not found — ", f)
    return(NULL)
  }

  dt  <- fread(f)
  thr <- thresholds[species == sp, threshold_tss]
  if (length(thr) == 0 || is.na(thr)) thr <- 0

  # join predictions to network
  net <- network_sf %>%
    left_join(dt %>% select(subc_id, ensemble_mean), by = "subc_id") %>%
    mutate(
      suitable = !is.na(ensemble_mean) & ensemble_mean >= thr,
      prob     = if_else(suitable, ensemble_mean, NA_real_)
    )

  ggplot() +
    # background — all reaches in light grey
    geom_sf(data = net,
            colour = COL_UNSUITABLE, linewidth = 0.3) +
    # suitable reaches coloured by ensemble probability
    geom_sf(data = net %>% filter(suitable),
            aes(colour = prob), linewidth = 0.65) +
    scale_colour_viridis_c(
      name   = "Ensemble\nprobability",
      option = "viridis",
      limits = c(0, 1),
      breaks = c(0, 0.5, 1),
      labels = c("0", "0.5", "1"),
      na.value = COL_UNSUITABLE
    ) +
    labs(title = SPECIES_LABELS[sp]) +
    theme_map_sdm()
}

make_panel_binary <- function(sp) {
  f <- file.path("sdm/ensemble", paste0("ensemble_", sp, ".csv"))
  if (!file.exists(f)) return(NULL)

  dt  <- fread(f)
  thr <- thresholds[species == sp, threshold_tss]
  if (length(thr) == 0 || is.na(thr)) thr <- 0

  net <- network_sf %>%
    left_join(dt %>% select(subc_id, ensemble_mean), by = "subc_id") %>%
    mutate(suitable = !is.na(ensemble_mean) & ensemble_mean >= thr)

  n_suitable <- sum(net$suitable, na.rm = TRUE)
  pct <- round(100 * n_suitable / nrow(net), 1)

  ggplot() +
    geom_sf(data = net %>% filter(!suitable),
            colour = COL_UNSUITABLE, linewidth = 0.3) +
    geom_sf(data = net %>% filter(suitable),
            colour = "#1a7a3c", linewidth = 0.65) +
    labs(title = paste0(SPECIES_LABELS[sp], "  (", pct, "% suitable)")) +
    theme_map_sdm()
}

# ── build panel lists ────────────────────────────────────────────────────────
message("Building panels...")

panels_cont <- lapply(TARGET_SPECIES, make_panel_continuous)
names(panels_cont) <- TARGET_SPECIES
panels_cont <- Filter(Negate(is.null), panels_cont)

panels_bin  <- lapply(TARGET_SPECIES, make_panel_binary)
names(panels_bin) <- TARGET_SPECIES
panels_bin  <- Filter(Negate(is.null), panels_bin)

# ── shared colour legend ─────────────────────────────────────────────────────
# extract legend from one panel by temporarily turning it on
legend_panel <- panels_cont[[1]] +
  theme(
    legend.position  = "bottom",
    legend.title     = element_text(size = 6, face = "bold"),
    legend.text      = element_text(size = 6),
    legend.key.width = unit(12, "mm"),
    legend.key.height = unit(2.5, "mm")
  ) +
  guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))

get_legend <- function(p) {
  # extract just the legend grob from a ggplot
  gt   <- ggplot_gtable(ggplot_build(p))
  leg  <- which(sapply(gt$grobs, function(g) g$name) == "guide-box")
  if (length(leg) == 0) return(NULL)
  gt$grobs[[leg]]
}

leg_grob <- get_legend(legend_panel)

# ── assemble figure — continuous ─────────────────────────────────────────────
message("Assembling continuous figure...")

p_cont <- wrap_plots(panels_cont, ncol = 4) +
  plot_annotation(
    title    = "Ensemble SDM predictions — Sarantaporos sub-basin",
    subtitle = "Reaches coloured by ensemble probability (grey = below TSS threshold)",
    theme    = theme(
      plot.title    = element_text(size = 9,  face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 7.5, colour = "grey40", hjust = 0.5)
    )
  )

# add shared legend below
if (!is.null(leg_grob)) {
  p_cont_final <- p_cont /
    wrap_elements(leg_grob) +
    plot_layout(heights = c(20, 1))
} else {
  p_cont_final <- p_cont
}

ggsave("figures/sdm/fig_ensemble_continuous.png",
       p_cont_final,
       width  = FIG_W_MM,
       height = FIG_W_MM * 0.65,   # roughly 2 rows of 4 panels
       units  = "mm",
       dpi    = FIG_DPI,
       bg     = "white")
message("Saved: figures/sdm/fig_ensemble_continuous.png")

# ── assemble figure — binary ─────────────────────────────────────────────────
message("Assembling binary figure...")

p_bin <- wrap_plots(panels_bin, ncol = 4) +
  plot_annotation(
    title    = "Ensemble SDM predictions — suitable habitat (TSS threshold)",
    subtitle = "Green = suitable reaches · Grey = unsuitable",
    theme    = theme(
      plot.title    = element_text(size = 9,  face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 7.5, colour = "grey40", hjust = 0.5)
    )
  )

ggsave("figures/sdm/fig_ensemble_binary.png",
       p_bin,
       width  = FIG_W_MM,
       height = FIG_W_MM * 0.65,
       units  = "mm",
       dpi    = FIG_DPI,
       bg     = "white")
message("Saved: figures/sdm/fig_ensemble_binary.png")

message("\n=== Done ===")
message("figures/sdm/fig_ensemble_continuous.png")
message("figures/sdm/fig_ensemble_binary.png")

# ============================================================
# BONUS FIGURE — Barbus prespensis continuous probability (single panel)
# ============================================================

message("\nBuilding Barbus prespensis standalone figure...")

sp_barbus <- "Barbus_prespensis"
dt_barbus <- fread(file.path("sdm/ensemble",
                             paste0("ensemble_", sp_barbus, ".csv")))
thr_barbus <- thresholds[species == sp_barbus, threshold_tss]
if (length(thr_barbus) == 0 || is.na(thr_barbus)) thr_barbus <- 0

net_barbus <- network_sf %>%
  left_join(dt_barbus %>% select(subc_id, ensemble_mean), by = "subc_id") %>%
  mutate(
    suitable = !is.na(ensemble_mean) & ensemble_mean >= thr_barbus,
    prob     = if_else(suitable, ensemble_mean, NA_real_)
  )

n_suit <- sum(net_barbus$suitable, na.rm = TRUE)
pct_suit <- round(100 * n_suit / nrow(net_barbus), 1)

p_barbus <- ggplot() +
  # unsuitable reaches as grey background
  geom_sf(data = net_barbus %>% filter(!suitable),
          colour = COL_UNSUITABLE, linewidth = 0.35) +
  # suitable reaches coloured by continuous probability
  geom_sf(data = net_barbus %>% filter(suitable),
          aes(colour = prob), linewidth = 0.9) +
  scale_colour_viridis_c(
    name   = "Ensemble probability",
    option = "viridis",
    limits = c(thr_barbus, 1),
    breaks = scales::breaks_pretty(n = 4),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    title    = expression(italic("Barbus prespensis") ~ "— suitable habitat"),
    subtitle = paste0("Sarantaporos sub-basin · ", n_suit, " reaches suitable (",
                      pct_suit, "%) · grey = below TSS threshold")
  ) +
  theme_void(base_size = 9) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    plot.title       = element_text(size = 9, hjust = 0.5,
                                    margin = margin(b = 2)),
    plot.subtitle    = element_text(size = 7, colour = "grey40", hjust = 0.5,
                                    margin = margin(b = 4)),
    plot.margin      = margin(5, 5, 5, 5),
    legend.position  = "bottom",
    legend.title     = element_text(size = 7, face = "bold"),
    legend.text      = element_text(size = 6.5),
    legend.key.width  = unit(18, "mm"),
    legend.key.height = unit(2.5, "mm")
  ) +
  guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))

ggsave("figures/sdm/fig_barbus_prespensis_habitat.png",
       p_barbus,
       width  = FIG_W_MM,
       height = FIG_W_MM * 0.85,
       units  = "mm",
       dpi    = FIG_DPI,
       bg     = "white")
message("Saved: figures/sdm/fig_barbus_prespensis_habitat.png")
