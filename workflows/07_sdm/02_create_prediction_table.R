# ============================================================================
# CREATE PREDICTION TABLE FOR SPECIES DISTRIBUTION MODELING
# ============================================================================
# Purpose: Create prediction table from downloaded Environment90m data
# Input: Downloaded environmental tables from script 01
# Output: pred_tab.csv - ready for SDM modeling
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
# VERIFY INPUT FILES EXIST
# ============================================================================

message("\n=== Verifying Input Files ===")

# Check if subc_ids file exists
if (!file.exists("env90m/subc_ids.txt")) {
  stop("ERROR: env90m/subc_ids.txt not found!",
       "\n  Please run script 01_download_env90m_data.R first.")
}

# Check if downloaded data directories exist
required_dirs <- c(
  "env90m/chelsa_bioclim_v2_1",
  "env90m/esa_cci_landcover_v2_1_1",
  "env90m/hydrography90m_v1_0"
)

for (dir in required_dirs) {
  if (!dir.exists(dir)) {
    stop("ERROR: Required directory not found: ", dir,
         "\n  Please run script 01_download_env90m_data.R first.")
  }
}

message("✓ All required input files found")

# ============================================================================
# CONFIGURATION
# ============================================================================

message("\n=== Configuration ===")

# Define variables to include in prediction table
# You can modify this list based on your modeling needs
variables <- c(
  "bio01_1981-2010_observed",
  "bio02_1981-2010_observed",
  "c60_2020",
  "accumulation",
  "length",
  "slope_grad_dw_cel"
)

# Define statistics to calculate
# Options: "mean", "sd", "range", or "ALL"
statistics <- c("mean", "sd")

# Define tile IDs (same as in script 01)
tile_id <- c("h18v04", "h20v04")

# Detect available cores for parallel processing
n_cores <- parallel::detectCores() - 1

message(sprintf("Variables: %s", paste(variables, collapse = ", ")))
message(sprintf("Statistics: %s", paste(statistics, collapse = ", ")))
message(sprintf("Tiles: %s", paste(tile_id, collapse = ", ")))
message(sprintf("CPU cores to use: %d", n_cores))

# ============================================================================
# CREATE PREDICTION TABLE
# ============================================================================

message("\n=== Creating Prediction Table ===")
message("This may take several minutes depending on data size...")

# Define output path
output_file <- "env90m/pred_tab.csv"

# Run get_predict_table
pred_tab <- get_predict_table(
  variable = variables,
  statistics = statistics,
  tile_id = tile_id,
  input_var_path = "env90m",
  subcatch_id = "env90m/subc_ids.txt",
  out_file_path = output_file,
  read = TRUE,
  quiet = FALSE,
  n_cores = n_cores,
  overwrite = TRUE
)

message(sprintf("\n✓ Prediction table created: %d rows, %d columns",
                nrow(pred_tab),
                ncol(pred_tab)))

# ============================================================================
# HANDLE MISSING DATA (IF ANY)
# ============================================================================

message("\n=== Checking for Missing Data ===")

# Count missing values per column
missing_counts <- pred_tab[, lapply(.SD, function(x) sum(is.na(x)))]

# Check if there are any missing values
has_missing <- any(missing_counts > 0)

if (has_missing) {
  message("\n⚠ Missing values detected:")

  for (col in names(missing_counts)) {
    if (col != "subc_id") {
      n_missing <- missing_counts[[col]]
      if (n_missing > 0) {
        pct_missing <- 100 * n_missing / nrow(pred_tab)
        message(sprintf("  %s: %d (%.1f%%)", col, n_missing, pct_missing))
      }
    }
  }

  # Ask user what to do
  message("\nOptions for handling missing data:")
  message("  1. Keep all rows (NAs will need to be handled in modeling)")
  message("  2. Remove rows with any missing values")
  message("  3. Remove only specific problematic variables")

  # For automation, we'll save both versions
  message("\nSaving two versions:")

  # Version 1: All data (with NAs)
  fwrite(pred_tab, "env90m/pred_tab_full.csv")
  message("  ✓ Saved: env90m/pred_tab_full.csv (all rows, including NAs)")

  # Version 2: Complete cases only
  pred_tab_complete <- na.omit(pred_tab)
  fwrite(pred_tab_complete, "env90m/pred_tab_complete.csv")
  message(sprintf("  ✓ Saved: env90m/pred_tab_complete.csv (%d rows, NAs removed)",
                  nrow(pred_tab_complete)))

  message(sprintf("\nRemoved %d rows (%.1f%%) with missing values",
                  nrow(pred_tab) - nrow(pred_tab_complete),
                  100 * (nrow(pred_tab) - nrow(pred_tab_complete)) / nrow(pred_tab)))

  # Use complete version for subsequent analyses
  pred_tab <- pred_tab_complete

} else {
  message("✓ No missing values detected")
}

# ============================================================================
# DATA QUALITY CHECKS
# ============================================================================

message("\n=== Data Quality Summary ===")

# Dimensions
message(sprintf("\nDimensions: %d rows × %d columns", nrow(pred_tab), ncol(pred_tab)))

# Column names
message("\nColumn names:")
print(names(pred_tab))

# Basic statistics
message("\nBasic statistics:")
print(summary(pred_tab))

# Check for outliers (optional)
message("\nChecking for extreme values...")
for (col in names(pred_tab)) {
  if (col != "subc_id" && is.numeric(pred_tab[[col]])) {
    q <- quantile(pred_tab[[col]], probs = c(0.01, 0.99), na.rm = TRUE)
    n_outliers <- sum(pred_tab[[col]] < q[1] | pred_tab[[col]] > q[2], na.rm = TRUE)
    if (n_outliers > 0) {
      pct_outliers <- 100 * n_outliers / nrow(pred_tab)
      message(sprintf("  %s: %d values (%.1f%%) outside 1st-99th percentile",
                      col, n_outliers, pct_outliers))
    }
  }
}


# ============================================================================
# OPTIONAL: CLEAN UP DOWNLOADED TABLES
# ============================================================================

message("\n=== Disk Space Management ===")

# Calculate size of downloaded tables
downloaded_size <- sum(
  file.info(list.files("env90m", pattern = ".txt$",
                       recursive = TRUE, full.names = TRUE))$size,
  na.rm = TRUE
) / 1024 / 1024 / 1024  # GB

# Calculate size of prediction table
pred_tab_size <- file.info("env90m/pred_tab.csv")$size / 1024 / 1024  # MB

message(sprintf("Downloaded tables: %.2f GB", downloaded_size))
message(sprintf("Prediction table: %.2f MB", pred_tab_size))
message(sprintf("Space savings if deleted: %.2f GB", downloaded_size))

message("\nTo free up disk space, you can delete the downloaded tables:")
message("  env90m/chelsa_bioclim_v2_1/")
message("  env90m/esa_cci_landcover_v2_1_1/")
message("  env90m/hydrography90m_v1_0/")

message("\nUncomment the following lines to delete:")
message("# unlink('env90m/chelsa_bioclim_v2_1', recursive = TRUE)")
message("# unlink('env90m/esa_cci_landcover_v2_1_1', recursive = TRUE)")
message("# unlink('env90m/hydrography90m_v1_0', recursive = TRUE)")

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
message("  ✓ env90m/pred_tab.csv (main file)")
if (has_missing) {
  message("  ✓ env90m/pred_tab_full.csv (with NAs)")
  message("  ✓ env90m/pred_tab_complete.csv (NAs removed)")
}
message("  ✓ spatial/stream_networks/stream_network_with_env.csv")

message("\nPrediction table summary:")
message(sprintf("  Rows: %d", nrow(pred_tab)))
message(sprintf("  Columns: %d", ncol(pred_tab)))
message(sprintf("  Variables: %d", length(variables)))
message(sprintf("  File size: %.2f MB", pred_tab_size))

message("\nVariables included:")
for (var in variables) {
  var_cols <- grep(var, names(pred_tab), value = TRUE)
  message(sprintf("  %s: %s", var, paste(var_cols, collapse = ", ")))
}

message("\nNext steps:")
message("  1. Load pred_tab.csv in your SDM workflow")
message("  2. Join with species occurrence data")
message("  3. Run species distribution models")
message("  4. Predict habitat suitability")

message("\nExample usage:")
message("  pred_tab <- fread('env90m/pred_tab.csv')")
message("  # Use pred_tab for modeling...")

message("\n========================================\n")
