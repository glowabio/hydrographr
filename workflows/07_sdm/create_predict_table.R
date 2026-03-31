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
wdir <- "C:/Users/labbadi/OneDrive/Artigo_Peixes" # Created working directory
setwd(wdir)

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
variables <- c(
  "bio01_1981-2010_observed","bio04_1981-2010_observed","bio05_1981-2010_observed","bio06_1981-2010_observed",
  "bio15_1981-2010_observed","bio17_1981-2010_observed","bio18_1981-2010_observed",
  
  # c10
  "c10_1992","c10_1993","c10_1994","c10_1995","c10_1996","c10_1997","c10_1998","c10_1999","c10_2000","c10_2001",
  "c10_2002","c10_2003","c10_2004","c10_2005","c10_2006","c10_2007","c10_2008","c10_2009","c10_2010","c10_2011",
  "c10_2012","c10_2013","c10_2014","c10_2015","c10_2016","c10_2017","c10_2018","c10_2019","c10_2020",
  
  # c20
  "c20_1992","c20_1993","c20_1994","c20_1995","c20_1996","c20_1997","c20_1998","c20_1999","c20_2000","c20_2001",
  "c20_2002","c20_2003","c20_2004","c20_2005","c20_2006","c20_2007","c20_2008","c20_2009","c20_2010","c20_2011",
  "c20_2012","c20_2013","c20_2014","c20_2015","c20_2016","c20_2017","c20_2018","c20_2019","c20_2020",
  
  # c30
  "c30_1992","c30_1993","c30_1994","c30_1995","c30_1996","c30_1997","c30_1998","c30_1999","c30_2000","c30_2001",
  "c30_2002","c30_2003","c30_2004","c30_2005","c30_2006","c30_2007","c30_2008","c30_2009","c30_2010","c30_2011",
  "c30_2012","c30_2013","c30_2014","c30_2015","c30_2016","c30_2017","c30_2018","c30_2019","c30_2020",
  
  # c40
  "c40_1992","c40_1993","c40_1994","c40_1995","c40_1996","c40_1997","c40_1998","c40_1999","c40_2000","c40_2001",
  "c40_2002","c40_2003","c40_2004","c40_2005","c40_2006","c40_2007","c40_2008","c40_2009","c40_2010","c40_2011",
  "c40_2012","c40_2013","c40_2014","c40_2015","c40_2016","c40_2017","c40_2018","c40_2019","c40_2020",
  
  # c50
  "c50_1992","c50_1993","c50_1994","c50_1995","c50_1996","c50_1997","c50_1998","c50_1999","c50_2000","c50_2001",
  "c50_2002","c50_2003","c50_2004","c50_2005","c50_2006","c50_2007","c50_2008","c50_2009","c50_2010","c50_2011",
  "c50_2012","c50_2013","c50_2014","c50_2015","c50_2016","c50_2017","c50_2018","c50_2019","c50_2020",
  
  # c60
  "c60_1992","c60_1993","c60_1994","c60_1995","c60_1996","c60_1997","c60_1998","c60_1999","c60_2000","c60_2001",
  "c60_2002","c60_2003","c60_2004","c60_2005","c60_2006","c60_2007","c60_2008","c60_2009","c60_2010","c60_2011",
  "c60_2012","c60_2013","c60_2014","c60_2015","c60_2016","c60_2017","c60_2018","c60_2019","c60_2020",
  
  # c120
  "c120_1992","c120_1993","c120_1994","c120_1995","c120_1996","c120_1997","c120_1998","c120_1999","c120_2000","c120_2001",
  "c120_2002","c120_2003","c120_2004","c120_2005","c120_2006","c120_2007","c120_2008","c120_2009","c120_2010","c120_2011",
  "c120_2012","c120_2013","c120_2014","c120_2015","c120_2016","c120_2017","c120_2018","c120_2019","c120_2020",
  
  # c130
  "c130_1992","c130_1993","c130_1994","c130_1995","c130_1996","c130_1997","c130_1998","c130_1999","c130_2000","c130_2001",
  "c130_2002","c130_2003","c130_2004","c130_2005","c130_2006","c130_2007","c130_2008","c130_2009","c130_2010","c130_2011",
  "c130_2012","c130_2013","c130_2014","c130_2015","c130_2016","c130_2017","c130_2018","c130_2019","c130_2020",
  
  # c150
  "c150_1992","c150_1993","c150_1994","c150_1995","c150_1996","c150_1997","c150_1998","c150_1999","c150_2000","c150_2001",
  "c150_2002","c150_2003","c150_2004","c150_2005","c150_2006","c150_2007","c150_2008","c150_2009","c150_2010","c150_2011",
  "c150_2012","c150_2013","c150_2014","c150_2015","c150_2016","c150_2017","c150_2018","c150_2019","c150_2020",
  
  # c160
  "c160_1992","c160_1993","c160_1994","c160_1995","c160_1996","c160_1997","c160_1998","c160_1999","c160_2000","c160_2001",
  "c160_2002","c160_2003","c160_2004","c160_2005","c160_2006","c160_2007","c160_2008","c160_2009","c160_2010","c160_2011",
  "c160_2012","c160_2013","c160_2014","c160_2015","c160_2016","c160_2017","c160_2018","c160_2019","c160_2020",
  
  # c180
  "c180_1992","c180_1993","c180_1994","c180_1995","c180_1996","c180_1997","c180_1998","c180_1999","c180_2000","c180_2001",
  "c180_2002","c180_2003","c180_2004","c180_2005","c180_2006","c180_2007","c180_2008","c180_2009","c180_2010","c180_2011",
  "c180_2012","c180_2013","c180_2014","c180_2015","c180_2016","c180_2017","c180_2018","c180_2019","c180_2020",
  
  # c190
  "c190_1992","c190_1993","c190_1994","c190_1995","c190_1996","c190_1997","c190_1998","c190_1999","c190_2000","c190_2001",
  "c190_2002","c190_2003","c190_2004","c190_2005","c190_2006","c190_2007","c190_2008","c190_2009","c190_2010","c190_2011",
  "c190_2012","c190_2013","c190_2014","c190_2015","c190_2016","c190_2017","c190_2018","c190_2019","c190_2020",
  
  # c200
  "c200_1992","c200_1993","c200_1994","c200_1995","c200_1996","c200_1997","c200_1998","c200_1999","c200_2000","c200_2001",
  "c200_2002","c200_2003","c200_2004","c200_2005","c200_2006","c200_2007","c200_2008","c200_2009","c200_2010","c200_2011",
  "c200_2012","c200_2013","c200_2014","c200_2015","c200_2016","c200_2017","c200_2018","c200_2019","c200_2020",
  
  # c210
  "c210_1992","c210_1993","c210_1994","c210_1995","c210_1996","c210_1997","c210_1998","c210_1999","c210_2000","c210_2001",
  "c210_2002","c210_2003","c210_2004","c210_2005","c210_2006","c210_2007","c210_2008","c210_2009","c210_2010","c210_2011",
  "c210_2012","c210_2013","c210_2014","c210_2015","c210_2016","c210_2017","c210_2018","c210_2019","c210_2020",
  
  "order_strahler","length","cum_length","gradient","elev_drop","accumulation",
  "channel_grad_dw_seg","channel_grad_up_seg","channel_elv_dw_seg","channel_elv_up_seg",
  "connections","stream_dist_dw_near","stream_dist_up_near","slope_grad_dw_cel")

# Define statistics to calculate
# Options: "mean"
statistics <- c("mean")

# Define tile IDs (same as in script 01)
tile_id <- c("h18v04", "h20v04")

# Detect available cores for parallel processing
n_cores <- parallel::detectCores() - 6

message(sprintf("Variables: %s", paste(variables, collapse = ", ")))
message(sprintf("Statistics: %s", paste(statistics, collapse = ", ")))
message(sprintf("Tiles: %s", paste(tile_id, collapse = ", ")))
message(sprintf("CPU cores to use: %d", n_cores))

# ============================================================================
# CREATE PREDICTION TABLE
# ============================================================================

message("\n=== Creating Prediction Table ===")
message("This may take several minutes depending on data size...")

# Define path
output_file <- "C:/Users/labbadi/OneDrive/Artigo_Peixes/env90m/pred_tab.csv"
input_file <- "C:/Users/labbadi/OneDrive/Artigo_Peixes/env90m"
subcatch_file <- "C:/Users/labbadi/OneDrive/Artigo_Peixes/env90m/subc_ids.txt"


# Run get_predict_table
pred_tab <- get_predict_table(
  variable = variables,
  statistics = statistics,
  tile_id = tile_id,
  input_var_path = input_file,
  subcatch_id = subcatch_file,
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

# Check for outliers
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

