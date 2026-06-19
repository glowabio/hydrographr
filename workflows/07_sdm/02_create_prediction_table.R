#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_create_prediction_table.R   (Module 7 -- SDM)
#
# Build the prediction table for species distribution modelling from the
# Environment90m tables downloaded in script 01. Extracts the per-variable
# mean for each subcatchment, simplifies column names, and rescales raw
# values to physical units using the Hydrography90m / CHELSA scale factors.
#
# Workflow:
#   1. Verify downloaded inputs exist
#   2. Build prediction table (get_predict_table, per-subcatchment means)
#   3. Rescale CTI, slope, bioclim variables to physical units
#   4. Handle missing data (save full + complete versions if any NAs)
#
# INPUT:
#   - env90m/subc_ids_basin.txt              (from script 01)
#   - env90m/{chelsa,esa_cci,hydrography}/... (from script 01)
#
# OUTPUT:
#   - env90m/predict_table.csv               (rescaled, main file)
#   - env90m/predict_table_full.csv          (with NAs, only if NAs present)
#   - env90m/predict_table_complete.csv      (NAs removed, only if NAs present)
#
# LOCATION: workflows/07_sdm/02_create_prediction_table.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(data.table)
library(dplyr)

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

# ============================================================
# STEP 1: Verify inputs
# ============================================================

if (!file.exists("env90m/subc_ids_basin.txt")) {
  stop("env90m/subc_ids_basin.txt not found. Run 01_download_env90m_data.R first.")
}
for (d in c("env90m/chelsa_bioclim_v2_1",
            "env90m/esa_cci_landcover_v2_1_1",
            "env90m/hydrography90m_v1_0")) {
  if (!dir.exists(d)) stop("Required directory not found: ", d,
                           ". Run 01_download_env90m_data.R first.")
}

# ============================================================
# PARAMETERS
# ============================================================

# Variables to extract. Land cover uses the 2020 layer (most recent; species
# observations run up to ~2024).
variables <- c(
  "bio01_1981-2010_observed", "bio04_1981-2010_observed", "bio05_1981-2010_observed",
  "bio06_1981-2010_observed", "bio15_1981-2010_observed", "bio17_1981-2010_observed",
  "bio18_1981-2010_observed",
  "c10_2020", "c20_2020", "c30_2020", "c40_2020", "c50_2020", "c60_2020",
  "c120_2020", "c130_2020", "c150_2020", "c160_2020", "c180_2020",
  "c190_2020", "c200_2020", "c210_2020",
  "cti", "order_strahler", "length", "cum_length", "gradient", "elev_drop",
  "accumulation", "channel_elv_dw_seg", "channel_elv_up_seg",
  "outlet_diff_dw_basin", "stream_dist_dw_near", "stream_dist_up_near",
  "slope_grad_dw_cel"
)
statistics <- c("mean")
tile_id    <- c("h18v04", "h20v04")
n_cores    <- parallel::detectCores() - 1

# ============================================================
# STEP 2: Build prediction table
# ============================================================

message("\n=== Building prediction table (", length(variables), " variables) ===")

predict_table <- get_predict_table(
  variable     = variables,
  statistics   = statistics,
  tile_id      = tile_id,
  input_var_path = "env90m",
  subcatch_id  = "env90m/subc_ids_basin.txt",
  out_file_path = "env90m/predict_table.csv",
  read = TRUE, quiet = FALSE, n_cores = n_cores, overwrite = TRUE
)

# Drop the temporal suffix: bio01_1981-2010_observed_mean -> bio01_mean
colnames(predict_table) <- gsub("_1981-2010_observed", "", colnames(predict_table))

message(sprintf("  %d rows, %d columns", nrow(predict_table), ncol(predict_table)))

# ============================================================
# STEP 3: Rescale to physical units
# ============================================================
# Raw Hydrography90m / CHELSA values are stored as scaled integers; convert
# them back to physical units using the published scale factors.

message("\n=== Rescaling variables ===")

# CTI scaled by 1e8 (-> dimensionless wetness index); slope by 1e6.
predict_table <- predict_table %>%
  mutate(cti_mean = cti_mean / 1e8,
         slope_grad_dw_cel_mean = slope_grad_dw_cel_mean / 1e6)

# Bioclim: all stored x10. Temperature vars are also in Kelvin, so /10 then
# -273.15 to get Celsius; the rest only need /10.
bio_kelvin     <- c("bio01", "bio05", "bio06", "bio08", "bio09", "bio10", "bio11")
bio_scale_only <- c("bio02", "bio03", "bio04", "bio07",
                    "bio12", "bio13", "bio14", "bio15",
                    "bio16", "bio17", "bio18", "bio19")

for (bio in bio_kelvin) {
  col <- paste0(bio, "_mean")
  if (col %in% names(predict_table)) {
    predict_table <- predict_table %>% mutate(!!col := (.data[[col]] / 10) - 273.15)
  }
}
for (bio in bio_scale_only) {
  col <- paste0(bio, "_mean")
  if (col %in% names(predict_table)) {
    predict_table <- predict_table %>% mutate(!!col := .data[[col]] / 10)
  }
}

fwrite(predict_table, "env90m/predict_table.csv")
message("  Saved rescaled table: env90m/predict_table.csv")

# ============================================================
# STEP 4: Handle missing data
# ============================================================

missing_counts <- predict_table[, lapply(.SD, function(x) sum(is.na(x)))]
has_missing <- any(missing_counts > 0)

if (has_missing) {
  # Keep a full version (with NAs) and a complete-case version; downstream
  # SDM scripts use the complete one.
  fwrite(predict_table, "env90m/predict_table_full.csv")
  predict_table_complete <- na.omit(predict_table)
  fwrite(predict_table_complete, "env90m/predict_table_complete.csv")

  message(sprintf("\n  Missing values present. Removed %d of %d rows (%.1f%%) for complete version.",
                  nrow(predict_table) - nrow(predict_table_complete), nrow(predict_table),
                  100 * (nrow(predict_table) - nrow(predict_table_complete)) / nrow(predict_table)))
  predict_table <- predict_table_complete
} else {
  message("\n  No missing values.")
}

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("PREDICTION TABLE COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message(sprintf("  %d rows, %d columns, %d variables",
                nrow(predict_table), ncol(predict_table), length(variables)))
message("\nNote: raw env90m/ tables can be deleted now to save disk space.")
message("\nNext: 03_check_multicollinearity.R / 03b_prepare_sdm_data.R")
