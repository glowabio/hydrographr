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
