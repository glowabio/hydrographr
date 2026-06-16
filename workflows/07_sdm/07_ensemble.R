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
#   - spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv
#   - spatial/subbasin_sarantaporos/stream_network_pruned.gpkg
#   - points_original/fish/species_list_sarantaporos.txt
#
# Output:
#   - sdm/ensemble/ensemble_{species}.csv          (full basin)
#   - sdm/ensemble/ensemble_summary.csv
#   - spatial/subbasin_sarantaporos/stream_network_ensemble.gpkg
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

subbasin_subc_ids <- fread("spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv") %>%
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

network <- st_read("spatial/subbasin_sarantaporos/stream_network_pruned.gpkg")

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
         "spatial/subbasin_sarantaporos/stream_network_ensemble.gpkg",
         delete_dsn = TRUE)
message("  Saved: spatial/subbasin_sarantaporos/stream_network_ensemble.gpkg")


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
message("  spatial/subbasin_sarantaporos/stream_network_ensemble.gpkg")
message("    — columns: ens_{species}, me_{species}, rf_{species},")
message("      ssn_{species} (Alburnoides + Barbus only)")
message("\nNext: check fragmentation in QGIS,")
message("  then proceed to connectivity integration")
