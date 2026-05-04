#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03b_prepare_sdm_data.R
#
# Prepare species-level presence/absence datasets for SSN modeling.
#
# For each target species:
#   - Presences: HCMR confirmed presences + GBIF records within basin
#   - True absences: HCMR surveyed sites where species was NOT recorded
#   - Pseudoabsences: randomly sampled from basin_subc_ids_pruned,
#     excluding subcatchments already covered by HCMR surveys,
#     number fixed to maintain prevalence = 0.1
#     (n_pseudoabs = n_presences * 9 - n_true_absences, min 0)
#
# Environmental predictors (VIF-filtered) joined to all sites.
#
# Input:
#   - points_snapped/fish/fish_all_species_snapped.csv
#   - points_snapped/basin/basin_subc_ids_pruned.csv
#   - env90m/predict_table.csv
#   - points_original/fish/species_list_sarantaporos.txt
#   - config/study_area_params.csv
#
# Output:
#   - sdm/input/occurr/{species}/occurr_{species}.csv  (per species)
#   - sdm/input/occurr/species_data_summary.csv
#
# LOCATION: workflows/06_sdm/03b_prepare_sdm_data.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(data.table)
library(dplyr)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

# Target prevalence for pseudoabsence sampling
# prevalence = n_presences / (n_presences + n_absences) = 0.1
# → total absences needed = n_presences * 9
# → pseudoabsences = max(0, total_absences_needed - n_true_absences)
TARGET_PREVALENCE <- 0.1

set.seed(42)  # reproducibility

# ============================================================
# SETUP
# ============================================================

dir.create("sdm/input/occurr", recursive = TRUE, showWarnings = FALSE)

# Load target species
target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>% unique()

message("Target species: ", length(target_species))

# Load basin ID
study_params <- fread("config/study_area_params.csv")
BASIN_ID <- study_params[param == "BASIN_ID", as.integer(value)]

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

predict_table <- fread("env90m/predict_table_vif.csv")
message("  Predict table: ", nrow(predict_table), " subcatchments, ",
        ncol(predict_table), " columns")

# Simplify climate column names
colnames(predict_table) <- gsub("_1981-2010_observed", "", colnames(predict_table))

# Identify predictor columns (VIF-filtered: _mean and land cover _y2020)
predictor_cols <- names(predict_table) %>%
  setdiff("subc_id") %>%
  .[grepl("_mean$|_y2020$", .)]

message("  Predictor columns: ", length(predictor_cols))

# ============================================================
# STEP 3: Load basin pruned subcatchment IDs
# ============================================================

message("\n=== Step 3: Loading basin subcatchment IDs ===")

basin_subc_ids_pruned <- fread("points_snapped/basin/basin_subc_ids_pruned.csv") %>%
  pull(subc_id)

message("  Basin pruned subcatchments: ", length(basin_subc_ids_pruned))

# Subcatchments covered by HCMR surveys (any species, any outcome)
# These are excluded from pseudoabsence sampling
surveyed_subcs <- fish_all %>%
  filter(source == "HCMR") %>%
  pull(subc_id) %>%
  unique()

message("  HCMR surveyed subcatchments: ", length(surveyed_subcs))

# Pool of subcatchments available for pseudoabsence sampling:
# basin pruned network minus HCMR surveyed subcatchments
pseudoabs_pool <- setdiff(basin_subc_ids_pruned, surveyed_subcs)
message("  Pseudoabsence pool size: ", length(pseudoabs_pool))

# uncomment if you don't have true absences
# pseudoabs_pool <- basin_subc_ids_pruned

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
  # HCMR confirmed presences + GBIF records in basin
  presences <- fish_all %>%
    filter(species == sp) %>%
    distinct(subc_id, source) %>%
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
  # HCMR sites surveyed for this species where it was NOT found
  # A site was "surveyed" if any species was recorded there (HCMR)
  # True absence = HCMR site not in this species' presence list
  true_abs_subcs <- setdiff(surveyed_subcs, presences$subc_id)

  true_absences <- data.frame(
    subc_id  = true_abs_subcs,
    source   = "HCMR_true_absence",
    pres_abs = 0L
  )

  n_true_abs <- nrow(true_absences)
  message("    True absences: ", n_true_abs)

  # ---- Pseudoabsences ----
  # Total absences needed for prevalence = 0.1:
  # prevalence = n_pres / (n_pres + n_abs) = 0.1
  # → n_abs = n_pres * (1 - 0.1) / 0.1 = n_pres * 9
  n_abs_needed    <- round(n_pres * (1 - TARGET_PREVALENCE) / TARGET_PREVALENCE)
  n_pseudoabs_needed <- max(0L, n_abs_needed - n_true_abs)

  message("    Total absences needed (prevalence=", TARGET_PREVALENCE, "): ",
          n_abs_needed)
  message("    Pseudoabsences to sample: ", n_pseudoabs_needed)

  # Remove presence subcatchments from pseudoabsence pool too
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
    )
  } else {
    pseudoabsences <- data.frame(
      subc_id  = integer(0),
      source   = character(0),
      pres_abs = integer(0)
    )
  }

  message("    Pseudoabsences sampled: ", nrow(pseudoabsences))

  # ---- Combine ----
  sp_data <- bind_rows(presences, true_absences, pseudoabsences)

  actual_prevalence <- mean(sp_data$pres_abs)
  message("    Actual prevalence: ", round(actual_prevalence, 3),
          " (target: ", TARGET_PREVALENCE, ")")

  # ---- Join environmental predictors ----
  sp_data <- sp_data %>%
    left_join(
      predict_table %>% select(subc_id, all_of(predictor_cols))
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

  # ---- Store summary ----
  species_summary[[sp]] <- data.frame(
    species          = sp,
    n_presences      = sum(sp_data$pres_abs == 1),
    n_hcmr_pres      = sum(sp_data$pres_abs == 1 & sp_data$source == "HCMR"),
    n_gbif_pres      = sum(sp_data$pres_abs == 1 & sp_data$source == "GBIF"),
    n_true_absences  = sum(sp_data$source == "HCMR_true_absence"),
    n_pseudoabsences = sum(sp_data$source == "pseudoabsence"),
    prevalence       = round(mean(sp_data$pres_abs), 3),
    n_env_predictors = length(predictor_cols)
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

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("SDM DATA PREPARATION COMPLETE")
message(paste(rep("=", 80), collapse = ""))
message("\nTarget prevalence:  ", TARGET_PREVALENCE)
message("Species processed:  ", nrow(summary_df))
message("\nOutputs:")
message("  sdm/input/occurr/{species}/occurr_{species}.csv  (one per species)")
message("  sdm/input/occurr/species_data_summary.csv")
message("\nNext: 04_ssn_models.R")
