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
# LOCATION: workflows/06_sdm/03b_prepare_sdm_data.R
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
