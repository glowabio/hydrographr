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
#   - spatial/subbasin_sarantaporos/stream_network_ensemble.gpkg
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
#   - spatial/subbasin_sarantaporos/stream_network_habitat_tss.gpkg  (primary)
#   - spatial/subbasin_sarantaporos/stream_network_habitat_mcc.gpkg  (sensitivity)
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
network_sf <- st_read("spatial/subbasin_sarantaporos/stream_network_ensemble.gpkg")
network_dt <- network_sf %>% st_drop_geometry()

message("  Network reaches: ", nrow(network_dt))

ens_cols <- names(network_dt)[grepl("^ens_", names(network_dt))]
message("  Ensemble columns: ", paste(ens_cols, collapse = ", "))

# Build graph for gap filling and isolation filter
# Rebuild with correct subc_id → target topology
ensemble_dt <- read_geopackage(
  "spatial/subbasin_sarantaporos/stream_network_ensemble.gpkg",
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
length_lookup <- ensemble_dt %>%
  mutate(subc_id = as.character(subc_id)) %>%
  select(subc_id, length) %>%
  distinct()

E(network_g)$weight <- length_lookup$length[
  match(edges_df$from, length_lookup$subc_id)
]

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
         "spatial/subbasin_sarantaporos/stream_network_habitat_tss.gpkg",
         delete_dsn = TRUE)
message("  Saved: spatial/subbasin_sarantaporos/stream_network_habitat_tss.gpkg (primary)")

# Save MCC gpkg — sensitivity analysis
st_write(network_mcc,
         "spatial/subbasin_sarantaporos/stream_network_habitat_mcc.gpkg",
         delete_dsn = TRUE)
message("  Saved: spatial/subbasin_sarantaporos/stream_network_habitat_mcc.gpkg (sensitivity)")

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
message("  spatial/subbasin_sarantaporos/stream_network_habitat_tss.gpkg  (primary)")
message("  spatial/subbasin_sarantaporos/stream_network_habitat_mcc.gpkg  (sensitivity)")
message("\nNext: 10_patch_metrics.R")
