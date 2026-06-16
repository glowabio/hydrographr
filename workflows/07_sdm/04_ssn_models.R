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
#   - spatial/subbasin_sarantaporos/stream_network_predictions.gpkg  (subbasin only)
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
subbasin_subc_ids <- fread("spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv") %>%
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
network <- st_read("spatial/subbasin_sarantaporos/stream_network_pruned.gpkg")

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
         "spatial/subbasin_sarantaporos/stream_network_predictions.gpkg",
         delete_dsn = TRUE)
message("  Saved: spatial/subbasin_sarantaporos/stream_network_predictions.gpkg")

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
message("  spatial/subbasin_sarantaporos/stream_network_predictions.gpkg  (subbasin)")
message("\nNext: 05_maxent.R, 06_random_forest.R, 07_ensemble.R")
