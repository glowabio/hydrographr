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
