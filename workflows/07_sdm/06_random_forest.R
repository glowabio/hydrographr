#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 06_random_forest.R
#
# Fit Random Forest species distribution models for freshwater fish species
# in the Vjosa/Aoos basin using the ranger package.
#
# Approach:
#   - Presence/absence modeling (HCMR + GBIF presences + pseudoabsences)
#   - Training extent: full basin
#   - Predictors: all VIF-filtered environmental variables
#   - Class balancing: bootstrap sample with equal presences/absences per tree
#     following Valavi et al. (2021) for low prevalence data
#   - Evaluation: AUC + TSS
#     * Jackknife (leave-one-out) for species with < 10 presences
#     * Random 20% hold-out for species with >= 10 presences
#   - Prediction: full basin → subbasin extracted for gpkg
#
# Input:
#   - sdm/input/occurr/{species}/occurr_{species}.csv  (from 03b)
#   - spatial/basin/basin_subc_ids_pruned.csv
#   - env90m/predict_table_vif.csv
#   - points_original/fish/species_list_sarantaporos.txt
#   - spatial/subbasin/stream_network_pruned.gpkg
#   - spatial/subbasin/subbasin_subc_ids_pruned.csv
#
# Output:
#   - sdm/rf_models/rf_{species}.rds
#   - sdm/rf_models/rf_evaluation.csv
#   - sdm/rf_models/rf_variable_importance.csv
#   - sdm/predictions/pred_rf_{species}.csv  (full basin)
#   - spatial/subbasin/stream_network_predictions_rf.gpkg
#
# References:
#   Valavi et al. (2021) Modelling species presence-only data with random forests
#   Ecography 44: 1731-1742
#
# LOCATION: workflows/07_sdm/06_random_forest.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(ranger)
library(data.table)
library(dplyr)
library(sf)
library(parallel)

select <- dplyr::select

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


source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

dir.create("sdm/rf_models",  recursive = TRUE, showWarnings = FALSE)
dir.create("sdm/predictions", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Minimum presences threshold for hold-out vs jackknife evaluation
MIN_PRES_HOLDOUT <- 10

# Hold-out proportion
HOLDOUT_PROP <- 0.2

# Number of trees
N_TREES <- 1000

# Number of cores
N_CORES <- detectCores() - 6

# TSS threshold sequence
THRESH_SEQ <- seq(0, 1, by = 0.01)

set.seed(42)

# Occurrence files directory
OCCURR_DIR <- "sdm/input/occurr"

# Target species
target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>% unique()

message("Target species (", length(target_species), "):")
for (sp in target_species) message("  ", sp)

# ============================================================
# STEP 1: Load data
# ============================================================

message("\n=== Step 1: Loading data ===")

# Environmental predictors — VIF-filtered + rescaled
predict_table <- fread("env90m/predict_table_vif.csv")
message("  Predict table: ", nrow(predict_table), " subcatchments, ",
        ncol(predict_table), " columns")

predictor_cols <- names(predict_table) %>% setdiff("subc_id")
message("  Predictor columns: ", length(predictor_cols))

# Basin subcatchment IDs — for prediction
basin_subc_ids <- fread("spatial/basin/basin_subc_ids_pruned.csv") %>%
  pull(subc_id)

# Subbasin subcatchment IDs — for output gpkg
subbasin_subc_ids <- fread("spatial/subbasin/subbasin_subc_ids_pruned.csv") %>%
  pull(subc_id)

# Full basin prediction data
basin_pred_data <- predict_table %>%
  filter(subc_id %in% basin_subc_ids) %>%
  filter(if_all(all_of(predictor_cols), ~ !is.na(.)))

message("  Basin prediction rows: ", nrow(basin_pred_data))

# ============================================================
# STEP 2: Species loop
# ============================================================

message("\n=== Step 2: Fitting Random Forest models per species ===")

evaluation_list   <- list()
importance_list   <- list()

for (sp in target_species) {

  message("\n", paste(rep("=", 60), collapse = ""))
  message("  SPECIES: ", sp)
  message(paste(rep("=", 60), collapse = ""))

  # ---- Load occurrence data from 03b ----
  occurr_file <- file.path(OCCURR_DIR, sp, paste0("occurr_", sp, ".csv"))

  if (!file.exists(occurr_file)) {
    message("  Skipping — occurrence file not found: ", occurr_file)
    next
  }

  sp_data <- fread(occurr_file) %>%
    filter(if_all(all_of(predictor_cols), ~ !is.na(.))) %>%
    mutate(pres_abs = as.factor(pres_abs))

  n_pres <- sum(sp_data$pres_abs == 1)
  n_abs  <- sum(sp_data$pres_abs == 0)

  message("  Presences: ", n_pres, " | Absences: ", n_abs)

  if (n_pres < 3) {
    message("  Skipping — fewer than 3 presences")
    next
  }

  # ---- Evaluation strategy ----
  use_jackknife <- n_pres < MIN_PRES_HOLDOUT
  message("  Evaluation strategy: ",
          ifelse(use_jackknife, "jackknife (leave-one-out)", "20% hold-out"))

  # ---- Formula ----
  formula_rf <- as.formula(paste("pres_abs ~",
                                 paste(predictor_cols, collapse = " + ")))

  if (use_jackknife) {

    # ---- Jackknife evaluation ----
    # Leave one presence out, train on rest + all absences, predict on held-out
    pres_rows <- which(sp_data$pres_abs == 1)
    abs_rows  <- which(sp_data$pres_abs == 0)
    jack_preds <- numeric(length(pres_rows))

    for (i in seq_along(pres_rows)) {

      train_idx <- c(pres_rows[-i], abs_rows)
      test_row  <- pres_rows[i]

      train_data <- sp_data[train_idx, ]
      test_data  <- sp_data[test_row, ]

      n_pres_train <- sum(train_data$pres_abs == 1)

      mod_i <- tryCatch(
        ranger(
          formula        = formula_rf,
          data           = train_data %>% select(pres_abs, all_of(predictor_cols)),
          num.trees      = N_TREES,
          probability    = TRUE,
          num.threads    = N_CORES,
          case.weights   = ifelse(train_data$pres_abs == 1,
                                  1,
                                  n_pres_train / sum(train_data$pres_abs == 0)),
          seed           = 42
        ),
        error = function(e) { message("    Fold ", i, " failed: ", e$message); NULL }
      )

      if (!is.null(mod_i)) {
        jack_preds[i] <- predict(mod_i,
                                 data = test_data %>%
                                   select(all_of(predictor_cols)))$predictions[, "1"]
      } else {
        jack_preds[i] <- NA
      }
    }

    # Fit full model on all data for predictions + variable importance
    mod_full <- tryCatch(
      ranger(
        formula      = formula_rf,
        data         = sp_data %>% select(pres_abs, all_of(predictor_cols)),
        num.trees    = N_TREES,
        probability  = TRUE,
        importance   = "permutation",
        num.threads  = N_CORES,
        case.weights = ifelse(sp_data$pres_abs == 1,
                              1,
                              n_pres / n_abs),
        seed         = 42
      ),
      error = function(e) { message("  Full model failed: ", e$message); NULL }
    )

    if (is.null(mod_full)) next

    # Predictions on all absences for AUC/TSS calculation
    abs_preds <- predict(mod_full,
                         data = sp_data[abs_rows, ] %>%
                           select(all_of(predictor_cols)))$predictions[, "1"]

    # AUC
    auc <- mean(sapply(jack_preds[!is.na(jack_preds)], function(p) {
      mean(p > abs_preds)
    }))

    # TSS
    sensitivity <- sapply(THRESH_SEQ,
                          function(t) mean(jack_preds >= t, na.rm = TRUE))
    specificity <- sapply(THRESH_SEQ,
                          function(t) mean(abs_preds < t))
    tss_vals    <- sensitivity + specificity - 1
    best_thresh <- THRESH_SEQ[which.max(tss_vals)]
    tss         <- max(tss_vals)

    message("  AUC (jackknife): ", round(auc, 3))
    message("  TSS (jackknife): ", round(tss, 3),
            " at threshold: ", round(best_thresh, 3))

    # MCC threshold — using jackknife preds vs background preds
    mcc_result <- compute_mcc_threshold(
      jack_preds[!is.na(jack_preds)], bg_preds
    )
    mcc_thresh <- mcc_result$threshold
    mcc_val    <- mcc_result$mcc
    message("  MCC threshold: ", round(mcc_thresh, 3),
            " | MCC: ", round(mcc_val, 3))
    mcc_thresh  <- mcc_result$threshold
    mcc_val     <- mcc_result$mcc
    message("  MCC threshold: ", round(mcc_thresh, 3),
            " | MCC: ", round(mcc_val, 3))


  } else {

    # ---- 20% hold-out evaluation ----
    pres_rows  <- which(sp_data$pres_abs == 1)
    abs_rows   <- which(sp_data$pres_abs == 0)

    hold_pres  <- sample(pres_rows,
                         size    = floor(length(pres_rows) * HOLDOUT_PROP),
                         replace = FALSE)
    train_pres <- setdiff(pres_rows, hold_pres)
    train_idx  <- c(train_pres, abs_rows)

    train_data <- sp_data[train_idx, ]
    test_data  <- sp_data[hold_pres, ]

    n_pres_train <- sum(train_data$pres_abs == 1)
    n_abs_train  <- sum(train_data$pres_abs == 0)

    mod_eval <- tryCatch(
      ranger(
        formula      = formula_rf,
        data         = train_data %>% select(pres_abs, all_of(predictor_cols)),
        num.trees    = N_TREES,
        probability  = TRUE,
        num.threads  = N_CORES,
        case.weights = ifelse(train_data$pres_abs == 1,
                              1,
                              n_pres_train / n_abs_train),
        seed         = 42
      ),
      error = function(e) { message("  Eval model failed: ", e$message); NULL }
    )

    if (is.null(mod_eval)) next

    test_preds <- predict(mod_eval,
                          data = test_data %>%
                            select(all_of(predictor_cols)))$predictions[, "1"]

    abs_preds  <- predict(mod_eval,
                          data = sp_data[abs_rows, ] %>%
                            select(all_of(predictor_cols)))$predictions[, "1"]

    # AUC
    auc <- mean(sapply(test_preds, function(p) mean(p > abs_preds)))

    # TSS
    sensitivity <- sapply(THRESH_SEQ, function(t) mean(test_preds >= t))
    specificity <- sapply(THRESH_SEQ, function(t) mean(abs_preds < t))
    tss_vals    <- sensitivity + specificity - 1
    best_thresh <- THRESH_SEQ[which.max(tss_vals)]
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



    # Refit on all data for final predictions + variable importance
    mod_full <- tryCatch(
      ranger(
        formula      = formula_rf,
        data         = sp_data %>% select(pres_abs, all_of(predictor_cols)),
        num.trees    = N_TREES,
        probability  = TRUE,
        importance   = "permutation",
        num.threads  = N_CORES,
        case.weights = ifelse(sp_data$pres_abs == 1,
                              1,
                              n_pres / n_abs),
        seed         = 42
      ),
      error = function(e) { message("  Full model failed: ", e$message); NULL }
    )

    if (is.null(mod_full)) next
  }

  # ---- Save model ----
  saveRDS(mod_full, paste0("sdm/rf_models/rf_", sp, ".rds"))
  message("  Model saved")

  # ---- Variable importance ----
  imp <- data.frame(
    species   = sp,
    predictor = names(mod_full$variable.importance),
    importance = as.numeric(mod_full$variable.importance)
  ) %>% arrange(desc(importance))

  importance_list[[sp]] <- imp

  message("  Top 5 predictors:")
  print(head(imp %>% select(predictor, importance), 5))

  # ---- Predict across full basin ----
  basin_preds <- predict(mod_full,
                         data = basin_pred_data %>%
                           select(all_of(predictor_cols)))$predictions[, "1"]

  pred_df <- data.frame(
    subc_id     = basin_pred_data$subc_id,
    probability = round(as.numeric(basin_preds), 4),
    species     = sp
  )

  fwrite(pred_df, paste0("sdm/predictions/pred_rf_", sp, ".csv"))
  message("  Predictions saved: ", nrow(pred_df), " basin subcatchments")
  message("  Probability range: ", round(min(pred_df$probability), 3),
          " — ", round(max(pred_df$probability), 3))

  # ---- Store evaluation ----
  evaluation_list[[sp]] <- data.frame(
    species          = sp,
    n_presences      = n_pres,
    eval_strategy    = ifelse(use_jackknife, "jackknife", "20pct_holdout"),
    AUC              = round(auc, 3),
    TSS              = round(tss, 3),
    best_threshold_tss = round(best_thresh, 3),  # rename from best_threshold
    MCC              = round(mcc_val, 3),
    best_threshold_mcc = round(mcc_thresh, 3)
  )
}

# ============================================================
# STEP 3: Save evaluation + variable importance
# ============================================================

message("\n=== Step 3: Saving outputs ===")

eval_df <- rbindlist(evaluation_list)
print(eval_df)
fwrite(eval_df, "sdm/rf_models/rf_evaluation.csv")
message("  Saved: sdm/rf_models/rf_evaluation.csv")

imp_df <- rbindlist(importance_list)
fwrite(imp_df, "sdm/rf_models/rf_variable_importance.csv")
message("  Saved: sdm/rf_models/rf_variable_importance.csv")

# ============================================================
# STEP 4: Join subbasin predictions to network gpkg
# ============================================================

message("\n=== Step 4: Joining predictions to subbasin network ===")

network <- st_read("spatial/subbasin/stream_network_pruned.gpkg")

pred_files <- list.files("sdm/predictions",
                         pattern = "pred_rf_.*\\.csv$",
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
         "spatial/subbasin/stream_network_predictions_rf.gpkg",
         delete_dsn = TRUE)
message("  Saved: spatial/subbasin/stream_network_predictions_rf.gpkg")

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("RANDOM FOREST MODELLING COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("\nOutputs:")
message("  sdm/rf_models/rf_{species}.rds")
message("  sdm/rf_models/rf_evaluation.csv")
message("  sdm/rf_models/rf_variable_importance.csv")
message("  sdm/predictions/pred_rf_{species}.csv  (full basin)")
message("  spatial/subbasin/stream_network_predictions_rf.gpkg")
message("\nNext: 07_ensemble.R")
