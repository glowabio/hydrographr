#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 04_ssn_models.R
#
# Fit Spatial Stream Network (SSN) models for freshwater fish species
# distribution in the Sarantaporos + Voidomatis subbasin.
#
# Workflow:
#   1. Load and reproject stream network + observation + prediction sites
#   2. Build Landscape Network (LSN) via SSNbler
#   3. Calculate upstream distances and additive function values
#   4. Assemble SSN object
#   5. Create hydrologic distance matrices
#   6. Explore spatial structure (Torgegram) per species
#   7. Fit SSN models per species (binomial, tail-up + tail-down + Euclidean)
#   8. Model selection via AIC
#   9. Predict habitat suitability across subbasin
#  10. Save outputs
#
# Input:
#   - spatial/subbasin/stream_network_pruned.gpkg
#   - points_snapped/subbasin/fish_sdm_subbasin.csv  (presences)
#   - points_snapped/subbasin/subbasin_subc_ids_pruned.csv (prediction sites)
#   - env90m/predict_table_subbasin.csv  (environmental predictors)
#   - points_original/fish/species_list_sarantaporos.txt
#
# Output:
#   - spatial/subbasin/ssn/              (.ssn folder)
#   - sdm/ssn_models/                    (model objects per species)
#   - sdm/predictions/                   (habitat suitability per species)
#
# LOCATION: workflows/06_sdm/04_ssn_models.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(SSNbler)
library(SSN2)
library(hydrographr)
library(sf)
library(data.table)
library(dplyr)
library(ggplot2)

select <- dplyr::select

source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

dir.create("spatial/subbasin/ssn", recursive = TRUE, showWarnings = FALSE)
dir.create("sdm/ssn_models",       recursive = TRUE, showWarnings = FALSE)
dir.create("sdm/predictions",      recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Target CRS: EPSG:27704 WGS 84 / Equi7 Europe (metres) — required by SSNbler
PROJ_CRS <- 27704

# Snap tolerance in metres for sites_to_lsn()
# Points are already snapped to network so this can be tight
SNAP_TOL <- 100

# Target species
target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>% unique()

message("Target species (", length(target_species), "):")
for (sp in target_species) message("  ", sp)


# Fixed-effect predictors — parsimonious set justified by smallest sample size
# (Chondrostoma_ohridanum: 6 presences, 17 true absences)
# Three ecologically meaningful axes: stream size, thermal constraint, hydraulic habitat
# SSN spatial covariance captures remaining spatial structure
FORMULA_PREDICTORS <- c(
  "accumulation_mean",
  "bio05_mean",
  "slope_grad_dw_cel_mean"
)


# ============================================================
# STEP 1: Load and reproject inputs
# ============================================================

message("\n=== Step 1: Loading and reprojecting inputs ===")

# Stream network — pruned subbasin
subbasin_streams <- st_read("spatial/subbasin/stream_network_pruned.gpkg") %>%
  st_transform(crs = PROJ_CRS)

message("  Stream segments: ", nrow(subbasin_streams))
message("  CRS: ", st_crs(subbasin_streams)$epsg)

# Verify geometry type is LINESTRING (SSNbler requirement)
geom_types <- unique(as.character(st_geometry_type(subbasin_streams)))
if (any(geom_types == "MULTILINESTRING")) {
  message("  Converting MULTILINESTRING to LINESTRING...")
  subbasin_streams <- st_cast(subbasin_streams, "LINESTRING")
}
message("  Geometry type: ", paste(unique(as.character(
  st_geometry_type(subbasin_streams))), collapse = ", "))

# Fish presences in subbasin
fish_subbasin <- fread("points_snapped/subbasin/fish_sdm_subbasin.csv") %>%
  filter(!is.na(longitude_snapped), !is.na(latitude_snapped))

message("  Fish records: ", nrow(fish_subbasin),
        " (", n_distinct(fish_subbasin$species), " species)")

# Predict table — environmental predictors per subcatchment
predict_table <- fread("env90m/predict_table_vif.csv")
message("  Predict table: ", nrow(predict_table), " subcatchments, ",
        ncol(predict_table), " columns")

# Subbasin pruned subcatchment IDs — used for prediction sites
subbasin_subc_ids <- fread("points_snapped/subbasin/subbasin_subc_ids_pruned.csv") %>%
  pull(subc_id)

# ============================================================
# STEP 2: Prepare observation + prediction site sf objects
# ============================================================

message("\n=== Step 2: Preparing site sf objects ===")

# Observation sites — all unique HCMR sites in subbasin
# (species-level presence/absence added per species in model loop)
obs_sites <- fish_subbasin %>%
  distinct(site_id, subc_id, longitude_snapped, latitude_snapped) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326) %>%
  st_transform(crs = PROJ_CRS)

message("  Observation sites: ", nrow(obs_sites))

# Prediction sites — subcatchment centroids from predict table
# SSNbler needs POINT geometry — use subcatchment centroids
# Here we create points from the predict table coordinates if available,
# or derive from the stream network centroids
# NOTE: if predict_table has no coordinate columns, derive from stream network
if (all(c("longitude", "latitude") %in% names(predict_table))) {
  pred_sites <- predict_table %>%
    filter(subcID %in% subbasin_subc_ids) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs = PROJ_CRS)
} else {
  # Derive centroids from stream network reaches
  pred_sites <- subbasin_streams %>%
    filter(subc_id %in% subbasin_subc_ids) %>%
    st_centroid() %>%
    select(subc_id)
  message("  WARNING: No coordinates in predict table — using stream reach centroids")
}

message("  Prediction sites: ", nrow(pred_sites))

# ============================================================
# STEP 3: Build Landscape Network (LSN)
# ============================================================

message("\n=== Step 3: Building Landscape Network ===")

lsn_path <- "spatial/subbasin/ssn/lsn"

# Remove duplicate edges before building LSN
# Duplicate edges cause divergence topology errors in SSNbler
coords_hash <- sapply(st_geometry(subbasin_streams),
                      function(x) paste(round(st_coordinates(x), 4),
                                        collapse = "_"))
dupes <- which(duplicated(coords_hash))
message("  Duplicate edges found: ", length(dupes))

subbasin_streams_clean <- subbasin_streams[-dupes, ]
message("  Edges after deduplication: ", nrow(subbasin_streams_clean))

edges <- lines_to_lsn(
  streams        = subbasin_streams_clean,
  lsn_path       = lsn_path,
  check_topology = TRUE,
  snap_tolerance = 1,
  topo_tolerance = 20,
  overwrite      = TRUE
)

message("  LSN edges: ", nrow(edges))

# ============================================================
# STEP 4: Snap sites to LSN
# ============================================================

message("\n=== Step 4: Snapping sites to LSN ===")

obs_snapped <- sites_to_lsn(
  sites          = obs_sites,
  edges          = edges,
  lsn_path       = lsn_path,
  file_name      = "obs",
  snap_tolerance = SNAP_TOL,
  save_local     = TRUE,
  overwrite      = TRUE
)

message("  Observation sites snapped: ", nrow(obs_snapped), " / ", nrow(obs_sites))
cat("  Max snap distance (m):", max(obs_snapped$snapdist, na.rm = TRUE), "\n")

pred_snapped <- sites_to_lsn(
  sites          = pred_sites,
  edges          = edges,
  lsn_path       = lsn_path,
  file_name      = "pred_subbasin",
  snap_tolerance = SNAP_TOL,
  save_local     = TRUE,
  overwrite      = TRUE
)

message("  Prediction sites snapped: ", nrow(pred_snapped), " / ", nrow(pred_sites))

# ============================================================
# STEP 5: Calculate upstream distances
# ============================================================

message("\n=== Step 5: Calculating upstream distances ===")

edges <- updist_edges(
  edges      = edges,
  save_local = TRUE,
  lsn_path   = lsn_path,
  calc_length = TRUE
)

site_list <- updist_sites(
  sites      = list(obs = obs_snapped, pred_subbasin = pred_snapped),
  edges      = edges,
  length_col = "Length",
  save_local = TRUE,
  lsn_path   = lsn_path
)

message("  Upstream distances calculated for obs + prediction sites")

# ============================================================
# STEP 6: Calculate Additive Function Values (AFVs)
# ============================================================

message("\n=== Step 6: Calculating AFVs ===")

# accumulation_mean is not in the stream network geometry —
# join it from the predict table using subc_id
if (!"accumulation_mean" %in% names(edges)) {
  message("  Joining accumulation_mean from predict table...")

  edges <- edges %>%
    left_join(
      predict_table %>%
        select(subc_id, accumulation_mean),
      by = "subc_id"
    )
}

afv_col_name <- "accumulation_mean"

# Check join worked
n_missing <- sum(is.na(edges[[afv_col_name]]))
if (n_missing > 0) {
  message("  WARNING: ", n_missing,
          " edges missing accumulation_mean after join — replacing with 1")
  edges[[afv_col_name]][is.na(edges[[afv_col_name]])] <- 1
}

# Replace zeros — zeros propagate through AFV calculation
n_zeros <- sum(edges[[afv_col_name]] == 0, na.rm = TRUE)
if (n_zeros > 0) {
  message("  WARNING: ", n_zeros,
          " edges have zero accumulation — replacing with 1")
  edges[[afv_col_name]][edges[[afv_col_name]] == 0] <- 1
}

edges <- afv_edges(
  edges     = edges,
  infl_col  = afv_col_name,
  segpi_col = "areaPI",
  afv_col   = "afvArea",
  lsn_path  = lsn_path
)

message("  AFV range: ", round(min(edges$afvArea), 4),
        " — ", round(max(edges$afvArea), 4))
message("  Most downstream edge AFV (should be ~1.0): ",
        round(max(edges$afvArea), 4))


site_list <- afv_sites(
  sites      = site_list,
  edges      = edges,
  afv_col    = "afvArea",
  save_local = TRUE,
  lsn_path   = lsn_path
)

# ============================================================
# STEP 7: Assemble SSN object
# ============================================================

message("\n=== Step 7: Assembling SSN object ===")

ssn_path <- "spatial/subbasin/ssn/subbasin.ssn"

ssn_obj <- ssn_assemble(
  edges      = edges,
  lsn_path   = lsn_path,
  obs_sites  = site_list$obs,
  preds_list = list(pred_subbasin = site_list$pred_subbasin),
  ssn_path   = ssn_path,
  import     = TRUE,
  check      = TRUE,
  afv_col    = "afvArea",
  overwrite  = TRUE
)

message("  SSN object assembled")
summary(ssn_obj)

# Create distance matrices — required before model fitting
ssn_create_distmat(
  ssn.object = ssn_obj,
  predpts    = "pred_subbasin",
  among_predpts = TRUE,
  overwrite  = TRUE
)
message("  Distance matrices created")

# ============================================================
# STEP 8: Join environmental predictors to SSN obs + pred sites
# ============================================================

message("\n=== Step 8: Joining environmental predictors ===")

# Join only the predictors used in the model (FORMULA_PREDICTORS)
# joining all predictors risks separation with small sample sizes
predict_table_model <- predict_table %>%
  select(subc_id, all_of(FORMULA_PREDICTORS))

obs_data <- ssn_get_data(ssn_obj, "obs")

if ("subc_id" %in% names(obs_data)) {
  obs_with_env <- obs_data %>%
    left_join(predict_table_model, by = "subc_id")
  ssn_obj <- ssn_put_data(obs_with_env, ssn_obj, "obs")
  message("  Predictors joined to obs sites: ",
          paste(FORMULA_PREDICTORS, collapse = ", "))
} else {
  stop("subc_id not in obs sites — cannot join predictors")
}

pred_data <- ssn_get_data(ssn_obj, "pred_subbasin")
if ("subc_id" %in% names(pred_data)) {
  pred_with_env <- pred_data %>%
    left_join(predict_table_model, by = "subc_id")
  ssn_obj <- ssn_put_data(pred_with_env, ssn_obj, "pred_subbasin")
  message("  Predictors joined to pred sites")
}

# ============================================================
# STEP 8b: Scale environmental predictors
# ============================================================

message("\n=== Step 8b: Scaling environmental predictors ===")

# Scale only the predictors used in the model
# Use obs site mean/SD — apply same transformation to pred sites
# to ensure predictions are on the correct scale
obs_data_base <- ssn_get_data(ssn_obj, "obs")

obs_data_scaled <- obs_data_base %>%
  mutate(across(
    all_of(FORMULA_PREDICTORS),
    ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)
  ))

ssn_obj <- ssn_put_data(obs_data_scaled, ssn_obj, "obs")

# Scale pred sites using the SAME obs mean/SD
pred_data <- ssn_get_data(ssn_obj, "pred_subbasin")

for (pred in FORMULA_PREDICTORS) {
  obs_vals <- obs_data_base %>% st_drop_geometry() %>% pull(pred)
  obs_mean <- mean(obs_vals, na.rm = TRUE)
  obs_sd   <- sd(obs_vals, na.rm = TRUE)
  if (obs_sd > 0) {
    pred_data[[pred]] <- (pred_data[[pred]] - obs_mean) / obs_sd
  }
}

ssn_obj <- ssn_put_data(pred_data, ssn_obj, "pred_subbasin")
message("  Predictors scaled: ", paste(FORMULA_PREDICTORS, collapse = ", "))

# Verify condition number
obs_env_scaled <- ssn_get_data(ssn_obj, "obs") %>%
  st_drop_geometry() %>%
  select(all_of(FORMULA_PREDICTORS))

X_check <- model.matrix(~ ., data = obs_env_scaled)
cat("Condition number:", kappa(X_check), "\n")

# ============================================================
# STEP 9: Fit SSN models per species
# ============================================================

message("\n=== Step 9: Fitting SSN models per species ===")



# Fixed-effect formula — parsimonious set works for all species
# including rarest (6 presences) — see predictor selection in PARAMETERS
formula_rhs <- paste(FORMULA_PREDICTORS, collapse = " + ")
message("  Fixed-effect formula: presence ~ ", formula_rhs)

ssn_results <- list()

for (sp in target_species) {

  message("\n  --- ", sp, " ---")

  # Add presence/absence column for this species to obs data
  obs_data <- ssn_get_data(ssn_obj, "obs")

  obs_data <- obs_data %>%
    mutate(presence = as.integer(site_id %in%
                                   (fish_subbasin %>% filter(species == sp) %>% pull(site_id))))

  cat("    Presences:", sum(obs_data$presence), "/", nrow(obs_data), "\n")

  if (sum(obs_data$presence) < 3) {
    message("    Skipping — fewer than 3 presences")
    next
  }

  ssn_obj <- ssn_put_data(obs_data, ssn_obj, "obs")

  # --- Torgegram: inspect spatial autocorrelation structure ---
  formula_torg <- as.formula(paste("presence ~", formula_rhs))

  tg <- tryCatch(
    Torgegram(formula  = formula_torg,
              ssn.object = ssn_obj,
              type = c("flowcon", "flowuncon", "euclid")),
    error = function(e) { message("    Torgegram failed: ", e$message); NULL }
  )

  if (!is.null(tg)) {
    png(paste0("sdm/ssn_models/torgegram_", sp, ".png"),
        width = 1200, height = 400)
    plot(tg)
    dev.off()
    message("    Torgegram saved")
  }

  # --- Fit models with different covariance structures ---
  # Per colleague's paper: for fish (active dispersers) tail-down is primary
  # Test all three and select by AICc

  formula_mod <- as.formula(paste("presence ~", formula_rhs))

  model_specs <- list(
    td_only  = list(taildown_type = "spherical"),
    tu_td    = list(tailup_type = "exponential", taildown_type = "spherical"),
    tu_td_eu = list(tailup_type = "exponential", taildown_type = "spherical",
                    euclid_type = "gaussian")
  )

  fitted_models <- list()

  for (mod_name in names(model_specs)) {
    spec <- model_specs[[mod_name]]
    message("    Fitting: ", mod_name)

    fitted_models[[mod_name]] <- tryCatch(
      do.call(ssn_glm, c(
        list(formula    = formula_mod,
             family     = "binomial",
             ssn.object = ssn_obj,
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

  # Remove failed models
  fitted_models <- Filter(Negate(is.null), fitted_models)

  if (length(fitted_models) == 0) {
    message("    All models failed for ", sp)
    next
  }

  # Compare models — handle single or multiple models
  if (length(fitted_models) == 1) {
    model_comparison <- glance(fitted_models[[1]])
  } else {
    model_comparison <- tryCatch(
      do.call(glances, fitted_models),
      error = function(e) {
        # fallback: compare AICc manually
        data.frame(
          model = names(fitted_models),
          AICc  = sapply(fitted_models, function(m) glance(m)$AICc)
        )
      }
    )
  }
  print(model_comparison)

  # Select best model by AICc
  best_name <- names(fitted_models)[which.min(
    sapply(fitted_models, function(m) glance(m)$AICc)
  )]
  best_model <- fitted_models[[best_name]]
  message("    Best model: ", best_name)

  # Refit best model with REML for final inference
  best_spec <- model_specs[[best_name]]
  best_model_reml <- tryCatch(
    do.call(ssn_glm, c(
      list(formula    = formula_mod,
           family     = "binomial",
           ssn.object = ssn_obj,
           additive   = "afvArea",
           estmethod  = "reml"),
      best_spec
    )),
    error = function(e) { message("    REML refit failed: ", e$message); best_model }
  )

  # Save model
  saveRDS(best_model_reml,
          paste0("sdm/ssn_models/ssn_", sp, ".rds"))

  # Predict across subbasin
  preds <- tryCatch(
    augment(best_model_reml, newdata = "pred_subbasin"),
    error = function(e) { message("    Prediction failed: ", e$message); NULL }
  )

  if (!is.null(preds)) {
    pred_out <- preds %>%
      st_drop_geometry() %>%
      select(subc_id, .fitted) %>%
      rename(suitability = .fitted) %>%
      mutate(species = sp,
             model   = best_name)

    fwrite(pred_out,
           paste0("sdm/predictions/pred_", sp, ".csv"))
    message("    Predictions saved: ", nrow(pred_out), " subcatchments")
  }

  ssn_results[[sp]] <- list(
    best_model  = best_name,
    n_presences = sum(obs_data$presence),
    AICc        = glance(best_model_reml)$AICc,
    model_comparison = model_comparison
  )
}

# ============================================================
# STEP 10: Summary
# ============================================================

message("\n=== Step 10: Summary ===")

results_summary <- lapply(names(ssn_results), function(sp) {
  r <- ssn_results[[sp]]
  data.frame(
    species     = sp,
    n_presences = r$n_presences,
    best_model  = r$best_model,
    AICc        = round(r$AICc, 2)
  )
}) %>% rbindlist()

print(results_summary)
fwrite(results_summary, "sdm/ssn_models/model_summary.csv")

message("\nOutputs:")
message("  spatial/subbasin/ssn/         — SSN object")
message("  sdm/ssn_models/               — model .rds files + torgegrams")
message("  sdm/predictions/              — habitat suitability per species")
message("  sdm/ssn_models/model_summary.csv")
message("\nNext: combine predictions with connectivity results")
