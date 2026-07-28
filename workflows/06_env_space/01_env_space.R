#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_env_space.R   (Module 6 -- Species in Environmental Space)
#
# Descriptive module: shows how species occurrences are distributed across
# the environmental gradients used for modelling, BEFORE any modelling
# (Module 7).
#
# It subsets the SDM prediction table to the sub-catchments of fish
# occurrences and produces:
#   (a) per-species violin/boxplots across the variables
#   (b) a PCA biplot of occurrences in environmental space
#
# Variables (10): the continuous predictors the SDM actually uses, i.e. the
# post-VIF set in env90m/selected_vars.csv minus the c*_y2020 land-cover
# fractions (compositional shares, not continuous gradients):
#   accumulation_mean         flow accumulation (stream size)
#   bio04_mean                temperature seasonality
#   bio05_mean                max temperature of warmest month (deg C)
#   bio15_mean                precipitation seasonality
#   bio18_mean                precipitation of warmest quarter (mm)
#   channel_elv_dw_seg_mean   downstream channel elevation (m)
#   cti_mean                  compound topographic index (wetness)
#   slope_grad_dw_cel_mean    downstream channel slope gradient
#   stream_dist_dw_near_mean  distance to nearest downstream stream (m)
#   stream_dist_up_near_mean  distance to nearest upstream stream (m)
#
# This module therefore describes the SAME environmental space the models
# were fitted in. The table is built by subsetting Module 7's
# env90m/predict_table.csv (already in analysis units), so the values are
# exactly those the SDM saw and no download or rescaling is needed. Steps
# 0-2 below remain, commented out, as a from-scratch rebuild path for the
# older standalone variable set.
#
# INPUT:
#   env90m/ (downloaded H90m + CHELSA tables; same as SDM module)
#   env90m/subc_ids_basin.txt
#   points_snapped/fish/fish_all_species_snapped.csv   (occurrence subc_ids)
#
# OUTPUT:
#   env90m/env_space_table.csv                          (distinct from predict_table.csv)
#   figures/env_space/violin_{variable}.png
#   figures/env_space/env_space_violins_panel.png
#   figures/env_space/env_space_pca.png
#
# LOCATION: workflows/06_env_space/01_env_space.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

select <- dplyr::select

if (!exists("WORKFLOWS_DIR"))
  WORKFLOWS_DIR <- Sys.getenv("WORKFLOWS_CODE", "")   # set WORKFLOWS_CODE in .Renviron, or put your path here
source(file.path(WORKFLOWS_DIR, "helpers", "config.R"))
setwd(BASE_DIR)

dir.create("figures/env_space", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

TARGET_SPECIES <- c(
  "Alburnoides_prespensis", "Anguilla_anguilla", "Barbus_prespensis",
  "Chondrostoma_ohridanum", "Oxynoemacheilus_pindus", "Salmo_farioides",
  "Squalius_platyceps"
)

# Five descriptor variables (raw env90m names) for this module only
variables <- c(
  "bio01_1981-2010_observed",   # annual mean temperature
  "bio15_1981-2010_observed",   # precipitation seasonality
  "order_strahler",             # Strahler stream order
  "outlet_diff_dw_basin",       # elevation difference to outlet
  "slope_grad_dw_cel"           # downstream channel slope gradient
)

statistics <- c("mean")
tile_id    <- c("h18v04", "h20v04")     # same tiles as the SDM module
n_cores    <- max(1, parallel::detectCores() - 1)

# ============================================================
# STEP 0: Download the env90m tables for the 5 variables
# ============================================================
# NOTE ON THE DOWNLOAD DIRECTORY:
#   We download into the SAME directory the SDM module uses ("env90m/"),
#   with the SAME tiles and source tables, so the values are
#   guaranteed identical to those the SDM used. This module only requests
#   the subset of variables it needs (two of the three download functions;
#   no land cover). If the SDM module has already been run, these tables
#   are already present and the downloads simply confirm/skip them, so no
#   data is duplicated.
#   Running this module on its own (without the SDM) downloads only these
#   five variables, keeping Module 6 self-contained.

message("\n=== Step 0: Downloading env90m tables (from-scratch path only) ===")

dir.create("env90m", showWarnings = FALSE, recursive = TRUE)

# # Climate: bio01 (temperature) + bio15 (precipitation seasonality)
# download_observed_climate_tables(
#   subset       = c("bio01_1981-2010_observed", "bio15_1981-2010_observed"),
#   tile_ids     = tile_id,
#   download      = TRUE,
#   download_dir = "env90m",
#   file_format  = "txt",
#   delete_zips  = TRUE,
#   ignore_missing = FALSE,
#   tempdir      = NULL,
#   quiet        = FALSE
# )
#
# # Hydrography: Strahler order, elevation diff to outlet, downstream slope gradient
# download_hydrography90m_tables(
#   subset       = c("order_strahler", "outlet_diff_dw_basin", "slope_grad_dw_cel"),
#   tile_ids     = tile_id,
#   download      = TRUE,
#   download_dir = "env90m",
#   file_format  = "txt",
#   delete_zips  = TRUE,
#   ignore_missing = FALSE,
#   tempdir      = NULL,
#   quiet        = FALSE
# )

# subc_ids_basin.txt is created by the SDM download step. If running this
# module independently, ensure it exists (it lists the basin sub-catchment
# IDs that get_predict_table extracts values for).
if (!file.exists("env90m/subc_ids_basin.txt"))
  stop("env90m/subc_ids_basin.txt not found. Create it (list of basin subc_ids) ",
       "or run the SDM download step first; it is shared between modules.")

# ============================================================
# STEP 1: Build the environmental table (this module's own copy)
# ============================================================

message("\n=== Step 1: Building env_space table (SDM predictor subset) ===")

env_space_file <- "env90m/env_space_table.csv"   # distinct from SDM predict_table.csv

# env_tbl <- get_predict_table(
#   variable      = variables,
#   statistics    = statistics,
#   tile_id       = tile_id,
#   input_var_path = "env90m",
#   subcatch_id   = "env90m/subc_ids_basin.txt",
#   out_file_path = env_space_file,
#   read          = TRUE,
#   quiet         = FALSE,
#   n_cores       = n_cores,
#   overwrite     = TRUE
# )
#
# # Strip temporal suffix on climate columns: bio01_1981-2010_observed_mean -> bio01_mean
# colnames(env_tbl) <- gsub("_1981-2010_observed", "", colnames(env_tbl))
#
# message("  Columns: ", paste(names(env_tbl), collapse = ", "))
#
# # ============================================================
# # STEP 2: Rescale (identical scale factors to the SDM module)
# # ============================================================
#
# message("\n=== Step 2: Rescaling ===")
#
# # slope gradient: /1e6
# if ("slope_grad_dw_cel_mean" %in% names(env_tbl))
#   env_tbl <- env_tbl %>% mutate(slope_grad_dw_cel_mean = slope_grad_dw_cel_mean / 1e6)
#
# # bio01: temperature in Kelvin x10 -> /10 then -273.15 (deg C)
# if ("bio01_mean" %in% names(env_tbl))
#   env_tbl <- env_tbl %>% mutate(bio01_mean = (bio01_mean / 10) - 273.15)
#
# # bio15: precipitation seasonality x10 -> /10
# if ("bio15_mean" %in% names(env_tbl))
#   env_tbl <- env_tbl %>% mutate(bio15_mean = bio15_mean / 10)
#
# # outlet_diff_dw_basin and order_strahler: no scale factor (raw units)
#
# fwrite(env_tbl, env_space_file)
# message("  Saved rescaled table: ", env_space_file)

# Steps 0-2 above (download + build + rescale) are commented out: this
# table was already built once and is being read from cache to avoid
# re-downloading on every run. On a fresh BASE_DIR without this file,
# uncomment Steps 0-2 (or run 07_sdm/02_create_prediction_table.R first,
# whose predict_table.csv rescaling this block mirrors) before rerunning.
# Variables: the continuous predictors the SDM (Module 7) actually uses, so
# this module describes the same environmental space the models were fitted in
# rather than a separate set of descriptors. Taken from env90m/selected_vars.csv
# (the post-VIF list); the c*_y2020 land-cover fractions are excluded because
# they are compositional shares rather than continuous gradients.
ENV_VARS <- c(
  "accumulation_mean",          # flow accumulation (stream size)
  "bio04_mean",                 # temperature seasonality
  "bio05_mean",                 # max temperature of warmest month
  "bio15_mean",                 # precipitation seasonality
  "bio18_mean",                 # precipitation of warmest quarter
  "channel_elv_dw_seg_mean",    # downstream channel elevation
  "cti_mean",                   # compound topographic index (wetness)
  "slope_grad_dw_cel_mean",     # downstream channel slope gradient
  "stream_dist_dw_near_mean",   # distance to nearest downstream stream
  "stream_dist_up_near_mean"    # distance to nearest upstream stream
)

# Built by subsetting Module 7's prediction table, which already holds these
# columns for every sub-catchment in analysis units -- no downloads needed and
# no separate rescaling, which also guarantees the values are byte-for-byte the
# ones the SDM saw. Steps 0-2 above are kept only for a from-scratch rebuild.
predict_table_file <- "env90m/predict_table.csv"

if (!file.exists(env_space_file)) {
  if (!file.exists(predict_table_file))
    stop(predict_table_file, " not found. Run 07_sdm/02_create_prediction_table.R ",
         "first, or re-enable Steps 0-2 above.")
  message("  Building ", env_space_file, " from ", predict_table_file)
  pt <- fread(predict_table_file)
  miss <- setdiff(ENV_VARS, names(pt))
  if (length(miss))
    stop("predict_table.csv is missing: ", paste(miss, collapse = ", "))
  fwrite(pt[, c("subc_id", ENV_VARS), with = FALSE], env_space_file)
  message("  Saved: ", env_space_file, " (", length(ENV_VARS), " variables)")
}

env_tbl <- fread(env_space_file)

# ============================================================
# STEP 3: Subset to fish occurrence sub-catchments
# ============================================================

message("\n=== Step 3: Subsetting to occurrence sub-catchments ===")

subbasin_streams <- read_geopackage("spatial/subbasin_sarantaporos/stream_network.gpkg",
                                    import_as = "data.table")

fish <- fread("points_snapped/fish/fish_all_species_snapped.csv") %>%
  filter(species %in% gsub("_", " ", TARGET_SPECIES) | species %in% TARGET_SPECIES) %>%
  mutate(species = gsub(" ", "_", species)) %>%
  select(species, subc_id) %>%
  distinct() %>% filter(subc_id %in% subbasin_streams$subc_id)


# fish_subbasin <- fish %>% filter(subc_id %in% subbasin_streams$subc_id)

message("  Occurrence records: ", nrow(fish))





# The columns we will plot -- the SDM predictor set defined above
plot_cols <- ENV_VARS

occ_env <- fish %>%
  left_join(env_tbl, by = "subc_id") %>%
  filter(if_all(all_of(plot_cols), ~ !is.na(.)))

message("  Occurrences with complete env data: ", nrow(occ_env))

# Long format for plotting; nicer labels
var_labels <- c(
  accumulation_mean         = "Flow accumulation (km\u00b2)",
  bio04_mean                = "Temp. seasonality",
  bio05_mean                = "Max temp. warmest month (\u00b0C)",
  bio15_mean                = "Precip. seasonality",
  bio18_mean                = "Precip. warmest quarter (mm)",
  channel_elv_dw_seg_mean   = "Downstream channel elev. (m)",
  cti_mean                  = "Compound topographic index",
  slope_grad_dw_cel_mean    = "Channel slope gradient",
  stream_dist_dw_near_mean  = "Dist. to downstream stream (m)",
  stream_dist_up_near_mean  = "Dist. to upstream stream (m)"
)

occ_long <- occ_env %>%
  pivot_longer(cols = all_of(plot_cols), names_to = "variable", values_to = "value") %>%
  mutate(species_label = gsub("_", " ", species),
         variable_label = var_labels[variable])

# ============================================================
# STEP 4: Violin/boxplots per variable
# ============================================================

message("\n=== Step 4: Violin plots ===")

make_violin <- function(df, var_key) {
  d <- df %>% filter(variable == var_key)
  ggplot(d, aes(x = species_label, y = value, fill = species_label)) +
    geom_violin(alpha = 0.6, colour = NA, scale = "width") +
    geom_boxplot(width = 0.15, outlier.size = 0.5, alpha = 0.9) +
    labs(x = NULL, y = var_labels[[var_key]], title = var_labels[[var_key]]) +
    theme_bw(base_size = 11) +
    theme(legend.position = "none",
          axis.text.x = element_text(face = "italic", angle = 35, hjust = 1),
          plot.title  = element_text(size = 11))
}

violin_plots <- lapply(plot_cols, function(v) make_violin(occ_long, v))
names(violin_plots) <- plot_cols

# individual files
for (v in plot_cols) {
  out <- paste0("figures/env_space/violin_", v, ".png")
  png(out, width = 7, height = 4, units = "in", res = 200)
  print(violin_plots[[v]]); dev.off()
  message("  Saved: ", out)
}

# combined panel
panel <- wrap_plots(violin_plots, ncol = 2) +
  plot_annotation(
    title = "Environmental conditions at species occurrences",
    theme = theme(plot.title = element_text(face = "bold", size = 13))
  )
png("figures/env_space/env_space_violins_panel.png",
    width = 11, height = 12, units = "in", res = 200)
print(panel); dev.off()
message("  Saved: figures/env_space/env_space_violins_panel.png")

# ============================================================
# STEP 5: PCA of occurrences in environmental space
# ============================================================

message("\n=== Step 5: PCA ===")

# PCA uses all of the SDM predictors -- every variable in ENV_VARS is
# continuous, so unlike the earlier version (which had to drop ordinal
# Strahler order) nothing needs excluding here.
pca_cols <- ENV_VARS

# Power-transform strongly right-skewed variables before PCA so it is not
# dominated by a few extreme reaches. These five have skew > 1.5 and are
# non-negative, so log1p is safe; the climate variables and cti_mean are
# near-symmetric (|skew| < 1.2) and are left untransformed. cti_mean also
# takes negative values, so log1p would not apply to it in any case.
SKEWED_VARS <- c("accumulation_mean", "channel_elv_dw_seg_mean",
                 "slope_grad_dw_cel_mean", "stream_dist_dw_near_mean",
                 "stream_dist_up_near_mean")

pca_dat <- occ_env %>%
  mutate(across(all_of(SKEWED_VARS), ~ log1p(pmax(.x, 0))))

pca_mat <- pca_dat %>% select(all_of(pca_cols)) %>% as.matrix()
pca     <- prcomp(pca_mat, center = TRUE, scale. = TRUE)
round(pca$rotation[, 1:2], 3)

# scores + loadings
scores <- as.data.frame(pca$x[, 1:2]) %>%
  mutate(species_label = gsub("_", " ", pca_dat$species))

load_scale <- 3
loadings <- as.data.frame(pca$rotation[, 1:2]) %>%
  tibble::rownames_to_column("variable") %>%
  mutate(variable_label = var_labels[variable],
         PC1 = PC1 * load_scale, PC2 = PC2 * load_scale)

var_expl <- round(100 * (pca$sdev^2) / sum(pca$sdev^2), 1)

p_pca <- ggplot() +
  geom_point(data = scores, aes(PC1, PC2, colour = species_label, shape = species_label),
             size = 2.2, alpha = 0.8,
             position = position_jitter(width = 0.08, height = 0.08, seed = 42)) +
  scale_shape_manual(values = c(16, 17, 15, 18, 8, 3, 4)) +
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")), colour = "grey25") +
  geom_text(data = loadings,
            aes(x = PC1 * 1.1, y = PC2 * 1.1, label = variable_label),
            size = 3, colour = "grey25") +
  labs(x = paste0("PC1 (", var_expl[1], "%)"),
       y = paste0("PC2 (", var_expl[2], "%)"),
       colour = "Species", shape = "Species",
       title = "Occurrences in environmental space (PCA)") +
  theme_bw(base_size = 11) +
  theme(legend.text = element_text(face = "italic"))

png("figures/env_space/env_space_pca.png",
    width = 8, height = 6, units = "in", res = 200)
print(p_pca); dev.off()
message("  Saved: figures/env_space/env_space_pca.png")

# ============================================================
# SUMMARY
# ============================================================
message("\n", paste(rep("=", 60), collapse = ""))
message("ENV-SPACE FIGURES COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("  env90m/env_space_table.csv")
message("  figures/env_space/violin_{variable}.png")
message("  figures/env_space/env_space_violins_panel.png")
message("  figures/env_space/env_space_pca.png")
