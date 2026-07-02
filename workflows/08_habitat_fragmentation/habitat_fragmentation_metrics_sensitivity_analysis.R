#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01b_habitat_fragmentation_metrics_sensitivity_analysis.R   (Module 8)
#
# Sensitivity analysis for the dam impact buffer radii used in Group B2
# (overlap of habitat patches with dam impact buffers) of
# 01_habitat_fragmentation_metrics.R.
#
# The main script fixes DAM_BUFFER_UP_M = 50 and DAM_BUFFER_DOWN_M = 2000.
# Here we repeat the buffer + overlap calculation across a grid of
# alternative radii to check how much the overlap metric (and therefore
# the MS numbers derived from it) depends on that choice.
#
#   Upstream radii   (m): 50, 200, 500
#   Downstream radii (m): 1000, 2000, 5000
#   -> 3 x 3 = 9 combinations, all applied to the future dam scenario
#      (existing + planned), same as Group B in the main script.
#
# The network, SDM predictions, dams and per-species patch membership are
# not recomputed from scratch; patch membership is read from the CSVs
# already written by 01_habitat_fragmentation_metrics.R (Step 6, A1-A2),
# so this script assumes that script has already been run once for the
# current extent.
#
# Prerequisite scripts:
#   01_habitat_fragmentation_metrics.R   (patch_membership_<species>.csv,
#                                         patch_summary_all.csv)
#
# Inputs:
#   spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv
#   spatial/subbasin/stream_network_habitat_tss.gpkg   (reach lengths, geometry)
#   points_snapped/dams/dams_snapped_points.csv
#   sdm/patch_metrics/patch_membership_<species>.csv
#   sdm/patch_metrics/patch_summary_all.csv
#
# Outputs (sdm/patch_metrics/sensitivity/):
#   dam_buffer_reaches_up<U>_down<D>.csv       one per combination
#   dam_buffer_overlap_sensitivity.csv         per-patch overlap, all combinations
#   dam_buffer_overlap_sensitivity_species.csv species-level overlap, all combinations
#
# LOCATION: workflows/08_habitat_fragmentation/01b_habitat_fragmentation_metrics_sensitivity_analyses.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(igraph)
library(sf)
library(data.table)
library(dplyr)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

TARGET_SPECIES <- c(
  "Alburnoides_prespensis",
  "Anguilla_anguilla",
  "Barbus_prespensis",
  "Chondrostoma_ohridanum",
  "Oxynoemacheilus_pindus",
  "Salmo_farioides",
  "Squalius_platyceps"
)

# Sensitivity grid, main script values (50 / 2000) are included on purpose
# so the grid output can be checked against the main script result.
UP_RADII_M   <- c(50, 200, 500)
DOWN_RADII_M <- c(1000, 2000, 5000)

THRESHOLD_METHOD <- "tss"

PATCH_METRICS_DIR <- "sdm/patch_metrics"
OUT_DIR            <- file.path(PATCH_METRICS_DIR, "sensitivity")

# ============================================================
# SETUP
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("DAM BUFFER RADII -- SENSITIVITY ANALYSIS")
message(paste(rep("=", 80), collapse = ""))

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# STEP 1: Reload network reaches and geometry (same extent as Module 8)
# ============================================================

message("\n=== Step 1: Loading Sarantaporos network reaches ===")

sarantaporos_ids_pruned <- fread(
  "spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv"
)$subc_id

hab_gpkg <- paste0("spatial/subbasin_sarantaporos/stream_network_habitat_",
                   THRESHOLD_METHOD, ".gpkg")

network_sf <- st_read(hab_gpkg, quiet = TRUE) %>%
  filter(subc_id %in% sarantaporos_ids_pruned)

network_dt <- network_sf %>%
  st_drop_geometry() %>%
  as.data.table()

# Reach length column name follows the main script (reach_length_m); fall
# back to "length" if the habitat gpkg stores it under that name instead.
if (!"reach_length_m" %in% names(network_dt) && "length" %in% names(network_dt)) {
  network_dt <- network_dt %>% rename(reach_length_m = length)
}

network_sf_buf <- network_sf %>%
  left_join(
    network_dt %>% select(subc_id, reach_length_m),
    by = "subc_id"
  )

message("  Reaches: ", nrow(network_dt))

# ============================================================
# STEP 2: Load future dams (existing + planned, same as Group B)
# ============================================================

message("\n=== Step 2: Loading dam data ===")

dams <- fread("points_snapped/dams/dams_snapped_points.csv") %>%
  filter(subc_id %in% network_dt$subc_id)

dams_future <- dams %>% filter(status %in% c("existing", "planned"))

message("  Dams (future scenario): ", nrow(dams_future))

# ============================================================
# STEP 3: Load patch membership per species (from Module 8 main run)
# ============================================================

message("\n=== Step 3: Loading patch membership from main run ===")

patch_membership_list <- list()

for (sp in TARGET_SPECIES) {
  f <- file.path(PATCH_METRICS_DIR, paste0("patch_membership_", sp, ".csv"))
  if (!file.exists(f)) {
    message("  Missing: ", f, " -- run 01_habitat_fragmentation_metrics.R first. Skipping ", sp)
    next
  }
  patch_membership_list[[sp]] <- fread(f)
}

if (length(patch_membership_list) == 0)
  stop("No patch_membership_<species>.csv files found in ", PATCH_METRICS_DIR,
       " -- run 01_habitat_fragmentation_metrics.R first.")

patch_summary_all <- fread(file.path(PATCH_METRICS_DIR, "patch_summary_all.csv"))

message("  Species with patch membership: ", length(patch_membership_list))

# ============================================================
# STEP 4: Helper -- overlap of one species' patches with one buffer set
# ============================================================

# Same logic as Group B2 in the main script: intersect each patch's reach
# ids with the union of upstream/downstream buffer reach ids, then sum the
# overlapping reach length.

compute_overlap_for_species <- function(patch_membership, up_ids, dn_ids,
                                        network_dt) {

  patch_len <- patch_membership %>%
    group_by(patch_id) %>%
    summarise(patch_length_km = round(sum(reach_length_m, na.rm = TRUE) / 1000, 3),
              .groups = "drop") %>%
    as.data.table()

  buf_ids <- union(up_ids, dn_ids)

  patch_membership %>%
    mutate(subc_id = as.character(subc_id)) %>%
    group_by(patch_id) %>%
    summarise(
      n_reaches_overlap = sum(subc_id %in% buf_ids),
      overlap_length_km = round(
        sum(reach_length_m[subc_id %in% buf_ids], na.rm = TRUE) / 1000, 3
      ),
      .groups = "drop"
    ) %>%
    left_join(patch_len, by = "patch_id") %>%
    mutate(pct_overlap = round(100 * overlap_length_km / patch_length_km, 1)) %>%
    as.data.table()
}

# ============================================================
# STEP 5: Grid loop -- buffer computation + overlap, all combinations
# ============================================================

message("\n=== Step 5: Running sensitivity grid ===")

grid <- expand.grid(up_radius = UP_RADII_M, down_radius = DOWN_RADII_M)

patch_rows   <- list()
species_rows <- list()

for (i in seq_len(nrow(grid))) {

  up_radius   <- grid$up_radius[i]
  down_radius <- grid$down_radius[i]

  message("\n  --- Combination ", i, "/", nrow(grid),
          ": upstream = ", up_radius, " m | downstream = ", down_radius, " m ---")

  buffer_dt <- tryCatch({
    get_buffer_along_the_network(
      lines_sf    = network_sf_buf,
      target_ids  = unique(as.character(dams_future$subc_id)),
      up_radius   = up_radius,
      down_radius = down_radius
    ) %>%
      as.data.table()
  }, error = function(e) {
    message("    ERROR in get_buffer_along_the_network: ", e$message)
    NULL
  })

  if (is.null(buffer_dt)) next

  fwrite(
    buffer_dt,
    file.path(OUT_DIR, paste0("dam_buffer_reaches_up", up_radius,
                              "_down", down_radius, ".csv"))
  )

  up_ids <- buffer_dt %>% filter(direction == "upstream")   %>% pull(subc_id) %>% as.character()
  dn_ids <- buffer_dt %>% filter(direction == "downstream") %>% pull(subc_id) %>% as.character()

  message("    Buffer reaches: upstream = ", length(up_ids),
          " | downstream = ", length(dn_ids))

  for (sp in names(patch_membership_list)) {

    overlap_sp <- compute_overlap_for_species(
      patch_membership_list[[sp]], up_ids, dn_ids, network_dt
    )

    if (nrow(overlap_sp) == 0) next

    overlap_sp <- overlap_sp %>%
      mutate(species = sp, up_radius_m = up_radius, down_radius_m = down_radius) %>%
      as.data.table()

    patch_rows[[paste(sp, up_radius, down_radius)]] <- overlap_sp

    total_len_km <- patch_summary_all[species == sp, total_length_km]

    species_rows[[paste(sp, up_radius, down_radius)]] <- data.table(
      species             = sp,
      up_radius_m         = up_radius,
      down_radius_m       = down_radius,
      total_length_km     = total_len_km,
      total_overlap_km    = round(sum(overlap_sp$overlap_length_km), 3),
      pct_overlap_species = round(100 * sum(overlap_sp$overlap_length_km) / total_len_km, 1)
    )
  }
}

# ============================================================
# STEP 6: Save combined outputs
# ============================================================

message("\n=== Step 6: Saving combined outputs ===")

patch_out <- rbindlist(patch_rows, fill = TRUE)
fwrite(patch_out, file.path(OUT_DIR, "dam_buffer_overlap_sensitivity.csv"))
message("  Saved: ", file.path(OUT_DIR, "dam_buffer_overlap_sensitivity.csv"),
        " (", nrow(patch_out), " rows)")

species_out <- rbindlist(species_rows, fill = TRUE) %>%
  arrange(species, up_radius_m, down_radius_m) %>%
  as.data.table()
fwrite(species_out, file.path(OUT_DIR, "dam_buffer_overlap_sensitivity_species.csv"))
message("  Saved: ", file.path(OUT_DIR, "dam_buffer_overlap_sensitivity_species.csv"),
        " (", nrow(species_out), " rows)")

# ============================================================
# STEP 7: Quick console summary -- range of pct overlap per species
# ============================================================

message("\n=== Step 7: Range of species-level overlap across the grid ===")

species_range <- species_out %>%
  group_by(species) %>%
  summarise(
    min_pct_overlap = min(pct_overlap_species, na.rm = TRUE),
    max_pct_overlap = max(pct_overlap_species, na.rm = TRUE),
    range_pct       = round(max_pct_overlap - min_pct_overlap, 1),
    .groups         = "drop"
  ) %>%
  arrange(desc(range_pct)) %>%
  as.data.table()

print(species_range)
fwrite(species_range, file.path(OUT_DIR, "dam_buffer_overlap_sensitivity_range.csv"))
message("  Saved: ", file.path(OUT_DIR, "dam_buffer_overlap_sensitivity_range.csv"))

message("\n", paste(rep("=", 60), collapse = ""))
message("SENSITIVITY ANALYSIS COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("\nGrid: upstream {", paste(UP_RADII_M, collapse = ", "),
        "} m x downstream {", paste(DOWN_RADII_M, collapse = ", "), "} m")
message("Main script default (50 m / 2000 m) is included in the grid for comparison.")
