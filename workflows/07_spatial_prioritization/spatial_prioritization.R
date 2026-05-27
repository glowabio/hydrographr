#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# spatial_prioritization/spatial_prioritization.R
#
# Spatial conservation prioritization of river reaches in the Sarantaporos
# sub-basin using prioritizr. Identifies which stream reaches are the highest
# conservation priority given species habitat suitability (SDM ensemble
# predictions) and human pressure (Human Footprint Index).
#
# Planning units are sub-catchments (Hydrography90m). Features are
# per-species habitat suitability scores (thresholded ensemble predictions).
# The boundary penalty uses the stream network connectivity matrix to
# encourage spatially connected priority areas rather than isolated reaches.
#
# One problem formulation is solved (free — no locked-in constraints,
# as no existing protected area layer is available for the study area):
#   Cost    = Human Footprint Index per sub-catchment (scaled 0–1)
#   Feature = per-species ensemble SDM probability (amount = habitat quality)
#   Boundary penalty = inverse network distance (rewards connected solutions)
#
# The problem is solved at four conservation targets: 20%, 30%, 40%, 50%.
#
# Workflow:
#   1. Load inputs
#   2. Build planning unit table (pu_dat)
#   3. Build species–planning-unit table (puvspr_dat)
#   4. Build boundary/connectivity matrix (bmat)
#   5. Define and solve prioritizr problems
#   6. Export solutions as CSV and GeoPackage
#   7. Map solutions (leaflet)
#   8. Summary plot
#
# Input:
#   - spatial/subbasin/stream_network_pruned.gpkg
#   - spatial/hfp_zonal_stats.csv
#   - data/sdm/ensemble/ensemble_{species}.csv
#   - spatial/stream_networks/river_graph_current.RDS
#
# Output:
#   - prioritization/pu_dat.csv
#   - prioritization/puvspr_dat.csv
#   - prioritization/solutions/solution_{target_pct}.csv
#   - prioritization/solutions/solution_{target_pct}.gpkg
#   - prioritization/maps/priority_map_{target_pct}.html
#   - prioritization/summary_table.csv
#   - prioritization/maps/summary_selected_reaches.png
#
# LOCATION: workflows/spatial_prioritization/spatial_prioritization.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(prioritizr)
library(lpsymphony)
library(sf)
library(data.table)
library(dplyr)
library(tidyr)
library(igraph)
library(leaflet)
library(htmlwidgets)
library(RColorBrewer)
library(ggplot2)

select <- dplyr::select

source("~/Documents/Postdoc/code/workflow_paper/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

# Conservation targets to evaluate
TARGETS <- c(0.2, 0.3, 0.4, 0.5)

# Boundary penalty weight — encourages spatially connected solutions
# (higher = more compact priority network along the stream)
BOUNDARY_PENALTY <- 0.03

# Maximum upstream network distance (m) used to define neighbours
# in the boundary matrix; pairs beyond this threshold are not linked
MAX_NETWORK_DIST_M <- 200000

# Solver optimality gap (0.1 = accept solutions within 10% of optimal)
SOLVER_GAP <- 0.1

# Number of solver threads
N_THREADS <- 4

# Target species (7 species modelled in Module 4)
TARGET_SPECIES <- c(
  "Alburnoides_prespensis",
  "Anguilla_anguilla",
  "Barbus_prespensis",
  "Chondrostoma_ohridanum",
  "Oxynoemacheilus_pindus",
  "Salmo_farioides",
  "Squalius_platyceps"
)

# ============================================================
# SETUP
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("SPATIAL CONSERVATION PRIORITIZATION")
message(paste(rep("=", 80), collapse = ""))

dir.create("prioritization/solutions", recursive = TRUE, showWarnings = FALSE)
dir.create("prioritization/maps",      recursive = TRUE, showWarnings = FALSE)

# ============================================================
# STEP 1: Load inputs
# ============================================================

message("\n=== Step 1: Loading inputs ===")

# --- Sub-catchment spatial layer (planning units) ---
# Stream network sub-catchments for the Sarantaporos sub-basin,
# after pruning (Module 3 output). Each sub-catchment is one planning unit.
subcatchments <- read_sf("spatial/subbasin/stream_network_pruned.gpkg")

# --- Stream network lines (for mapping) ---
# Same GeoPackage, line geometry — used for leaflet visualisation
stream_lines <- read_sf("spatial/subbasin/stream_network_pruned.gpkg",
                        layer = "stream_network_pruned")

message("  Sub-catchments (planning units): ", nrow(subcatchments))

# --- Human Footprint Index (HFI) per sub-catchment ---
# Aggregated per sub-catchment as zonal statistics (mean HFP value).
# Uses the HFP 2021 dataset on a 0–50000 scale. Sub-catchments outside
# the raster extent (no coverage) receive the median value of matched
# sub-catchments rather than 0, to avoid treating unsampled reaches as pristine.
hfi <- fread("spatial/hfp_zonal_stats.csv")

message("  HFI rows: ", nrow(hfi))

# --- SDM ensemble predictions per species ---
# Ensemble predictions from Module 4, one CSV per species.
# Columns: subc_id, prob_maxent, prob_rf, ensemble_mean, n_models,
#          species, models_used.
# ensemble_mean = 0 for sub-catchments below the TSS threshold
# (post-processed semi-binary output from Module 4).
message("  Loading SDM predictions for ", length(TARGET_SPECIES), " species...")

# Guard against any 'preds' object lingering in the environment from
# previous scripts (e.g. SSN prediction objects)
if (exists("preds")) rm(preds)

sdm_list <- lapply(TARGET_SPECIES, function(sp) {
  f <- file.path("sdm/ensemble", paste0("ensemble_", sp, ".csv"))
  if (!file.exists(f)) {
    message("    WARNING: file not found — ", f)
    return(NULL)
  }
  dt <- fread(f)
  # Verify expected columns are present
  if (!"ensemble_mean" %in% names(dt)) {
    message("    WARNING: 'ensemble_mean' column missing in ", f)
    message("    Columns found: ", paste(names(dt), collapse = ", "))
    return(NULL)
  }
  dt
})
names(sdm_list) <- TARGET_SPECIES

n_loaded <- sum(!sapply(sdm_list, is.null))
message("  Loaded predictions for ", n_loaded, " / ", length(TARGET_SPECIES),
        " species")
if (n_loaded > 0) {
  first_sp <- names(Filter(Negate(is.null), sdm_list))[1]
  message("  Columns in first loaded file: ",
          paste(names(sdm_list[[first_sp]]), collapse = ", "))
}

# --- Apply TSS threshold to ensemble predictions ---
# The ensemble CSVs contain continuous probabilities. We apply the
# ensemble TSS threshold (threshold_tss) from the thresholds file to
# zero out sub-catchments below the threshold, following the semi-binary
# scheme from Module 4: values below threshold → 0, values above retain
# their original ensemble probability.
thresholds <- fread("sdm/ensemble/ensemble_thresholds.csv")
message("  Applying TSS thresholds to ensemble predictions...")

sdm_list <- lapply(TARGET_SPECIES, function(sp) {
  dt   <- sdm_list[[sp]]
  if (is.null(dt)) return(NULL)

  thr <- thresholds[species == sp, threshold_tss]
  if (length(thr) == 0 || is.na(thr)) {
    message("    WARNING: no TSS threshold found for ", sp, " — keeping all")
    return(dt)
  }
  message("    ", sp, ": threshold_tss = ", thr,
          "  (", dt[ensemble_mean >= thr, .N], " / ", nrow(dt), " reaches suitable)")
  dt[ensemble_mean < thr, ensemble_mean := 0]
  dt
})
names(sdm_list) <- TARGET_SPECIES

# --- River graph (for boundary matrix) ---
# Directed igraph object encoding upstream–downstream topology of the
# pruned Sarantaporos network, produced in Module 5.
river_graph_current <- readRDS(
  "spatial/stream_networks/river_graph_current.RDS"
)

message("  River graph: ",
        igraph::vcount(river_graph_current), " nodes, ",
        igraph::ecount(river_graph_current), " edges")

# ============================================================
# STEP 2: Build planning unit table (pu_dat)
# ============================================================

message("\n=== Step 2: Building planning unit table ===")

# Planning unit IDs are sub-catchment IDs from the pruned network
pu_dat <- subcatchments %>%
  st_drop_geometry() %>%
  select(subc_id) %>%
  rename(id = subc_id)

# --- Cost: Human Footprint Index (scaled 0–1) ---
# The solver minimises total HFI cost of selected planning units, so it
# preferentially selects reaches under low human pressure. Sub-catchments
# flagged as open water (hfi >= 50) are assigned cost = 0.
pu_dat <- pu_dat %>%
  left_join(
    hfi %>%
      mutate(id = as.integer(subc_id)) %>%
      select(id, hfp_wgs_mean),
    by = "id"
  ) %>%
  rename(hfi_mean = hfp_wgs_mean) %>%
  mutate(
    # HFP 2021 uses a 0–50000 scale (not 0–50).
    # Sub-catchments with no HFI coverage (NA, i.e. outside the raster extent)
    # are assigned the median value of matched sub-catchments to avoid
    # artificially treating unsampled reaches as pristine (cost = 0).
    hfi_mean = ifelse(is.na(hfi_mean),
                      median(hfi_mean, na.rm = TRUE),
                      hfi_mean)
  )

message("  Planning units: ", nrow(pu_dat))
message("  HFI column names available: ", paste(names(hfi), collapse = ", "))

# --- Diagnostics: check HFI join ---
n_na_hfi <- sum(is.na(pu_dat$hfi_mean))

message("  HFI unmatched (NA after join): ", n_na_hfi,
        " (", round(100 * n_na_hfi / nrow(pu_dat), 1), "% of PUs)")
message("  HFI range (raw): [",
        round(min(pu_dat$hfi_mean, na.rm = TRUE), 2), ", ",
        round(max(pu_dat$hfi_mean, na.rm = TRUE), 2), "]")

if (n_na_hfi > 0) {
  message("  WARNING: ", n_na_hfi, " PUs have no HFI match — setting hfi_mean to 0")
  pu_dat <- pu_dat %>% mutate(hfi_mean = ifelse(is.na(hfi_mean), 0, hfi_mean))
}

# Re-compute cost after any NA fill — guard against constant vector (all same
# value → division by zero → NaN); assign uniform cost = 1 in that edge case
hfi_range <- max(pu_dat$hfi_mean) - min(pu_dat$hfi_mean)

if (hfi_range == 0) {
  message("  WARNING: all HFI values identical after clamping — assigning cost = 1")
  pu_dat <- pu_dat %>% mutate(cost = 1)
} else {
  pu_dat <- pu_dat %>%
    mutate(cost = (hfi_mean - min(hfi_mean)) / hfi_range)
}

# prioritizr requires cost > 0 for all PUs; replace exact zeros with a small
# positive value so the solver can distinguish between planning units
n_zero_cost <- sum(pu_dat$cost == 0)
if (n_zero_cost > 0) {
  message("  NOTE: ", n_zero_cost, " PUs have cost = 0 — replacing with 1e-6")
  pu_dat <- pu_dat %>% mutate(cost = ifelse(cost == 0, 1e-6, cost))
}

# Final check — abort early with a clear message rather than a cryptic solver error
n_bad_cost <- sum(is.na(pu_dat$cost) | is.nan(pu_dat$cost) | is.infinite(pu_dat$cost))
if (n_bad_cost > 0) {
  stop("cost column still contains ", n_bad_cost,
       " NA/NaN/Inf values after fixes — check hfp_zonal_stats.csv column names ",
       "and subc_id matching.\nHFI column names: ",
       paste(names(hfi), collapse = ", "))
}

message("  HFI cost range (scaled): [", round(min(pu_dat$cost), 4),
        ", ",                            round(max(pu_dat$cost), 4), "]")
message("  Mean HFI cost:           ",  round(mean(pu_dat$cost), 4))

fwrite(pu_dat, "prioritization/pu_dat.csv")
message("  Saved: prioritization/pu_dat.csv")

# ============================================================
# STEP 3: Build species–planning-unit table (puvspr_dat)
# ============================================================

message("\n=== Step 3: Building species-planning-unit table ===")

# Integer species IDs for prioritizr
spec_dat <- data.frame(
  id   = seq_along(TARGET_SPECIES),
  name = TARGET_SPECIES
)

# For each species, retain only sub-catchments with suitable habitat
# (ensemble_mean > 0, i.e. above the TSS threshold applied in Module 4).
# The 'amount' column holds the continuous ensemble probability so that
# sub-catchments with higher habitat quality contribute more towards
# meeting the conservation target (proportion-based target applied to
# total summed amount rather than reach count).
puvspr_list <- lapply(seq_along(TARGET_SPECIES), function(i) {
  sp    <- TARGET_SPECIES[i]
  preds <- sdm_list[[sp]]
  if (is.null(preds)) return(NULL)

  # Use data.table syntax explicitly — avoids any dplyr/sf column conflicts
  preds <- as.data.table(preds)
  preds <- preds[ensemble_mean > 0, .(pu = subc_id, amount = ensemble_mean)]
  preds[, species := i]
  preds
})

puvspr_dat <- rbindlist(puvspr_list, use.names = TRUE, fill = TRUE)

# Keep only PUs that exist in pu_dat — ensemble CSVs cover the full basin
# but pu_dat is restricted to the pruned Sarantaporos sub-basin
valid_pu_ids <- pu_dat$id
n_before     <- nrow(puvspr_dat)
puvspr_dat   <- puvspr_dat[pu %in% valid_pu_ids]
message("  Filtered out ", n_before - nrow(puvspr_dat),
        " rows with PU IDs outside the study sub-basin")

# Diagnostics: number of suitable reaches per species
suitable_per_sp <- puvspr_dat %>%
  left_join(spec_dat, by = c("species" = "id")) %>%
  group_by(name) %>%
  summarise(n_suitable = n_distinct(pu),
            total_amount = round(sum(amount), 2),
            .groups = "drop")

message("  Species–PU rows: ",       nrow(puvspr_dat))
message("  Suitable reaches per species:")
for (i in seq_len(nrow(suitable_per_sp))) {
  message("    ", suitable_per_sp$name[i],
          ":  ", suitable_per_sp$n_suitable[i], " reaches",
          "  (total amount = ", suitable_per_sp$total_amount[i], ")")
}

fwrite(puvspr_dat, "prioritization/puvspr_dat.csv")
message("  Saved: prioritization/puvspr_dat.csv")

# ============================================================
# STEP 4: Build boundary/connectivity matrix (bmat)
# ============================================================

message("\n=== Step 4: Building boundary connectivity matrix ===")

# We define neighbours as sub-catchment pairs connected in the river graph,
# i.e. directly adjacent (one edge apart) in the upstream–downstream topology.
# The boundary strength is set uniformly to 1 for all adjacent pairs, so the
# boundary penalty simply counts the number of unshared boundaries between
# selected and non-selected planning units — rewarding contiguous stretches
# of river rather than isolated reaches.
#
# For larger basins where a distance-weighted boundary matrix is preferable,
# replace this section with a routing-distance approach (see comments below).

edge_df <- igraph::as_data_frame(river_graph_current, what = "edges")

# Keep only edges connecting nodes (sub-catchments) present in pu_dat
subc_ids_basin <- as.character(pu_dat$id)

bmat <- edge_df %>%
  rename(id1 = from, id2 = to) %>%
  filter(id1 %in% subc_ids_basin,
         id2 %in% subc_ids_basin) %>%
  mutate(
    id1      = as.integer(id1),
    id2      = as.integer(id2),
    boundary = 1
  ) %>%
  select(id1, id2, boundary)

# Add the reverse direction so the matrix is symmetric
# (prioritizr expects undirected boundary data)
bmat <- bind_rows(bmat, bmat %>% rename(id1 = id2, id2 = id1)) %>%
  distinct(id1, id2, .keep_all = TRUE)

message("  Boundary matrix rows (adjacent pairs): ", nrow(bmat))

# --- Alternative: distance-weighted boundary matrix ---
# If you have a routing distance table (produced in Module 5), you can
# replace the adjacency-based bmat above with an inverse-distance version:
#
# stream_routing <- readRDS("spatial/stream_networks/stream_routing_upstream_distance.RDS")
# bmat <- stream_routing %>%
#   filter(cat %in% subc_ids_basin, cat_up %in% subc_ids_basin,
#          dist_m <= MAX_NETWORK_DIST_M) %>%
#   mutate(boundary = 1 - (dist_m / MAX_NETWORK_DIST_M)) %>%
#   select(id1 = cat, id2 = cat_up, boundary) %>%
#   bind_rows(., rename(., id1 = id2, id2 = id1)) %>%
#   distinct(id1, id2, .keep_all = TRUE)

# ============================================================
# STEP 5: Define and solve prioritizr problems
# ============================================================

message("\n=== Step 5: Solving prioritizr problems ===")
message("  Formulation: minimum-set, free (no locked-in constraints)")
message("  Targets: ", paste0(TARGETS * 100, "%", collapse = ", "))

# Helper: build and solve one problem at a given target
solve_prioritization <- function(pu_dat, spec_dat, puvspr_dat,
                                 bmat, target,
                                 gap = SOLVER_GAP, threads = N_THREADS) {

  p <- problem(pu_dat, spec_dat,
               cost_column = "cost",
               rij         = puvspr_dat) %>%
    add_min_set_objective() %>%
    add_relative_targets(target) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(penalty = BOUNDARY_PENALTY, data = bmat)

  # Solver: lpsymphony (open-source, install with BiocManager::install("lpsymphony")).
  # For larger problems, Gurobi is faster (requires licence):
  #   add_gurobi_solver(gap = gap, threads = threads, verbose = FALSE)
  if (!requireNamespace("lpsymphony", quietly = TRUE)) {
    stop("lpsymphony not found. Install with:\n",
         "  BiocManager::install('lpsymphony')")
  }
  p <- p %>% add_lpsymphony_solver(gap = gap, verbose = FALSE)

  solve(p)
}

all_solutions <- list()
summary_rows  <- list()

for (target in TARGETS) {

  target_pct <- paste0(target * 100, "pct")
  message("\n  Target: ", target * 100, "%")

  s <- tryCatch(
    solve_prioritization(pu_dat, spec_dat, puvspr_dat, bmat, target),
    error = function(e) {
      message("  ERROR: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(s)) {
    message("  Skipping — solver failed")
    next
  }

  n_selected  <- sum(s$solution_1 == 1)
  pct_network <- 100 * n_selected / nrow(pu_dat)
  total_cost  <- sum(s$cost[s$solution_1 == 1], na.rm = TRUE)

  message("  Selected reaches:  ", n_selected,
          " (", round(pct_network, 1), "% of network)")
  message("  Total HFI cost:    ", round(total_cost, 2))

  all_solutions[[target_pct]] <- s

  summary_rows[[target_pct]] <- data.frame(
    target         = target,
    target_pct     = target_pct,
    n_selected     = n_selected,
    pct_network    = round(pct_network, 1),
    total_hfi_cost = round(total_cost, 2)
  )

  # ---- Save CSV ----
  out_csv <- paste0("prioritization/solutions/solution_", target_pct, ".csv")
  fwrite(s, out_csv)
  message("  Saved: ", out_csv)

  # ---- Save GeoPackage ----
  solution_sf <- stream_lines %>%
    left_join(
      s %>% select(id, solution_1) %>% rename(subc_id = id),
      by = "subc_id"
    ) %>%
    left_join(
      pu_dat %>% select(id, cost) %>% rename(subc_id = id),
      by = "subc_id"
    ) %>%
    mutate(priority = as.integer(solution_1 == 1))

  out_gpkg <- paste0("prioritization/solutions/solution_", target_pct, ".gpkg")
  # save_to_nimbus(solution_sf, out_gpkg)
  message("  Saved: ", out_gpkg)
}

# ============================================================
# STEP 6: Save summary table
# ============================================================

message("\n=== Step 6: Saving summary table ===")

summary_df <- rbindlist(summary_rows)
fwrite(summary_df, "prioritization/summary_table.csv")
message("  Saved: prioritization/summary_table.csv")
cat("\n")
print(summary_df)

# ============================================================
# STEP 7: Map solutions (leaflet interactive maps)
# ============================================================

message("\n=== Step 7: Creating leaflet maps ===")

for (target_pct in names(all_solutions)) {

  s <- all_solutions[[target_pct]]
  tgt_label <- gsub("pct", "%", target_pct)

  solution_sf <- stream_lines %>%
    left_join(
      s %>% select(id, solution_1) %>% rename(subc_id = id),
      by = "subc_id"
    ) %>%
    left_join(
      pu_dat %>% select(id, cost) %>% rename(subc_id = id),
      by = "subc_id"
    ) %>%
    mutate(priority = as.integer(solution_1 == 1)) %>%
    st_transform(4326)

  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron,   group = "Light") %>%
    addProviderTiles(providers$Esri.WorldImagery,  group = "Satellite") %>%

    # Non-priority reaches
    addPolylines(
      data   = solution_sf %>% filter(priority == 0),
      color  = "grey70",
      weight = 1,
      opacity = 0.6,
      popup  = ~paste0("<b>Sub-catchment:</b> ", subc_id,
                       "<br><b>HFI cost:</b> ",  round(cost, 3),
                       "<br><b>Priority:</b> No")
    ) %>%

    # Priority reaches
    addPolylines(
      data    = solution_sf %>% filter(priority == 1),
      color   = "#c0392b",
      weight  = 3,
      opacity = 1,
      popup   = ~paste0("<b>Sub-catchment:</b> ", subc_id,
                        "<br><b>HFI cost:</b> ",  round(cost, 3),
                        "<br><b>Priority:</b> Yes")
    ) %>%

    addLegend(
      position = "bottomright",
      colors   = c("grey90", "#c0392b"),
      labels   = c("Non-priority", "Priority reach"),
      title    = paste0("Conservation priority<br>",
                        "(target = ", tgt_label, ")")
    ) %>%

    addLayersControl(
      baseGroups = c("Light", "Satellite"),
      options    = layersControlOptions(collapsed = FALSE)
    )

  out_html <- paste0("prioritization/maps/priority_map_", target_pct, ".html")
  # save_to_nimbus(m, out_html)
  message("  Saved: ", out_html)
}

# ============================================================
# STEP 8: Summary plot — selected reaches by target
# ============================================================

message("\n=== Step 8: Summary plot ===")

p_summary <- ggplot(summary_df,
                    aes(x = factor(target * 100),
                        y = pct_network)) +
  geom_col(fill = "#2980b9", alpha = 0.85, width = 0.5) +
  geom_text(aes(label = paste0(n_selected, " reaches")),
            vjust = -0.4, size = 3.2) +
  labs(
    x        = "Conservation target (%)",
    y        = "Selected reaches (% of network)",
    title    = "Prioritization solutions — Sarantaporos sub-basin",
    subtitle = paste0("Minimum-set objective, cost = Human Footprint Index,\n",
                      "boundary penalty = ", BOUNDARY_PENALTY)
  ) +
  theme_bw(base_size = 11)

ggsave("prioritization/maps/summary_selected_reaches.png",
       p_summary, width = 6, height = 5, dpi = 300)
message("  Saved: prioritization/maps/summary_selected_reaches.png")

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("SPATIAL PRIORITIZATION COMPLETE")
message(paste(rep("=", 80), collapse = ""))
message("\nPlanning units:     ", nrow(pu_dat), " sub-catchments")
message("Features (species): ", nrow(spec_dat))
message("Targets solved:     ", paste0(TARGETS * 100, "%", collapse = ", "))
message("\nOutputs:")
message("  prioritization/pu_dat.csv")
message("  prioritization/puvspr_dat.csv")
message("  prioritization/summary_table.csv")
message("  prioritization/solutions/solution_{target_pct}.csv")
message("  prioritization/solutions/solution_{target_pct}.gpkg")
message("  prioritization/maps/priority_map_{target_pct}.html")
message("  prioritization/maps/summary_selected_reaches.png")
message("\nNext: overlay priority reaches with planned dam locations to")
message("identify which dams conflict with the highest-priority network.")
