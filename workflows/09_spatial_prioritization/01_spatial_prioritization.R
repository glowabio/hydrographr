#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_spatial_prioritization.R   (Module 9 -- Spatial Conservation Prioritization)
#
# Spatial conservation prioritization of river reaches in the Sarantaporos
# sub-basin using prioritizr, under TWO barrier scenarios.
#
# Planning units = sub-catchments. Features = per-species thresholded
# ensemble SDM predictions. Cost = Human Footprint Index. A boundary
# penalty rewards spatially connected priority networks.
#
# BARRIER-AWARE CONNECTIVITY (reviewer suggestion):
#   The boundary (connectivity) file lists which planning units are
#   adjacent. A dam breaks the connection between the reaches it sits
#   between. We therefore build the boundary file from edges that are
#   NOT blocked by a dam, separately for each scenario:
#     current : drop the single existing dam's edge
#     future  : drop all dam edges (existing + planned)
#   Habitat and cost are identical between scenarios; only the
#   connectivity changes. The shift in priority areas between scenarios
#   therefore reflects how dams reshape the connected priority network.
#   (Note: a barrier is treated as fully blocking for all species —
#   prioritizr cannot sever connectivity per species.)
#
# Targets solved: 20%, 30%, 40%, 50% (summary table for all; the
# current-vs-future comparison map is drawn at the 30% target).
#
# Input:
#   - spatial/subbasin_sarantaporos/stream_network_pruned.gpkg
#   - spatial/basin/stream_network.gpkg            (reach lengths)
#   - spatial/hfp_zonal_stats.csv
#   - sdm/ensemble/ensemble_{species}.csv
#   - sdm/ensemble/ensemble_thresholds.csv
#   - spatial/stream_networks/river_graph_current.RDS
#   - spatial/stream_networks/river_graph_future.RDS
#
# Output:
#   - prioritization/pu_dat.csv, puvspr_dat.csv
#   - prioritization/solutions/solution_{scenario}_{target_pct}.csv / .gpkg
#   - prioritization/summary_table.csv          (all scenarios x targets)
#   - prioritization/comparison_30pct.csv        (current vs future, 30%)
#   - prioritization/maps/priority_comparison_30pct.png
#   - prioritization/maps/summary_selected_reaches.png
#
# LOCATION: workflows/09_spatial_prioritization/01_spatial_prioritization.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(prioritizr)
# CBC solver required
library(sf)
library(data.table)
library(dplyr)
library(tidyr)
library(igraph)
library(ggplot2)
library(patchwork)
library(hydrographr)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

TARGETS          <- c(0.2, 0.3, 0.4, 0.5)
COMPARISON_TARGET <- 0.3            # target used for the current-vs-future map
BOUNDARY_PENALTY <- 0.03            # Domisch et al. (2019)
SOLVER_GAP       <- 0.1
N_THREADS        <- 4

TARGET_SPECIES <- c(
  "Alburnoides_prespensis", "Anguilla_anguilla", "Barbus_prespensis",
  "Chondrostoma_ohridanum", "Oxynoemacheilus_pindus", "Salmo_farioides",
  "Squalius_platyceps"
)

# ============================================================
# SETUP
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("SPATIAL CONSERVATION PRIORITIZATION (current vs future barriers)")
message(paste(rep("=", 80), collapse = ""))

dir.create("prioritization/solutions", recursive = TRUE, showWarnings = FALSE)
dir.create("prioritization/maps",      recursive = TRUE, showWarnings = FALSE)

# ============================================================
# STEP 1: Load inputs
# ============================================================

message("\n=== Step 1: Loading inputs ===")

stream_lines <- read_sf(
  "spatial/subbasin_sarantaporos/stream_network_pruned.gpkg",
  layer = "stream_network_pruned"
)
message("  Stream reaches (planning units): ", nrow(stream_lines))

stream_network_full <- read_sf("spatial/basin/stream_network.gpkg") %>%
  st_drop_geometry() %>%
  select(subc_id, length)

stream_lines <- left_join(stream_lines, stream_network_full)

hfi <- fread("spatial/hfp_zonal_stats.csv")
message("  HFI rows: ", nrow(hfi))

message("  Loading SDM predictions for ", length(TARGET_SPECIES), " species...")
if (exists("preds")) rm(preds)

sdm_list <- lapply(TARGET_SPECIES, function(sp) {
  f <- file.path("sdm/ensemble", paste0("ensemble_", sp, ".csv"))
  if (!file.exists(f)) { message("    WARNING: file not found — ", f); return(NULL) }
  dt <- fread(f)
  if (!"ensemble_mean" %in% names(dt)) {
    message("    WARNING: 'ensemble_mean' column missing in ", f); return(NULL)
  }
  dt
})
names(sdm_list) <- TARGET_SPECIES

# Apply TSS thresholds (semi-binary: below threshold -> 0)
thresholds <- fread("sdm/ensemble/ensemble_thresholds.csv")
sdm_list <- lapply(TARGET_SPECIES, function(sp) {
  dt <- sdm_list[[sp]]; if (is.null(dt)) return(NULL)
  thr <- thresholds[species == sp, threshold_tss]
  if (length(thr) == 0 || is.na(thr)) return(dt)
  dt[ensemble_mean < thr, ensemble_mean := 0]
  dt
})
names(sdm_list) <- TARGET_SPECIES

# Both scenario graphs — used to build barrier-aware connectivity
river_graph_current <- readRDS("spatial/stream_networks/river_graph_current.RDS")
river_graph_future  <- readRDS("spatial/stream_networks/river_graph_future.RDS")
message("  Graphs loaded (current + future)")

# ============================================================
# STEP 2: Planning unit table (pu_dat)
# ============================================================

message("\n=== Step 2: Building planning unit table ===")

pu_dat <- stream_lines %>%
  st_drop_geometry() %>%
  select(subc_id) %>%
  rename(id = subc_id) %>%
  left_join(stream_network_full %>% rename(id = subc_id), by = "id") %>%
  mutate(length_km = length / 1000) %>%
  left_join(
    hfi %>% mutate(id = as.integer(subc_id)) %>% select(id, hfp_wgs_mean),
    by = "id"
  ) %>%
  rename(hfi_mean = hfp_wgs_mean)

n_na_hfi <- sum(is.na(pu_dat$hfi_mean))
if (n_na_hfi > 0) stop("HFI join failed: ", n_na_hfi, " NAs in hfi_mean")

hfi_range <- max(pu_dat$hfi_mean) - min(pu_dat$hfi_mean)
pu_dat <- pu_dat %>%
  mutate(cost = (hfi_mean - min(hfi_mean)) / hfi_range,
         cost = ifelse(cost == 0, 1e-6, cost))

message("  Planning units: ", nrow(pu_dat),
        " | total length: ", round(sum(pu_dat$length_km, na.rm = TRUE), 1), " km")

fwrite(pu_dat, "prioritization/pu_dat.csv")




# ============================================================
# STEP 2b: Barrier opportunity-cost layer (planned dams, MW)
# ============================================================
# Opportunity cost = energy forgone by NOT building a PLANNED dam.
# Existing dams carry no opportunity cost (already built) but DO still
# sever connectivity in the boundary file — different layer, different
# logic. Per-reach cost = scaled summed planned-dam MW; dam-free reaches
# contribute 0. Reaches with >1 planned dam get summed MW (avoiding the
# barrier means forgoing all dams on it). Min-max scaled to match HFI.

message("\n=== Step 2b: Building MW opportunity-cost layer (planned only) ===")

dam_mw <- fread("points_snapped/dams/dams_snapped_points_june.csv")
# power   <- read_geopackage("points_cleaned/dams/dams_sarantaporos_clean.gpkg",
#                            import_as = "data.table")

# attach MW to each snapped dam by site_id
# dam_mw <- snapped[, .(site_id, subc_id, status)][
#   power[, .(site_id, power_mw)], on = "site_id", nomatch = NULL
# ]


# PLANNED dams only — existing dam carries no opportunity cost
n_all <- nrow(dam_mw)
dam_mw <- dam_mw[status == "planned"]
message("  Dams in basin: ", n_all,
        " | planned (kept for cost): ", nrow(dam_mw))

# sum MW per reach (co-located planned dams -> summed)
reach_mw   <- dam_mw[, .(mw_sum = sum(power_mw, na.rm = TRUE)), by = subc_id]
shared_ids <- dam_mw[, .N, by = subc_id][N > 1, subc_id]
message("  Reaches with a planned dam: ", nrow(reach_mw),
        " | reaches with >1 (MW summed): ", length(shared_ids))

# join onto planning units; dam-free reaches -> 0
pu_dat <- pu_dat %>%
  left_join(reach_mw %>% rename(id = subc_id), by = "id") %>%
  mutate(mw_sum = ifelse(is.na(mw_sum), 0, mw_sum))

# scale MW 0-1 (min is 0, so mw / max)
mw_max  <- max(pu_dat$mw_sum)
pu_dat  <- pu_dat %>% mutate(mw_cost = mw_sum / mw_max)

# two cost columns: HFI-only (Scenario A) and HFI+MW (Scenario B)
pu_dat <- pu_dat %>%
  mutate(cost_hfi      = cost,                              # Scenario A
         cost_hfi_mw   = cost + mw_cost,                    # Scenario B
         cost_hfi      = ifelse(cost_hfi    == 0, 1e-6, cost_hfi),
         cost_hfi_mw   = ifelse(cost_hfi_mw == 0, 1e-6, cost_hfi_mw))

message("  Reaches w/ MW cost: ", sum(pu_dat$mw_cost > 0),
        " | max planned MW (", round(mw_max, 2), ") -> mw_cost = 1")

fwrite(pu_dat, "prioritization/pu_dat.csv")



# ============================================================
# Scenario A vs B at the comparison target (30%)
#   A: HFI cost            B: HFI + MW opportunity cost
#   both under FUTURE barriers (the planned-dam scenario)
# ============================================================
message("\n=== Cost scenarios A (HFI) vs B (HFI+MW), future barriers, ",
        COMPARISON_TARGET * 100, "% ===")

sol_A <- solve_one(bmat_future, COMPARISON_TARGET, cost_col = "cost_hfi")
sol_B <- solve_one(bmat_future, COMPARISON_TARGET, cost_col = "cost_hfi_mw")

sel_A <- sol_A$id[sol_A$solution_1 == 1]
sel_B <- sol_B$id[sol_B$solution_1 == 1]

cost_cmp <- data.frame(
  scenario     = c("A_HFI", "B_HFI_MW"),
  n_selected   = c(length(sel_A), length(sel_B)),
  selected_km  = round(c(sum(pu_dat$length_km[pu_dat$id %in% sel_A], na.rm = TRUE),
                         sum(pu_dat$length_km[pu_dat$id %in% sel_B], na.rm = TRUE)), 1),
  # how many selected reaches carry a planned dam (the conflict count)
  n_dam_reaches = c(sum(pu_dat$mw_cost[pu_dat$id %in% sel_A] > 0),
                    sum(pu_dat$mw_cost[pu_dat$id %in% sel_B] > 0))
)
print(cost_cmp)

shared  <- length(intersect(sel_A, sel_B))
jaccard <- shared / length(union(sel_A, sel_B))
message("  Shared: ", shared,
        " | A only: ", length(setdiff(sel_A, sel_B)),
        " | B only: ", length(setdiff(sel_B, sel_A)),
        " | Jaccard: ", round(jaccard, 3))
message("  (n_dam_reaches: selected reaches with a planned dam = conflict reaches;",
        " B should pull these DOWN by penalising high-MW reaches)")








# ============================================================
# STEP 3: Species-planning-unit table (puvspr_dat)
#         Gap-filled reaches (suitable in classification but below the
#         TSS threshold, so semibinary = 0) are assigned the species'
#         TSS threshold value. This preserves the habitat continuity
#         established in Module 8 (08_habitat_classification.R) inside
#         the prioritization, instead of dropping those reaches to 0.
# ============================================================

message("\n=== Step 3: Building species-planning-unit table ===")

spec_dat <- data.frame(id = seq_along(TARGET_SPECIES), name = TARGET_SPECIES)

puvspr_dat <- lapply(seq_along(TARGET_SPECIES), function(i) {
  sp <- TARGET_SPECIES[i]

  hab_f <- file.path("sdm/habitat", paste0("habitat_", sp, ".csv"))
  if (!file.exists(hab_f)) {
    message("    WARNING: habitat file not found — ", hab_f); return(NULL)
  }
  hab <- fread(hab_f)

  thr <- thresholds[species == sp, threshold_tss]
  if (length(thr) == 0 || is.na(thr)) thr <- 0

  # suitability = semibinary value, but gap-filled reaches (suitable,
  # below threshold) get the threshold value instead of 0.
  hab[, amount := semibinary_tss]
  hab[binary_tss == 1L & gap_filled_tss == 1L & semibinary_tss == 0,
      amount := thr]

  out <- hab[amount > 0, .(pu = subc_id, amount)]
  out[, species := i]
  n_gap <- hab[binary_tss == 1L & gap_filled_tss == 1L & semibinary_tss == 0, .N]
  message("    ", sp, ": ", nrow(out), " suitable reaches (",
          n_gap, " gap-filled set to threshold ", thr, ")")
  out
}) %>% rbindlist(use.names = TRUE, fill = TRUE)

puvspr_dat <- puvspr_dat[pu %in% pu_dat$id]
message("  Species-PU rows: ", nrow(puvspr_dat))

fwrite(puvspr_dat, "prioritization/puvspr_dat.csv")

# ============================================================
# STEP 4: Barrier-aware boundary files (one per scenario)
#         Multi-hop connectivity with distance decay.
# ============================================================

message("\n=== Step 4: Building barrier-aware boundary files ===")

subc_ids_basin <- as.character(pu_dat$id)

# Maximum connectivity neighbourhood, in reaches (hops). k = 1 reproduces
# the immediate-neighbour boundary file; k > 1 lets prioritizr register
# that two non-adjacent reaches are (or are not) separated by a barrier,
# for species whose populations span several consecutive reaches.
# Boundary weight decays as 1 / hop_distance: adjacent pairs contribute 1,
# 2-hop pairs 0.5, 3-hop pairs 0.33. This keeps the dominant compactness
# signal on close neighbours while letting distant pairs register weakly.
# NOTE: the 1/hop decay is a deliberate simplification and could be
# refined (e.g. by reach length or a dispersal-kernel weighting).
CONNECTIVITY_K <- 3

# Build a symmetric, multi-hop boundary table from a scenario graph.
# Barrier edges are deleted FIRST, so any path computed afterwards is
# barrier-free by construction: a dam between two reaches removes every
# path between them, and they never appear as a connected pair at any
# hop distance. Reachability is treated as UNDIRECTED (fish move both
# ways; dams block both ways), matching the weak-component fragmentation.
build_boundary <- function(g, label, k = CONNECTIVITY_K) {

  # 1. delete barrier edges on the ORIGINAL (directed) graph, where the
  #    barrier attribute is known to be intact, THEN make it undirected.
  is_barrier <- igraph::E(g)$barrier == TRUE
  g_open <- igraph::delete_edges(g, igraph::E(g)[is_barrier])
  n_barrier_edges <- sum(is_barrier)

  g_open <- igraph::as_undirected(g_open, mode = "collapse")

  # keep only nodes inside the study sub-basin
  g_open <- igraph::induced_subgraph(
    g_open,
    igraph::V(g_open)[names(igraph::V(g_open)) %in% subc_ids_basin]
  )

  # 2. all-pairs hop distances on the barrier-free graph
  hop <- igraph::distances(g_open)

  # 3. keep pairs within k hops (upper triangle), decay weight = 1 / hops
  hop[!is.finite(hop)] <- Inf
  hop[lower.tri(hop, diag = TRUE)] <- Inf

  idx <- which(hop >= 1 & hop <= k, arr.ind = TRUE)
  node_ids <- as.integer(rownames(hop))

  keep <- data.frame(
    id1      = node_ids[idx[, "row"]],
    id2      = node_ids[idx[, "col"]],
    boundary = 1 / hop[idx]
  )

  # 4. symmetrise
  bmat <- bind_rows(keep, keep %>% rename(id1 = id2, id2 = id1)) %>%
    distinct(id1, id2, .keep_all = TRUE)

  n_adj   <- sum(keep$boundary == 1)
  n_multi <- sum(keep$boundary <  1)
  message("  [", label, "] k = ", k,
          " | barrier edges removed: ", n_barrier_edges,
          " | adjacent pairs: ", n_adj,
          " | multi-hop pairs: ", n_multi)
  bmat
}

bmat_current <- build_boundary(river_graph_current, "current")
bmat_future  <- build_boundary(river_graph_future,  "future")

# ============================================================
# STEP 5: Solve (both scenarios x all targets)
# ============================================================

message("\n=== Step 5: Solving (2 scenarios x ", length(TARGETS), " targets) ===")

if (!requireNamespace("rcbc", quietly = TRUE))
  stop("rcbc not found. install.packages('rcbc', repos = 'https://cran.r-universe.dev')")

# ============================================================
# solve_one() — parameterised by cost column
# ============================================================
solve_one <- function(bmat, target, cost_col = "cost_hfi") {
  problem(pu_dat, spec_dat, cost_column = cost_col, rij = puvspr_dat) %>%
    add_min_set_objective() %>%
    add_relative_targets(target) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(penalty = BOUNDARY_PENALTY, data = bmat) %>%
    add_cbc_solver(gap = SOLVER_GAP, threads = N_THREADS, verbose = FALSE) %>%
    solve()
}



# ============================================================
# SENSITIVITY: k = 1 (immediate neighbours) vs k = 3 (multi-hop)
#   Current scenario, 30% target. Checks whether multi-hop
#   connectivity changes the solution and by how much.
# ============================================================

message("\n=== Sensitivity check: k = 1 vs k = 3 (current, 30%) ===")

bmat_k1 <- build_boundary(river_graph_current, "current_k1", k = 1)
bmat_k3 <- build_boundary(river_graph_current, "current_k3", k = 3)

sol_k1 <- solve_one(bmat_k1, 0.3)
sol_k3 <- solve_one(bmat_k3, 0.3)

sel_k1 <- sol_k1$id[sol_k1$solution_1 == 1]
sel_k3 <- sol_k3$id[sol_k3$solution_1 == 1]

km_k1 <- sum(pu_dat$length_km[pu_dat$id %in% sel_k1], na.rm = TRUE)
km_k3 <- sum(pu_dat$length_km[pu_dat$id %in% sel_k3], na.rm = TRUE)

sensitivity <- data.frame(
  k            = c(1, 3),
  n_selected   = c(length(sel_k1), length(sel_k3)),
  selected_km  = round(c(km_k1, km_k3), 1),
  cost         = round(c(sum(sol_k1$cost[sol_k1$solution_1 == 1]),
                         sum(sol_k3$cost[sol_k3$solution_1 == 1])), 3)
)

# overlap between the two solutions
shared    <- length(intersect(sel_k1, sel_k3))
only_k1   <- length(setdiff(sel_k1, sel_k3))
only_k3   <- length(setdiff(sel_k3, sel_k1))
jaccard   <- shared / length(union(sel_k1, sel_k3))

message("\n  Solution comparison:")
print(sensitivity)
message("\n  Shared reaches: ", shared,
        " | k=1 only: ", only_k1,
        " | k=3 only: ", only_k3)
message("  Jaccard similarity: ", round(jaccard, 3))
message("  (Jaccard near 1 = multi-hop barely changes the solution;",
        " lower = k matters)")












scenarios <- list(current = bmat_current, future = bmat_future)

all_solutions <- list()   # keyed "scenario_targetpct"
summary_rows  <- list()

for (scen in names(scenarios)) {
  for (target in TARGETS) {

    key        <- paste0(scen, "_", target * 100, "pct")
    message("\n  [", scen, "] target ", target * 100, "%")

    s <- tryCatch(solve_one(scenarios[[scen]], target),
                  error = function(e) { message("  ERROR: ", conditionMessage(e)); NULL })
    if (is.null(s)) next

    # s is pu_dat + solution_1, so it already carries length_km and cost —
    # do NOT re-join length_km (that creates length_km.x/.y and breaks sums).
    n_selected  <- sum(s$solution_1 == 1)
    selected_km <- sum(s$length_km[s$solution_1 == 1], na.rm = TRUE)
    total_km    <- sum(pu_dat$length_km, na.rm = TRUE)
    total_cost  <- sum(s$cost[s$solution_1 == 1], na.rm = TRUE)

    message("    selected: ", n_selected, " reaches (",
            round(selected_km, 1), " km, ",
            round(100 * selected_km / total_km, 1), "%)")

    all_solutions[[key]] <- s
    summary_rows[[key]]  <- data.frame(
      scenario       = scen,
      target         = target,
      n_selected     = n_selected,
      pct_network    = round(100 * n_selected / nrow(pu_dat), 1),
      selected_km    = round(selected_km, 1),
      pct_length     = round(100 * selected_km / total_km, 1),
      total_hfi_cost = round(total_cost, 2)
    )

    fwrite(s, paste0("prioritization/solutions/solution_", key, ".csv"))
  }
}


# ============================================================
# STEP 6: Summary table (all scenarios x targets)
# ============================================================

message("\n=== Step 6: Summary table ===")

summary_df <- rbindlist(summary_rows)
fwrite(summary_df, "prioritization/summary_table.csv")
print(summary_df)

# ============================================================
# STEP 7: Current vs future comparison at the chosen target
# ============================================================

message("\n=== Step 7: Current vs future comparison (",
        COMPARISON_TARGET * 100, "%) ===")

key_cur <- paste0("current_", COMPARISON_TARGET * 100, "pct")
key_fut <- paste0("future_",  COMPARISON_TARGET * 100, "pct")

sol_cur <- all_solutions[[key_cur]] %>% select(id, sol_current = solution_1)
sol_fut <- all_solutions[[key_fut]] %>% select(id, sol_future  = solution_1)

comparison <- pu_dat %>%
  select(id, length_km) %>%
  left_join(sol_cur, by = "id") %>%
  left_join(sol_fut, by = "id") %>%
  mutate(
    status = case_when(
      sol_current == 1 & sol_future == 1 ~ "Both",
      sol_current == 1 & sol_future == 0 ~ "Current only",
      sol_current == 0 & sol_future == 1 ~ "Future only",
      TRUE                               ~ "Neither"
    )
  )

fwrite(comparison, "prioritization/comparison_30pct.csv")

# how much the priority network shifts when dams are added
shift_tbl <- comparison %>%
  group_by(status) %>%
  summarise(n_reaches = n(),
            length_km = round(sum(length_km, na.rm = TRUE), 1), .groups = "drop")
message("  Priority-area shift (current -> future):")
print(shift_tbl)

# ---- Comparison map ----
comp_sf <- stream_lines %>%
  left_join(comparison %>% rename(subc_id = id), by = "subc_id") %>%
  mutate(status = factor(status,
                         levels = c("Both", "Current only", "Future only", "Neither"))) %>%
  st_transform(4326)

status_cols <- c("Both" = "#1a7a3c", "Current only" = "#d7191c",
                 "Future only" = "#2c7bb6", "Neither" = "grey85")

p_comp <- ggplot(comp_sf) +
  geom_sf(aes(colour = status, linewidth = status == "Neither")) +
  scale_colour_manual(values = status_cols, name = "Priority status") +
  scale_linewidth_manual(values = c(`TRUE` = 0.3, `FALSE` = 0.9), guide = "none") +
  labs(title = paste0("Priority reaches: current vs future barrier scenario (",
                      COMPARISON_TARGET * 100, "% target)"),
       subtitle = "Red = priority only without planned dams; blue = priority only once dams sever connectivity") +
  theme_void(base_size = 11) +
  theme(plot.subtitle = element_text(size = 9, colour = "grey40"))

png("prioritization/maps/priority_comparison_30pct.png",
    width = 9, height = 7, units = "in", res = 200)
print(p_comp); dev.off()
message("  Saved: prioritization/maps/priority_comparison_30pct.png")

# ============================================================
# STEP 8: Summary plot — selected length by target and scenario
# ============================================================

message("\n=== Step 8: Summary plot ===")

p_summary <- ggplot(summary_df,
                    aes(x = factor(target * 100), y = selected_km,
                        fill = scenario)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.9) +
  scale_fill_manual(values = c("current" = "#74add1", "future" = "#d73027"),
                    name = "Scenario") +
  labs(x = "Conservation target (%)",
       y = "Selected network length (km)",
       title = "Prioritization solutions — current vs future barriers",
       subtitle = paste0("Minimum-set, cost = HFI, boundary penalty = ",
                         BOUNDARY_PENALTY)) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top",
        plot.subtitle = element_text(size = 9, colour = "grey40"))

png("prioritization/maps/summary_selected_reaches.png",
    width = 7, height = 5, units = "in", res = 200)
print(p_summary); dev.off()
message("  Saved: prioritization/maps/summary_selected_reaches.png")

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("SPATIAL PRIORITIZATION COMPLETE")
message(paste(rep("=", 80), collapse = ""))
message("Planning units: ", nrow(pu_dat),
        " | targets: ", paste0(TARGETS * 100, "%", collapse = ", "))
message("Scenarios: current (existing dam) vs future (existing + planned)")
message("Comparison map drawn at ", COMPARISON_TARGET * 100, "% target")




