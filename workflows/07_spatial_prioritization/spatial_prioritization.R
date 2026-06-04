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
# CBC solver required: install.packages("rcbc", repos = "https://cran.r-universe.dev")
library(sf)
library(data.table)
library(dplyr)
library(tidyr)
library(igraph)
library(ggplot2)
library(patchwork)

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
# STEP 3: Species-planning-unit table (puvspr_dat)
# ============================================================

message("\n=== Step 3: Building species-planning-unit table ===")

spec_dat <- data.frame(id = seq_along(TARGET_SPECIES), name = TARGET_SPECIES)

puvspr_dat <- lapply(seq_along(TARGET_SPECIES), function(i) {
  preds <- sdm_list[[TARGET_SPECIES[i]]]
  if (is.null(preds)) return(NULL)
  preds <- as.data.table(preds)
  preds <- preds[ensemble_mean > 0, .(pu = subc_id, amount = ensemble_mean)]
  preds[, species := i]
  preds
}) %>% rbindlist(use.names = TRUE, fill = TRUE)

puvspr_dat <- puvspr_dat[pu %in% pu_dat$id]
message("  Species-PU rows: ", nrow(puvspr_dat))

fwrite(puvspr_dat, "prioritization/puvspr_dat.csv")

# ============================================================
# STEP 4: Barrier-aware boundary files (one per scenario)
# ============================================================

message("\n=== Step 4: Building barrier-aware boundary files ===")

subc_ids_basin <- as.character(pu_dat$id)

# Build a symmetric boundary table from a scenario graph, keeping only
# edges that are NOT blocked by a dam (barrier == FALSE). Dropping a dam
# edge means the two reaches it joined are no longer treated as connected.
build_boundary <- function(g, label) {
  edge_df <- igraph::as_data_frame(g, what = "edges")

  # keep only non-barrier edges within the study sub-basin
  keep <- edge_df %>%
    filter(barrier == FALSE,
           from %in% subc_ids_basin,
           to   %in% subc_ids_basin) %>%
    rename(id1 = from, id2 = to) %>%
    mutate(id1 = as.integer(id1), id2 = as.integer(id2), boundary = 1) %>%
    select(id1, id2, boundary)

  bmat <- bind_rows(keep, keep %>% rename(id1 = id2, id2 = id1)) %>%
    distinct(id1, id2, .keep_all = TRUE)

  n_total_edges <- sum(edge_df$from %in% subc_ids_basin &
                         edge_df$to   %in% subc_ids_basin)
  n_dropped     <- n_total_edges - nrow(keep)
  message("  [", label, "] adjacencies kept: ", nrow(keep),
          " | dam-severed: ", n_dropped)
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

solve_one <- function(bmat, target) {
  problem(pu_dat, spec_dat, cost_column = "cost", rij = puvspr_dat) %>%
    add_min_set_objective() %>%
    add_relative_targets(target) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(penalty = BOUNDARY_PENALTY, data = bmat) %>%
    add_cbc_solver(gap = SOLVER_GAP, threads = N_THREADS, verbose = FALSE) %>%
    solve()
}

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
