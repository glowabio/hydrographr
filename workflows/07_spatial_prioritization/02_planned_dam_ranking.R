#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_planned_dam_ranking.R   (Module 11 -- Planned-dam ranking)
#
# Ranks planned dams by the connectivity damage each causes, expressed
# per MW of forgone energy. Damage = summed SDM habitat suitability
# (across the target species) of the reach-set that each dam isolates
# from the otherwise-connected current network. No re-solving; this is a
# graph-component analysis layered on the Module 9 prioritization inputs.
#
# A dam sits on one network edge. Removing that edge splits the network
# into the piece still connected to the outlet and a smaller piece
# stranded above the dam (the "severed set"). The severed set's summed
# suitability is the dam's damage. Dams isolating much good habitat for
# little energy rank worst.
#
# Each dam is scored independently against the CURRENT network (existing
# dam present, planned dams absent), so the score is a marginal measure:
# "what this dam alone would isolate". Cumulative multi-dam effects are a
# separate question (see Discussion).
#
# Input:
#   - prioritization/puvspr_dat.csv          (species x PU suitability)
#   - prioritization/pu_dat.csv              (planning units, length_km)
#   - spatial/stream_networks/river_graph_current.RDS
#   - points_snapped/dams/dams_snapped_points.csv
#   - points_cleaned/dams/dams_sarantaporos_power.gpkg
#
# Output:
#   - prioritization/planned_dam_ranking.csv
#
# LOCATION: workflows/07_spatial_prioritization/02_planned_dam_ranking.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(sf)
library(data.table)
library(dplyr)
library(igraph)
library(hydrographr)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

message("\n", paste(rep("=", 80), collapse = ""))
message("PLANNED-DAM RANKING (connectivity damage per MW)")
message(paste(rep("=", 80), collapse = ""))

# ============================================================
# STEP 1: Read inputs (this script is self-contained)
# ============================================================

message("\n=== Step 1: Reading inputs ===")

puvspr_dat <- fread("prioritization/puvspr_dat.csv")
pu_dat     <- fread("prioritization/pu_dat.csv")
subc_ids_basin <- as.character(pu_dat$id)
message("  Planning units: ", nrow(pu_dat),
        " | species-PU rows: ", nrow(puvspr_dat))

river_graph_current <- readRDS("spatial/stream_networks/river_graph_current.RDS")
message("  Current network graph loaded")

# ============================================================
# STEP 2: Per-reach habitat value (summed SDM suitability)
# ============================================================

message("\n=== Step 2: Per-reach habitat value ===")

reach_value <- puvspr_dat[, .(suitability = sum(amount, na.rm = TRUE)), by = pu]
setnames(reach_value, "pu", "subc_id")
message("  Reaches with non-zero suitability: ", nrow(reach_value))

# ============================================================
# STEP 3: Planned dams — reach + summed MW (co-located summed)
# ============================================================

message("\n=== Step 3: Planned-dam reaches and MW ===")

snapped <- fread("points_snapped/dams/dams_snapped_points.csv")
power   <-   read_geopackage("points_cleaned/dams/dams_sarantaporos_power.gpkg",
                             import_as = "data.table")

planned <- snapped[status == "planned", .(site_id, subc_id)][
  power[, .(site_id, power_mw)], on = "site_id", nomatch = NULL]
planned <- planned[subc_id %in% as.integer(subc_ids_basin)]

dam_reach <- planned[, .(power_mw = sum(power_mw, na.rm = TRUE),
                         n_dams   = .N,
                         site_ids = paste(site_id, collapse = "; ")),
                     by = subc_id]
message("  Planned-dam reaches to rank: ", nrow(dam_reach),
        " (from ", nrow(planned), " dams)")

# ============================================================
# STEP 4: Current network — directed (to find downstream edge)
#         and undirected (to measure the stranded component)
# ============================================================

message("\n=== Step 4: Building current network ===")

# directed graph (upstream -> downstream): used ONLY to find which edge of
# a dam reach points downstream, i.e. the edge a dam actually breaks.
g_dir <- induced_subgraph(
  river_graph_current,
  V(river_graph_current)[names(V(river_graph_current)) %in% subc_ids_basin])
message("  Nodes: ", vcount(g_dir), " | edges: ", ecount(g_dir))

# severed set for a dam reach: cut its single DOWNSTREAM edge, then take the
# component (treated undirected) that contains the dam reach. That component
# is the dam reach plus everything upstream of it — what the dam isolates
# from the outlet. Direction is used only to pick the edge; reachability
# within the stranded piece is undirected (weak components), matching the
# fragmentation logic.
severed_reaches <- function(dam_subc) {
  v <- as.character(dam_subc)
  if (!v %in% names(V(g_dir))) return(character(0))

  # outgoing edge(s) = downstream. In a tree each reach has exactly one.
  out_e <- incident(g_dir, v, mode = "out")
  if (length(out_e) == 0) {
    # no downstream edge: dam reach is the outlet itself — nothing upstream
    # is separable from the outlet by this cut. Treat severed set as empty
    # of *additional* reaches beyond the dam reach.
    return(v)
  }

  g_cut <- delete_edges(g_dir, out_e)
  comp  <- components(g_cut, mode = "weak")   # undirected reachability
  dam_comp <- comp$membership[v]
  names(comp$membership)[comp$membership == dam_comp]
}



# Downstream dewatering zone for a run-of-river dam: walk downstream from
# the dam reach, accumulating reach length, and keep reaches until the
# cumulative downstream distance reaches DEWATER_M (2 km, the bypassed/
# dewatered channel). Returns the dam reach plus the downstream reaches
# within that distance. Uses the directed graph (downstream = "out").
DEWATER_M <- 2000

dewatered_reaches <- function(dam_subc) {
  v <- as.character(dam_subc)
  if (!v %in% names(V(g_dir))) return(character(0))

  reach_len <- setNames(pu_dat$length_km * 1000, as.character(pu_dat$id))

  visited <- character(0)
  cum_m   <- 0
  current <- v
  repeat {
    out_e <- incident(g_dir, current, mode = "out")
    if (length(out_e) == 0) break
    nxt <- ends(g_dir, out_e)[1, 2]          # single downstream neighbour
    len <- reach_len[as.character(nxt)]
    if (is.na(len)) break
    visited <- c(visited, nxt)
    cum_m   <- cum_m + len
    if (cum_m >= DEWATER_M) break
    current <- nxt
  }
  visited
}

# ============================================================
# STEP 5: Score and rank
#   Damage footprint = upstream-isolated reaches (connectivity loss)
#   PLUS downstream dewatered reaches (RoR habitat degradation).
#   Reported as two columns; ranked on their sum per MW.
# ============================================================

message("\n=== Step 5: Scoring planned dams ===")

rank_rows <- lapply(seq_len(nrow(dam_reach)), function(i) {
  ds <- dam_reach$subc_id[i]

  sev_int  <- as.integer(severed_reaches(ds))    # upstream isolated
  dew_int  <- as.integer(dewatered_reaches(ds))  # downstream dewatered
  # a reach cannot be both; downstream set excludes the upstream component
  dew_int  <- setdiff(dew_int, sev_int)

  dmg_up  <- reach_value[subc_id %in% sev_int, sum(suitability, na.rm = TRUE)]
  dmg_dn  <- reach_value[subc_id %in% dew_int, sum(suitability, na.rm = TRUE)]
  dmg_tot <- dmg_up + dmg_dn

  km_up <- sum(pu_dat$length_km[pu_dat$id %in% sev_int], na.rm = TRUE)
  km_dn <- sum(pu_dat$length_km[pu_dat$id %in% dew_int], na.rm = TRUE)

  data.frame(
    subc_id            = ds,
    n_dams             = dam_reach$n_dams[i],
    power_mw           = dam_reach$power_mw[i],
    n_isolated         = length(sev_int),
    n_dewatered        = length(dew_int),
    km_isolated        = round(km_up, 2),
    km_dewatered       = round(km_dn, 2),
    damage_isolated    = round(dmg_up, 3),
    damage_dewatered   = round(dmg_dn, 3),
    damage_total       = round(dmg_tot, 3),
    damage_per_mw      = round(dmg_tot / dam_reach$power_mw[i], 3)
  )
})

dam_rank <- rbindlist(rank_rows)[order(-damage_per_mw)]
dam_rank[, rank := seq_len(.N)]
setcolorder(dam_rank, c("rank", "subc_id", "n_dams", "power_mw",
                        "n_isolated", "n_dewatered",
                        "km_isolated", "km_dewatered",
                        "damage_isolated", "damage_dewatered",
                        "damage_total", "damage_per_mw"))

message("\n--- Planned dams ranked by total damage (isolated + dewatered) per MW ---")
print(dam_rank)

fwrite(dam_rank, "prioritization/planned_dam_ranking.csv")
message("\nSaved: prioritization/planned_dam_ranking.csv")
