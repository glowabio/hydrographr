#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_network_fragmentation.R   (Module 5 -- Network Fragmentation)
#
# Species-FREE structural fragmentation of the physical river network.
# Treats every dam as a fully blocking barrier, cuts the network at dam
# edges, and decomposes it into free-flowing fragments (weak components).
#
# Reports, per scenario (current / future):
#   - number of fragments
#   - fragment lengths (total / median / max free-flowing length)
#   - Strahler-order distribution of reaches across fragments
#
# This is the network-level counterpart to the species-level habitat
# fragmentation in Module 8. No fish data are used here.
#
# INPUT:
#   - spatial/stream_network_graphs/river_graph_current.RDS
#   - spatial/stream_network_graphs/river_graph_future.RDS
#     (built by 02_generate_network_graph.R; carry V()$length_reach,
#      V()$strahler, E()$barrier)
#
# OUTPUT:
#   - connectivity/network_fragmentation_summary.csv
#   - connectivity/fragment_strahler_distribution.csv
#
# LOCATION: workflows/05_network_fragmentation/01_network_fragmentation.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(data.table)

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("connectivity", showWarnings = FALSE)

# ============================================================
# LOAD SCENARIO GRAPHS
# ============================================================

message("Loading scenario graphs...")
river_graph_current <- readRDS("spatial/stream_network_graphs/river_graph_current.RDS")
river_graph_future  <- readRDS("spatial/stream_network_graphs/river_graph_future.RDS")

message("  Current: ", vcount(river_graph_current), " nodes, ",
        ecount(river_graph_current), " edges")
message("  Future:  ", vcount(river_graph_future), " nodes, ",
        ecount(river_graph_future), " edges")

# ============================================================
# FUNCTION: cut network at dams and decompose into fragments
# ============================================================
# A "fragment" = a free-flowing stretch: a weakly connected component
# of the network after removing all edges flagged as barriers.
fragment_network <- function(g, scenario_name) {

  # Remove barrier edges (dam locations). Treat mode = "weak" so a fragment
  # spans both upstream and downstream reaches, independent of flow direction.
  barrier_edges <- which(E(g)$barrier)
  g_cut <- delete_edges(g, barrier_edges)

  comp <- components(g_cut, mode = "weak")

  # Per-node table with fragment membership, reach length, Strahler order
  node_tbl <- tibble(
    name         = V(g_cut)$name,
    fragment     = comp$membership,
    length_reach = V(g_cut)$length_reach,
    strahler     = V(g_cut)$strahler
  ) %>%
    filter(name != "0")   # drop artificial root node

  # Fragment-level lengths (km)
  frag_lengths <- node_tbl %>%
    group_by(fragment) %>%
    summarise(length_km = sum(length_reach, na.rm = TRUE) / 1000,
              n_reaches = n(),
              .groups = "drop")

  summary_row <- tibble(
    scenario          = scenario_name,
    n_barrier_edges   = length(barrier_edges),
    n_fragments       = nrow(frag_lengths),
    total_length_km   = sum(frag_lengths$length_km),
    median_frag_km    = median(frag_lengths$length_km),
    max_frag_km       = max(frag_lengths$length_km),
    # free-flowing length = the same fragment lengths, reported as the
    # total km sitting in uninterrupted (barrier-free) stretches
    mean_freeflow_km  = mean(frag_lengths$length_km)
  )

  # Strahler-order distribution of reaches (length per order)
  strahler_dist <- node_tbl %>%
    filter(!is.na(strahler)) %>%
    group_by(strahler) %>%
    summarise(length_km = sum(length_reach, na.rm = TRUE) / 1000,
              n_reaches = n(),
              .groups = "drop") %>%
    mutate(scenario = scenario_name, .before = 1)

  list(summary = summary_row,
       fragments = frag_lengths %>% mutate(scenario = scenario_name, .before = 1),
       strahler = strahler_dist)
}

# ============================================================
# RUN BOTH SCENARIOS
# ============================================================

message("\nFragmenting current scenario...")
res_current <- fragment_network(river_graph_current, "current")

message("Fragmenting future scenario...")
res_future  <- fragment_network(river_graph_future, "future")

# ============================================================
# COMBINE + REPORT
# ============================================================

summary_df <- bind_rows(res_current$summary, res_future$summary)
strahler_df <- res_current$strahler
fragments_df <- bind_rows(res_current$fragments, res_future$fragments)

message("\n=== NETWORK FRAGMENTATION SUMMARY ===")
print(summary_df)

message("\nStrahler-order distribution (length km per order):")
print(strahler_df)

fwrite(summary_df,  "connectivity/network_fragmentation_summary.csv")
fwrite(strahler_df, "connectivity/fragment_strahler_distribution.csv")
fwrite(fragments_df, "connectivity/fragment_lengths.csv")

message("\nSaved:")
message("  connectivity/network_fragmentation_summary.csv")
message("  connectivity/fragment_strahler_distribution.csv")
message("  connectivity/fragment_lengths.csv")

# ============================================================
# DELTA (impact of planned dams)
# ============================================================
d_frag <- res_future$summary$n_fragments - res_current$summary$n_fragments
message("\nAdditional fragments from planned dams: +", d_frag)
