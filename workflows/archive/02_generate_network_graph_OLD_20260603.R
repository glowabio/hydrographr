#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_generate_network_graph.R
# Build igraph river network from H90M subcatchments + barriers
# TWO SCENARIOS: current (existing dam only) vs future (existing + planned)
#
# NOTE ON PASSABILITY:
#   This script is species-agnostic. It carries the number of dams per
#   reach (n_shp) on nodes and edges, but does NOT assign a passability
#   value. Structural fragmentation (Module 5) treats any edge with
#   n_shp > 0 as a blocking barrier. Species-specific passability
#   (0.8 / 0.5 / 0) is applied later, at PCI computation time
#   (Module 10 / pci script), as pass = species_passability ^ n_shp.
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(sf)
library(igraph)
library(dplyr)

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select   <- dplyr::select
rename   <- dplyr::rename
group_by <- dplyr::group_by

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# INPUT FILES
# ============================================================
subcatchments <- st_read("spatial/subbasin_sarantaporos/stream_network_pruned.gpkg")

# temporary fix until length is returned by api_getupstreamstreamsegments
basin_stream_length <- read_geopackage("spatial/basin/stream_network_pruned.gpkg",
                                       import_as = "data.table") %>%
  select(subc_id, length)

subcatchments <- subcatchments %>% left_join(basin_stream_length)

# New snapped dam inventory (status column: "existing" / "planned")
barriers <- read.csv("points_snapped/dams/dams_snapped_points.csv") %>%
  filter(subc_id %in% subcatchments$subc_id)   # keep dams on the Sarantaporos network

message("Barriers on network: ", nrow(barriers),
        "  (existing: ", sum(barriers$status == "existing"),
        ", planned: ",   sum(barriers$status == "planned"), ")")

# ============================================================
# BARRIER SCENARIOS  (driven by the 'status' column)
# ============================================================

# Current: only the existing operational dam
barrier_counts_current <- barriers %>%
  filter(status == "existing") %>%
  group_by(subc_id) %>%
  summarise(n_shp = n(), .groups = "drop")

# Future: existing + all planned (rejected/EXCLUDE already removed upstream)
barrier_counts_future <- barriers %>%
  group_by(subc_id) %>%
  summarise(n_shp = n(), .groups = "drop")

message("Current scenario: ", sum(barrier_counts_current$n_shp), " dams in ",
        nrow(barrier_counts_current), " subcatchments")
message("Future scenario:  ", sum(barrier_counts_future$n_shp), " dams in ",
        nrow(barrier_counts_future), " subcatchments")

# ============================================================
# FUNCTION: Build river graph from subcatchments + barrier counts
#   Carries n_shp (dam count) on nodes and edges. No passability.
# ============================================================
build_river_graph <- function(subcatchments_raw, barrier_counts) {

  # Step 1: Join barrier counts with subcatchments
  sc <- subcatchments_raw %>%
    left_join(barrier_counts, by = "subc_id") %>%
    mutate(n_shp = ifelse(is.na(n_shp), 0, n_shp))

  # Step 2: Deduplicate
  sc <- sc %>%
    distinct(subc_id, .keep_all = TRUE)

  # Step 3: Build edges
  edges_df <- sc %>%
    st_drop_geometry() %>%
    select(subc_id, target) %>%
    rename(from = subc_id, to = target) %>%
    mutate(from = as.character(from),
           to   = as.character(to))

  # Step 4: Build vertices (carry reach length, dam count, Strahler order)
  vertices_df <- sc %>%
    st_drop_geometry() %>%
    select(subc_id, length, strahler, n_shp) %>%
    rename(name = subc_id, length_reach = length) %>%
    mutate(name         = as.character(name),
           length_reach = ifelse(is.na(length_reach), 0, length_reach),
           n_shp        = ifelse(is.na(n_shp), 0, n_shp))

  # Step 5: Create igraph (first pass) and attach vertex attributes
  rg <- igraph::graph_from_data_frame(edges_df)

  rg_v_df <- igraph::as_data_frame(rg, "vertices") %>%
    left_join(vertices_df, by = "name") %>%
    mutate(
      length_reach = ifelse(name == "0" | is.na(length_reach), 1, length_reach),
      n_shp        = ifelse(name == "0" | is.na(n_shp),        0, n_shp)
    )

  rg_tmp <- igraph::graph_from_data_frame(edges_df, v = rg_v_df)

  # Step 6: Transfer dam count from nodes -> edges
  #   A dam sits ON a reach (node), but a barrier severs ONE connection.
  #   The network is a directed tree rooted at the outlet, so each node has
  #   exactly one downstream edge (the edge where from == node). We attribute
  #   the dam to that single downstream edge only, so that one dam cuts the
  #   network once and produces two fragments (rather than isolating the
  #   dammed reach by cutting on both sides).
  graph_v_df <- igraph::as_data_frame(rg_tmp, "vertices")

  graph_e_df <- igraph::as_data_frame(rg_tmp, "edges") %>%
    left_join(graph_v_df %>% select(name, n_shp) %>% rename(from = name),
              by = "from") %>%
    rename(n_shp_edge = n_shp) %>%
    mutate(n_shp_edge = ifelse(is.na(n_shp_edge), 0, n_shp_edge)) %>%
    select(from, to, n_shp_edge)

  # Step 7: Final graph
  rg_final <- igraph::graph_from_data_frame(d = graph_e_df, vertices = graph_v_df)

  V(rg_final)$weight   <- 1
  E(rg_final)$n_shp    <- ifelse(is.na(E(rg_final)$n_shp_edge), 0,
                                 E(rg_final)$n_shp_edge)
  # convenience flag for structural fragmentation (Module 5)
  E(rg_final)$barrier  <- E(rg_final)$n_shp > 0

  return(rg_final)
}

# ============================================================
# BUILD BOTH GRAPHS
# ============================================================

message("\nBuilding current scenario graph...")
river_graph_current <- build_river_graph(subcatchments, barrier_counts_current)
message("Current: ", vcount(river_graph_current), " nodes, ",
        ecount(river_graph_current), " edges")

message("\nBuilding future scenario graph...")
river_graph_future <- build_river_graph(subcatchments, barrier_counts_future)
message("Future:  ", vcount(river_graph_future), " nodes, ",
        ecount(river_graph_future), " edges")

# ============================================================
# SAVE
# ============================================================
saveRDS(river_graph_current, "spatial/stream_networks/river_graph_current.RDS")
saveRDS(river_graph_future,  "spatial/stream_networks/river_graph_future.RDS")

message("\nBoth graphs saved!")

# ============================================================
# DIAGNOSTICS
# ============================================================
message("\n--- Current scenario ---")
message("Barriered edges: ", sum(E(river_graph_current)$barrier),
        " out of ", ecount(river_graph_current))

message("\n--- Future scenario ---")
message("Barriered edges: ", sum(E(river_graph_future)$barrier),
        " out of ", ecount(river_graph_future))

# How many additional edges are barriered in the future?
n_affected <- sum(E(river_graph_future)$barrier) -
  sum(E(river_graph_current)$barrier)
message("\nAdditional barriered edges in future scenario: ", n_affected)
