#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_generate_network_graph.R
# Build igraph river network from H90M subcatchments + barriers
# TWO SCENARIOS: current (operational dams) vs future (+ planned dams)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(sf)
library(igraph)
library(riverconn)
library(dplyr)

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select
rename <- dplyr::rename
group_by <- dplyr::group_by

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# INPUT FILES
# ============================================================
subcatchments <- st_read("spatial/stream_networks/partial_stream_network.gpkg")
barriers <- read.csv("points_snapped/dams/dams_snapped_points.csv")

# ============================================================
# PARAMETERS
# ============================================================
PASS_SHP <- 0.2  # passability of small hydropower plants

# ============================================================
# BARRIER SCENARIOS
# ============================================================

# Current: only operational dams (OL + amber = existing)
barrier_counts_current <- barriers %>%
  filter(phase %in% c("OL", "amber")) %>%
  group_by(subc_id) %>%
  summarise(n_shp = n(), .groups = "drop")

# Future: operational + all planned (everything except rejected)
barrier_counts_future <- barriers %>%
  filter(phase != "R") %>%
  group_by(subc_id) %>%
  summarise(n_shp = n(), .groups = "drop")

message("Current scenario: ", sum(barrier_counts_current$n_shp), " dams in ",
        nrow(barrier_counts_current), " subcatchments")
message("Future scenario:  ", sum(barrier_counts_future$n_shp), " dams in ",
        nrow(barrier_counts_future), " subcatchments")

# ============================================================
# FUNCTION: Build river graph from subcatchments + barrier counts
# ============================================================
build_river_graph <- function(subcatchments_raw, barrier_counts, pass_shp) {

  # Step 1: Join barrier counts with subcatchments
  sc <- subcatchments_raw %>%
    left_join(barrier_counts, by = "subc_id") %>%
    mutate(n_shp = ifelse(is.na(n_shp), 0, n_shp))

  # Step 2: Compute passability
  sc <- sc %>%
    mutate(pass_tot = pass_shp ^ n_shp)

  # Step 3: Deduplicate
  sc <- sc %>%
    distinct(subc_id, .keep_all = TRUE)

  # Step 4: Build edges
  edges_df <- sc %>%
    st_drop_geometry() %>%
    select(subc_id, target) %>%
    rename(from = subc_id, to = target) %>%
    mutate(from = as.character(from),
           to   = as.character(to))

  # Step 5: Build vertices
  vertices_df <- sc %>%
    st_drop_geometry() %>%
    select(subc_id, length, pass_tot, basin_id) %>%
    rename(name = subc_id, length_reach = length) %>%
    mutate(name = as.character(name),
           length_reach = ifelse(is.na(length_reach), 0, length_reach))

  # Step 6: Create igraph (first pass)
  rg <- igraph::graph_from_data_frame(edges_df)

  rg_v_df <- igraph::as_data_frame(rg, "vertices") %>%
    left_join(vertices_df, by = "name") %>%
    mutate(length_reach = ifelse(name == "0" | is.na(length_reach), 1, length_reach),
           pass_tot     = ifelse(name == "0" | is.na(pass_tot),     1, pass_tot),
           basin_id     = ifelse(name == "0" | is.na(basin_id),     NA, basin_id))

  rg_tmp <- igraph::graph_from_data_frame(edges_df, v = rg_v_df)

  # Step 7: Transfer passability from nodes -> edges
  graph_v_df <- igraph::as_data_frame(rg_tmp, "vertices")

  graph_e_df <- igraph::as_data_frame(rg_tmp, "edges") %>%
    left_join(graph_v_df %>% select(name, pass_tot) %>% rename(from = name),
              by = "from") %>%
    rename(d_att_from = pass_tot) %>%
    left_join(graph_v_df %>% rename(to = name) %>% select(to, pass_tot),
              by = "to") %>%
    rename(d_att_to = pass_tot) %>%
    mutate(pass_tot = (d_att_from ^ 0.5) * (d_att_to ^ 0.5),
           pass_u   = sqrt(pass_tot),
           pass_d   = sqrt(pass_tot)) %>%
    select(from, to, pass_u, pass_d)

  # Step 8: Final graph
  rg_final <- igraph::graph_from_data_frame(d = graph_e_df, vertices = graph_v_df)

  V(rg_final)$weight <- 1
  E(rg_final)$pass_u <- ifelse(is.na(E(rg_final)$pass_u), 1, E(rg_final)$pass_u)
  E(rg_final)$pass_d <- ifelse(is.na(E(rg_final)$pass_d), 1, E(rg_final)$pass_d)

  return(rg_final)
}

# ============================================================
# BUILD BOTH GRAPHS
# ============================================================

message("\nBuilding current scenario graph...")
river_graph_current <- build_river_graph(subcatchments, barrier_counts_current, PASS_SHP)
message("Current: ", vcount(river_graph_current), " nodes, ", ecount(river_graph_current), " edges")

message("\nBuilding future scenario graph...")
river_graph_future <- build_river_graph(subcatchments, barrier_counts_future, PASS_SHP)
message("Future:  ", vcount(river_graph_future), " nodes, ", ecount(river_graph_future), " edges")

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
message("Passability summary (edges):")
print(summary(E(river_graph_current)$pass_u))

message("\n--- Future scenario ---")
message("Passability summary (edges):")
print(summary(E(river_graph_future)$pass_u))

# Compare: how many edges have lower passability in future?
pass_current <- E(river_graph_current)$pass_u
pass_future  <- E(river_graph_future)$pass_u
n_affected <- sum(pass_future < pass_current)
message("\nEdges with reduced passability in future: ", n_affected,
        " out of ", length(pass_current))
