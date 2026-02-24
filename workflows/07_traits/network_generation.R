#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01 -- network_generation.R
# Build igraph river network from H90M subcatchments + barriers
# REPLACES: Mekong 01 -- network_generation_v2.R
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
group_by <- dplyr::rename


# Set working directory
wdir <- "/run/user/1000/gvfs/dav:host=nimbus.igb-berlin.de,ssl=true,user=grigoropoulou,prefix=%2Fremote.php%2Fwebdav/workflow_paper/data"
wdir <- "~/Documents/Postdoc/projects/workflow_paper/data"

setwd(wdir)

# ============================================================
# INPUT FILES
# ============================================================

# H90M stream network (lines or polygons with subcatchment info)
# Required fields: subc_id, target (=next downstream), length (meters), basin_id
subcatchments <- st_read("spatial/stream_networks/partial_stream_network.gpkg")

# Dams already snapped to subcatchments — CSV format
# Required field: subc_id (already contains the spatial join)
barriers <- read.csv("points_snapped/dams/dams_snapped_points.csv")

# ============================================================
# PARAMETERS
# ============================================================
PASS_SHP         <- 0.2      # passability of small hydropower plants

# ============================================================
# STEP 1: Count barriers per subcatchment
# Since your CSV already has subc_id, no spatial join needed!
# ============================================================
barrier_counts <- barriers %>%
  dplyr::group_by(subc_id) %>%
  dplyr::summarise(n_shp = n(), .groups = "drop")

# Join with subcatchments before deduplication
subcatchments <- subcatchments %>%
  left_join(barrier_counts, by = "subc_id") %>%
  mutate(n_shp = ifelse(is.na(n_shp), 0, n_shp))

# ============================================================
# STEP 2: Compute per-subcatchment passability
# pass_tot = PASS_SHP ^ number_of_barriers
# ============================================================
subcatchments <- subcatchments %>%
  mutate(pass_tot = PASS_SHP ^ n_shp)

# ============================================================
# STEP 3: Deduplicate (barrier info already attached)
# ============================================================
message("Before deduplication: ", nrow(subcatchments), " rows")

subcatchments <- subcatchments %>%
  distinct(subc_id, .keep_all = TRUE)

message("After deduplication: ", nrow(subcatchments), " rows (unique subcatchments)")

# ============================================================
# STEP 4: Build edges dataframe from target column
# from = subc_id, to = target (next downstream)
# ============================================================
edges_df <- subcatchments %>%
  st_drop_geometry() %>%
  select(subc_id, target) %>%
  rename(from = subc_id, to = target) %>%
  mutate(from = as.character(from),
         to   = as.character(to))

# ============================================================
# STEP 5: Build vertices dataframe
# Keep length in meters (no conversion)
# IMPORTANT: Include basin_id here!
# ============================================================
vertices_df <- subcatchments %>%
  st_drop_geometry() %>%
  select(subc_id, length, pass_tot, basin_id) %>%
  rename(name = subc_id, length_reach = length) %>%
  mutate(name = as.character(name),
         length_reach = ifelse(is.na(length_reach), 0, length_reach))

# ============================================================
# STEP 6: Create igraph object (first pass)
# ============================================================
river_graph <- igraph::graph_from_data_frame(edges_df)

river_graph_v_df <- igraph::as_data_frame(river_graph, "vertices") %>%
  left_join(vertices_df, by = "name") %>%
  # Handle outlet node (target = 0 or NA): neutral attributes
  mutate(length_reach = ifelse(name == "0" | is.na(length_reach), 1, length_reach),
         pass_tot     = ifelse(name == "0" | is.na(pass_tot),     1, pass_tot),
         basin_id     = ifelse(name == "0" | is.na(basin_id),     NA, basin_id))

river_graph_tmp <- igraph::graph_from_data_frame(edges_df, v = river_graph_v_df)

# ============================================================
# STEP 7: Transfer passability from nodes -> edges
# Using geometric mean of connected nodes, split symmetrically
# ============================================================
graph_v_df <- igraph::as_data_frame(river_graph_tmp, "vertices")

graph_e_df <- igraph::as_data_frame(river_graph_tmp, "edges") %>%
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

# ============================================================
# STEP 8: Recreate final river graph with edge passabilities
# ============================================================
river_graph <- igraph::graph_from_data_frame(d = graph_e_df, vertices = graph_v_df)

# Set default vertex weight
V(river_graph)$weight <- 1

# Replace NA passabilities with 1 (no barrier)
E(river_graph)$pass_u <- ifelse(is.na(E(river_graph)$pass_u), 1, E(river_graph)$pass_u)
E(river_graph)$pass_d <- ifelse(is.na(E(river_graph)$pass_d), 1, E(river_graph)$pass_d)

# ============================================================
# SAVE
# ============================================================
saveRDS(river_graph, "spatial/stream_networks/river_graph.RDS")
message("\nriver_graph.RDS saved — ", vcount(river_graph), " nodes, ", ecount(river_graph), " edges")

# Optional: quick diagnostics
message("\nBarrier summary:")
print(summary(subcatchments$n_shp))
message("\nPassability summary:")
print(summary(subcatchments$pass_tot))
message("\nBasins in network:")
print(table(V(river_graph)$basin_id, useNA = "ifany"))
