#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_centrality.R   (Module 4 -- Network Analyses)
#
# Compute betweeness centrality on the bare Sarantaporos network graph
# and map it onto the stream network. Species-free, barrier-free:
# centrality describes the undisturbed network topology.
#
# High-betweeness reaches are network bottlenecks through which most
# shortest paths are routed; they flag locations where barrier placement
# would disconnect a disproportionate share of the network (linking to
# the fragmentation analysis in Module 5).
#
# Also reports a set of descriptive network-structure metrics used in the
# Module 4 methods paragraph (reach length, branching structure, longest
# path, drainage density).
#
# NOTE on reach length:
#   In the scenario graphs, reach length is a VERTEX attribute
#   (V()$length_reach, metres): each node is one reach. Edges are the
#   downstream connections between reaches. For along-network distances we
#   therefore map each edge's weight to the length of its SOURCE (upstream)
#   node, and we compute per-reach length statistics directly from the node
#   attribute, not from edges.
#
# INPUT:
#   - spatial/stream_network_graphs/river_graph_current.RDS
#       (any scenario graph works -- centrality uses topology only, not dams;
#        we use the current graph, which carries the full reach set)
#   - spatial/subbasin_sarantaporos/stream_network_pruned.gpkg
#       (for mapping centrality back onto reaches)
#   - spatial/subbasin_sarantaporos/subbasin_polygon.gpkg
#       (for drainage density)
#
# OUTPUT:
#   - connectivity/centrality_table.csv
#   - spatial/stream_network_graphs/stream_betweeness.gpkg   (stream network + bc)
#   - connectivity/centrality_map.png
#   - connectivity/network_summary.csv                       (descriptive metrics)
#
# LOCATION: workflows/04_network_analyses/02_centrality.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(tidyverse)
library(igraph)
library(sf)
library(data.table)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("connectivity", showWarnings = FALSE)

# ============================================================
# LOAD NETWORK GRAPH
# ============================================================
# Centrality uses topology only; dams are irrelevant here. We load the
# current-scenario graph simply because it carries the full reach set.
message("Loading network graph...")
river_graph <- readRDS("spatial/stream_network_graphs/river_graph_current.RDS")

# Drop the artificial root node so it does not distort centrality or
# the length/branching statistics.
pruned_ids <- fread("spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv")$subc_id
river_graph <- induced_subgraph(
  river_graph,
  vids = V(river_graph)[name %in% as.character(pruned_ids)]
)
message("  Filtered to pruned Sarantaporos reaches: ", vcount(river_graph))


message("  Network: ", vcount(river_graph), " nodes, ",
        ecount(river_graph), " edges")

# ============================================================
# COMPUTE betweeness CENTRALITY
# ============================================================
# get_centrality() (hydrographr) wraps igraph centrality on the directed
# stream graph. mode = "in" considers only upstream-connected segments.
message("\nComputing betweeness centrality...")

centrality <- get_centrality(river_graph, index = "betweenness", mode = "in")

# get_centrality returns a data.frame keyed by subc_id; keep it tidy
centrality <- centrality %>%
  mutate(subc_id = as.character(subc_id))

message("  Reaches with centrality values: ", nrow(centrality))
message("  betweeness range: ",
        round(min(centrality$betweeness, na.rm = TRUE), 1), " - ",
        round(max(centrality$betweeness, na.rm = TRUE), 1))

fwrite(centrality, "connectivity/centrality_table.csv")
message("  Saved: connectivity/centrality_table.csv")

# ============================================================
# MAP CENTRALITY ONTO THE STREAM NETWORK
# ============================================================
# Join betweeness back to the stream geometries so it can be plotted /
# exported. (Equivalent to reclass_raster() in the raster-based workflow,
# but on the vector stream network.)
message("\nMapping centrality onto stream network...")

streams <- st_read("spatial/subbasin_sarantaporos/stream_network_pruned.gpkg",
                   quiet = TRUE) %>%
  mutate(subc_id = as.character(subc_id))

streams_bc <- streams %>%
  left_join(centrality %>% select(subc_id, betweeness), by = "subc_id")

st_write(streams_bc,
         "spatial/stream_network_graphs/stream_betweeness.gpkg",
         delete_dsn = TRUE, quiet = TRUE)
message("  Saved: spatial/stream_network_graphs/stream_betweeness.gpkg")

# ============================================================
# QUICK MAP (ggplot) -- betweeness along the network
# ============================================================
message("\nDrawing centrality map...")

p <- ggplot(streams_bc) +
  geom_sf(aes(colour = betweeness, linewidth = betweeness)) +
  scale_colour_viridis_c(option = "magma", name = "betweenness") +
  scale_linewidth_continuous(range = c(0.2, 1.5), guide = "none") +
  theme_minimal() +
  labs(title = "Betweenness centrality of the Sarantaporos network")

png("connectivity/centrality_map.png", width = 1800, height = 1600, res = 200)
print(p)
dev.off()
message("  Saved: connectivity/centrality_map.png")

# ============================================================
# NETWORK STRUCTURE SUMMARY (descriptive metrics for Module 4 text)
# ============================================================
# Reports the numbers used in the Module 4 methods paragraph: total and
# mean/median reach length, Strahler composition, branching structure
# (source nodes / confluences / outlet), longest along-network path, and
# drainage density. All computed on the pruned Sarantaporos graph.
message("\n=== Network structure summary ===")

# --- reach length: from NODE attribute (each node = one reach) ---
reach_len_m <- V(river_graph)$length_reach
# drop the root placeholder length (generator sets node "0" length_reach = 1)
reach_len_m <- reach_len_m[!is.na(reach_len_m) & reach_len_m > 1]

total_km <- sum(reach_len_m) / 1000

message("  Reaches:            ", vcount(river_graph))
message("  Total length:       ", round(total_km, 1), " km")
message("  Mean reach length:  ", round(mean(reach_len_m)), " m")
message("  Median reach length:", round(median(reach_len_m)), " m")

# --- branching structure ---
indeg  <- degree(river_graph, mode = "in")
outdeg <- degree(river_graph, mode = "out")
n_head <- sum(indeg == 0)          # source nodes (pruned-network leaves)
n_conf <- sum(indeg >= 2)          # confluences (junctions)
n_out  <- sum(outdeg == 0)         # outlet(s)
message("  Source nodes:       ", n_head)
message("  Confluences:        ", n_conf)
message("  Outlet nodes:       ", n_out)

# --- longest along-network path (network diameter, km) ---
# Edge weight = length of the edge's SOURCE (upstream) node, since length
# is stored on nodes. This makes weights length == ecount(graph).
el         <- as_edgelist(river_graph)                 # from, to (names)
edge_w_m   <- V(river_graph)$length_reach[match(el[, 1], V(river_graph)$name)]
edge_w_m[is.na(edge_w_m)] <- 0                         # guard NA (e.g. root)

stopifnot(length(edge_w_m) == ecount(river_graph))     # 632

d_mat      <- distances(river_graph, mode = "all", weights = edge_w_m)
longest_km <- max(d_mat[is.finite(d_mat)], na.rm = TRUE) / 1000
message("  Longest path:       ", round(longest_km, 1), " km")

# --- Strahler composition (if strahler attribute present) ---
if ("strahler" %in% vertex_attr_names(river_graph)) {
  strahler_tab <- igraph::as_data_frame(river_graph, "vertices") %>%
    mutate(length_km = length_reach / 1000) %>%
    filter(!is.na(length_reach) & length_reach > 1) %>%   # drop root placeholder
    group_by(strahler) %>%
    summarise(n_reaches = n(),
              length_km = round(sum(length_km, na.rm = TRUE), 1),
              .groups = "drop") %>%
    arrange(strahler)
  message("  Strahler composition:")
  print(strahler_tab)
}

# --- drainage density (pruned network) ---
subbasin_poly <- st_read("spatial/subbasin_sarantaporos/subbasin_polygon.gpkg",
                         quiet = TRUE)
area_km2 <- as.numeric(sum(st_area(subbasin_poly))) / 1e6
message("  Basin area:         ", round(area_km2, 1), " km2")
message("  Drainage density:   ", round(total_km / area_km2, 2),
        " km/km2 (pruned network)")

# --- save a one-row summary for the manuscript ---
network_summary <- data.frame(
  n_reaches         = vcount(river_graph),
  total_length_km   = round(total_km, 1),
  mean_reach_m      = round(mean(reach_len_m)),
  median_reach_m    = round(median(reach_len_m)),
  n_source_nodes    = n_head,
  n_confluences     = n_conf,
  n_outlets         = n_out,
  longest_path_km   = round(longest_km, 1),
  basin_area_km2    = round(area_km2, 1),
  drainage_density  = round(total_km / area_km2, 2)
)
fwrite(network_summary, "connectivity/network_summary.csv")
message("  Saved: connectivity/network_summary.csv")

message("\nDone.")
