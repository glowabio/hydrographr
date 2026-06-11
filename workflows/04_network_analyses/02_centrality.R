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
# INPUT:
#   - spatial/stream_networks/river_graph_current.RDS
#       (any scenario graph works -- centrality uses topology only, not dams;
#        we use the current graph, which carries the same 642 reaches)
#   - spatial/subbasin_sarantaporos/stream_network_pruned.gpkg
#       (for mapping centrality back onto reaches)
#
# OUTPUT:
#   - connectivity/centrality_table.csv
#   - spatial/stream_networks/stream_betweeness.gpkg   (stream network + bc)
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
river_graph <- readRDS("spatial/stream_networks/river_graph_current.RDS")

# Drop the artificial root node "0" so it does not distort centrality
if ("0" %in% V(river_graph)$name) {
  river_graph <- delete_vertices(river_graph, "0")
}

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
         "spatial/stream_networks/stream_betweeness.gpkg",
         delete_dsn = TRUE, quiet = TRUE)
message("  Saved: spatial/stream_networks/stream_betweeness.gpkg")

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

message("\nDone.")
