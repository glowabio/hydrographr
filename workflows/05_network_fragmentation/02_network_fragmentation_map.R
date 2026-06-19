#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_network_fragmentation_map.R   (Module 5 -- Network Fragmentation)
#
# Two-panel map: the Sarantaporos network under the current vs. future
# dam scenario, with reaches coloured by structural fragment membership.
# Species-free. Visualises the "isolate rather than destroy" result:
# one near-continuous network (current) shattered into many short
# fragments (future).
#
# INPUT:
#   - spatial/stream_network_graphs/river_graph_current.RDS
#   - spatial/stream_network_graphs/river_graph_future.RDS
#   - spatial/subbasin_sarantaporos/stream_network_pruned.gpkg
#   - points_snapped/dams/dams_snapped_points.csv   (to mark dam locations)
#
# OUTPUT:
#   - figures/fragmentation_network_map.png
#
# LOCATION: workflows/05_network_fragmentation/02_network_fragmentation_map.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(sf)
library(data.table)
library(patchwork)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("figures", showWarnings = FALSE)

# ============================================================
# LOAD
# ============================================================
river_graph_current <- readRDS("spatial/stream_network_graphs/river_graph_current.RDS")
river_graph_future  <- readRDS("spatial/stream_network_graphs/river_graph_future.RDS")

streams <- st_read("spatial/subbasin_sarantaporos/stream_network_pruned.gpkg",
                   quiet = TRUE) %>%
  mutate(subc_id = as.character(subc_id))

dams <- fread("points_snapped/dams/dams_snapped_points.csv") %>%
  mutate(subc_id = as.character(subc_id)) %>%
  filter(subc_id %in% streams$subc_id)

dams_sf <- st_as_sf(dams,
                    coords = c("longitude_snapped", "latitude_snapped"),
                    crs = st_crs(streams))

# ============================================================
# FRAGMENT MEMBERSHIP PER SCENARIO
# ============================================================
# Cut at barrier edges, weak components = fragments. Return a subc_id ->
# fragment lookup, with fragments ordered by length so colours are stable.
fragment_membership <- function(g) {
  g_cut <- delete_edges(g, which(E(g)$barrier))
  comp  <- components(g_cut, mode = "weak")

  tibble(
    subc_id      = V(g_cut)$name,
    fragment     = comp$membership,
    length_reach = V(g_cut)$length_reach
  ) %>%
    filter(subc_id != "0") %>%
    group_by(fragment) %>%
    mutate(frag_length = sum(length_reach, na.rm = TRUE)) %>%
    ungroup() %>%
    # relabel fragments largest -> smallest for a stable colour order
    mutate(fragment = dense_rank(desc(frag_length))) %>%
    select(subc_id, fragment)
}

memb_current <- fragment_membership(river_graph_current)
memb_future  <- fragment_membership(river_graph_future)

streams_cur <- streams %>% left_join(memb_current, by = "subc_id")
streams_fut <- streams %>% left_join(memb_future,  by = "subc_id")

n_cur <- max(memb_current$fragment, na.rm = TRUE)
n_fut <- max(memb_future$fragment,  na.rm = TRUE)

# ============================================================
# PLOT
# ============================================================
# Cycle a qualitative palette across fragments. With many fragments the
# legend is uninformative, so we drop it; the eye reads "few large blocks"
# vs "many small pieces", which is the point.
pal <- rep(RColorBrewer::brewer.pal(12, "Paired"), length.out = max(n_cur, n_fut))

make_panel <- function(net_sf, n_frag, title_label) {
  ggplot() +
    geom_sf(data = net_sf,
            aes(colour = factor(fragment)),
            linewidth = 0.6) +
    scale_colour_manual(values = pal, na.value = "grey80", guide = "none") +
    labs(title = title_label,
         subtitle = paste0(n_frag, " free-flowing fragment",
                           ifelse(n_frag == 1, "", "s"))) +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 10))
}

p_cur <- make_panel(streams_cur, n_cur, "Current") +
  geom_sf(data = dams_sf %>% filter(status == "existing"),
          shape = 24,
          fill = "#E41A1C", colour = "white", size = 2.5, stroke = 0.5)

p_fut <- make_panel(streams_fut, n_fut, "Future") +
  geom_sf(data = dams_sf,shape = 24,
          fill = "#E41A1C", colour = "white", size = 2.5, stroke = 0.5)

combined <- p_cur + p_fut +
  plot_annotation(
    title = "Structural fragmentation of the Sarantaporos network",
    theme = theme(plot.title = element_text(face = "bold", size = 14)))

png("figures/fragmentation_network_map.png",
    width = 2400, height = 1400, res = 200)
print(combined)
dev.off()

message("Saved: figures/fragmentation_network_map.png")
message("  Current: ", n_cur, " fragments | Future: ", n_fut, " fragments")
