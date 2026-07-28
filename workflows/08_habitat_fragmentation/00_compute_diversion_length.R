#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 00_compute_diversion_length.R   (Module 8 -- buffer justification)
#
# Quantify the diverted (dewatered / bypassed) reach length of the small
# hydropower plants, to justify the downstream dam-impact buffer radius.
#
# Each RAE licence ('aa') has a dam/intake point (expert type DAM) and a
# powerhouse point (FACTORY). Water is diverted from the intake and
# returned at the powerhouse, so the along-network reach between them is
# the dewatered channel. Both point sets are snapped to the stream network
# with the same cascade (Module 3); here we pair them by 'aa' and measure
# the directed downstream distance dam -> factory on the network graph.
#
# INPUT:
#   points_snapped/dams/dams_snapped_points.csv        (subc_id, aa, type)
#   points_snapped/dams/factories_snapped_points.csv   (subc_id, aa)
#   spatial/stream_network_graphs/river_graph_current.RDS
#
# OUTPUT:
#   points_cleaned/dams/diversion_lengths.csv
#   + printed mean/median/range (the number for the manuscript)
#
# LOCATION: workflows/08_habitat_fragmentation/00_compute_diversion_length.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(data.table)

select <- dplyr::select

if (!exists("WORKFLOWS_DIR"))
  WORKFLOWS_DIR <- Sys.getenv("WORKFLOWS_CODE", "/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows")
source(file.path(WORKFLOWS_DIR, "helpers", "config.R"))
setwd(BASE_DIR)

# ============================================================
# STEP 1: Load snapped dams + factories, pair by licence 'aa'
# ============================================================

dams <- fread("points_snapped/dams/dams_snapped_points.csv") %>%
  filter(type == "DAM") %>%
  select(aa, site_id_dam = site_id, subc_dam = subc_id) %>%
  mutate(subc_dam = as.character(subc_dam))

facs <- fread("points_snapped/dams/factories_snapped_points.csv") %>%
  select(aa, site_id_fac = site_id, subc_fac = subc_id) %>%
  mutate(subc_fac = as.character(subc_fac))

pairs <- inner_join(dams, facs, by = "aa", relationship = "many-to-many")
message("Licences with dam+factory: ", n_distinct(pairs$aa),
        " | candidate pairs: ", nrow(pairs))

# ============================================================
# STEP 2: Load network graph, build edge weights (metres)
# ============================================================

g <- readRDS("spatial/stream_network_graphs/river_graph_current.RDS")
if ("0" %in% V(g)$name) g <- delete_vertices(g, "0")

# length is a VERTEX attribute; edge weight = source-node reach length
el     <- as_edgelist(g)
E(g)$w <- V(g)$length_reach[match(el[, 1], V(g)$name)]
E(g)$w[is.na(E(g)$w)] <- 0

# ============================================================
# STEP 3: Directed downstream distance dam -> factory (metres)
# ============================================================
# Factory is downstream of the intake, so take the directed path
# (mode = "out"). Inf => factory not downstream of that dam -> flag,
# exclude from the average.

dist_dam_fac <- function(a, b) {
  if (!(a %in% V(g)$name) || !(b %in% V(g)$name)) return(NA_real_)
  as.numeric(distances(g, v = a, to = b, mode = "out", weights = E(g)$w))
}

pairs <- pairs %>%
  mutate(diversion_m = map2_dbl(subc_dam, subc_fac, dist_dam_fac))

pairs_valid <- pairs %>%
  filter(is.finite(diversion_m)) %>%
  group_by(aa) %>%
  summarise(diversion_m = min(diversion_m), .groups = "drop")

n_no_ds <- pairs %>%
  group_by(aa, site_id_dam) %>%
  summarise(any_ds = any(is.finite(diversion_m)), .groups = "drop") %>%
  filter(!any_ds) %>% nrow()

if (n_no_ds > 0)
  message("  WARNING: ", n_no_ds,
          " dam(s) have no factory reachable downstream (check snapping/pairing)")

# ============================================================
# STEP 4: Report -- the number for the manuscript
# ============================================================

cat("\n=== Diverted (dewatered) reach length: dam -> powerhouse ===\n")
cat("  Plants (dams) with a downstream factory: ", nrow(pairs_valid), "\n")
cat("  Mean:   ", round(mean(pairs_valid$diversion_m)), " m  (",
    round(mean(pairs_valid$diversion_m) / 1000, 2), " km)\n", sep = "")
cat("  Median: ", round(median(pairs_valid$diversion_m)), " m\n")
cat("  Range:  ", round(min(pairs_valid$diversion_m)), " - ",
    round(max(pairs_valid$diversion_m)), " m\n")

fwrite(pairs_valid, "points_cleaned/dams/diversion_lengths.csv")
message("\nSaved: points_cleaned/dams/diversion_lengths.csv")
