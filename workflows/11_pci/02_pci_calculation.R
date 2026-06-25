#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_pci_calculation.R
#
# Population Connectivity Index (PCI; Baldan et al. 2022) for the seven
# Sarantaporos fish species, under the current (single existing dam) and
# future (existing + planned dams) scenarios, and the resulting
# Fragmentation Index (FI = percent connectivity loss).
#
# SCOPE
#   Single basin (Sarantaporos), whole pruned network as the extent for
#   every species (632 reaches). Each species' occurrences are laid over
#   the full network as binary presence weights; PCI integrates over all
#   occupied-pair paths. No per-species subgraph extraction, no cross-basin
#   loop.
#
# PASSABILITY (per-species, applied here at PCI time)
#   The scenario graphs (Module: network graph) are species-agnostic and
#   carry only the dam count per edge (n_shp). Here we convert that to a
#   species-specific edge passability:
#       pass        = species_passability ^ n_shp        (non-barrier: n_shp = 0 -> 1)
#       pass_u = pass_d = sqrt(pass)                      (symmetric split)
#   so that pci_sparse's per-edge term pass_u * pass_d = pass, and the
#   path passability c_ij is the cumulative product of per-reach values.
#   Species-specific passabilities (0.8 / 0.5 / 0.0) come from expert
#   knowledge of movement ecology and live in species_passability.csv.
#   Blocked species (passability 0) are fully severed at any barriered
#   reach (0 ^ n_shp = 0), which can drive FI to 100% if planned dams
#   isolate all occurrences from one another, this is intended.
#
# DISPERSAL
#   Dispersal probability per species (the B_ij kernel base) comes from
#   01_dispersal_estimation.R (traits/fish_dis_class.txt): three migration
#   tiers mapped to 0.3 / 0.6 / 0.9. Reach lengths are converted to tens of
#   km so that dispersal_prob ^ distance is well scaled.
#
# REQUIRES
#   tidyverse, igraph, data.table
#   helpers/pci_sparse.R
#
# INPUT
#   spatial/stream_network_graphs/river_graph_current.RDS   (carries n_shp)
#   spatial/stream_network_graphs/river_graph_future.RDS    (carries n_shp)
#   points_snapped/fish/fish_all_species_snapped.csv         (species, subc_id)
#   traits/fish_dis_class.txt                                (dispersal_prob)
#   traits/species_passability.csv                           (passability)
#
# OUTPUT
#   connectivity/pci/pci_full.RDS        (per-species PCI, both scenarios)
#   connectivity/pci/fi_summary.txt      (7 rows: species, PCI_cur, PCI_fut, FI)
#
# LOCATION
#   workflows/<connectivity_module>/02_pci_calculation.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(data.table)
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/pci_sparse.R")

# ============================================================
# FIX: prevent MASS::select etc. from masking dplyr verbs
# ============================================================
select   <- dplyr::select
rename   <- dplyr::rename
group_by <- dplyr::group_by

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================
MIN_SUBCATCHMENTS <- 2        # a species needs >= 2 occupied reaches for PCI
LENGTH_DIVISOR    <- 10000    # meters -> tens of km (matches dispersal scaling)

# ============================================================
# READ INPUTS
# ============================================================
river_graph_current <- readRDS("spatial/stream_network_graphs/river_graph_current.RDS")
river_graph_future  <- readRDS("spatial/stream_network_graphs/river_graph_future.RDS")

# Reach length: meters -> tens of km
V(river_graph_current)$length_reach <- V(river_graph_current)$length_reach / LENGTH_DIVISOR
V(river_graph_future)$length_reach  <- V(river_graph_future)$length_reach  / LENGTH_DIVISOR

occurrences <- fread("points_snapped/fish/fish_all_species_snapped.csv") %>%
  dplyr::mutate(species = gsub(" ", "_", species))

fish_dis_class <- fread("traits/fish_dis_class.txt") %>%
  dplyr::filter(!is.na(dispersal_prob))

passability <- fread("traits/species_passability.csv")

# ============================================================
# SPECIES LIST: occupy >= MIN_SUBCATCHMENTS, with dispersal + passability
# ============================================================
fish_distribution <- occurrences %>%
  dplyr::select(species, subc_id) %>%
  dplyr::distinct() %>%
  group_by(species) %>%
  dplyr::filter(n() >= MIN_SUBCATCHMENTS) %>%
  ungroup()

fish_species <- fish_distribution %>%
  dplyr::filter(species %in% fish_dis_class$species,
                species %in% passability$species) %>%
  dplyr::pull(species) %>%
  unique() %>%
  sort()

message(length(fish_species), " species will be processed:")
print(fish_species)

# ============================================================
# HELPER: write per-species passability onto the graph edges
#   pass        = species_passability ^ n_shp
#   pass_u = pass_d = sqrt(pass)  (symmetric; product = pass)
# ============================================================
set_passability <- function(g, species_pass) {
  n_shp <- igraph::edge_attr(g, "n_shp")
  n_shp <- ifelse(is.na(n_shp), 0, n_shp)

  pass <- species_pass ^ n_shp          # n_shp = 0 -> 1 (fully passable)
  pass <- ifelse(n_shp == 0, 1, pass)   # explicit, guards 0^0 edge cases

  E(g)$pass_u <- sqrt(pass)
  E(g)$pass_d <- sqrt(pass)
  g
}

# ============================================================
# HELPER: attach binary presence weights to graph vertices
# ============================================================
attach_presence <- function(g, presence_df) {
  graph_from_data_frame(
    as_data_frame(g, "edges"),
    vertices = as_data_frame(g, "vertices") %>%
      dplyr::select(-dplyr::any_of("weight")) %>%
      dplyr::left_join(presence_df, by = "name") %>%
      dplyr::mutate(weight = ifelse(is.na(weight), 0, weight))
  )
}

# ============================================================
# PROCESS ALL SPECIES
# ============================================================
pci_list   <- list()
start_time <- Sys.time()

for (sp in fish_species) {

  # dispersal probability (B_ij base) and expert passability
  disp_prob <- fish_dis_class %>% dplyr::filter(species == sp) %>% dplyr::pull(dispersal_prob)
  sp_pass   <- passability    %>% dplyr::filter(species == sp) %>% dplyr::pull(passability)

  # binary presence over the WHOLE network
  presence <- fish_distribution %>%
    dplyr::filter(species == sp) %>%
    dplyr::mutate(name = as.character(subc_id)) %>%
    dplyr::select(name) %>%
    dplyr::distinct() %>%
    dplyr::mutate(weight = 1)

  # write species passability onto both scenario graphs
  g_cur <- set_passability(river_graph_current, sp_pass)
  g_fut <- set_passability(river_graph_future,  sp_pass)

  # attach presence
  g_cur <- attach_presence(g_cur, presence)
  g_fut <- attach_presence(g_fut, presence)

  # PCI: current and future
  pci_cur <- pci_sparse(g_cur, weight = "weight", field_B = "length_reach",
                        param = disp_prob, index_type = "full") %>%
    dplyr::mutate(species = sp, scenario = "current")

  pci_fut <- pci_sparse(g_fut, weight = "weight", field_B = "length_reach",
                        param = disp_prob, index_type = "full") %>%
    dplyr::mutate(species = sp, scenario = "future")

  # Fragmentation Index (guard PCI_current == 0)
  fi_val <- if (pci_cur$index > 0) {
    (pci_cur$index - pci_fut$index) / pci_cur$index * 100
  } else NA_real_

  pci_list[[sp]] <- dplyr::bind_rows(pci_cur, pci_fut) %>%
    dplyr::mutate(FI = ifelse(scenario == "future", fi_val, NA_real_))

  rm(g_cur, g_fut, pci_cur, pci_fut)
}

elapsed <- Sys.time() - start_time
message("Done in ", round(elapsed, 2), " ", units(elapsed))

# ============================================================
# SAVE
# ============================================================
pci_full <- do.call(rbind, pci_list)

dir.create("connectivity/pci", recursive = TRUE, showWarnings = FALSE)
saveRDS(pci_full, "connectivity/pci/pci_full.RDS")

fi_summary <- pci_full %>%
  pivot_wider(id_cols = species, names_from = scenario,
              values_from = c(index, FI)) %>%
  dplyr::rename(PCI_current = index_current,
                PCI_future  = index_future,
                FI          = FI_future) %>%
  dplyr::select(species, PCI_current, PCI_future, FI) %>%
  dplyr::arrange(desc(FI))

fwrite(fi_summary, "connectivity/pci/fi_summary.txt", sep = "\t")

# ============================================================
# DIAGNOSTICS
# ============================================================
cat("\n========== PCI / FI SUMMARY ==========\n")
as.data.frame(fi_summary) %>% print()

cat("\nFI distribution:\n")
fi_summary %>%
  dplyr::summarise(
    n            = dplyr::n(),
    n_FI_zero    = sum(FI == 0, na.rm = TRUE),
    n_FI_above0  = sum(FI > 0,  na.rm = TRUE),
    median_FI    = round(median(FI, na.rm = TRUE), 1),
    max_FI       = round(max(FI,    na.rm = TRUE), 1)
  ) %>% as.data.frame() %>% print()
