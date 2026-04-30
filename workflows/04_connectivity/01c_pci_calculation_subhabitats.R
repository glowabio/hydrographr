#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03_pci_calculation.R
# Calculate PCI for current vs future dam scenarios + Fragmentation Index
# ADAPTED FOR SPECIES HABITAT SUBGRAPHS (occurrence-based, not basin-based)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(riverconn)
library(data.table)
library(dplyr)
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/pci_sparse.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/05_connectivity/get_subgraph_between_points.R")

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select
rename <- dplyr::rename
group_by <- dplyr::group_by

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================
MIN_SUBCATCHMENTS <- 2   # species must occupy >= this many subcatchments
UPSTREAM_BUFFER   <- 3   # reaches to extend upstream from occurrence points

# ============================================================
# READ INPUTS
# ============================================================

river_graph_current <- readRDS("spatial/stream_networks/river_graph_current.RDS")
river_graph_future  <- readRDS("spatial/stream_networks/river_graph_future.RDS")

# Convert length_reach from meters to tens of km
V(river_graph_current)$length_reach <- V(river_graph_current)$length_reach / 10000
V(river_graph_future)$length_reach  <- V(river_graph_future)$length_reach  / 10000

occurrences    <- fread("points_snapped/fish/fish_all_species_snapped.csv")
fish_dis_class <- fread("traits/fish_dis_class.txt") %>%
  filter(!is.na(dispersal_prob))

# ============================================================
# EXTRACT BASIN_ID FROM GRAPH
# (kept for output labelling — still useful to know which basin
#  a species' subgraph falls in)
# ============================================================
node_basins <- data.frame(
  name     = V(river_graph_current)$name,
  basin_id = V(river_graph_current)$basin_id
) %>%
  filter(!is.na(basin_id))

message("Network has ", length(unique(node_basins$basin_id)), " basins")

# ============================================================
# PREPARE SPECIES LIST
# ============================================================
fish_distribution <- occurrences %>%
  select(species, subc_id) %>%
  distinct() %>%
  group_by(species) %>%
  filter(n() >= MIN_SUBCATCHMENTS) %>%
  ungroup()

fish_species <- fish_distribution %>%
  filter(species %in% fish_dis_class$species) %>%
  pull(species) %>%
  unique()

message(length(fish_species), " species will be processed")

# ============================================================
# HELPER: Attach species presence weights to a graph
# ============================================================
attach_presence <- function(g, presence_df) {
  graph_from_data_frame(
    as_data_frame(g, "edges"),
    vertices = as_data_frame(g, "vertices") %>%
      select(-weight) %>%
      left_join(presence_df, by = "name") %>%
      mutate(weight = ifelse(is.na(weight), 0, weight))
  )
}

# ============================================================
# PRE-COMPUTE SPECIES HABITAT SUBGRAPH NODE SETS
# (topology from river_graph_current; same nodes used for future)
# ============================================================
message("Pre-computing species habitat subgraphs...")

species_habitat_nodes <- lapply(fish_species, function(sp) {
  sp_reach_ids <- fish_distribution %>%
    filter(species == sp) %>%
    pull(subc_id)

  sg <- get_subgraph_between_points(
    graph             = river_graph_current,
    species_reach_ids = sp_reach_ids,
    upstream_buffer   = UPSTREAM_BUFFER
  )

  if (is.null(sg)) return(NULL)
  igraph::V(sg)$name
})
names(species_habitat_nodes) <- fish_species

n_null <- sum(sapply(species_habitat_nodes, is.null))
message("  Subgraphs built: ", length(fish_species) - n_null,
        " | Failed (NULL): ", n_null)

# ============================================================
# PROCESS ALL SPECIES
# ============================================================

catchment_pci_list <- list()
start_time <- Sys.time()

for (name_loop in fish_species) {

  habitat_nodes <- species_habitat_nodes[[name_loop]]
  if (is.null(habitat_nodes)) next

  # Species presence (node weights)
  subcatchments_presence <- fish_distribution %>%
    filter(species == name_loop) %>%
    mutate(name = as.character(subc_id)) %>%
    select(name) %>%
    distinct() %>%
    mutate(weight = 1)

  # Label output with basin_id(s) — informational only
  # A species may span multiple basins; collapse to string
  species_basins <- node_basins %>%
    filter(name %in% subcatchments_presence$name) %>%
    pull(basin_id) %>%
    unique()

  if (length(species_basins) == 0) next

  # Extract habitat subgraphs for both scenarios from same node set
  habitat_graph_current <- induced_subgraph(
    river_graph_current,
    V(river_graph_current)[V(river_graph_current)$name %in% habitat_nodes]
  )
  habitat_graph_future <- induced_subgraph(
    river_graph_future,
    V(river_graph_future)[V(river_graph_future)$name %in% habitat_nodes]
  )

  # Attach presence weights
  graph_current <- attach_presence(habitat_graph_current, subcatchments_presence)
  graph_future  <- attach_presence(habitat_graph_future,  subcatchments_presence)

  # Calculate PCI — CURRENT scenario
  pci_current <- pci_sparse(
    graph_current,
    weight     = "weight",
    field_B    = "length_reach",
    param      = fish_dis_class %>% filter(species_name == name_loop) %>% pull(dispersal_prob),
    index_type = "full"
  ) %>%
    mutate(species  = name_loop,
           basin_id = paste(species_basins, collapse = ";"),
           scenario = "current")

  # Calculate PCI — FUTURE scenario
  pci_future <- pci_sparse(
    graph_future,
    weight     = "weight",
    field_B    = "length_reach",
    param      = fish_dis_class %>% filter(species_name == name_loop) %>% pull(dispersal_prob),
    index_type = "full"
  ) %>%
    mutate(species  = name_loop,
           basin_id = paste(species_basins, collapse = ";"),
           scenario = "future")

  # Fragmentation Index
  fi_val <- (pci_current$index - pci_future$index) / pci_current$index * 100

  catchment_pci_list[[name_loop]] <- bind_rows(pci_current, pci_future) %>%
    mutate(FI = ifelse(scenario == "future", fi_val, NA))

  # Cleanup
  rm(habitat_graph_current, habitat_graph_future, graph_current, graph_future,
     pci_current, pci_future)

  # Progress
  sp_idx <- which(fish_species == name_loop)
  if (sp_idx %% 10 == 0) {
    message("Processed ", sp_idx, " / ", length(fish_species), " species")
    gc(verbose = FALSE)
  }

} # End species loop

end_time <- Sys.time()
elapsed_time <- end_time - start_time
message("Processing complete!")
message("Time elapsed: ", round(elapsed_time, 2), " ", units(elapsed_time))

# ============================================================
# SAVE RESULTS
# ============================================================
catchment_pci_full <- do.call(rbind, catchment_pci_list)

dir.create("connectivity/pci", recursive = TRUE, showWarnings = FALSE)
saveRDS(catchment_pci_full, "connectivity/pci/catchment_pci_full_subhabitats.RDS")

# ============================================================
# SUMMARY TABLE
# ============================================================
fi_summary <- catchment_pci_full %>%
  pivot_wider(
    id_cols     = c(species, basin_id),
    names_from  = scenario,
    values_from = c(index, FI)
  ) %>%
  rename(
    PCI_current = index_current,
    PCI_future  = index_future,
    FI          = FI_future
  ) %>%
  select(species, basin_id, PCI_current, PCI_future, FI) %>%
  arrange(desc(FI))

write.table(fi_summary, "connectivity/pci/fi_summary_subhabitats.txt",
            row.names = FALSE, quote = FALSE, sep = "\t")

message("\nDone! ", nrow(fi_summary), " species processed")

# ============================================================
# QUICK DIAGNOSTICS
# ============================================================
cat("\n========== RESULTS SUMMARY ==========\n")

cat("\nPCI by scenario:\n")
catchment_pci_full %>%
  group_by(scenario) %>%
  summarize(
    n          = n(),
    min_pci    = round(min(index, na.rm = TRUE), 3),
    median_pci = round(median(index, na.rm = TRUE), 3),
    max_pci    = round(max(index, na.rm = TRUE), 3),
    .groups    = "drop"
  ) %>% print()

cat("\nFragmentation Index (connectivity loss from planned dams):\n")
fi_summary %>%
  summarize(
    n_total      = n(),
    n_FI_zero    = sum(FI == 0,  na.rm = TRUE),
    n_FI_above0  = sum(FI > 0,   na.rm = TRUE),
    n_FI_above10 = sum(FI > 10,  na.rm = TRUE),
    n_FI_above25 = sum(FI > 25,  na.rm = TRUE),
    median_FI    = round(median(FI, na.rm = TRUE), 1),
    max_FI       = round(max(FI,    na.rm = TRUE), 1)
  ) %>% print()

cat("\nTop 10 most impacted species:\n")
fi_summary %>% head(10) %>% print()

# ============================================================
# CHECK VJOSA
# ============================================================
cat("\n========== VJOSA BASIN (1292502) ==========\n")
fi_summary %>% filter(grepl("1292502", basin_id)) %>% print()
