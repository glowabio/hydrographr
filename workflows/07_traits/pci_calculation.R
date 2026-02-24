#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03 -- pci_calculation.R
# Calculate PCI (open + fragmented) and FI per species
# ADAPTED FOR MULTIPLE DISCONNECTED BASINS
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(riverconn)
library(data.table)
library(dplyr)
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/07_traits/index_calculation_efficient.R")

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select
rename <- dplyr::rename

# Set working directory
wdir <- "/run/user/1000/gvfs/dav:host=nimbus.igb-berlin.de,ssl=true,user=grigoropoulou,prefix=%2Fremote.php%2Fwebdav/workflow_paper/data"
setwd(wdir)

# ============================================================
# PARAMETERS
# ============================================================
MIN_SUBCATCHMENTS <- 2   # species must occupy >= this many subcatchments


# ============================================================
# READ INPUTS
# ============================================================

river_graph <- readRDS("spatial/stream_networks/river_graph.RDS")

occurrences <- fread("points_snapped/fish/fish_all_species_snapped.csv") %>%
  filter(source == "HCMR")

fish_dis_class <- fread("traits/fish_dis_class.txt") %>%
  filter(!is.na(dispersal_prob))

# ============================================================
# EXTRACT BASIN_ID FROM GRAPH
# ============================================================
node_basins <- data.frame(
  name = V(river_graph)$name,
  basin_id = V(river_graph)$basin_id
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
# PROCESS ALL SPECIES (no batching needed with efficient function!)
# ============================================================

catchment_pci_list    <- list()
subcatchment_pci_list <- list()
start_time <- Sys.time()

for (name_loop in fish_species) {

  # Species presence
  subcatchments_presence <- fish_distribution %>%
    filter(species == name_loop) %>%
    mutate(name = as.character(subc_id)) %>%
    select(name) %>%
    distinct()

  # Which basins?
  species_basins <- node_basins %>%
    filter(name %in% subcatchments_presence$name) %>%
    pull(basin_id) %>%
    unique()

  if (length(species_basins) == 0) next

  # Dispersal parameter
  dispersal_exp <- fish_dis_class %>%
    filter(species == name_loop) %>%
    pull(dispersal_prob)

  # Loop over basins
  for (basin_loop in species_basins) {

    # Extract basin subgraph
    basin_nodes <- node_basins %>%
      filter(basin_id == basin_loop) %>%
      pull(name)

    basin_graph <- induced_subgraph(river_graph, basin_nodes)

    # Species presence in this basin
    basin_presence <- subcatchments_presence %>%
      filter(name %in% basin_nodes) %>%
      mutate(weight = 1)

    if (nrow(basin_presence) < MIN_SUBCATCHMENTS) {
      rm(basin_graph)
      next
    }

    # Attach presence to graph
    attach_presence <- function(g) {
      graph_from_data_frame(
        as_data_frame(g, "edges"),
        v = as_data_frame(g, "vertices") %>%
          select(-weight) %>%
          left_join(basin_presence, by = "name") %>%
          mutate(weight = ifelse(is.na(weight), 0, weight))
      )
    }

    # Create open version
    basin_graph_open <- basin_graph
    E(basin_graph_open)$pass_u <- 1
    E(basin_graph_open)$pass_d <- 1

    graph_loop      <- attach_presence(basin_graph)
    graph_loop_open <- attach_presence(basin_graph_open)

    # Calculate PCI with EFFICIENT function
    pci_f <- index_calculation_efficient(
      graph_loop,
      weight   = "weight",
      field_B  = "length_reach",
      param    = dispersal_exp,
      index_type = "full"
    ) %>%
      mutate(species = name_loop,
             basin_id = basin_loop,
             scenario = "fragmented")

    pci_o <- index_calculation_efficient(
      graph_loop_open,
      weight   = "weight",
      field_B  = "length_reach",
      param    = dispersal_exp,
      index_type = "full"
    ) %>%
      mutate(species = name_loop,
             basin_id = basin_loop,
             scenario = "open")

    # Fragmentation Index
    fi_val <- (pci_o$index - pci_f$index) / pci_o$index * 100

    result_key <- paste0(name_loop, "_basin", basin_loop)

    catchment_pci_list[[result_key]] <- bind_rows(pci_f, pci_o) %>%
      mutate(FI = ifelse(scenario == "fragmented", fi_val, NA))

    # Subcatchment-level (optional - comment out if not needed)
    subcatchment_pci_list[[result_key]] <- index_calculation_efficient(
      graph_loop,
      weight     = "weight",
      field_B    = "length_reach",
      param      = dispersal_exp,
      index_type = "reach",
      index_mode = "to"
    ) %>%
      mutate(species = name_loop, basin_id = basin_loop)

    # Cleanup
    rm(basin_graph, basin_graph_open, graph_loop, graph_loop_open,
       pci_f, pci_o, basin_presence)

  } # End basin loop

  # Progress
  if (which(fish_species == name_loop) %% 10 == 0) {
    message("Processed ", which(fish_species == name_loop), " / ",
            length(fish_species), " species")
    gc(verbose = FALSE)
  }

} # End species loop

end_time <- Sys.time()
elapsed_time <- end_time - start_time

message("Parallel processing complete!")
message("Time elapsed: ", round(elapsed_time, 2), " ", units(elapsed_time))



# ============================================================
# SAVE RESULTS
# ============================================================

catchment_pci_full    <- do.call(rbind, catchment_pci_list)
subcatchment_pci_full <- do.call(rbind, subcatchment_pci_list)

saveRDS(catchment_pci_full,    "catchment_pci_full.RDS")
saveRDS(subcatchment_pci_full, "subcatchment_pci_full.RDS")

fi_summary <- catchment_pci_full %>%
  filter(scenario == "fragmented") %>%
  select(species, basin_id, index, FI) %>%
  rename(PCI_fragmented = index)

write.table(fi_summary, "fi_summary.txt", row.names = FALSE, quote = FALSE)

message("\nDone! ", nrow(fi_summary), " species × basin combinations")
