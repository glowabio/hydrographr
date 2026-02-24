#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03 -- pci_calculation.R (PARALLELIZED VERSION)
# Calculate PCI (open + fragmented) and FI per species
# ADAPTED FOR MULTIPLE DISCONNECTED BASINS WITH PARALLEL PROCESSING
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(riverconn)
library(data.table)
library(parallel)
library(foreach)
library(doParallel)

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select
rename <- dplyr::rename

# Set working directory
wdir <- "/run/user/1000/gvfs/dav:host=nimbus.igb-berlin.de,ssl=true,user=grigoropoulou,prefix=%2Fremote.php%2Fwebdav/workflow_paper/data"
setwd(wdir)

# Load efficient function
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/07_traits/index_calculation_efficient.R")

# ============================================================
# PARAMETERS
# ============================================================
MIN_SUBCATCHMENTS <- 2
N_CORES <- detectCores() - 1  # Leave one core free for system

message("Using ", N_CORES, " cores for parallel processing")

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
# BUILD SPECIES × BASIN TASK LIST
# ============================================================

# Create all species × basin combinations that need processing
task_list <- list()

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

  # Add each basin as a task
  for (basin_loop in species_basins) {

    basin_presence <- subcatchments_presence %>%
      filter(name %in% node_basins$name[node_basins$basin_id == basin_loop])

    # Skip if insufficient presence
    if (nrow(basin_presence) < MIN_SUBCATCHMENTS) next

    task_list[[length(task_list) + 1]] <- list(
      species = name_loop,
      basin_id = basin_loop,
      dispersal_prob = dispersal_exp,
      presence_nodes = basin_presence$name
    )
  }
}

message("Total tasks to process: ", length(task_list))

# ============================================================
# SET UP PARALLEL CLUSTER
# ============================================================

cl <- makeCluster(N_CORES, type = "FORK")  # FORK is Linux-only but more efficient
registerDoParallel(cl)

message("Parallel cluster initialized with ", N_CORES, " workers")

# ============================================================
# PARALLEL PROCESSING FUNCTION
# ============================================================

process_species_basin <- function(task, river_graph, node_basins) {

  # Extract task parameters
  species_name <- task$species
  basin_id_loop <- task$basin_id
  dispersal_exp <- task$dispersal_prob
  presence_nodes <- task$presence_nodes

  # Extract basin subgraph
  basin_nodes <- node_basins %>%
    filter(basin_id == basin_id_loop) %>%
    pull(name)

  basin_graph <- induced_subgraph(river_graph, basin_nodes)

  # Create presence data frame
  basin_presence <- data.frame(
    name = presence_nodes,
    weight = 1
  )

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
    mutate(species = species_name,
           basin_id = basin_id_loop,
           scenario = "fragmented")

  pci_o <- index_calculation_efficient(
    graph_loop_open,
    weight   = "weight",
    field_B  = "length_reach",
    param    = dispersal_exp,
    index_type = "full"
  ) %>%
    mutate(species = species_name,
           basin_id = basin_id_loop,
           scenario = "open")

  # Fragmentation Index
  fi_val <- (pci_o$index - pci_f$index) / pci_o$index * 100

  catchment_result <- bind_rows(pci_f, pci_o) %>%
    mutate(FI = ifelse(scenario == "fragmented", fi_val, NA))

  # Subcatchment-level
  subcatchment_result <- index_calculation_efficient(
    graph_loop,
    weight     = "weight",
    field_B    = "length_reach",
    param      = dispersal_exp,
    index_type = "reach",
    index_mode = "to"
  ) %>%
    mutate(species = species_name, basin_id = basin_id_loop)

  # Return both results as a list
  return(list(
    catchment = catchment_result,
    subcatchment = subcatchment_result
  ))
}

# ============================================================
# RUN PARALLEL PROCESSING
# ============================================================

message("\nStarting parallel processing...")
start_time <- Sys.time()

# Process all tasks in parallel with progress bar
results <- foreach(
  i = 1:length(task_list),
  .packages = c("igraph", "dplyr", "tidyverse"),
  .export = c("index_calculation_efficient", "river_graph", "node_basins"),
  .errorhandling = "pass",  # Continue even if one task fails
  .verbose = FALSE
) %dopar% {

  # Progress tracking (will print to worker logs)
  if (i %% 10 == 0) {
    cat(sprintf("Worker processing task %d / %d\n", i, length(task_list)))
  }

  # Process this task
  tryCatch({
    process_species_basin(task_list[[i]], river_graph, node_basins)
  }, error = function(e) {
    # Return error info instead of crashing
    list(
      error = TRUE,
      species = task_list[[i]]$species,
      basin_id = task_list[[i]]$basin_id,
      message = as.character(e)
    )
  })
}

end_time <- Sys.time()
elapsed_time <- end_time - start_time

message("Parallel processing complete!")
message("Time elapsed: ", round(elapsed_time, 2), " ", units(elapsed_time))

# Stop cluster
stopCluster(cl)

# ============================================================
# COLLECT AND COMBINE RESULTS
# ============================================================

message("\nCollecting results...")

# Check for errors
errors <- sapply(results, function(x) !is.null(x$error) && x$error)
if (any(errors)) {
  error_tasks <- results[errors]
  message("WARNING: ", sum(errors), " tasks failed:")
  for (err in error_tasks) {
    message("  - ", err$species, " in basin ", err$basin_id, ": ", err$message)
  }
}

# Extract successful results
successful_results <- results[!errors]

# Separate catchment and subcatchment results
catchment_pci_list <- lapply(successful_results, function(x) x$catchment)
subcatchment_pci_list <- lapply(successful_results, function(x) x$subcatchment)

# Combine all results
catchment_pci_full <- do.call(rbind, catchment_pci_list)
subcatchment_pci_full <- do.call(rbind, subcatchment_pci_list)

message("Collected ", nrow(catchment_pci_full), " catchment results")
message("Collected ", nrow(subcatchment_pci_full), " subcatchment results")

# ============================================================
# SAVE RESULTS
# ============================================================

saveRDS(catchment_pci_full,    "traits/catchment_pci_full.RDS")
saveRDS(subcatchment_pci_full, "traits/subcatchment_pci_full.RDS")

fi_summary <- catchment_pci_full %>%
  filter(scenario == "fragmented") %>%
  select(species, basin_id, index, FI) %>%
  rename(PCI_fragmented = index)

fwrite(fi_summary, "traits/fi_summary.txt")

# ============================================================
# FINAL SUMMARY
# ============================================================

message("\n=== FINAL SUMMARY ===")
message("Total species × basin combinations: ", nrow(fi_summary))
message("Unique species: ", length(unique(fi_summary$species)))
message("Unique basins: ", length(unique(fi_summary$basin_id)))
message("Failed tasks: ", sum(errors))
message("Total processing time: ", round(elapsed_time, 2), " ", units(elapsed_time))
message("Average time per task: ",
        round(as.numeric(elapsed_time) / length(task_list), 2), " seconds")
message("\nFiles saved:")
message("  - catchment_pci_full.RDS")
message("  - subcatchment_pci_full.RDS")
message("  - fi_summary.txt")

