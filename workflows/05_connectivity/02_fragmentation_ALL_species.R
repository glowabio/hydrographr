#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02 -- fragmentation_ALL_species.R
# Calculate network fragmentation metrics for ALL species in ALL basins
# WITH SCENARIO ANALYSIS (existing vs existing+planned dams)
# Parallelized for efficiency
#
# SCENARIOS:
#   1. Current state (existing dams only)
#   2. Future scenario (existing + planned dams)
#
# INPUT:
#   - river_graph.RDS
#   - dams_snapped_points.csv
#   - fish_all_species_snapped.csv
#
# OUTPUT:
#   - fragmentation_all_scenarios.csv (all combinations, both scenarios)
#   - fragmentation_by_species.csv (species summaries)
#   - fragmentation_by_basin.csv (basin summaries)
#   - scenario_impact_summary.csv (additional impact of planned dams)
#
# LOCATION: workflows/05_connectivity/02_fragmentation_ALL_species.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(sf)
library(data.table)
library(parallel)
library(foreach)
library(doParallel)

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)

# Create output directories
dir.create("connectivity", showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

MIN_OCCURRENCES <- 2   # Minimum species occurrences in a basin to analyze
N_CORES <- detectCores() - 1  # Parallel processing cores

message("=== NETWORK FRAGMENTATION ANALYSIS - ALL SPECIES & BASINS WITH SCENARIOS ===")
message("Date: ", Sys.Date())
message("Cores: ", N_CORES)

# ============================================================
# STEP 1: Load river network
# ============================================================

message("\n[1/7] Loading river network graph...")
river_graph <- readRDS("spatial/stream_networks/river_graph.RDS")

message("  Network loaded: ",
        vcount(river_graph), " nodes, ",
        ecount(river_graph), " edges")

# Extract node data
node_data <- data.frame(
  name = V(river_graph)$name,
  basin_id = V(river_graph)$basin_id,
  length_reach = V(river_graph)$length_reach
) %>%
  filter(!is.na(basin_id))

# ============================================================
# STEP 2: Load fish occurrences and dams
# ============================================================

message("\n[2/7] Loading fish and dam data...")

fish_all <- fread("points_snapped/fish/fish_all_species_snapped.csv")
dams_all <- fread("points_snapped/dams/dams_snapped_points.csv")

# Filter dams by status
dams_existing <- dams_all %>%
  filter(status == "existing")

dams_planned <- dams_all %>%
  filter(status == "planned") %>%
  filter(phase != "R")  # Exclude rejected dams

dams_all_future <- dams_all %>%
  filter(phase != "R") %>%
  filter(status %in% c("existing", "planned"))

# Add basin_id to dams
dams_existing <- dams_existing %>%
  mutate(subc_id = as.character(subc_id)) %>%
  left_join(
    node_data %>% select(name, basin_id),
    by = c("subc_id" = "name")
  ) %>%
  filter(!is.na(basin_id))

dams_planned <- dams_planned %>%
  mutate(subc_id = as.character(subc_id)) %>%
  left_join(
    node_data %>% select(name, basin_id),
    by = c("subc_id" = "name")
  ) %>%
  filter(!is.na(basin_id))

dams_all_future <- dams_all_future %>%
  mutate(subc_id = as.character(subc_id)) %>%
  left_join(
    node_data %>% select(name, basin_id),
    by = c("subc_id" = "name")
  ) %>%
  filter(!is.na(basin_id))

message("  Fish occurrences: ", nrow(fish_all))
message("  Species: ", length(unique(fish_all$species)))
message("  Existing dams: ", nrow(dams_existing))
message("  Planned dams: ", nrow(dams_planned))
message("  Total future dams: ", nrow(dams_all_future))

# ============================================================
# STEP 3: Identify species × basin combinations to analyze
# ============================================================

message("\n[3/7] Identifying species × basin combinations...")

# Count occurrences per species per basin
species_basin_counts <- fish_all %>%
  mutate(subc_id = as.character(subc_id)) %>%
  left_join(
    node_data %>% select(name, basin_id),
    by = c("subc_id" = "name")
  ) %>%
  filter(!is.na(basin_id), basin_id != "") %>%
  group_by(species, basin_id) %>%
  summarise(
    n_occurrences = n(),
    .groups = "drop"
  ) %>%
  filter(n_occurrences >= MIN_OCCURRENCES)

# Keep only combinations where basin has dams (existing OR planned)
basins_with_dams <- unique(c(
  dams_existing$basin_id,
  dams_planned$basin_id
)) %>% as.character()

species_basin_counts <- species_basin_counts %>%
  filter(basin_id %in% basins_with_dams)

message("  Basins with dams: ", length(basins_with_dams))
message("  Total combinations to analyze: ", nrow(species_basin_counts))
message("  Species: ", length(unique(species_basin_counts$species)))
message("  Basins: ", length(unique(species_basin_counts$basin_id)))

# ============================================================
# STEP 4: Define analysis function for one species × basin × scenario
# ============================================================

analyze_species_basin_scenario <- function(species_name, basin_id_val,
                                           river_graph, node_data,
                                           fish_all, dams_existing, dams_all_future) {

  tryCatch({

    # Extract basin subgraph
    basin_nodes <- node_data %>%
      filter(basin_id == basin_id_val) %>%
      pull(name)

    if (length(basin_nodes) == 0) {
      return(NULL)
    }

    basin_graph <- induced_subgraph(river_graph, basin_nodes)

    # Get species occurrences
    species_occurrences <- fish_all %>%
      filter(species == species_name,
             as.character(subc_id) %in% basin_nodes) %>%
      distinct(subc_id, .keep_all = TRUE)

    n_occurrences <- nrow(species_occurrences)

    if (n_occurrences < 2) {
      return(NULL)
    }

    species_nodes <- as.character(species_occurrences$subc_id)
    species_node_indices <- which(V(basin_graph)$name %in% species_nodes)

    # Get dams in this basin
    dams_basin_existing <- dams_existing %>%
      filter(as.character(basin_id) == basin_id_val)

    dams_basin_future <- dams_all_future %>%
      filter(as.character(basin_id) == basin_id_val)

    # Calculate original (unfragmented) distance matrix
    distance_matrix_original <- distances(
      basin_graph,
      v = species_node_indices,
      to = species_node_indices,
      weights = E(basin_graph)$length_reach,
      mode = "all"
    )

    # ------------------------------------------------------------
    # SCENARIO 1: Existing dams
    # ------------------------------------------------------------

    if (nrow(dams_basin_existing) > 0) {
      # Fragment by existing dams
      dam_nodes_existing <- unique(as.character(dams_basin_existing$subc_id))
      dam_nodes_in_graph_existing <- dam_nodes_existing[dam_nodes_existing %in% V(basin_graph)$name]

      edge_df <- as_data_frame(basin_graph, "edges")
      dam_edge_ids_existing <- which(edge_df$from %in% dam_nodes_in_graph_existing |
                                       edge_df$to %in% dam_nodes_in_graph_existing)

      if (length(dam_edge_ids_existing) > 0) {
        basin_fragmented_existing <- delete_edges(basin_graph, dam_edge_ids_existing)
      } else {
        basin_fragmented_existing <- basin_graph
      }
    } else {
      basin_fragmented_existing <- basin_graph
      dam_edge_ids_existing <- integer(0)
    }

    # Calculate distances in existing scenario
    distance_matrix_existing <- distances(
      basin_fragmented_existing,
      v = species_node_indices,
      to = species_node_indices,
      weights = E(basin_fragmented_existing)$length_reach,
      mode = "all"
    )

    # Analyze pairs - existing scenario
    results_existing <- analyze_pairs(
      distance_matrix_original,
      distance_matrix_existing,
      species_nodes
    )

    # ------------------------------------------------------------
    # SCENARIO 2: Future (existing + planned dams)
    # ------------------------------------------------------------

    if (nrow(dams_basin_future) > 0) {
      # Fragment by all future dams
      dam_nodes_future <- unique(as.character(dams_basin_future$subc_id))
      dam_nodes_in_graph_future <- dam_nodes_future[dam_nodes_future %in% V(basin_graph)$name]

      edge_df <- as_data_frame(basin_graph, "edges")
      dam_edge_ids_future <- which(edge_df$from %in% dam_nodes_in_graph_future |
                                     edge_df$to %in% dam_nodes_in_graph_future)

      if (length(dam_edge_ids_future) > 0) {
        basin_fragmented_future <- delete_edges(basin_graph, dam_edge_ids_future)
      } else {
        basin_fragmented_future <- basin_graph
      }
    } else {
      basin_fragmented_future <- basin_graph
      dam_edge_ids_future <- integer(0)
    }

    # Calculate distances in future scenario
    distance_matrix_future <- distances(
      basin_fragmented_future,
      v = species_node_indices,
      to = species_node_indices,
      weights = E(basin_fragmented_future)$length_reach,
      mode = "all"
    )

    # Analyze pairs - future scenario
    results_future <- analyze_pairs(
      distance_matrix_original,
      distance_matrix_future,
      species_nodes
    )

    # ------------------------------------------------------------
    # Combine results
    # ------------------------------------------------------------

    data.frame(
      species = species_name,
      basin_id = basin_id_val,
      n_occurrences = n_occurrences,

      # Existing scenario
      n_dams_existing = nrow(dams_basin_existing),
      n_dam_edges_removed_existing = length(dam_edge_ids_existing),
      total_pairs = results_existing$n_total,
      disconnected_pairs_existing = results_existing$n_disconnected,
      percent_lost_existing = round(100 * results_existing$n_disconnected / results_existing$n_total, 2),

      # Future scenario
      n_dams_future = nrow(dams_basin_future),
      n_dam_edges_removed_future = length(dam_edge_ids_future),
      disconnected_pairs_future = results_future$n_disconnected,
      percent_lost_future = round(100 * results_future$n_disconnected / results_future$n_total, 2),

      # Additional impact of planned dams
      n_dams_planned = nrow(dams_basin_future) - nrow(dams_basin_existing),
      additional_pairs_disconnected = results_future$n_disconnected - results_existing$n_disconnected,
      additional_percent_lost = round(
        (100 * results_future$n_disconnected / results_future$n_total) -
          (100 * results_existing$n_disconnected / results_existing$n_total), 2
      ),

      # Distance statistics (existing scenario)
      mean_dist_connected_existing = results_existing$mean_dist_connected,
      mean_dist_disconnected_existing = results_existing$mean_dist_disconnected,
      t_test_p_existing = results_existing$t_test_p,

      # Distance statistics (future scenario)
      mean_dist_connected_future = results_future$mean_dist_connected,
      mean_dist_disconnected_future = results_future$mean_dist_disconnected,
      t_test_p_future = results_future$t_test_p,

      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    # Return error info
    data.frame(
      species = species_name,
      basin_id = basin_id_val,
      error = TRUE,
      error_message = as.character(e),
      stringsAsFactors = FALSE
    )
  })
}

# Helper function to analyze pairs
analyze_pairs <- function(dist_original, dist_fragmented, species_nodes) {

  n_total <- 0
  n_disconnected <- 0

  dist_connected <- c()
  dist_disconnected_original <- c()

  for (i in 1:(length(species_nodes) - 1)) {
    for (j = (i + 1):length(species_nodes)) {

      n_total <- n_total + 1

      dist_orig <- dist_original[i, j]
      dist_frag <- dist_fragmented[i, j]

      if (is.finite(dist_orig) && !is.finite(dist_frag)) {
        # Newly disconnected
        n_disconnected <- n_disconnected + 1
        dist_disconnected_original <- c(dist_disconnected_original, dist_orig)
      } else if (is.finite(dist_frag)) {
        # Still connected
        dist_connected <- c(dist_connected, dist_frag)
      }
    }
  }

  # Statistical test - ONLY if BOTH groups have data
  t_test_p <- NA
  if (length(dist_connected) > 1 && length(dist_disconnected_original) > 1) {
    t_result <- tryCatch(
      t.test(dist_connected, dist_disconnected_original),
      error = function(e) NULL
    )
    if (!is.null(t_result)) {
      t_test_p <- round(t_result$p.value, 4)
    }
  }

  list(
    n_total = n_total,
    n_disconnected = n_disconnected,
    n_connected = n_total - n_disconnected,  # ADD THIS
    mean_dist_connected = if (length(dist_connected) > 0) round(mean(dist_connected), 1) else NA,
    mean_dist_disconnected = if (length(dist_disconnected_original) > 0) {
      round(mean(dist_disconnected_original), 1)
    } else {
      NA
    },
    t_test_p = t_test_p,
    test_feasible = length(dist_connected) > 1 && length(dist_disconnected_original) > 1  # ADD THIS
  )
}

# ============================================================
# STEP 5: Run parallel analysis
# ============================================================

message("\n[4/7] Running parallel scenario analysis...")
message("  Processing ", nrow(species_basin_counts), " combinations...")

# Set up parallel cluster
cl <- makeCluster(N_CORES, type = "FORK")
registerDoParallel(cl)

start_time <- Sys.time()

# Run analysis in parallel
results <- foreach(
  i = 1:nrow(species_basin_counts),
  .packages = c("igraph", "dplyr"),
  .combine = rbind,
  .errorhandling = "pass"
) %dopar% {

  if (i %% 50 == 0) {
    cat(sprintf("Processed %d / %d combinations\n", i, nrow(species_basin_counts)))
  }

  row <- species_basin_counts[i, ]

  analyze_species_basin_scenario(
    species_name = row$species,
    basin_id_val = row$basin_id,
    river_graph = river_graph,
    node_data = node_data,
    fish_all = fish_all,
    dams_existing = dams_existing,
    dams_all_future = dams_all_future
  )
}

stopCluster(cl)

end_time <- Sys.time()
elapsed <- end_time - start_time

message("  Analysis complete!")
message("  Time elapsed: ", round(elapsed, 2), " ", units(elapsed))

# ============================================================
# STEP 6: Process and save results
# ============================================================

message("\n[5/7] Processing results...")

# Filter out errors
if ("error" %in% colnames(results)) {
  errors <- results %>% filter(error == TRUE)
  if (nrow(errors) > 0) {
    message("  WARNING: ", nrow(errors), " combinations failed")
    fwrite(errors, "connectivity/fragmentation_errors.csv")
  }
  results <- results %>% filter(is.na(error) | error == FALSE)
}

results <- results %>% select(-matches("^error"))

message("  Successfully analyzed: ", nrow(results), " combinations")

# Save full results
fwrite(results, "connectivity/fragmentation_all_scenarios.csv")
message("  Saved: connectivity/fragmentation_all_scenarios.csv")

# ============================================================
# STEP 7: Create summaries
# ============================================================

message("\n[6/7] Creating summary statistics...")

# Overall summary
overall_summary <- results %>%
  summarise(
    n_combinations = n(),
    n_species = n_distinct(species),
    n_basins = n_distinct(basin_id),

    # Existing scenario
    mean_loss_existing = mean(percent_lost_existing, na.rm = TRUE),
    median_loss_existing = median(percent_lost_existing, na.rm = TRUE),

    # Future scenario
    mean_loss_future = mean(percent_lost_future, na.rm = TRUE),
    median_loss_future = median(percent_lost_future, na.rm = TRUE),

    # Impact of planned dams
    mean_additional_loss = mean(additional_percent_lost, na.rm = TRUE),
    n_with_planned_impact = sum(additional_pairs_disconnected > 0, na.rm = TRUE),

    total_dams_existing = sum(n_dams_existing, na.rm = TRUE),
    total_dams_planned = sum(n_dams_planned, na.rm = TRUE)
  )

print(overall_summary)

fwrite(overall_summary, "connectivity/scenario_impact_summary.csv")

# Species summary
species_summary <- results %>%
  group_by(species) %>%
  summarise(
    n_basins = n(),
    mean_loss_existing = mean(percent_lost_existing, na.rm = TRUE),
    mean_loss_future = mean(percent_lost_future, na.rm = TRUE),
    mean_additional_loss = mean(additional_percent_lost, na.rm = TRUE),
    max_loss_existing = max(percent_lost_existing, na.rm = TRUE),
    max_loss_future = max(percent_lost_future, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_loss_future))

fwrite(species_summary, "connectivity/fragmentation_by_species.csv")

# Basin summary
basin_summary <- results %>%
  group_by(basin_id) %>%
  summarise(
    n_species = n(),
    mean_loss_existing = mean(percent_lost_existing, na.rm = TRUE),
    mean_loss_future = mean(percent_lost_future, na.rm = TRUE),
    mean_additional_loss = mean(additional_percent_lost, na.rm = TRUE),
    total_dams_existing = first(n_dams_existing),
    total_dams_planned = first(n_dams_planned),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_loss_future))

fwrite(basin_summary, "connectivity/fragmentation_by_basin.csv")

# ============================================================
# STEP 8: Create visualizations
# ============================================================

message("\n[7/7] Creating visualizations...")

png("connectivity/scenario_comparison_greece.png",
    width = 14, height = 6, units = "in", res = 300)

par(mfrow = c(1, 3), mar = c(5, 5, 4, 2))

# Panel A: Distribution comparison
hist(results$percent_lost_existing,
     breaks = 30, col = rgb(0.99, 0.71, 0.38, 0.5),
     border = "white",
     main = "Connectivity Loss Distribution",
     xlab = "Connectivity loss (%)",
     ylab = "Number of combinations",
     xlim = c(0, 100),
     cex.lab = 1.3, cex.main = 1.5)

hist(results$percent_lost_future,
     breaks = 30, col = rgb(0.99, 0.55, 0.38, 0.5),
     border = "white", add = TRUE)

legend("topright",
       c("Existing dams", "Existing + Planned"),
       fill = c(rgb(0.99, 0.71, 0.38, 0.5), rgb(0.99, 0.55, 0.38, 0.5)),
       border = "white", bty = "n")

# Panel B: Additional impact
barplot(c(
  overall_summary$mean_loss_existing,
  overall_summary$mean_loss_future
),
names.arg = c("Current", "Future"),
col = c("#FDB462", "#FC8D62"),
border = "white",
main = "Mean Connectivity Loss",
ylab = "Mean loss (%)",
ylim = c(0, max(c(overall_summary$mean_loss_existing, overall_summary$mean_loss_future)) * 1.2),
cex.lab = 1.3, cex.main = 1.5)

text(0.7, overall_summary$mean_loss_existing + 2,
     paste0(round(overall_summary$mean_loss_existing, 1), "%"),
     cex = 1.2, font = 2)

text(1.9, overall_summary$mean_loss_future + 2,
     paste0(round(overall_summary$mean_loss_future, 1), "%"),
     cex = 1.2, font = 2)

# Panel C: Most affected basins
top_basins <- basin_summary %>%
  head(15) %>%
  arrange(mean_loss_future)

barplot(top_basins$mean_loss_future,
        names.arg = top_basins$basin_id,
        horiz = TRUE,
        col = "#FC8D62",
        border = "white",
        main = "Top 15 Most Fragmented Basins",
        xlab = "Mean connectivity loss (%)",
        cex.lab = 1.3, cex.main = 1.5,
        cex.names = 0.8,
        las = 1)

dev.off()

message("  Saved: connectivity/scenario_comparison_greece.png")

# ============================================================
# FINAL SUMMARY
# ============================================================

message("\n=== SCENARIO ANALYSIS COMPLETE ===")
message("Output files:")
message("  • connectivity/fragmentation_all_scenarios.csv")
message("  • connectivity/fragmentation_by_species.csv")
message("  • connectivity/fragmentation_by_basin.csv")
message("  • connectivity/scenario_impact_summary.csv")
message("  • connectivity/scenario_comparison_greece.png")
message("\nKey findings:")
message("  • ", nrow(results), " species × basin combinations analyzed")
message("  • ", overall_summary$n_species, " species across ", overall_summary$n_basins, " basins")
message("  • Current (existing dams): ", round(overall_summary$mean_loss_existing, 1), "% mean loss")
message("  • Future (+ planned): ", round(overall_summary$mean_loss_future, 1), "% mean loss")
message("  • Additional impact: +", round(overall_summary$mean_additional_loss, 1), "% from planned dams")
message("  • ", overall_summary$n_with_planned_impact, " combinations affected by planned dams")
message("\nAnalysis completed: ", Sys.time())
