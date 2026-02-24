#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01 -- network_fragmentation_analysis_ALL.R
# Calculate network fragmentation metrics for ALL species in ALL basins
# Parallelized for efficiency
#
# INPUT:
#   - river_graph.RDS (from workflows/03_spatial_network/01_network_generation.R)
#   - dams_snapped_points.csv
#   - fish_all_species_snapped.csv
#
# OUTPUT:
#   - fragmentation_all_species_basins.csv (summary for all combinations)
#   - fragmentation_detailed/ (folder with per-species-basin details)
#
# LOCATION: workflows/04_network_analysis/01_network_fragmentation_analysis_ALL.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(sf)
library(data.table)
library(parallel)
library(foreach)
library(doParallel)

# Set working directory
wdir <- "/run/user/1000/gvfs/dav:host=nimbus.igb-berlin.de,ssl=true,user=grigoropoulou,prefix=%2Fremote.php%2Fwebdav/workflow_paper/data"
setwd(wdir)

# Create output directories
dir.create("connectivity", showWarnings = FALSE)
dir.create("connectivity/fragmentation_detailed", showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

MIN_OCCURRENCES <- 2   # Minimum species occurrences in a basin to analyze
N_CORES <- detectCores() - 1  # Parallel processing cores

message("=== NETWORK FRAGMENTATION ANALYSIS - ALL SPECIES & BASINS ===")
message("Date: ", Sys.Date())
message("Cores: ", N_CORES)

# ============================================================
# STEP 1: Load river network
# ============================================================

message("\n[1/6] Loading river network graph...")
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

message("\n[2/6] Loading fish and dam data...")

fish_all <- fread("points_snapped/fish/fish_all_species_snapped.csv")
dams_all <- fread("points_snapped/dams/dams_snapped_points.csv")

# Filter to existing dams only
dams_existing <- dams_all %>%
  filter(status == "existing")

# CRITICAL: Add basin_id to dams by matching subc_id
dams_existing <- dams_existing %>%
  mutate(subc_id = as.character(subc_id)) %>%
  left_join(
    node_data %>% select(name, basin_id),
    by = c("subc_id" = "name")
  ) %>%
  filter(!is.na(basin_id))

message("  Fish occurrences: ", nrow(fish_all))
message("  Species: ", length(unique(fish_all$species)))
message("  Existing dams: ", nrow(dams_existing))
message("  Dams with basin_id: ", sum(!is.na(dams_existing$basin_id)))

# ============================================================
# STEP 3: Identify species × basin combinations to analyze
# ============================================================

message("\n[3/6] Identifying species × basin combinations...")

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

# Check which basins have dams
basins_with_dams <- dams_existing %>%
  pull(basin_id) %>%
  unique() %>%
  as.character()

message("  Basins with dams: ", length(basins_with_dams))

# Keep only combinations where basin has dams
species_basin_counts <- species_basin_counts %>%
  filter(basin_id %in% basins_with_dams)

message("  Total combinations to analyze: ", nrow(species_basin_counts))
message("  Species: ", length(unique(species_basin_counts$species)))
message("  Basins: ", length(unique(species_basin_counts$basin_id)))


# ============================================================
# STEP 4: Define analysis function for one species × basin
# ============================================================

analyze_species_basin <- function(species_name, basin_id_val,
                                  river_graph, node_data,
                                  fish_all, dams_existing) {

  tryCatch({

    # Extract basin subgraph
    basin_nodes <- node_data %>%
      filter(basin_id == basin_id_val) %>%
      pull(name)

    if (length(basin_nodes) == 0) {
      return(NULL)
    }

    basin_graph <- induced_subgraph(river_graph, basin_nodes)

    # Get species occurrences in this basin
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

    # Calculate distances in CONNECTED network
    distance_matrix_connected <- distances(
      basin_graph,
      v = species_node_indices,
      to = species_node_indices,
      weights = E(basin_graph)$length_reach,
      mode = "all"
    )

    # Get dams in this basin
    dams_basin <- dams_existing %>%
      filter(as.character(basin_id) == basin_id_val)

    n_dams <- nrow(dams_basin)

    if (n_dams == 0) {
      return(NULL)
    }

    # Fragment network by removing dam edges
    dam_nodes <- unique(as.character(dams_basin$subc_id))
    dam_nodes_in_graph <- dam_nodes[dam_nodes %in% V(basin_graph)$name]

    edge_df <- as_data_frame(basin_graph, "edges")
    dam_edge_ids <- which(edge_df$from %in% dam_nodes_in_graph |
                            edge_df$to %in% dam_nodes_in_graph)

    if (length(dam_edge_ids) == 0) {
      return(NULL)
    }

    basin_fragmented <- delete_edges(basin_graph, dam_edge_ids)

    # Calculate distances in FRAGMENTED network
    distance_matrix_fragmented <- distances(
      basin_fragmented,
      v = species_node_indices,
      to = species_node_indices,
      weights = E(basin_fragmented)$length_reach,
      mode = "all"
    )

    # Analyze all pairwise connections
    n_total <- 0
    n_unchanged <- 0
    n_increased <- 0
    n_disconnected <- 0

    dist_unchanged <- c()
    dist_disconnected_original <- c()

    for (i in 1:(length(species_nodes) - 1)) {
      for (j in (i + 1):length(species_nodes)) {

        n_total <- n_total + 1

        dist_conn <- distance_matrix_connected[i, j]
        dist_frag <- distance_matrix_fragmented[i, j]

        if (is.finite(dist_conn) && !is.finite(dist_frag)) {
          # Newly disconnected
          n_disconnected <- n_disconnected + 1
          dist_disconnected_original <- c(dist_disconnected_original, dist_conn)
        } else if (is.finite(dist_conn) && is.finite(dist_frag)) {
          if (abs(dist_conn - dist_frag) < 0.001) {
            # Unchanged
            n_unchanged <- n_unchanged + 1
            dist_unchanged <- c(dist_unchanged, dist_conn)
          } else if (dist_frag > dist_conn) {
            # Increased (detour)
            n_increased <- n_increased + 1
            dist_unchanged <- c(dist_unchanged, dist_conn)  # Still connected
          }
        }
      }
    }

    # Calculate statistics
    mean_dist_unchanged <- if (length(dist_unchanged) > 0) mean(dist_unchanged) else NA
    sd_dist_unchanged <- if (length(dist_unchanged) > 1) sd(dist_unchanged) else NA

    mean_dist_disconnected <- if (length(dist_disconnected_original) > 0) {
      mean(dist_disconnected_original)
    } else {
      NA
    }
    sd_dist_disconnected <- if (length(dist_disconnected_original) > 1) {
      sd(dist_disconnected_original)
    } else {
      NA
    }

    # Statistical test (if both groups have data)
    t_test_p <- NA
    if (length(dist_unchanged) > 1 && length(dist_disconnected_original) > 1) {
      t_result <- tryCatch(
        t.test(dist_unchanged, dist_disconnected_original),
        error = function(e) NULL
      )
      if (!is.null(t_result)) {
        t_test_p <- t_result$p.value
      }
    }

    # Return results
    data.frame(
      species = species_name,
      basin_id = basin_id_val,
      n_occurrences = n_occurrences,
      n_dams = n_dams,
      n_dam_edges_removed = length(dam_edge_ids),
      total_pairs = n_total,
      unchanged_pairs = n_unchanged,
      increased_pairs = n_increased,
      disconnected_pairs = n_disconnected,
      percent_connectivity_lost = round(100 * n_disconnected / n_total, 2),
      mean_dist_connected_group = round(mean_dist_unchanged, 1),
      sd_dist_connected_group = round(sd_dist_unchanged, 1),
      mean_dist_disconnected_group = round(mean_dist_disconnected, 1),
      sd_dist_disconnected_group = round(sd_dist_disconnected, 1),
      t_test_p_value = round(t_test_p, 4),
      significant_difference = !is.na(t_test_p) && t_test_p < 0.05,
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

# ============================================================
# STEP 5: Run parallel analysis
# ============================================================

message("\n[4/6] Running parallel fragmentation analysis...")
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

  analyze_species_basin(
    species_name = row$species,
    basin_id_val = row$basin_id,
    river_graph = river_graph,
    node_data = node_data,
    fish_all = fish_all,
    dams_existing = dams_existing
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

message("\n[5/6] Processing results...")

# Filter out errors and NULL results
if ("error" %in% colnames(results)) {
  errors <- results %>% filter(error == TRUE)
  if (nrow(errors) > 0) {
    message("  WARNING: ", nrow(errors), " combinations failed")
    fwrite(errors, "connectivity/fragmentation_errors.csv")
  }
  results <- results %>% filter(is.na(error) | error == FALSE)
}

# Remove error columns if present
results <- results %>% select(-matches("^error"))

# Calculate summary statistics
message("  Successfully analyzed: ", nrow(results), " combinations")
message("  Species analyzed: ", length(unique(results$species)))
message("  Basins analyzed: ", length(unique(results$basin_id)))

# Summary statistics
summary_stats <- results %>%
  summarise(
    n_combinations = n(),
    n_species = n_distinct(species),
    n_basins = n_distinct(basin_id),
    mean_connectivity_loss = mean(percent_connectivity_lost, na.rm = TRUE),
    median_connectivity_loss = median(percent_connectivity_lost, na.rm = TRUE),
    max_connectivity_loss = max(percent_connectivity_lost, na.rm = TRUE),
    n_with_loss = sum(disconnected_pairs > 0),
    n_significant = sum(significant_difference == TRUE, na.rm = TRUE)
  )

message("\n  Summary Statistics:")
message("    Mean connectivity loss: ", round(summary_stats$mean_connectivity_loss, 1), "%")
message("    Median connectivity loss: ", round(summary_stats$median_connectivity_loss, 1), "%")
message("    Max connectivity loss: ", round(summary_stats$max_connectivity_loss, 1), "%")
message("    Combinations with connectivity loss: ", summary_stats$n_with_loss)
message("    Combinations with significant distance difference: ", summary_stats$n_significant)

# Save full results
fwrite(results, "connectivity/fragmentation_all_species_basins.csv")

# Save summary by species
species_summary <- results %>%
  group_by(species) %>%
  summarise(
    n_basins = n(),
    mean_connectivity_loss = mean(percent_connectivity_lost, na.rm = TRUE),
    max_connectivity_loss = max(percent_connectivity_lost, na.rm = TRUE),
    n_basins_with_loss = sum(disconnected_pairs > 0),
    mean_pairs_disconnected = mean(disconnected_pairs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_connectivity_loss))

fwrite(species_summary, "connectivity/fragmentation_by_species.csv")

# Save summary by basin
basin_summary <- results %>%
  group_by(basin_id) %>%
  summarise(
    n_species = n(),
    mean_connectivity_loss = mean(percent_connectivity_lost, na.rm = TRUE),
    max_connectivity_loss = max(percent_connectivity_lost, na.rm = TRUE),
    n_species_affected = sum(disconnected_pairs > 0),
    total_dams = first(n_dams),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_connectivity_loss))

fwrite(basin_summary, "connectivity/fragmentation_by_basin.csv")

# ============================================================
# STEP 6: Create summary visualizations
# ============================================================

message("\n[6/6] Creating summary visualizations...")

# Distribution of connectivity loss
png("connectivity/connectivity_loss_distribution.png",
    width = 10, height = 6, units = "in", res = 300)

par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

# Histogram
hist(results$percent_connectivity_lost,
     breaks = 30,
     col = "#FC8D62",
     border = "white",
     main = "Distribution of Connectivity Loss",
     xlab = "Connectivity loss (%)",
     ylab = "Number of species × basin combinations",
     cex.lab = 1.3,
     cex.main = 1.5,
     cex.axis = 1.2)

abline(v = mean(results$percent_connectivity_lost, na.rm = TRUE),
       col = "darkred",
       lwd = 3,
       lty = 2)

text(mean(results$percent_connectivity_lost, na.rm = TRUE),
     par("usr")[4] * 0.9,
     paste("Mean =", round(mean(results$percent_connectivity_lost, na.rm = TRUE), 1), "%"),
     pos = 4,
     cex = 1.2,
     font = 2)

# Top affected species
top_species <- species_summary %>%
  head(15) %>%
  arrange(mean_connectivity_loss)

barplot(top_species$mean_connectivity_loss,
        names.arg = gsub("_", " ", top_species$species),
        horiz = TRUE,
        col = "#66C2A5",
        border = "white",
        main = "Top 15 Most Fragmented Species",
        xlab = "Mean connectivity loss (%)",
        cex.lab = 1.3,
        cex.main = 1.5,
        cex.names = 0.9,
        cex.axis = 1.2,
        las = 1)

dev.off()

message("  Saved: connectivity/connectivity_loss_distribution.png")

# ============================================================
# FINAL SUMMARY
# ============================================================

message("\n=== ANALYSIS COMPLETE ===")
message("Output files:")
message("  • connectivity/fragmentation_all_species_basins.csv")
message("  • connectivity/fragmentation_by_species.csv")
message("  • connectivity/fragmentation_by_basin.csv")
message("  • connectivity/connectivity_loss_distribution.png")
message("\nKey findings:")
message("  • ", nrow(results), " species × basin combinations analyzed")
message("  • ", summary_stats$n_species, " species across ", summary_stats$n_basins, " basins")
message("  • Mean connectivity loss: ", round(summary_stats$mean_connectivity_loss, 1), "%")
message("  • ", summary_stats$n_with_loss, " combinations show connectivity loss")
message("  • ", summary_stats$n_significant, " combinations have significant distance differences")
message("\nAnalysis completed: ", Sys.time())
