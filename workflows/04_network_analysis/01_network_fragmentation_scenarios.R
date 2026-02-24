#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01 -- network_fragmentation_scenarios.R
# Calculate network fragmentation metrics for 2 dam scenarios
# Uses igraph (consistent with PCI workflow)
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
#   - Scenario comparison figures (2-panel plots)
#   - Summary statistics for both scenarios
#
# LOCATION: workflows/04_network_analysis/01_network_fragmentation_scenarios.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(sf)
library(data.table)
library(leaflet)
library(htmlwidgets)
library(ggplot2)
library(ggspatial)

# Set working directory
wdir <- "/run/user/1000/gvfs/dav:host=nimbus.igb-berlin.de,ssl=true,user=grigoropoulou,prefix=%2Fremote.php%2Fwebdav/workflow_paper/data"
setwd(wdir)

# Create output directory
dir.create("connectivity", showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

TARGET_BASIN_ID <- 1292502
EXAMPLE_SPECIES <- "Barbus_prespensis"

message("=== NETWORK FRAGMENTATION SCENARIOS ANALYSIS ===")
message("Basin: ", TARGET_BASIN_ID)
message("Species: ", EXAMPLE_SPECIES)
message("Date: ", Sys.Date())

# ============================================================
# STEP 1: Load river network
# ============================================================

message("\n[1/9] Loading river network graph...")
river_graph <- readRDS("spatial/stream_networks/river_graph.RDS")

message("  Network loaded: ",
        vcount(river_graph), " nodes, ",
        ecount(river_graph), " edges")

# ============================================================
# STEP 2: Extract target basin
# ============================================================

message("\n[2/9] Extracting target basin...")

# Extract basin info from nodes
node_data <- data.frame(
  name = V(river_graph)$name,
  basin_id = V(river_graph)$basin_id,
  length_reach = V(river_graph)$length_reach
) %>%
  filter(!is.na(basin_id))

# Get nodes in target basin
basin_nodes <- node_data %>%
  filter(basin_id == TARGET_BASIN_ID) %>%
  pull(name)

# Extract subgraph
basin_graph <- induced_subgraph(river_graph, basin_nodes)

message("  Basin extracted:")
message("    Reaches: ", vcount(basin_graph))

# ============================================================
# STEP 3: Load fish occurrences
# ============================================================

message("\n[3/9] Loading fish occurrence data...")

fish_all <- fread("points_snapped/fish/fish_all_species_snapped.csv")

# Filter to target basin
fish_basin <- fish_all %>%
  filter(as.character(subc_id) %in% basin_nodes)

# Extract example species
species_occurrences <- fish_basin %>%
  filter(species == EXAMPLE_SPECIES |
           species == gsub("_", " ", EXAMPLE_SPECIES)) %>%
  distinct(subc_id, .keep_all = TRUE)

n_occurrences <- nrow(species_occurrences)
message("  Species occurrences: ", n_occurrences)

# Get species node indices
species_nodes <- as.character(species_occurrences$subc_id)
species_nodes <- species_nodes[species_nodes %in% V(basin_graph)$name]
species_node_indices <- which(V(basin_graph)$name %in% species_nodes)

max_connections <- (length(species_nodes) * (length(species_nodes) - 1)) / 2
message("  Maximum possible connections: ", max_connections)

# ============================================================
# STEP 4: Load and categorize dams
# ============================================================

message("\n[4/9] Loading dam data...")

dams_all <- fread("points_snapped/dams/dams_snapped_points.csv")

# Filter to target basin
dams_basin <- dams_all %>%
  filter(as.character(subc_id) %in% basin_nodes)

message("  Total dams in basin: ", nrow(dams_basin))

# Separate by status
dams_existing <- dams_basin %>%
  filter(status == "existing")

dams_planned <- dams_basin %>%
  filter(status == "planned") %>%
  filter(phase != "R")

dams_all_future <- dams_basin %>%
  filter(phase != "R") %>%
  filter(status %in% c("existing", "planned"))

message("  Existing dams (current): ", nrow(dams_existing))
message("  Planned dams: ", nrow(dams_planned))
message("  Total future dams: ", nrow(dams_all_future))

# ============================================================
# STEP 5: Define fragmentation function
# ============================================================

calculate_fragmentation <- function(basin_graph, species_node_indices,
                                    dam_subcatchments = NULL,
                                    scenario_name = "Baseline") {

  # Fragment network if dams provided
  if (!is.null(dam_subcatchments) && length(dam_subcatchments) > 0) {

    dam_nodes_in_graph <- dam_subcatchments[dam_subcatchments %in% V(basin_graph)$name]

    if (length(dam_nodes_in_graph) > 0) {
      edge_df <- as_data_frame(basin_graph, "edges")
      dam_edge_ids <- which(edge_df$from %in% dam_nodes_in_graph |
                              edge_df$to %in% dam_nodes_in_graph)

      if (length(dam_edge_ids) > 0) {
        fragmented_graph <- delete_edges(basin_graph, dam_edge_ids)
        n_edges_removed <- length(dam_edge_ids)
      } else {
        fragmented_graph <- basin_graph
        n_edges_removed <- 0
      }
    } else {
      fragmented_graph <- basin_graph
      n_edges_removed <- 0
    }
  } else {
    fragmented_graph <- basin_graph
    n_edges_removed <- 0
  }

  # Calculate distances
  distance_matrix <- distances(
    fragmented_graph,
    v = species_node_indices,
    to = species_node_indices,
    weights = E(fragmented_graph)$length_reach,
    mode = "all"
  )

  # Analyze pairwise connections
  n_total <- 0
  n_connected <- 0
  n_disconnected <- 0

  dist_connected_vals <- c()
  dist_disconnected_original <- c()

  # Also need original (unfragmented) distances for comparison
  distance_matrix_original <- distances(
    basin_graph,
    v = species_node_indices,
    to = species_node_indices,
    weights = E(basin_graph)$length_reach,
    mode = "all"
  )

  for (i in 1:(nrow(distance_matrix) - 1)) {
    for (j in (i + 1):ncol(distance_matrix)) {

      n_total <- n_total + 1
      dist_current <- distance_matrix[i, j]
      dist_original <- distance_matrix_original[i, j]

      if (is.finite(dist_current)) {
        n_connected <- n_connected + 1
        dist_connected_vals <- c(dist_connected_vals, dist_current)
      } else {
        n_disconnected <- n_disconnected + 1
        dist_disconnected_original <- c(dist_disconnected_original, dist_original)
      }
    }
  }

  # Component analysis
  comp <- components(fragmented_graph)

  # Statistical test
  t_test_p <- NA
  if (length(dist_connected_vals) > 1 && length(dist_disconnected_original) > 1) {
    t_result <- tryCatch(
      t.test(dist_connected_vals, dist_disconnected_original),
      error = function(e) NULL
    )
    if (!is.null(t_result)) {
      t_test_p <- t_result$p.value
    }
  }

  # Return results
  list(
    scenario = scenario_name,
    n_total = n_total,
    n_connected = n_connected,
    n_disconnected = n_disconnected,
    percent_lost = round(100 * n_disconnected / n_total, 2),
    dist_connected = dist_connected_vals,
    dist_disconnected_original = dist_disconnected_original,
    mean_dist_connected = ifelse(length(dist_connected_vals) > 0,
                                 mean(dist_connected_vals), NA),
    mean_dist_disconnected = ifelse(length(dist_disconnected_original) > 0,
                                    mean(dist_disconnected_original), NA),
    t_test_p = t_test_p,
    n_components = comp$no,
    n_edges_removed = n_edges_removed,
    graph = fragmented_graph
  )
}

# ============================================================
# STEP 6: Run both scenarios
# ============================================================

message("\n[5/9] Calculating fragmentation for both scenarios...")

# Scenario 1: Current state (existing dams)
message("  Scenario 1: Current state (existing dams)")
scenario_current <- calculate_fragmentation(
  basin_graph,
  species_node_indices,
  dam_subcatchments = unique(as.character(dams_existing$subc_id)),
  scenario_name = "Current"
)

# Scenario 2: Future state (existing + planned dams)
message("  Scenario 2: Future state (existing + planned dams)")
scenario_future <- calculate_fragmentation(
  basin_graph,
  species_node_indices,
  dam_subcatchments = unique(as.character(dams_all_future$subc_id)),
  scenario_name = "Future"
)

# ============================================================
# STEP 7: Create summary comparison
# ============================================================

message("\n[6/9] Creating scenario comparison summary...")

summary_df <- data.frame(
  Scenario = c("Current (existing dams)", "Future (existing + planned)"),
  Total_pairs = c(scenario_current$n_total, scenario_future$n_total),
  Connected_pairs = c(scenario_current$n_connected, scenario_future$n_connected),
  Disconnected_pairs = c(scenario_current$n_disconnected, scenario_future$n_disconnected),
  Percent_lost = c(scenario_current$percent_lost, scenario_future$percent_lost),
  N_components = c(scenario_current$n_components, scenario_future$n_components),
  N_dams = c(nrow(dams_existing), nrow(dams_all_future)),
  Mean_dist_connected = c(scenario_current$mean_dist_connected,
                          scenario_future$mean_dist_connected),
  Mean_dist_disconnected = c(scenario_current$mean_dist_disconnected,
                             scenario_future$mean_dist_disconnected)
)

print(summary_df)

write.csv(summary_df,
          "connectivity/scenario_comparison_summary.csv",
          row.names = FALSE)

# Calculate additional impact of planned dams
additional_disconnected <- scenario_future$n_disconnected - scenario_current$n_disconnected
additional_percent <- scenario_future$percent_lost - scenario_current$percent_lost

message("\nAdditional impact of planned dams:")
message("  Additional pairs disconnected: ", additional_disconnected)
message("  Additional connectivity loss: ", round(additional_percent, 1), " percentage points")

# ============================================================
# FINAL SUMMARY
# ============================================================

message("\n=== SCENARIO ANALYSIS COMPLETE ===")
message("Basin: ", TARGET_BASIN_ID)
message("Species: ", EXAMPLE_SPECIES)
message("\nScenario Comparison:")
message("  CURRENT STATE (existing dams):")
message("     - Dams: ", nrow(dams_existing))
message("     - Disconnected pairs: ", scenario_current$n_disconnected, " / ", scenario_current$n_total)
message("     - Connectivity loss: ", scenario_current$percent_lost, "%")
message("     - Network components: ", scenario_current$n_components)
message("")
message("  FUTURE STATE (existing + planned dams):")
message("     - Dams: ", nrow(dams_all_future), " (+", nrow(dams_planned), " planned)")
message("     - Disconnected pairs: ", scenario_future$n_disconnected, " / ", scenario_future$n_total)
message("     - Connectivity loss: ", scenario_future$percent_lost, "%")
message("     - Network components: ", scenario_future$n_components)
message("")
message("  ADDITIONAL IMPACT OF PLANNED DAMS:")
message("     - Additional pairs disconnected: +", additional_disconnected)
message("     - Additional connectivity loss: +", round(additional_percent, 1), " percentage points")
message("     - Additional components created: +",
        scenario_future$n_components - scenario_current$n_components)
message("\nConservation message:")
message("  Preventing construction of ", nrow(dams_planned), " planned dams would avoid")
message("  disconnecting ", additional_disconnected, " additional population pairs.")
message("\nOutput Files:")
message("  • connectivity/scenario_comparison_summary.csv")
message("  • connectivity/scenario_comparison_bars.png")
message("  • connectivity/Figure_Scenarios_MEE.png")
message("  • connectivity/scenario_comparison_map.html")
message("  • connectivity/Figure_Scenarios_caption.txt")
message("\nAnalysis complete: ", Sys.time())
