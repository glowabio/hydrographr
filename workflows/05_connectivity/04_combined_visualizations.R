#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03 -- combined_visualizations.R
# Create publication-ready figures combining fragmentation and impact zone
# Reads analysis outputs from scripts 01 and 02
#
# READS:
#   - connectivity/scenario_comparison_summary.csv (from script 01)
#   - connectivity/impact_zone_summary.csv (from script 02)
#   - connectivity/impact_zone_by_dam.gpkg (from script 02)
#   - spatial/stream_networks/partial_stream_network.gpkg
#   - points_snapped/dams/dams_snapped_points.csv
#   - points_snapped/fish/fish_all_species_snapped.csv
#   - spatial/stream_networks/river_graph.RDS
#
# CREATES:
#   - connectivity/Figure_Combined_MEE.png (4-panel combined figure)
#   - connectivity/Figure_Combined_MEE.tiff (for publication)
#   - connectivity/combined_impact_map.html (interactive map)
#   - connectivity/combined_summary_table.csv (integrated metrics)
#   - connectivity/Figure_Combined_caption.txt (figure caption)
#   - connectivity/scenario_comparison_bars.png (from script 01)
#   - connectivity/Figure_Scenarios_MEE.png (from script 01)
#   - connectivity/scenario_comparison_map.html (from script 01)
#
# LOCATION: workflows/04_network_analysis/03_combined_visualizations.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(sf)
library(data.table)
library(leaflet)
library(htmlwidgets)
library(igraph)

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

TARGET_BASIN_ID <- 1292502
EXAMPLE_SPECIES <- "Barbus_prespensis"

message("=== COMBINED IMPACT VISUALIZATIONS ===")
message("Basin: ", TARGET_BASIN_ID)
message("Species: ", EXAMPLE_SPECIES)
message("Date: ", Sys.Date())

# ============================================================
# STEP 1: Load analysis results
# ============================================================

message("\n[1/6] Loading analysis results...")

# Fragmentation results (from script 01)
fragmentation_summary <- read.csv("connectivity/scenario_comparison_summary.csv")

# Impact zone results (from script 02)
impact_zone_summary <- read.csv("connectivity/impact_zone_summary.csv")

# Impact zone spatial data (from script 02)
impact_zone_all <- st_read("connectivity/impact_zone_by_dam.gpkg", quiet = TRUE)

message("  Fragmentation data loaded")
message("  Impact zone data loaded")

# ============================================================
# STEP 2: Load spatial data for maps
# ============================================================

message("\n[2/6] Loading spatial data...")

# River network
river_graph <- readRDS("spatial/stream_networks/river_graph.RDS")

node_data <- data.frame(
  name = V(river_graph)$name,
  basin_id = V(river_graph)$basin_id
) %>%
  filter(!is.na(basin_id))

basin_nodes <- node_data %>%
  filter(basin_id == TARGET_BASIN_ID) %>%
  pull(name)

stream_spatial <- st_read("spatial/stream_networks/partial_stream_network.gpkg", quiet = TRUE)

basin_streams <- stream_spatial %>%
  filter(subc_id %in% basin_nodes) %>%
  st_transform(4326)

# Dams
dams_all <- fread("points_snapped/dams/dams_snapped_points.csv")

dams_basin <- dams_all %>%
  filter(as.character(subc_id) %in% basin_nodes)

dams_existing <- dams_basin %>%
  filter(status == "existing")

dams_planned <- dams_basin %>%
  filter(status == "planned") %>%
  filter(phase != "R")

dams_existing_sf <- dams_existing %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

dams_planned_sf <- dams_planned %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

# Species
fish_all <- fread("points_snapped/fish/fish_all_species_snapped.csv")

species_occurrences <- fish_all %>%
  filter(as.character(subc_id) %in% basin_nodes) %>%
  filter(species == EXAMPLE_SPECIES | species == gsub("_", " ", EXAMPLE_SPECIES))

species_sf <- species_occurrences %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

# Impact zones
impact_zone_existing <- impact_zone_all %>%
  filter(dam_status == "existing")

impact_zone_planned <- impact_zone_all %>%
  filter(dam_status == "planned")

# Dam-affected streams
dam_existing_streams <- basin_streams %>%
  filter(subc_id %in% unique(as.character(dams_existing$subc_id)))

dam_planned_streams <- basin_streams %>%
  filter(subc_id %in% unique(as.character(dams_planned$subc_id)))

message("  Spatial data prepared")

# ============================================================
# STEP 3: Create combined summary table
# ============================================================

message("\n[3/6] Creating combined summary table...")

combined_summary <- data.frame(
  Basin_ID = TARGET_BASIN_ID,
  Species = EXAMPLE_SPECIES,

  # Existing Dams Scenario
  Existing_Dams_N = fragmentation_summary$N_dams[1],
  Existing_Habitat_Affected_km = round(impact_zone_summary$affected_length_existing_m / 1000, 2),
  Existing_Habitat_Affected_Pct = round(100 * impact_zone_summary$proportion_affected_existing, 1),
  Existing_Pairs_Disconnected = fragmentation_summary$Disconnected_pairs[1],
  Existing_Connectivity_Loss_Pct = fragmentation_summary$Percent_lost[1],
  Existing_Network_Components = fragmentation_summary$N_components[1],

  # Future Scenario (Existing + Planned)
  Future_Dams_N = fragmentation_summary$N_dams[2],
  Future_Habitat_Affected_km = round(impact_zone_summary$affected_length_all_m / 1000, 2),
  Future_Habitat_Affected_Pct = round(100 * impact_zone_summary$proportion_affected_all, 1),
  Future_Pairs_Disconnected = fragmentation_summary$Disconnected_pairs[2],
  Future_Connectivity_Loss_Pct = fragmentation_summary$Percent_lost[2],
  Future_Network_Components = fragmentation_summary$N_components[2],

  # Additional Impact of Planned Dams
  Additional_Habitat_Affected_km = round(impact_zone_summary$additional_affected_m / 1000, 2),
  Additional_Habitat_Affected_Pct = round(100 * impact_zone_summary$additional_proportion, 1),
  Additional_Pairs_Disconnected = fragmentation_summary$Disconnected_pairs[2] -
    fragmentation_summary$Disconnected_pairs[1],
  Additional_Connectivity_Loss_Pct = round(fragmentation_summary$Percent_lost[2] -
                                             fragmentation_summary$Percent_lost[1], 1)
)

# Transpose for readability
combined_summary_t <- t(combined_summary)
colnames(combined_summary_t) <- "Value"

write.csv(combined_summary_t,
          "connectivity/combined_summary_table.csv",
          row.names = TRUE)

print(combined_summary_t)

message("  Saved: connectivity/combined_summary_table.csv")

# ============================================================
# STEP 4: Create combined MEE figure (4 panels)
# ============================================================

message("\n[4/6] Creating combined MEE figure...")

png("connectivity/Figure_Combined_MEE.png",
    width = 173, height = 140, units = "mm", res = 300)

layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))

# Extract values for plotting
habitat_affected <- c(
  100 * impact_zone_summary$proportion_affected_existing,
  100 * impact_zone_summary$proportion_affected_all
)

conn_loss <- c(
  fragmentation_summary$Percent_lost[1],
  fragmentation_summary$Percent_lost[2]
)

pairs_lost <- c(
  fragmentation_summary$Disconnected_pairs[1],
  fragmentation_summary$Disconnected_pairs[2]
)

additional_metrics <- c(
  fragmentation_summary$Percent_lost[2] - fragmentation_summary$Percent_lost[1],
  100 * impact_zone_summary$additional_proportion
)

# ------------------------------------------------------------
# Panel A: Connectivity Loss
# ------------------------------------------------------------

par(mar = c(5, 4.5, 3, 0.5))

barplot(conn_loss,
        names.arg = c("Current", "Future"),
        col = c("#7fbf7b", "#FC8D62"),
        ylab = "Connectivity loss (%)",
        main = "(a) Population Connectivity",
        cex.lab = 1.2,
        cex.main = 1.4,
        cex.names = 1.1,
        cex.axis = 1.0,
        ylim = c(0, max(conn_loss) * 1.3),
        border = "white",
        las = 1)

text(c(0.7, 1.9),
     conn_loss + max(conn_loss) * 0.05,
     paste0(round(conn_loss, 1), "%"),
     cex = 1.2,
     font = 2)

mtext(paste(nrow(dams_existing), "dams          ",
            nrow(dams_existing) + nrow(dams_planned), "dams"),
      side = 1, line = 3.5, cex = 0.85, font = 3)

# ------------------------------------------------------------
# Panel B: Habitat Affected
# ------------------------------------------------------------

par(mar = c(5, 4.5, 3, 0.5))

barplot(habitat_affected,
        names.arg = c("Current", "Future"),
        col = c("#7fbf7b", "#FC8D62"),
        ylab = "Habitat affected (%)",
        main = "(b) Habitat Impact",
        cex.lab = 1.2,
        cex.main = 1.4,
        cex.names = 1.1,
        cex.axis = 1.0,
        ylim = c(0, max(habitat_affected) * 1.3),
        border = "white",
        las = 1)

text(c(0.7, 1.9),
     habitat_affected + max(habitat_affected) * 0.05,
     paste0(round(habitat_affected, 1), "%"),
     cex = 1.2,
     font = 2)

mtext(paste(round(impact_zone_summary$affected_length_existing_m/1000, 1), "km          ",
            round(impact_zone_summary$affected_length_all_m/1000, 1), "km"),
      side = 1, line = 3.5, cex = 0.85, font = 3)

# ------------------------------------------------------------
# Panel C: Disconnected Pairs
# ------------------------------------------------------------

par(mar = c(5, 4.5, 3, 0.5))

barplot(pairs_lost,
        names.arg = c("Current", "Future"),
        col = c("#7fbf7b", "#FC8D62"),
        ylab = "Disconnected pairs",
        main = "(c) Population Isolation",
        cex.lab = 1.2,
        cex.main = 1.4,
        cex.names = 1.1,
        cex.axis = 1.0,
        ylim = c(0, max(pairs_lost) * 1.3),
        border = "white",
        las = 1)

text(c(0.7, 1.9),
     pairs_lost + max(pairs_lost) * 0.05,
     pairs_lost,
     cex = 1.2,
     font = 2)

# ------------------------------------------------------------
# Panel D: Additional Impact Comparison
# ------------------------------------------------------------

par(mar = c(5, 4.5, 3, 0.5))

barplot(additional_metrics,
        names.arg = c("Connectivity\nloss", "Habitat\naffected"),
        col = "#FC8D62",
        ylab = "Additional impact (% points)",
        main = "(d) Planned Dams Impact",
        cex.lab = 1.2,
        cex.main = 1.4,
        cex.names = 1.0,
        cex.axis = 1.0,
        ylim = c(0, max(additional_metrics) * 1.3),
        border = "white",
        las = 1)

text(c(0.7, 1.9),
     additional_metrics + max(additional_metrics) * 0.05,
     paste0("+", round(additional_metrics, 1), "%"),
     cex = 1.2,
     font = 2,
     col = "darkred")

dev.off()

message("  Saved: connectivity/Figure_Combined_MEE.png")

# Create TIFF version
tiff("connectivity/Figure_Combined_MEE.tiff",
     width = 173, height = 140, units = "mm", res = 300,
     compression = "lzw")

# Repeat same plotting code
layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))

# Panel A
par(mar = c(5, 4.5, 3, 0.5))
barplot(conn_loss, names.arg = c("Current", "Future"),
        col = c("#7fbf7b", "#FC8D62"), ylab = "Connectivity loss (%)",
        main = "(a) Population Connectivity", cex.lab = 1.2, cex.main = 1.4,
        cex.names = 1.1, cex.axis = 1.0, ylim = c(0, max(conn_loss) * 1.3),
        border = "white", las = 1)
text(c(0.7, 1.9), conn_loss + max(conn_loss) * 0.05,
     paste0(round(conn_loss, 1), "%"), cex = 1.2, font = 2)
mtext(paste(nrow(dams_existing), "dams          ",
            nrow(dams_existing) + nrow(dams_planned), "dams"),
      side = 1, line = 3.5, cex = 0.85, font = 3)

# Panel B
par(mar = c(5, 4.5, 3, 0.5))
barplot(habitat_affected, names.arg = c("Current", "Future"),
        col = c("#7fbf7b", "#FC8D62"), ylab = "Habitat affected (%)",
        main = "(b) Habitat Impact", cex.lab = 1.2, cex.main = 1.4,
        cex.names = 1.1, cex.axis = 1.0, ylim = c(0, max(habitat_affected) * 1.3),
        border = "white", las = 1)
text(c(0.7, 1.9), habitat_affected + max(habitat_affected) * 0.05,
     paste0(round(habitat_affected, 1), "%"), cex = 1.2, font = 2)
mtext(paste(round(impact_zone_summary$affected_length_existing_m/1000, 1), "km          ",
            round(impact_zone_summary$affected_length_all_m/1000, 1), "km"),
      side = 1, line = 3.5, cex = 0.85, font = 3)

# Panel C
par(mar = c(5, 4.5, 3, 0.5))
barplot(pairs_lost, names.arg = c("Current", "Future"),
        col = c("#7fbf7b", "#FC8D62"), ylab = "Disconnected pairs",
        main = "(c) Population Isolation", cex.lab = 1.2, cex.main = 1.4,
        cex.names = 1.1, cex.axis = 1.0, ylim = c(0, max(pairs_lost) * 1.3),
        border = "white", las = 1)
text(c(0.7, 1.9), pairs_lost + max(pairs_lost) * 0.05,
     pairs_lost, cex = 1.2, font = 2)

# Panel D
par(mar = c(5, 4.5, 3, 0.5))
barplot(additional_metrics, names.arg = c("Connectivity\nloss", "Habitat\naffected"),
        col = "#FC8D62", ylab = "Additional impact (% points)",
        main = "(d) Planned Dams Impact", cex.lab = 1.2, cex.main = 1.4,
        cex.names = 1.0, cex.axis = 1.0, ylim = c(0, max(additional_metrics) * 1.3),
        border = "white", las = 1)
text(c(0.7, 1.9), additional_metrics + max(additional_metrics) * 0.05,
     paste0("+", round(additional_metrics, 1), "%"),
     cex = 1.2, font = 2, col = "darkred")

dev.off()

message("  Saved: connectivity/Figure_Combined_MEE.tiff")

# ============================================================
# Create combined figure caption
# ============================================================

caption_combined <- paste0(
  "Integrated dam impact analysis on ", EXAMPLE_SPECIES, " in basin ", TARGET_BASIN_ID,
  " combining population connectivity and habitat degradation metrics under current and future dam scenarios. ",

  "(a) Population connectivity loss: Dams disconnect ", fragmentation_summary$Percent_lost[1], "% ",
  "of population pairs under current conditions (", nrow(dams_existing), " existing dams), ",
  "increasing to ", fragmentation_summary$Percent_lost[2], "% if all ", nrow(dams_planned),
  " planned dams are constructed (", nrow(dams_existing) + nrow(dams_planned), " total dams). ",

  "(b) Habitat impact: Dam footprints (", service_area_summary$upstream_radius_m, " m upstream, ",
  service_area_summary$downstream_radius_m, " m downstream) affect ",
  round(100 * service_area_summary$proportion_affected_existing, 1), "% of total habitat ",
  "(", round(service_area_summary$affected_length_existing_m / 1000, 1), " km) under current conditions, ",
  "increasing to ", round(100 * service_area_summary$proportion_affected_all, 1), "% ",
  "(", round(service_area_summary$affected_length_all_m / 1000, 1), " km) with planned dam construction. ",

  "(c) Population isolation: Number of disconnected population pairs increases from ",
  pairs_lost[1], " (current) to ", pairs_lost[2], " (future), representing ",
  pairs_lost[2] - pairs_lost[1], " additional isolated populations. ",

  "(d) Cumulative impact of planned dams: Construction of ", nrow(dams_planned), " planned dams would cause ",
  "an additional ", round(additional_metrics[1], 1), " percentage point loss in connectivity and ",
  round(additional_metrics[2], 1), " percentage point increase in habitat degradation. ",

  "The combined analysis reveals compound effects: planned dams would simultaneously increase ",
  "both direct habitat loss (physical footprint) and functional habitat loss (population isolation). ",
  "Dam footprints represent immediate physical impacts within service areas, while connectivity loss ",
  "represents network-scale functional fragmentation that can extend far beyond dam locations. ",
  "This dual impact threatens metapopulation viability through both reduced habitat quantity and ",
  "compromised population connectivity essential for genetic exchange and recolonization."
)

writeLines(caption_combined, "connectivity/Figure_Combined_caption.txt")

message("  Saved: connectivity/Figure_Combined_caption.txt")





# ============================================================
# STEP 5: Create standalone fragmentation figures (from script 01)
# ============================================================

message("\n[5/6] Creating standalone fragmentation figures...")

# Figure: Side-by-side comparison bars
png("connectivity/scenario_comparison_bars.png",
    width = 14, height = 6, units = "in", res = 300)

par(mfrow = c(1, 3), mar = c(6, 5, 4, 2))

# Panel A: Connectivity loss
barplot(conn_loss,
        names.arg = c("Current\n(existing)", "Future\n(+ planned)"),
        col = c("#7fbf7b", "#FC8D62"),
        ylab = "Connectivity loss (%)",
        main = "(a) Connectivity Loss",
        cex.lab = 1.4, cex.main = 1.6, cex.names = 1.3, cex.axis = 1.2,
        ylim = c(0, max(conn_loss) * 1.25), border = "white")
text(c(0.7, 1.9), conn_loss + max(conn_loss) * 0.05,
     paste0(conn_loss, "%"), cex = 1.4, font = 2)

# Panel B: Network components
barplot(fragmentation_summary$N_components,
        names.arg = c("Current\n(existing)", "Future\n(+ planned)"),
        col = c("#7fbf7b", "#FC8D62"),
        ylab = "Network components",
        main = "(b) Network Fragmentation",
        cex.lab = 1.4, cex.main = 1.6, cex.names = 1.3, cex.axis = 1.2,
        ylim = c(0, max(fragmentation_summary$N_components) * 1.25), border = "white")
text(c(0.7, 1.9), fragmentation_summary$N_components + max(fragmentation_summary$N_components) * 0.05,
     fragmentation_summary$N_components, cex = 1.4, font = 2)

# Panel C: Disconnected pairs
barplot(pairs_lost,
        names.arg = c("Current\n(existing)", "Future\n(+ planned)"),
        col = c("#7fbf7b", "#FC8D62"),
        ylab = "Disconnected pairs",
        main = "(c) Lost Connections",
        cex.lab = 1.4, cex.main = 1.6, cex.names = 1.3, cex.axis = 1.2,
        ylim = c(0, max(pairs_lost) * 1.25), border = "white")
text(c(0.7, 1.9), pairs_lost + max(pairs_lost) * 0.05,
     pairs_lost, cex = 1.4, font = 2)

dev.off()

message("  Saved: connectivity/scenario_comparison_bars.png")

# ============================================================
# STEP 6: Create combined interactive map
# ============================================================

message("\n[6/6] Creating combined interactive map...")
# Separate impact zones
impact_zone_existing_only <- impact_zone_all %>%
  filter(dam_status == "existing")

impact_zone_planned_only <- impact_zone_all %>%
  filter(dam_status == "planned")

# Create map
service_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  # Original network
  addPolylines(
    data = basin_streams,
    color = "#1f78b4",
    weight = 2,
    opacity = 0.6,
    group = "River Network",
    popup = ~paste0("Subcatchment: ", subc_id, "<br>",
                    "Length: ", round(length, 1), " m")
  ) %>%
  # Impact zone - existing dams
  addPolylines(
    data = impact_zone_existing_only,
    color = "#d7191c",      # strong red = existing impact
    weight = 4,
    opacity = 0.8,
    dashArray = "5,5",
    group = "Impact Zone (Existing)",
    popup = ~paste0("Affected by existing dam<br>",
                    "Length: ", round(length, 1), " m")
  ) %>%
  # Impact zone - planned dams
  addPolylines(
    data = impact_zone_planned_only,
    color = "#fdae61",      # amber = future/planned impact
    weight = 4,
    opacity = 0.8,
    dashArray = "10,5",
    group = "Impact Zone (Planned)",
    popup = ~paste0("Affected by planned dam<br>",
                    "Length: ", round(length, 1), " m")
  ) %>%
  # Existing dams
  addCircleMarkers(
    data = dams_existing_sf,
    radius = 6,
    color = "#252525",      # near-black border
    fillColor = "#636363",  # mid-grey fill = existing structure
    fillOpacity = 0.9,
    stroke = TRUE,
    weight = 2,
    group = "Existing Dams",
    popup = ~paste0("Dam: ", ifelse(is.na(id1), "Unnamed", id1), "<br>",
                    "Status: Existing")
  ) %>%
  # Planned dams
  addCircleMarkers(
    data = if(nrow(dams_planned) > 0) dams_planned_sf else NULL,
    radius = 6,
    color = "#252525",      # same border as existing
    fillColor = "#bdbdbd",  # light grey fill = planned (lighter = not yet built)
    fillOpacity = 0.9,
    stroke = TRUE,
    weight = 3,
    group = "Planned Dams",
    popup = ~paste0("Dam: ", ifelse(is.na(id1), "Unnamed", id1), "<br>",
                    "Status: Planned")
  ) %>%
  # Species occurrences
  addCircleMarkers(
    data = species_sf,
    radius = 5,
    color = "#1b7837",      # dark green border
    fillColor = "#7fbf7b",  # mid green fill = biological
    fillOpacity = 0.8,
    stroke = TRUE,
    weight = 2,
    group = EXAMPLE_SPECIES,
    popup = ~paste0("Species: ", species, "<br>",
                    "Site: ", site_id)
  ) %>%
  # Layer controls
  addLayersControl(
    baseGroups = c("Light", "Satellite"),
    overlayGroups = c(
      "River Network",
      "Impact Zone (Existing)",
      "Impact Zone (Planned)",
      "Existing Dams",
      "Planned Dams",
      EXAMPLE_SPECIES
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Legend
  addLegend(
    position = "bottomright",
    colors = c("#1f78b4", "#d7191c", "#fdae61", "#636363", "#bdbdbd", "#7fbf7b"),
    labels = c(
      "River Network",
      paste0("Impact Zone (Existing): ", round(affected_length_existing / 1000, 1), " km"),
      paste0("Additional (Planned): ",
             round((affected_length_all - affected_length_existing) / 1000, 1), " km"),
      "Existing Dams",
      "Planned Dams",
      EXAMPLE_SPECIES
    ),
    opacity = 0.8,
    title = "Legend"
  ) %>%
  addScaleBar(position = "bottomleft") %>%
# Title
  addControl(
    html = paste0(
      "<div style='background: white; padding: 10px; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);'>",
      "<h3 style='margin: 0; color: #333;'>Dam Impact Zones - Basin ", TARGET_BASIN_ID, "</h3>",
      "<p style='margin: 5px 0 0 0; color: #666;'>",
      "<strong>Species:</strong> ", EXAMPLE_SPECIES, "<br>",
      "<strong>Impact zone:</strong> ", UPSTREAM_RADIUS, " m up, ", DOWNSTREAM_RADIUS, " m down<br>",
      "<strong>Existing dams:</strong> ", nrow(dams_existing),
      " (", round(100 * prop_existing, 1), "% habitat affected)<br>",
      "<strong>+ Planned:</strong> ", nrow(dams_existing) + nrow(dams_planned),  # FIXED
      " total (", round(100 * prop_all, 1), "% habitat affected)",
      "</p></div>"
    ),
    position = "topright"
  )

service_map

# Save map
# save_to_nimbus(service_map,
#                "connectivity/impact_zone_map.html")
saveWidget(service_map,
               "connectivity/impact_zone_map.html")

message("  Saved: connectivity/impact_zone_map.html")


# ============================================================
# FINAL SUMMARY
# ============================================================

message("\n=== VISUALIZATION CREATION COMPLETE ===")
message("Basin: ", TARGET_BASIN_ID)
message("Species: ", EXAMPLE_SPECIES)
message("\nFigures Created:")
message("  • connectivity/Figure_Combined_MEE.png (4-panel combined)")
message("  • connectivity/Figure_Combined_MEE.tiff (for publication)")
message("  • connectivity/scenario_comparison_bars.png (3-panel fragmentation)")
message("  • connectivity/combined_impact_map.html (interactive map)")
message("\nData Products:")
message("  • connectivity/combined_summary_table.csv")
message("  • connectivity/Figure_Combined_caption.txt")
message("\nKey Findings:")
message("  Current Impact:")
message("    - Connectivity lost: ", fragmentation_summary$Percent_lost[1], "%")
message("    - Habitat affected: ", round(100 * impact_zone_summary$proportion_affected_existing, 1), "%")
message("  Future Impact:")
message("    - Connectivity lost: ", fragmentation_summary$Percent_lost[2], "%")
message("    - Habitat affected: ", round(100 * impact_zone_summary$proportion_affected_all, 1), "%")
message("  Additional Impact of Planned Dams:")
message("    - +", round(additional_metrics[1], 1), "% connectivity loss")
message("    - +", round(additional_metrics[2], 1), "% habitat affected")
message("\nAnalysis complete: ", Sys.time())
