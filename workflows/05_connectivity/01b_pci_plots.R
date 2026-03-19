#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 04_pci_visualizations.R
# Visualize PCI results: current vs future dam scenarios
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(data.table)
library(ggrepel)
library(leaflet)
library(sf)
library(htmlwidgets)
library(rnaturalearth)


# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")

BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# READ INPUTS
# ============================================================

fi_summary <- read.table("connectivity/pci/fi_summary.txt", header = TRUE)

basin_lookup <- fread("spatial/basin_name_lookup.csv") %>%
  mutate(basin_id = as.integer(basin_id))

fi_summary <- fi_summary %>%
  left_join(basin_lookup, by = "basin_id")

fish_dis_class <- fread("traits/fish_dis_class.txt")

# Join traits for coloring/faceting
fi_plot <- fi_summary %>%
  left_join(fish_dis_class %>%
              select(species, Migration_label, Caudal_fin_label,
                     dispersal_prob, max_TL),
            by = "species")


basin_polygons <- read_sf("spatial/stream_networks/basin_polygons.gpkg")

# ============================================================
# SCATTERPLOT: PCI current vs PCI future
# ============================================================

# Label only the most impacted points (top FI)
fi_plot <- fi_plot %>%
  mutate(label = ifelse(FI > 25,
                        paste0(gsub("_", " ", species), "\n(FI=", round(FI, 0), "%)"),
                        NA))

p1 <- ggplot(fi_plot, aes(x = PCI_current, y = PCI_future)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey50", linewidth = 0.5) +
  geom_point(aes(color = Migration_label), size = 2, alpha = 0.6) +
  geom_text_repel(aes(label = label), size = 2.5, max.overlaps = 15,
                  segment.color = "grey60", segment.size = 0.3,
                  na.rm = TRUE) +
  scale_color_manual(
    values = c("Non-migratory" = "#2196F3",
               "Potamodromous" = "#FF9800",
               "Long_distance" = "#F44336")
  ) +
  labs(x = "PCI (current dams)",
       y = "PCI (current + planned dams)",
       color = "Migration type") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_bw() +
  theme(legend.position = "right")

p1
ggsave("connectivity/pci/pci_current_vs_future.png",
       p1, width = 10, height = 8, dpi = 300)

cat("Plot saved: connectivity/pci/pci_current_vs_future.png\n")




# ============================================================
# TOP IMPACTED SPECIES × BASIN COMBINATIONS
# ============================================================

# Add readable basin names if available, otherwise use basin_id
# You can replace this with actual river names if you have a lookup table
fi_plot_top <- fi_summary %>%
  filter(FI > 0) %>%
  left_join(fish_dis_class %>%
              select(species, Migration_label),
            by = "species") %>%
  mutate(
    species_label = gsub("_", " ", species),
    # Combine species + basin for unique labels
    combo_label =paste0(species_label, " — ", basin_name)
  ) %>%
  arrange(desc(FI)) %>%
  head(25)


# Reorder factor by FI for plotting
fi_plot_top <- fi_plot_top %>%
  mutate(combo_label = fct_reorder(combo_label, FI))

p_top <- ggplot(fi_plot_top, aes(x = FI, y = combo_label, fill = Migration_label)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(
    values = c("Non-migratory" = "#2196F3",
               "Potamodromous" = "#FF9800",
               "Long_distance" = "#F44336")
  ) +
  labs(x = "Fragmentation Index (%)",
       y = NULL,
       fill = "Migration type",
       title = "Top 25 most impacted species × basin combinations",
       subtitle = "% connectivity loss from planned dams") +
  theme_bw() +
  theme(axis.text.y = element_text(face = "italic", size = 9))

p_top

ggsave("connectivity/pci/top_impacted_species.png",
       p_top, width = 10, height = 8, dpi = 300)

cat("Plot saved: connectivity/pci/top_impacted_species.png\n")


# ============================================================
# LEAFLET MAP: Mean FI by basin
# ============================================================

# Aggregate FI by basin
fi_by_basin <- fi_summary %>%
  group_by(basin_name, basin_id) %>%
  summarize(
    mean_FI = mean(FI, na.rm = TRUE),
    n_species = n(),
    n_impacted = sum(FI > 0, na.rm = TRUE),
    .groups = "drop"
  )

# Join with spatial data and transform to WGS84
basins_plot <- basin_polygons %>%
  left_join(fi_by_basin, by = "basin_id") %>%
  st_transform(4326)

# Color palette
pal_fill <- colorNumeric("YlOrRd", domain = basins_plot$mean_FI, na.color = "grey90")

# Build map
m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

  addPolygons(
    data = basins_plot,
    fillColor = ~pal_fill(mean_FI),
    fillOpacity = 0.7,
    color = "grey40",
    weight = 0.5,
    popup = ~paste0(
      "<b>Basin: </b>", basin_name, "<br>",
      "<b>Mean FI: </b>", round(mean_FI, 1), "%<br>",
      "<b>Species assessed: </b>", n_species, "<br>",
      "<b>Species impacted: </b>", n_impacted
    )
  ) %>%

  addLegend(
    position = "bottomright",
    pal = pal_fill,
    values = basins_plot$mean_FI,
    title = "Mean FI (%)",
    na.label = "No data"
  ) %>%

  addLayersControl(
    baseGroups = c("Light", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  )

m
saveWidget(m, "connectivity/pci/fi_map_by_basin.html", selfcontained = TRUE)
cat("Map saved: connectivity/pci/fi_map_by_basin.html\n")



#################
# same map but with ggplot
# ============================================================

# STATIC MAP: Mean FI by basin (ggplot)

# ============================================================



# Aggregate FI by basin

fi_by_basin <- fi_summary %>%
  group_by(basin_name, basin_id) %>%
  summarize(
    mean_FI = mean(FI, na.rm = TRUE),
    n_species = n(),
    n_impacted = sum(FI > 0, na.rm = TRUE),
    .groups = "drop"

  )

# Join with spatial data

basins_plot <- basin_polygons %>%
  left_join(fi_by_basin, by = "basin_id") %>%
  st_transform(4326)


# Get country boundaries
greece <- ne_countries(scale = 50, country = "Greece", returnclass = "sf")
neighbors <- ne_countries(scale = 50,
                          country = c("Turkey", "Bulgaria", "North Macedonia",
                                      "Albania", "Italy"),
                          returnclass = "sf")

# Get bounding box of your basins for map limits
bbox <- st_bbox(basins_plot)

p_map <- ggplot() +
  geom_sf(data = neighbors, fill = "grey95", color = "grey70", linewidth = 0.2) +
  geom_sf(data = greece, fill = "grey85", color = "grey50", linewidth = 0.3) +
  geom_sf(data = basins_plot, aes(fill = mean_FI), color = "grey40", linewidth = 0.2) +
  scale_fill_gradient(low = "#FFF3E0", high = "#BF360C", name = "Mean FI (%)",
                      na.value = "grey90") +
  coord_sf(xlim = c(bbox["xmin"] - 0.5, bbox["xmax"] + 0.5),
           ylim = c(bbox["ymin"] - 0.5, bbox["ymax"] + 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 8))
p_map

ggsave("connectivity/pci/fi_map_by_basin_static.png",
       p_map, width = 10, height = 8, dpi = 300)

#######################



# ============================================================
# BAR CHART: Species affected vs unaffected per basin
# ============================================================

fi_by_basin <- fi_summary %>%
  group_by(basin_id, basin_name) %>%
  summarize(
    n_impacted = sum(FI > 0, na.rm = TRUE),
    n_unaffected = sum(FI == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_impacted > 0) %>%
  pivot_longer(cols = c(n_impacted, n_unaffected),
               names_to = "status",
               values_to = "n_species") %>%
  mutate(
    basin_id = as.factor(basin_id),
    status = ifelse(status == "n_impacted", "Affected", "Unaffected")
  )

fi_by_basin <- fi_by_basin %>%
  mutate(basin_label = paste0(basin_name, " (", basin_id, ")"))

# order basins by number of affected species
basin_order <- fi_by_basin %>%
  filter(status == "Affected") %>%
  arrange(n_species) %>%
  pull(basin_label)

fi_by_basin <- fi_by_basin %>%
  mutate(basin_label = factor(basin_label, levels = basin_order))

p_affected <- ggplot(fi_by_basin, aes(x = basin_label, y = n_species, fill = status)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(values = c("Affected" = "#E07A5A", "Unaffected" = "#E8E0D5"),
                    name = NULL) +
  coord_flip() +
  labs(x = NULL,
       y = "Number of species",
       title = "Species affected by planned dams per river basin") +
  theme_bw() +
  theme(legend.position = "top")

p_affected
ggsave("connectivity/pci/species_affected_per_basin.png",
       p_affected, width = 8, height = 6, dpi = 300)
