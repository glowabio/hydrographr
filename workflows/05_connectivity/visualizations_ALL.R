library(sf)
library(leaflet)
library(RColorBrewer)
library(htmlwidgets)

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)


basin_file <- "/home/grigoropoulou/Documents/Postdoc/projects/hydrographr_use_case_greece/r.watershed/basin_tiles20d/basin_greece.gpkg"
basins <- read_geopackage(basin_file, import_as = "sf",subc_id = basin_summary$basin_id,
                          name = "ID")


# Get basin centroids from stream network
basin_centroids <- basins %>%
  group_by(ID) %>%
  summarize(geometry = st_union(geom)) %>%
  st_centroid() %>%
  st_transform(4326)

# Join with results
basin_map_data <- basin_summary %>%
  left_join(
    basin_centroids %>% st_drop_geometry() %>%
      mutate(lon = st_coordinates(basin_centroids)[,1],
             lat = st_coordinates(basin_centroids)[,2]),
    by = c("basin_id" = "ID")
  )





# CRITICAL FIX: Single color palette using the MAXIMUM value across BOTH scenarios
max_loss <- max(c(basin_summary$mean_loss_existing,
                  basin_summary$mean_loss_future), na.rm = TRUE)
# Create THREE layers:
# 1. Existing (baseline)
# 2. Future (total impact)
# 3. Change (additional impact from planned)

# Color palette for change (diverging)
pal_change <- colorNumeric(
  palette = "RdYlGn",
  domain = c(0, max(basin_summary$mean_additional_loss, na.rm = TRUE)),
  reverse = TRUE  # Red = high additional loss
)

# Color palette for absolute loss (consistent)
pal_abs <- colorNumeric(
  palette = "YlOrRd",
  domain = c(0, max_loss)
)

greece_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%

  # Layer 1: Existing scenario (yellow-red)
  addCircleMarkers(
    data = basin_map_data,
    lng = ~lon, lat = ~lat,
    radius = ~sqrt(n_species) * 3,
    color = "black",
    fillColor = ~pal_abs(mean_loss_existing),
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 1,
    group = "Current State",
    popup = ~paste0(
      "<b>Basin:</b> ", basin_id, "<br>",
      "<b>Species:</b> ", n_species, "<br>",
      "<b>Existing dams:</b> ", total_dams_existing, "<br>",
      "<b>Current loss:</b> ", round(mean_loss_existing, 1), "%"
    )
  ) %>%

  # Layer 2: Future scenario (same scale)
  addCircleMarkers(
    data = basin_map_data,
    lng = ~lon, lat = ~lat,
    radius = ~sqrt(n_species) * 3,
    color = "black",
    fillColor = ~pal_abs(mean_loss_future),
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 1,
    group = "Future State",
    popup = ~paste0(
      "<b>Basin:</b> ", basin_id, "<br>",
      "<b>Species:</b> ", n_species, "<br>",
      "<b>Total dams:</b> ", total_dams_existing + total_dams_planned,
      " (+", total_dams_planned, " planned)<br>",
      "<b>Future loss:</b> ", round(mean_loss_future, 1), "%"
    )
  ) %>%

  # Layer 3: Additional impact (different palette!)
  addCircleMarkers(
    data = basin_map_data,
    lng = ~lon, lat = ~lat,
    radius = ~sqrt(n_species) * 3,
    color = "black",
    fillColor = ~pal_change(mean_additional_loss),
    fillOpacity = 0.8,
    stroke = TRUE,
    weight = 1,
    group = "Additional Impact (Planned Dams)",
    popup = ~paste0(
      "<b>Basin:</b> ", basin_id, "<br>",
      "<b>Species:</b> ", n_species, "<br>",
      "<b>Planned dams:</b> ", total_dams_planned, "<br>",
      "<b>Additional loss:</b> <span style='color:red'>+",
      round(mean_additional_loss, 1), "%</span><br>",
      "<b>From:</b> ", round(mean_loss_existing, 1),
      "% → ", round(mean_loss_future, 1), "%"
    )
  ) %>%

  # Legend for Current/Future
  addLegend(
    "bottomright",
    pal = pal_abs,
    values = c(0, max_loss),
    title = "Connectivity loss (%)<br>(Current & Future)",
    opacity = 0.7,
    group = c("Current State", "Future State")
  ) %>%

  # Legend for Additional Impact
  addLegend(
    "bottomleft",
    pal = pal_change,
    values = c(0, max(basin_summary$mean_additional_loss, na.rm = TRUE)),
    title = "Additional impact<br>from planned dams (%)",
    opacity = 0.7,
    group = "Additional Impact (Planned Dams)"
  ) %>%

  # Layer controls
  addLayersControl(
    baseGroups = c("Current State", "Future State", "Additional Impact (Planned Dams)"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # Info box
  addControl(
    html = paste0(
      "<div style='background: white; padding: 10px; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);'>",
      "<h4 style='margin: 0 0 10px 0;'>Dam Impact Scenarios - Greece</h4>",
      "<p style='margin: 0; font-size: 12px;'>",
      "<b>Current:</b> ", sum(basin_summary$total_dams_existing), " existing dams<br>",
      "<b>Future:</b> +", sum(basin_summary$total_dams_planned), " planned dams<br>",
      "<b>Circle size</b> = number of species<br>",
      "<b>Circle color</b> = connectivity loss",
      "</p></div>"
    ),
    position = "topright"
  )

saveWidget(greece_map, "connectivity/connectivity_loss_greece.html")
save_to_nimbus(greece_map, "connectivity/greece_scenario_map.html")
