#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Study area map: raw fish and barrier points on OSM basemap,
# with Greece inset showing basin location.
#
# Input:
#   - points_cleaned/fish/fish_basin_hcmr.csv
#   - points_cleaned/fish/fish_gbif_clean.csv
#   - points_cleaned/dams/dams_sarantaporos_clean.csv
#   - spatial/subbasin_sarantaporos/subbasin_polygon.gpkg
#   - spatial/basin/basin_polygon.gpkg
#
# Output:
#   - figures/study_area_map.png
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(rnaturalearth)
library(patchwork)
library(data.table)

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

MARGIN <- 0.05

COL_SURVEY <- "#1b7837"   # dark green -- survey fish
COL_GBIF   <- "#4393c3"   # blue       -- GBIF fish
COL_DAM_E  <- "#e31a1c"   # bright red -- existing dam
COL_DAM_P  <- "#e31a1c"   # bright red -- planned dams
COL_BASIN  <- "#2166ac"   # blue       -- Aoos/Vjosa basin (inset)

# ============================================================
# LOAD DATA
# ============================================================

message("Loading data...")

fish_survey <- fread("points_cleaned/fish/fish_basin_hcmr.csv") %>%
  dplyr::distinct(Sites, longitude, latitude) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

fish_gbif <- fread("points_cleaned/fish/fish_gbif_clean.csv") %>%
  dplyr::distinct(decimalLongitude, decimalLatitude) %>%
  dplyr::rename(longitude = decimalLongitude, latitude = decimalLatitude) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

dams_existing <- fread("points_cleaned/dams/dams_sarantaporos_clean.csv") %>%
  dplyr::filter(status == "existing") %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

dams_planned <- fread("points_cleaned/dams/dams_sarantaporos_clean.csv") %>%
  dplyr::filter(status == "planned") %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

subbasin <- sf::st_read("spatial/subbasin_sarantaporos/subbasin_polygon.gpkg",
                        quiet = TRUE)
basin    <- sf::st_read("spatial/basin/basin_polygon.gpkg",
                        quiet = TRUE)

message("Data loaded.")

# ============================================================
# BOUNDING BOX
# ============================================================

bb_wgs <- sf::st_bbox(subbasin)
xlim   <- c(bb_wgs["xmin"] - MARGIN, bb_wgs["xmax"] + MARGIN)
ylim   <- c(bb_wgs["ymin"] - MARGIN, bb_wgs["ymax"] + MARGIN)

# ============================================================
# MAIN MAP
# ============================================================

message("Building main map...")

main_map <- ggplot() +

  # OSM basemap
  annotation_map_tile(type = "osm", zoom = 11, quiet = TRUE) +

  # Basin outline (dashed grey)
  geom_sf(data = sf::st_transform(basin, 4326),
          fill = NA, colour = "grey20",
          linewidth = 0.5, linetype = "dashed") +

  # Subbasin outline (solid dark)
  geom_sf(data = sf::st_transform(subbasin, 4326),
          fill = NA, colour = "grey10", linewidth = 1.0) +

  # GBIF fish -- blue circles
  geom_sf(data = fish_gbif,
          aes(colour = "GBIF"),
          shape = 21, fill = COL_GBIF, colour = COL_GBIF,
          size = 3.5, stroke = 0.4, alpha = 0.85) +

  # Survey fish -- green circles
  geom_sf(data = fish_survey,
          aes(colour = "Survey data"),
          shape = 21, fill = COL_SURVEY, colour = "white",
          size = 4.5, stroke = 0.6, alpha = 0.95) +

  # Planned dams -- bright red triangles
  geom_sf(data = dams_planned,
          aes(colour = "Planned dam"),
          shape = 24, fill = COL_DAM_P, colour = "white",
          size = 6.0, stroke = 0.4) +

  # Existing dam -- bright red triangle, black outline
  geom_sf(data = dams_existing,
          aes(colour = "Existing dam"),
          shape = 24, fill = COL_DAM_E, colour = "black",
          size = 7.5, stroke = 1.0) +

  # Colour scale drives the legend only; override.aes sets correct symbols
  scale_colour_manual(
    name   = NULL,
    values = c(
      "Survey data"  = COL_SURVEY,
      "GBIF"         = COL_GBIF,
      "Planned dam"  = COL_DAM_P,
      "Existing dam" = COL_DAM_E
    ),
    guide = guide_legend(
      override.aes = list(
        shape  = c(21,  21,  24,  24),
        fill   = c(COL_SURVEY, COL_GBIF, COL_DAM_P, COL_DAM_E),
        colour = c("white", COL_GBIF, "white", "black"),
        size   = c(4.5, 3.5, 6.0, 7.5),
        stroke = c(0.6, 0.4, 0.4, 1.0),
        alpha  = c(1, 1, 1, 1)
      )
    )
  ) +

  # Scale bar + north arrow
  annotation_scale(
    location = "tl", width_hint = 0.25,
    text_col = "grey10", line_col = "grey10",
    bar_cols = c("grey10", "white")
  ) +
  annotation_north_arrow(
    location = "tl",
    pad_x    = unit(0.15, "in"),
    pad_y    = unit(0.32, "in"),
    style    = north_arrow_fancy_orienteering(
      fill     = c("grey10", "white"),
      line_col = "grey10",
      text_col = "grey10"
    )
  ) +

  coord_sf(xlim = xlim, ylim = ylim, crs = 4326, expand = FALSE) +

  theme_void() +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background    = element_rect(fill = alpha("white", 0.80),
                                        colour = "grey60", linewidth = 0.3),
    legend.text          = element_text(colour = "grey10", size = 11),
    legend.key           = element_rect(fill = NA, colour = NA),
    legend.margin        = margin(6, 10, 6, 10),
    plot.margin          = margin(0, 0, 0, 0)
  )

# ============================================================
# INSET: Greece
# ============================================================

message("Building inset...")

greece    <- ne_countries(scale = "medium", country = "Greece",
                          returnclass = "sf")
albania    <- ne_countries(scale = "medium", country = "Albania",
                          returnclass = "sf")
neighbors <- ne_countries(scale = "medium",
                          country = c("North Macedonia", "Serbia",
                                      "Bulgaria", "Turkey", "Kosovo"),
                          returnclass = "sf")

subbasin_box <- sf::st_as_sfc(sf::st_bbox(subbasin)) %>%
  sf::st_set_crs(4326)

greece_panel <- ggplot() +
  geom_sf(data = neighbors, fill = "grey88", colour = "grey65",
          linewidth = 0.2) +
  geom_sf(data = greece,    fill = "grey75", colour = "grey45",
          linewidth = 0.3) +
  geom_sf(data = albania,    fill = "grey75", colour = "grey45",
          linewidth = 0.3) +
  geom_sf(data = basin,     fill = alpha(COL_BASIN, 0.40),
          colour = COL_BASIN, linewidth = 0.7) +
  geom_sf(data = subbasin_box, fill = NA,
          colour = "red", linewidth = 1.2) +
  coord_sf(xlim = c(18.8, 27.5), ylim = c(34.2, 42.6), expand = FALSE) +  theme_void() +
  theme(
    panel.background = element_rect(fill = "#d6eaf8", colour = NA),
    panel.border     = element_rect(fill = NA, colour = "grey35",
                                    linewidth = 0.6),
    plot.margin      = margin(0, 0, 0, 0)
  )

# ============================================================
# COMBINE + SAVE
# ============================================================

message("Combining...")

combined <- main_map + greece_panel +
  plot_layout(ncol = 2, widths = c(1, 0.7)) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(
    plot.tag          = element_text(size = 13, hjust = 0),
    plot.tag.position = c(0, 0.98)
  )

dir.create("figures", showWarnings = FALSE)

pdf("figures/study_area_map.pdf",
    bg = "white")
print(combined)
dev.off()

message("Saved: figures/study_area_map.png")
