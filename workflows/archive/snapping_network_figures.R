#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 00_placeholder_maps.R
# Workflow paper (Paper 1) — introductory placeholder figures
#
# Produces four figures for the SVG workflow diagram / manuscript:
#   Figure A  — map of occurrences (OpenStreetMap basemap + species points, raw)
#   Figure B  — map of barriers (OpenStreetMap basemap + dam points, raw)
#   Figure C  — overview map: basin polygon + pruned network + snapped species & dams
#   Figure D  — snapping inset map: basin + network + zoomed inset showing
#               raw (open circle) vs snapped (filled) positions, with legend
#
# READS:
#   points_snapped/fish/fish_all_species_snapped.csv
#   points_snapped/dams/dams_snapped_points.csv
#   spatial/subbasin/subbasin_polygon.gpkg
#   spatial/subbasin/stream_network_pruned.gpkg
#
# WRITES:
#   figures/placeholder/figA_species_occurrences.png
#   figures/placeholder/figB_barriers.png
#   figures/placeholder/figC_overview_network.png
#   figures/placeholder/figD_snapping_inset.png
#
# All figures: 173 mm wide (MEE double-column), 300 dpi
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(patchwork)

source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

dir.create("figures/placeholder", recursive = TRUE, showWarnings = FALSE)

# ── MEE figure dimensions ────────────────────────────────────────────────────
FIG_W_MM  <- 173
FIG_H_MM  <- 130
FIG_DPI   <- 300

# ── Colour palette (colour-blind friendly) ───────────────────────────────────
COL_NETWORK    <- "#4393c3"   # steel blue  — pruned stream reaches
COL_SPECIES    <- "#d6604d"   # warm red    — fish occurrences (raw)
COL_SNAPPED    <- "#1a9641"   # green       — snapped positions
COL_DAM_EX     <- "#252525"   # near-black  — existing dams
COL_DAM_PL     <- "#969696"   # grey        — planned dams
COL_BASIN      <- "#f7f7f7"   # near-white  — basin fill
COL_BASIN_BD   <- "#636363"   # basin border
COL_INSET_BOX  <- "#e08214"   # orange      — inset rectangle

# ── shared map theme ─────────────────────────────────────────────────────────
theme_map <- function(base_size = 9) {
  theme_void(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      plot.margin      = margin(4, 4, 4, 4),
      legend.position  = "bottom",
      legend.title     = element_text(size = 7, face = "bold"),
      legend.text      = element_text(size = 6),
      legend.key.size  = unit(3, "mm"),
      plot.title       = element_text(size = 8, face = "bold", hjust = 0.5),
      plot.subtitle    = element_text(size = 7, colour = "grey40", hjust = 0.5)
    )
}

save_fig <- function(p, filename, w = FIG_W_MM, h = FIG_H_MM) {
  ggsave(filename, p, width = w, height = h, units = "mm", dpi = FIG_DPI,
         bg = "white")
  message("Saved: ", filename)
}

# ============================================================
# LOAD DATA
# ============================================================

message("Loading data...")

# Fish — raw original coordinates
fish_raw <- read.csv("points_snapped/fish/fish_all_species_snapped.csv") %>%
  filter(!is.na(longitude_original), !is.na(latitude_original)) %>%
  st_as_sf(coords = c("longitude_original", "latitude_original"), crs = 4326)

# Fish — snapped coordinates
fish_snapped <- read.csv("points_snapped/fish/fish_all_species_snapped.csv") %>%
  filter(!is.na(longitude_snapped), !is.na(latitude_snapped)) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

# Dams — raw original coordinates
dams_raw <- read.csv("points_snapped/dams/dams_snapped_points.csv") %>%
  filter(!is.na(longitude_original), !is.na(latitude_original)) %>%
  mutate(type = if_else(
    tolower(status) %in% c("operational licence", "production licence"),
    "Existing", "Planned"
  )) %>%
  st_as_sf(coords = c("longitude_original", "latitude_original"), crs = 4326)

# Dams — snapped coordinates
dams_snapped <- read.csv("points_snapped/dams/dams_snapped_points.csv") %>%
  filter(!is.na(longitude_snapped), !is.na(latitude_snapped)) %>%
  mutate(type = if_else(
    tolower(status) %in% c("operational licence", "production licence"),
    "Existing", "Planned"
  )) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

# Basin polygon
basin_sf <- st_read("spatial/subbasin/subbasin_polygon.gpkg", quiet = TRUE) %>%
  st_transform(4326)

# Pruned stream network (Strahler >= 4, used in C & D)
network_sf <- st_read("spatial/subbasin/stream_network_pruned.gpkg", quiet = TRUE) %>%
  st_transform(4326)

network_full_sf <- st_read("spatial/subbasin/stream_network.gpkg", quiet = TRUE) %>%
  st_transform(4326)

message("Data loaded.")

# ============================================================
# FIGURE A — Species occurrence points
# Background: full open stream network (no basin polygon)
# ============================================================

message("\n=== Figure A: Species occurrences ===")

p_A <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 10, alpha = 1) +
  geom_sf(data = fish_raw,
          shape = 21, size = 2.8,
          fill = COL_SPECIES, colour = "white", stroke = 0.4,
          alpha = 1) +
  annotation_scale(location = "bl", width_hint = 0.25,
                   text_cex = 0.6, line_width = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(6, "mm"), width = unit(5, "mm"),
                         style = north_arrow_minimal()) +
  labs(
    title    = "Fish occurrence records",
    subtitle = paste0(nrow(fish_raw), " records · GBIF + HCMR")
  ) +
  theme_map()

save_fig(p_A, "figures/placeholder/figA_species_occurrences.png")

# ============================================================
# FIGURE B — Barrier points
# Background: full open stream network (no basin polygon)
# ============================================================

message("\n=== Figure B: Barriers ===")

dam_cols   <- c("Existing" = COL_DAM_EX, "Planned" = COL_DAM_PL)
dam_shapes <- c("Existing" = 24, "Planned" = 25)

p_B <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 10, alpha = 1) +
  geom_sf(data = dams_raw,
          aes(colour = type, fill = type, shape = type),
          size = 2.8, stroke = 0.5) +
  scale_colour_manual(values = dam_cols, name = "Dam status") +
  scale_fill_manual(values   = dam_cols, name = "Dam status") +
  scale_shape_manual(values  = dam_shapes, name = "Dam status") +
  annotation_scale(location = "bl", width_hint = 0.25,
                   text_cex = 0.6, line_width = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(6, "mm"), width = unit(5, "mm"),
                         style = north_arrow_minimal()) +
  labs(
    title    = "Hydropower barriers",
    subtitle = paste0(nrow(dams_raw), " dams · RAE + AMBER")
  ) +
  theme_map()

save_fig(p_B, "figures/placeholder/figB_barriers.png")

# ============================================================
# FIGURE C — Overview: basin + pruned network + snapped species & dams
# ============================================================

message("\n=== Figure C: Overview network map ===")

p_C <- ggplot() +
  geom_sf(data = basin_sf,
          fill = COL_BASIN, colour = COL_BASIN_BD, linewidth = 0.5) +
  geom_sf(data = network_full_sf,
          colour = COL_NETWORK, linewidth = 0.35, alpha = 0.8) +
  geom_sf(data = fish_snapped,
          shape = 21, size = 1.6,
          fill = COL_SPECIES, colour = "white", stroke = 0.35,
          alpha = 0.85) +
  geom_sf(data = dams_snapped,
          aes(shape = type),
          colour = COL_DAM_EX, fill = COL_DAM_EX,
          size = 2.4, stroke = 0.5) +
  scale_shape_manual(values = c("Existing" = 24, "Planned" = 25),
                     name = "Dam status") +
  annotation_scale(location = "bl", width_hint = 0.25,
                   text_cex = 0.6, line_width = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(6, "mm"), width = unit(5, "mm"),
                         style = north_arrow_minimal()) +
  labs(
    title    = "Sarantaporos and Voidomatis sub-basin — full stream network",
    subtitle = "Strahler ≥ 2 network · snapped species (circles) & barriers (triangles)"
  ) +
  theme_map()

save_fig(p_C, "figures/placeholder/figC_overview_full_network.png")


message("\n=== Figure C: Overview network map ===")

p_C <- ggplot() +
  geom_sf(data = basin_sf,
          fill = COL_BASIN, colour = COL_BASIN_BD, linewidth = 0.5) +
  geom_sf(data = network_sf,
          colour = COL_NETWORK, linewidth = 0.35, alpha = 0.8) +
  geom_sf(data = fish_snapped,
          shape = 21, size = 1.6,
          fill = COL_SPECIES, colour = "white", stroke = 0.35,
          alpha = 0.85) +
  geom_sf(data = dams_snapped,
          aes(shape = type),
          colour = COL_DAM_EX, fill = COL_DAM_EX,
          size = 2.4, stroke = 0.5) +
  scale_shape_manual(values = c("Existing" = 24, "Planned" = 25),
                     name = "Dam status") +
  annotation_scale(location = "bl", width_hint = 0.25,
                   text_cex = 0.6, line_width = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(6, "mm"), width = unit(5, "mm"),
                         style = north_arrow_minimal()) +
  labs(
    title    = "Sarantaporos and Voidomatis sub-basin — pruned stream network",
    subtitle = "Strahler ≥ 4 network · snapped species (circles) & barriers (triangles)"
  ) +
  theme_map()

save_fig(p_C, "figures/placeholder/figC_overview_network.png")

# ============================================================
# FIGURE D — Snapping inset map
# Main panel: basin + pruned network + snapped points + orange inset box
# Inset: tight zoom, raw (open circle, red) vs snapped (filled, green) + legend
# ============================================================

message("\n=== Figure D: Snapping inset map ===")

# --- define inset bounding box ---
# Centred on the spatial mean of snapped fish; tighter than before
cx <- 20.9692849
cy <- 40.2074501
INSET_HALF_DEG <- 0.001  # ~100 m — tight enough to see individual reaches

inset_box <- st_bbox(c(
  xmin = cx - INSET_HALF_DEG, xmax = cx + INSET_HALF_DEG,
  ymin = cy - INSET_HALF_DEG, ymax = cy + INSET_HALF_DEG
), crs = 4326) %>% st_as_sfc()

# clip layers to inset extent
network_inset <- st_crop(network_sf,   inset_box)
fish_raw_in   <- st_crop(fish_raw,     inset_box)
fish_snap_in  <- st_crop(fish_snapped, inset_box)
dams_snap_in  <- st_crop(dams_snapped, inset_box)

# displacement lines: raw → snapped
# rows are aligned because both sf objects come from the same CSV (same row order)
make_snap_lines <- function(raw_sf, snapped_sf) {
  n <- min(nrow(raw_sf), nrow(snapped_sf))
  if (n == 0) return(NULL)
  coords_raw  <- st_coordinates(raw_sf[seq_len(n), ])
  coords_snap <- st_coordinates(snapped_sf[seq_len(n), ])
  lines <- lapply(seq_len(n), function(i) {
    st_linestring(rbind(coords_raw[i, ], coords_snap[i, ]))
  })
  st_sf(geometry = st_sfc(lines, crs = 4326))
}

snap_lines_in <- make_snap_lines(fish_raw_in, fish_snap_in)

# ---- main panel ----
p_D_main <- ggplot() +
  geom_sf(data = basin_sf,
          fill = COL_BASIN, colour = COL_BASIN_BD, linewidth = 0.5) +
  geom_sf(data = network_sf,
          colour = COL_NETWORK, linewidth = 0.35, alpha = 0.8) +
  # raw fish — open circles in red
  geom_sf(data = fish_raw,
          shape = 1, size = 1.6,
          colour = COL_SPECIES, stroke = 0.5) +
  # snapped fish — filled circles in green
  geom_sf(data = fish_snapped,
          shape = 21, size = 1.6,
          fill = COL_SNAPPED, colour = "white", stroke = 0.3) +
  # snapped dams — triangles
  geom_sf(data = dams_snapped,
          aes(shape = type),
          colour = COL_DAM_EX, fill = COL_DAM_EX,
          size = 2.2, stroke = 0.5) +
  geom_sf(data = inset_box,
          fill = NA, colour = COL_INSET_BOX, linewidth = 0.8) +
  scale_shape_manual(values = c("Existing" = 24, "Planned" = 25),
                     name = "Dam status") +
  annotation_scale(location = "bl", width_hint = 0.25,
                   text_cex = 0.6, line_width = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(6, "mm"), width = unit(5, "mm"),
                         style = north_arrow_minimal()) +
  labs(title    = "Network snapping",
       subtitle = "Orange box = inset detail") +
  theme_map()

# ---- inset panel ----
# Build a dummy data frame for the legend (raw vs snapped)
legend_df <- data.frame(
  label    = c("Original", "Snapped"),
  x        = c(0, 0), y = c(0, 0)   # dummy coords, not plotted
)

p_D_inset <- ggplot() +
  geom_sf(data = network_inset,
          colour = COL_NETWORK, linewidth = 0.7) +
  # displacement dashes: raw → snapped
  {if (!is.null(snap_lines_in) && nrow(snap_lines_in) > 0)
    geom_sf(data = snap_lines_in,
            colour = "#737373", linewidth = 0.45, linetype = "dashed")
  } +
  # raw positions — open circle in red (drawn first, underneath)
  geom_sf(data = fish_raw_in %>% mutate(position = "Original"),
          aes(colour = position, fill = position, shape = position),
          size = 2.8, stroke = 0.8) +
  # snapped positions — filled circle in green (drawn on top)
  geom_sf(data = fish_snap_in %>% mutate(position = "Snapped"),
          aes(colour = position, fill = position, shape = position),
          size = 2.8, stroke = 0.5) +
  # snapped dams
  {if (nrow(dams_snap_in) > 0)
    geom_sf(data = dams_snap_in,
            shape = 24, size = 3,
            colour = COL_DAM_EX, fill = COL_DAM_EX, stroke = 0.5)
  } +
  scale_colour_manual(
    name   = "Position",
    values = c("Original" = COL_SPECIES, "Snapped" = COL_SNAPPED)
  ) +
  scale_fill_manual(
    name   = "Position",
    values = c("Original" = NA,          "Snapped" = COL_SNAPPED)
  ) +
  scale_shape_manual(
    name   = "Position",
    values = c("Original" = 1,           "Snapped" = 21)   # 1=open circle, 21=filled
  ) +
  guides(
    colour = guide_legend(override.aes = list(
      shape  = c(1, 21),
      fill   = c(NA, COL_SNAPPED),
      colour = c(COL_SPECIES, COL_SNAPPED),
      size   = 2.5
    ))
  ) +
  theme_void(base_size = 7) +
  theme(
    panel.border     = element_rect(colour = COL_INSET_BOX, fill = NA,
                                    linewidth = 1.2),
    plot.background  = element_rect(fill = "white", colour = NA),
    plot.margin      = margin(2, 2, 2, 2),
    legend.position  = "bottom",
    legend.title     = element_text(size = 6, face = "bold"),
    legend.text      = element_text(size = 5.5),
    legend.key.size  = unit(2.5, "mm")
  )

# ---- combine: inset placed lower-right of main panel ----
p_D <- p_D_main +
  inset_element(p_D_inset,
                left   = 0.55, right  = 1.0,
                bottom = 0.0,  top    = 0.45,
                align_to = "plot")

save_fig(p_D, "figures/placeholder/figD_snapping_inset.png",
         w = FIG_W_MM, h = FIG_H_MM + 20)


# ============================================================
message("\n=== All placeholder figures done ===")
message("figures/placeholder/figA_species_occurrences.png")
message("figures/placeholder/figB_barriers.png")
message("figures/placeholder/figC_overview_network.png")
message("figures/placeholder/figD_snapping_inset.png")
