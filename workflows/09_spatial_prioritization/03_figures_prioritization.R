#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03_figures_prioritization.R   (Module 9 -- Planned-dam ranking figures)
#
# Three figures from the planned-dam ranking and the prioritization
# solutions:
#   FIG 1. Ranked bar chart of planned dams by damage-per-MW, with the
#          isolated/dewatered split shown within each bar. The outlet
#          dam fragments essentially the whole sub-basin and is orders
#          of magnitude above the rest, so it is set aside and annotated
#          off-scale; the remaining dams are shown on a linear axis where
#          differences among the actionable dams are readable.
#   FIG 2. Map of planned dams on the Sarantaporos network, sized and
#          coloured by total damage (isolated + dewatered habitat).
#   FIG 3. Current vs future priority reaches at the 30% target, drawn
#          as four-category status (Both / Current only / Future only /
#          Neither) on the stream network.
#
# Self-contained: reads everything from disk.
#
# Input:
#   - prioritization/planned_dam_ranking.csv          (from 02_)
#   - prioritization/comparison_30pct.csv             (from 01_, Step 7)
#   - spatial/subbasin_sarantaporos/stream_network_pruned.gpkg
#   - points_snapped/dams/dams_snapped_points.csv
#
# Output:
#   - prioritization/maps/fig1_dam_ranking_bars.png
#   - prioritization/maps/fig2_dam_damage_map.png
#   - prioritization/maps/fig3_priority_comparison_30pct.png
#
# LOCATION: workflows/09_spatial_prioritization/03_figures_prioritization.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(sf)
library(data.table)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(patchwork)

select <- dplyr::select

if (!exists("WORKFLOWS_DIR"))
  WORKFLOWS_DIR <- Sys.getenv("WORKFLOWS_CODE", "/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows")
source(file.path(WORKFLOWS_DIR, "helpers", "config.R"))
setwd(BASE_DIR)

dir.create("prioritization/maps", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Damage-per-MW above this is treated as "off-scale" and annotated
# rather than plotted (the outlet dam that fragments the whole basin).
OFFSCALE_CUTOFF <- 1000

col_isolated  <- "#d73027"   # upstream connectivity loss
col_dewatered <- "#4575b4"   # downstream dewatering (run-of-river)

# Okabe-Ito palette: distinguishable under protanopia, deuteranopia
# and tritanopia (the previous green/red pair was not).
status_cols <- c("Both"         = "#009E73",   # bluish green
                 "Current only" = "#D55E00",   # vermillion
                 "Future only"  = "#0072B2",   # blue
                 "Neither"      = "grey65")


# ---- STYLE PARAMETERS (shared by all three figures) --------
FIG_W_IN    <- 5.8
FIG_H_IN    <- 4.1
FIG_DPI     <- 300
BASE_SIZE   <- 16

SIZE_TITLE    <- 15
SIZE_SUBTITLE <- 10
SIZE_LEG_TTL  <- 13
SIZE_LEG_TXT  <- 12
LEG_KEY_CM    <- 0.6

COL_NETWORK   <- "grey55"
LWD_NETWORK   <- 0.7
COL_BASIN     <- "grey30"
LWD_BASIN     <- 0.6

theme_map_prio <- function() {
  theme_void(base_size = BASE_SIZE) +
    theme(
      plot.title          = element_text(size = SIZE_TITLE, face = "bold",
                                         hjust = 0, lineheight = 1.05),
      plot.subtitle       = element_text(size = SIZE_SUBTITLE, colour = "grey40",
                                         hjust = 0, lineheight = 1.1),
      plot.title.position = "plot",
      legend.title        = element_text(size = SIZE_LEG_TTL, face = "bold"),
      legend.text         = element_text(size = SIZE_LEG_TXT),
      legend.key.size     = unit(LEG_KEY_CM, "cm"),
      legend.position     = "right",
      plot.margin         = margin(6, 6, 6, 6)
    )
}

# Place-name lookup (Greek -> Latin). Edit values as preferred.
thesh_lookup <- tribble(
  ~thesh,                            ~thesh_lat,
  "Π.ΣΑΡΑΝΤΑΠΟΡΟΣ",                  "Sarantaporos",
  "ΕΠΤΑΧΩΡΙ",                        "Eptachori",
  "ΠΛΑΓΙΑ",                          "Plagia",
  "ΚΕΦΑΛΟΒΡΥΣΟ",                     "Kefalovryso",
  "ΒΟΥΡΜΠΙΑΝΗ - Ρ. ΒΟΥΡΜΠΙΑΝΙΤΙΚΟ",  "Vourbianitiko",
  "ΡΕΜΑ ΜΕΣΟΠΟΤΑΜΟΣ",               "Mesopotamos",
  "Ρ.Χελιμόδι",                      "Chelimodi",
  "ΡΕΜΑ ΜΑΝΟΥΡΑΣ",                   "Manouras",
  "ΡΕΜΑ ΠΗΓΩΝ ΣΑΡΑΝΤΑΠΟΡΟΥ",        "Piges Sarantaporou",
  "ΑΜΑΡΑΝΤΟΣ",                       "Amarantos",
  "ΡΑΧΟΒΙΤΣΑ",                       "Rachovitsa",
  "ΡΕΜΑ ΕΛΛΗΝΙΚΟ",                   "Elliniko",
  "ΛΑΪΣΤΑ",                          "Laista"
)

# ============================================================
# STEP 1: Load inputs
# ============================================================

message("\n=== Step 1: Loading inputs ===")

dam_rank <- fread("prioritization/planned_dam_ranking.csv")
message("  Dam ranking rows: ", nrow(dam_rank))

stream_lines <- read_sf(
  "spatial/subbasin_sarantaporos/stream_network_pruned.gpkg",
  layer = "stream_network_pruned"
) %>% st_transform(4326)

# --- basin outline ---
basin_outline <- read_sf(
  "spatial/subbasin_sarantaporos/subbasin_polygon.gpkg"
) %>% st_transform(4326)


subbasin_subc_ids <- fread("spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv")

# filter dams to those inside the sub-basin
dams_snapped <- fread("points_snapped/dams/dams_snapped_points.csv") %>%
  filter(subc_id %in% subbasin_subc_ids$subc_id)

# one place-name + one snapped coordinate per ranked reach (co-located
# dams share the reach; the ranking already summed their MW and damage).
dam_meta <- dams_snapped %>%
  group_by(subc_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(subc_id, thesh, longitude_snapped, latitude_snapped) %>%
  left_join(thesh_lookup, by = "thesh") %>%
  mutate(thesh_lat = coalesce(thesh_lat, as.character(subc_id)))

dam_rank <- dam_rank %>%
  left_join(dam_meta, by = "subc_id")

n_no_xy <- sum(is.na(dam_rank$longitude_snapped))
if (n_no_xy > 0)
  message("  WARNING: ", n_no_xy, " ranked dams have no snapped coordinates")

# ============================================================
# STEP 2: FIG 1 -- ranked bar chart (damage-per-MW)
# ============================================================

message("\n=== Step 2: Fig 1 -- ranked bar chart ===")

offscale <- dam_rank %>% filter(damage_per_mw > OFFSCALE_CUTOFF)
onscale  <- dam_rank %>% filter(damage_per_mw <= OFFSCALE_CUTOFF)

message("  Off-scale dams (annotated): ", nrow(offscale),
        " | on-scale dams (plotted): ", nrow(onscale))

# long format so each bar is split into isolated vs dewatered damage.
# Components scaled to per-MW so the stacked bar height equals damage_per_mw.
bar_long <- onscale %>%
  mutate(
    per_mw_isolated  = damage_isolated  / power_mw,
    per_mw_dewatered = damage_dewatered / power_mw,
    dam_label        = paste0(thesh_lat, " (", subc_id, ")",
                              ifelse(n_dams > 1,
                                     paste0(" [", n_dams, " dams]"), ""))
  ) %>%
  arrange(damage_per_mw) %>%                       # ascending -> worst on top
  mutate(dam_label = factor(dam_label, levels = unique(dam_label))) %>%
  select(dam_label, power_mw,
         Isolated = per_mw_isolated, Dewatered = per_mw_dewatered) %>%
  pivot_longer(c(Isolated, Dewatered),
               names_to = "component", values_to = "per_mw") %>%
  mutate(component = factor(component, levels = c("Dewatered", "Isolated")))

offscale_note <- if (nrow(offscale) > 0) {
  paste0("Off-scale: ", offscale$thesh_lat[1], " (reach ", offscale$subc_id[1],
         ", damage/MW = ", round(offscale$damage_per_mw[1]),
         "), near the outlet, fragments the whole sub-basin")
} else NULL

p1 <- ggplot(bar_long,
             aes(x = dam_label, y = per_mw, fill = component)) +
  geom_col(width = 0.7, alpha = 0.9) +
  coord_flip() +
  scale_fill_manual(values = c("Isolated"  = col_isolated,
                               "Dewatered" = col_dewatered),
                    name = "Damage component",
                    breaks = c("Isolated", "Dewatered")) +
  labs(
    x = "Planned dam (location, reach)",
    y = "Connectivity + dewatering damage per MW",
    title = "Planned dams ranked by habitat damage per MW",
    subtitle = paste0("Damage = summed SDM suitability of isolated (upstream) ",
                      "and dewatered (downstream 2 km) reaches\n",
                      if (!is.null(offscale_note)) paste0(offscale_note) else "")
  ) +
  # bar chart keeps theme_bw (it needs axes) but takes the shared sizing
  theme_bw(base_size = BASE_SIZE) +
  theme(
    plot.title      = element_text(size = SIZE_TITLE, face = "bold",
                                   hjust = 0, lineheight = 1.05),
    plot.subtitle   = element_text(size = SIZE_SUBTITLE, colour = "grey40",
                                   hjust = 0, lineheight = 1.1),
    plot.title.position = "plot",
    legend.title    = element_text(size = SIZE_LEG_TTL, face = "bold"),
    legend.text     = element_text(size = SIZE_LEG_TXT),
    legend.key.size = unit(LEG_KEY_CM, "cm"),
    legend.position = "top",
    axis.title      = element_text(size = SIZE_LEG_TTL),
    axis.text.y     = element_text(size = SIZE_SUBTITLE),
    axis.text.x     = element_text(size = SIZE_SUBTITLE),
    panel.border    = element_rect(colour = "grey30", linewidth = 0.6),
    panel.grid.major = element_line(colour = "grey88", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

# taller than the maps: one row per dam label needs vertical room
png("prioritization/maps/fig1_dam_ranking_bars.png",
    width = FIG_W_IN * 1.6, height = FIG_H_IN * 1.6,
    units = "in", res = FIG_DPI)
print(p1); dev.off()
message("  Saved: prioritization/maps/fig1_dam_ranking_bars.png")

# ============================================================
# STEP 3: FIG 2 -- dam damage map
# ============================================================

message("\n=== Step 3: Fig 2 -- dam damage map ===")

dam_sf <- dam_rank %>%
  filter(!is.na(longitude_snapped)) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

# Split off the off-scale (outlet) dam so it doesn't compress the colour
# and size scales for the other dams.
dam_sf_on  <- dam_sf %>% filter(damage_per_mw <= OFFSCALE_CUTOFF)
dam_sf_off <- dam_sf %>% filter(damage_per_mw >  OFFSCALE_CUTOFF)

message("  On-scale dams: ", nrow(dam_sf_on),
        " | off-scale (outlet, drawn separately): ", nrow(dam_sf_off))

p2 <- ggplot() +
  geom_sf(data = basin_outline, fill = NA,
          colour = COL_BASIN, linewidth = LWD_BASIN) +
  geom_sf(data = stream_lines,
          colour = COL_NETWORK, linewidth = LWD_NETWORK) +
  geom_sf(data = dam_sf_on,
          aes(size = damage_total, colour = damage_total),
          alpha = 0.85) +
  scale_colour_viridis_c(option = "inferno", direction = -1,
                         name = "Total damage\n(SDM suitability)") +
  scale_size_continuous(range = c(2, 9),
                        name = "Total damage\n(SDM suitability)") +
  guides(colour = guide_legend(), size = guide_legend())     # merge legends

if (nrow(dam_sf_off) > 0) {
  p2 <- p2 +
    geom_sf(data = dam_sf_off, shape = 21, fill = NA,
            colour = "black", stroke = 1.1, size = 9) +
    geom_sf_text(data = dam_sf_off,
                 aes(label = paste0(thesh_lat, "\n(outlet, damage ",
                                    round(damage_total), ")")),
                 size = 3.4, colour = "black", fontface = "bold",
                 nudge_y = 0.012, lineheight = 0.9)
}

p2 <- p2 +
  labs(title = "Planned-dam habitat damage across the Sarantaporos network",
       subtitle = paste0("Point size and colour = summed isolated + ",
                         "dewatered SDM suitability ",
                         "(outlet dam shown off-scale)")) +
  theme_map_prio()

png("prioritization/maps/fig2_dam_damage_map.png",
    width = FIG_W_IN, height = FIG_H_IN, units = "in", res = FIG_DPI)
print(p2); dev.off()
message("  Saved: prioritization/maps/fig2_dam_damage_map.png")

# ============================================================
# STEP 4: FIG 3 -- current vs future priority comparison (30%)
# ============================================================

message("\n=== Step 4: Fig 3 -- priority comparison (30%) ===")

comparison <- fread("prioritization/comparison_30pct.csv")

comp_sf <- stream_lines %>%
  left_join(comparison %>% rename(subc_id = id), by = "subc_id") %>%
  mutate(status = factor(status,
                         levels = c("Both", "Current only",
                                    "Future only", "Neither")))


# --- dam points, coloured by whether the reach they sit on was selected ---
# NOTE: dams_snapped already has its own `status` column (planned/existing),
# so the priority status from `comparison` is renamed to `priority_status`
# to avoid a .x/.y name clash on the join.
dam_status_sf <- dams_snapped %>%
  left_join(comparison %>% rename(subc_id = id, priority_status = status) %>%
              select(subc_id, priority_status),
            by = "subc_id") %>%
  mutate(priority_status = factor(coalesce(priority_status, "Neither"),
                                  levels = c("Both", "Current only",
                                             "Future only", "Neither"))) %>%
  filter(!is.na(longitude_snapped)) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

p3 <- ggplot() +
  geom_sf(data = basin_outline, fill = NA,
          colour = COL_BASIN, linewidth = LWD_BASIN) +
  geom_sf(data = comp_sf,
          aes(colour = status, linewidth = status == "Neither")) +
  scale_linewidth_manual(values = c(`TRUE` = LWD_NETWORK, `FALSE` = 1.3),
                         guide = "none") +
  geom_sf(data = dam_status_sf, aes(fill = priority_status),
          shape = 21, colour = "black", stroke = 0.5, size = 4) +
  scale_colour_manual(values = status_cols, name = "Priority status",
                      aesthetics = c("colour", "fill")) +
  labs(
    title = "Priority reaches: current vs future barrier scenario (30% target)",
    subtitle = paste0("Vermillion = priority only without planned dams (lost once ",
                      "dams sever connectivity); blue = priority only under future ",
                      "dams.\nPoints = dams, coloured by the priority status of ",
                      "their reach")
  ) +
  theme_map_prio()


# A4 = 8.27 x 11.69 in. Roughly a quarter page, landscape.
png("prioritization/maps/fig3_priority_comparison_30pct.png",
    width = 5.8, height = 4.1, units = "in", res = 300)
print(p3); dev.off()
message("  Saved: prioritization/maps/fig3_priority_comparison_30pct.png")

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("DAM RANKING FIGURES COMPLETE")
message(paste(rep("=", 80), collapse = ""))
message("  Fig 1: prioritization/maps/fig1_dam_ranking_bars.png")
message("  Fig 2: prioritization/maps/fig2_dam_damage_map.png")
message("  Fig 3: prioritization/maps/fig3_priority_comparison_30pct.png")
