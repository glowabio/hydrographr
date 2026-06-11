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
#   - prioritization/solutions/solution_current_30pct.csv  (from 01_)
#   - prioritization/solutions/solution_future_30pct.csv   (from 01_)
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
library(ggplot2)
library(patchwork)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
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

status_cols <- c("Both"         = "#1a7a3c",
                 "Current only" = "#d7191c",
                 "Future only"  = "#2c7bb6",
                 "Neither"      = "grey85")

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

dams_snapped <- fread("points_snapped/dams/dams_snapped_points.csv")

# attach snapped coordinates to each ranked dam reach. For reaches with
# two co-located dams, take the first snapped point (same reach, ~same
# location); the ranking already summed their MW and damage.
dam_xy <- dams_snapped[, .(longitude_snapped, latitude_snapped),
                       by = subc_id][, .SD[1], by = subc_id]

dam_rank <- dam_rank %>%
  left_join(dam_xy, by = "subc_id")

n_no_xy <- sum(is.na(dam_rank$longitude_snapped))
if (n_no_xy > 0)
  message("  WARNING: ", n_no_xy, " ranked dams have no snapped coordinates")

# ============================================================
# STEP 2: FIG 1 — ranked bar chart (damage-per-MW)
# ============================================================

message("\n=== Step 2: Fig 1 — ranked bar chart ===")

# split off the off-scale dam(s)
offscale <- dam_rank %>% filter(damage_per_mw > OFFSCALE_CUTOFF)
onscale  <- dam_rank %>% filter(damage_per_mw <= OFFSCALE_CUTOFF)

message("  Off-scale dams (annotated): ", nrow(offscale),
        " | on-scale dams (plotted): ", nrow(onscale))

# long format so each bar is split into isolated vs dewatered damage.
# We scale the two damage components to per-MW so the stacked bar height
# equals damage_per_mw (the ranking metric).
bar_long <- onscale %>%
  mutate(
    per_mw_isolated  = damage_isolated  / power_mw,
    per_mw_dewatered = damage_dewatered / power_mw,
    dam_label        = paste0(subc_id,
                              ifelse(n_dams > 1,
                                     paste0(" (", n_dams, " dams)"), ""))
  ) %>%
  arrange(damage_per_mw) %>%                       # ascending -> worst on top
  mutate(dam_label = factor(dam_label, levels = dam_label)) %>%
  select(dam_label, power_mw,
         Isolated = per_mw_isolated, Dewatered = per_mw_dewatered) %>%
  pivot_longer(c(Isolated, Dewatered),
               names_to = "component", values_to = "per_mw") %>%
  mutate(component = factor(component, levels = c("Dewatered", "Isolated")))

offscale_note <- if (nrow(offscale) > 0) {
    paste0("Off-scale: reach ", offscale$subc_id[1],
         " (damage/MW = ", round(offscale$damage_per_mw[1]),
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
    x = "Planned dam (reach subc_id)",
    y = "Connectivity + dewatering damage per MW",
    title = "Planned dams ranked by habitat damage per MW",
    subtitle = paste0("Damage = summed SDM suitability of isolated (upstream) ",
                      "and dewatered (downstream 2 km) reaches",
                      if (!is.null(offscale_note)) paste0("\n", offscale_note) else "")
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top",
        plot.subtitle = element_text(size = 8, colour = "grey40"),
        axis.text.y   = element_text(size = 7))

png("prioritization/maps/fig1_dam_ranking_bars.png",
    width = 8, height = 8, units = "in", res = 200)
print(p1); dev.off()
message("  Saved: prioritization/maps/fig1_dam_ranking_bars.png")

# ============================================================
# STEP 3: FIG 2 — dam damage map
# ============================================================

message("\n=== Step 3: Fig 2 — dam damage map ===")

dam_sf <- dam_rank %>%
  filter(!is.na(longitude_snapped)) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

# Split off the off-scale (outlet) dam so it doesn't compress the colour
# and size scales for the other dams. It is drawn separately with a fixed
# marker and a label, so it stays on the map without distorting the legend.
dam_sf_on  <- dam_sf %>% filter(damage_per_mw <= OFFSCALE_CUTOFF)
dam_sf_off <- dam_sf %>% filter(damage_per_mw >  OFFSCALE_CUTOFF)

message("  On-scale dams: ", nrow(dam_sf_on),
        " | off-scale (outlet, drawn separately): ", nrow(dam_sf_off))

p2 <- ggplot() +
  geom_sf(data = stream_lines, colour = "grey80", linewidth = 0.3) +
  geom_sf(data = dam_sf_on,
          aes(size = damage_total, colour = damage_total),
          alpha = 0.85) +
  scale_colour_viridis_c(option = "inferno", direction = -1,
                         name = "Total damage\n(SDM suitability)") +
  scale_size_continuous(range = c(1.5, 8),
                        name = "Total damage\n(SDM suitability)") +
  guides(colour = guide_legend(), size = guide_legend())     # merge legends

# add the outlet dam as a distinct hollow marker + label, if present
if (nrow(dam_sf_off) > 0) {
  p2 <- p2 +
    geom_sf(data = dam_sf_off, shape = 21, fill = NA,
            colour = "black", stroke = 1.1, size = 9) +
    geom_sf_text(data = dam_sf_off,
                 aes(label = paste0("outlet dam\n(damage ",
                                    round(damage_total), ")")),
                 size = 2.8, colour = "black",
                 nudge_y = 0.012, lineheight = 0.9)
}

p2 <- p2 +
  labs(title = "Planned-dam habitat damage across the Sarantaporos network",
       subtitle = paste0("Point size and colour = summed isolated + dewatered ",
                         "SDM suitability (outlet dam shown off-scale)")) +
  theme_void(base_size = 11) +
  theme(plot.subtitle = element_text(size = 9, colour = "grey40"),
        legend.position = "right")

png("prioritization/maps/fig2_dam_damage_map.png",
    width = 9, height = 7, units = "in", res = 200)
print(p2); dev.off()
message("  Saved: prioritization/maps/fig2_dam_damage_map.png")

# ============================================================
# STEP 4: FIG 3 — current vs future priority comparison (30%)
# ============================================================

message("\n=== Step 4: Fig 3 — priority comparison (30%) ===")

comparison <- fread("prioritization/comparison_30pct.csv")

comp_sf <- stream_lines %>%
  left_join(comparison %>% rename(subc_id = id), by = "subc_id") %>%
  mutate(status = factor(status,
                         levels = c("Both", "Current only",
                                    "Future only", "Neither")))

p3 <- ggplot(comp_sf) +
  geom_sf(aes(colour = status, linewidth = status == "Neither")) +
  scale_colour_manual(values = status_cols, name = "Priority status") +
  scale_linewidth_manual(values = c(`TRUE` = 0.3, `FALSE` = 0.9),
                         guide = "none") +
  labs(
    title = "Priority reaches: current vs future barrier scenario (30% target)",
    subtitle = paste0("Red = priority only without planned dams (lost once dams sever ",
                      "connectivity); blue = priority only under future dams")
  ) +
  theme_void(base_size = 11) +
  theme(plot.subtitle = element_text(size = 8, colour = "grey40"),
        legend.position = "right")

png("prioritization/maps/fig3_priority_comparison_30pct.png",
    width = 9, height = 7, units = "in", res = 200)
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
