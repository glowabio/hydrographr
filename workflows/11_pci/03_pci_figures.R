#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03_pci_figures.R
#
# Figures for the Population Connectivity Index results (seven Sarantaporos
# fish species):
#   fig_fi_barplot.png   Fragmentation Index per species (ranked bars)
#   fig_pci_dotplot.png  PCI per species: dot at PCI_future, faint line back
#                        to PCI_current (~1.0). No error bars: PCI is a
#                        single deterministic value per species.
#   fig_pci_panel.png/.pdf  Combined two-panel figure (a: FI bars,
#                        b: PCI dots) sized for a ~17 cm half-page,
#                        one shared "Dispersal rank" legend, species labels
#                        only on the left panel.
# Both panels coloured by dispersal rank (0.3 / 0.6 / 0.9).
#
# NOTE: a current-vs-future PCI scatter was dropped because PCI under the
# current scenario is ~1.0 for every species; the dot panel conveys the
# current->future drop more legibly.
#
# REQUIRES
#   tidyverse, data.table, RColorBrewer, patchwork
#
# INPUT
#   connectivity/pci/fi_summary.txt          (species, PCI_current, PCI_future, FI)
#   traits/fish_dispersal_rank.txt           (dispersal_prob, Migration_label)
#
# OUTPUT
#   connectivity/pci/fig_fi_barplot.png
#   connectivity/pci/fig_pci_dotplot.png
#   connectivity/pci/fig_pci_panel.png
#   connectivity/pci/fig_pci_panel.pdf
#
# LOCATION
#   workflows/<connectivity_module>/03_pci_figures.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
library(tidyverse)
library(data.table)
library(RColorBrewer)
library(patchwork)
# ============================================================
# FIX: prevent MASS::select etc. from masking dplyr verbs
# ============================================================
select   <- dplyr::select
rename   <- dplyr::rename
group_by <- dplyr::group_by
# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- BASE_DIR
setwd(BASE_DIR)
# ============================================================
# BASE FONT SIZE (drives all text; bumped up for A4 legibility)
# ============================================================
BASE_SIZE <- 11
# ============================================================
# READ INPUTS
# ============================================================
fi_summary <- fread("connectivity/pci/fi_summary.txt")
fish_dis_class <- fread("traits/fish_dispersal_rank.txt") %>%
  dplyr::select(species, dispersal_prob, Migration_label)
fi_plot <- fi_summary %>%
  dplyr::left_join(fish_dis_class, by = "species") %>%
  dplyr::mutate(
    species_lab = gsub("_", " ", species),
    rank = factor(
      dplyr::case_when(
        dispersal_prob == 0.3 ~ "Non-migratory (0.3)",
        dispersal_prob == 0.6 ~ "Potamodromous (0.6)",
        dispersal_prob == 0.9 ~ "Long-distance (0.9)",
        TRUE ~ "Other"
      ),
      levels = c("Non-migratory (0.3)", "Potamodromous (0.6)", "Long-distance (0.9)")
    ),
    # shared species ordering (by FI) so both panels align row-for-row
    species_lab = fct_reorder(species_lab, FI)
  )
# Colours: BuGn sequential (light -> dark, low -> high dispersal)
pal <- brewer.pal(3, "BuGn")
rank_cols <- setNames(pal,
                      c("Non-migratory (0.3)",
                        "Potamodromous (0.6)",
                        "Long-distance (0.9)"))
# ============================================================
# PANEL a: Fragmentation Index per species
#   Extra headroom (mult 0.18) so the "27.8%" label is not clipped.
# ============================================================
p_fi <- ggplot(fi_plot,
               aes(x = species_lab, y = FI, fill = rank)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", FI)),
            hjust = -0.15, size = BASE_SIZE / 3.2) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = rank_cols, name = "Dispersal rank", drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(x = NULL, y = "Fragmentation Index\n(% connectivity loss)") +
  theme_bw(base_size = BASE_SIZE) +
  theme(axis.text.y = element_text(face = "italic"),
        legend.position = "bottom",
        plot.margin = margin(5, 10, 5, 5))
# ============================================================
# PANEL b: PCI per species (dot at future, faint line back to current)
#   Species labels suppressed (shared with panel a). x truncated at 0.6.
#   Shape is fixed (not mapped) so it does NOT create a second legend.
# ============================================================
p_dot <- ggplot(fi_plot,
                aes(y = species_lab)) +
  geom_segment(aes(x = PCI_current, xend = PCI_future, yend = species_lab),
               colour = "grey75", linewidth = 1) +
  geom_point(aes(x = PCI_current),
             shape = 21, fill = "white", colour = "grey60", size = 2.6) +
  geom_point(aes(x = PCI_future, fill = rank),
             shape = 21, colour = "grey30", size = 4) +
  scale_fill_manual(values = rank_cols, name = "Dispersal rank", drop = FALSE) +
  scale_x_continuous(limits = c(0.2, 0.8), breaks = seq(0.2, 0.8, 0.25)) +
  labs(x = "Population\nConnectivity Index", y = NULL) +
  theme_bw(base_size = BASE_SIZE) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(5, 5, 5, 10))
# ============================================================
# WRITE: separate PNGs
# ============================================================
dir.create("connectivity/pci", recursive = TRUE, showWarnings = FALSE)
png("connectivity/pci/fig_fi_barplot.png",
    width = 1900, height = 1200, res = 300)
print(p_fi)
dev.off()
png("connectivity/pci/fig_pci_dotplot.png",
    width = 1900, height = 1200, res = 300)
print(p_dot + theme(axis.text.y = element_text(face = "italic"),
                    axis.ticks.y = element_line()))
dev.off()
# ============================================================
# WRITE: combined two-panel figure (a | b), ONE shared legend
#   - plot_spacing keeps the two x-axis titles from colliding
#   - override.aes gives the legend a filled key (not an open circle)
#   - guides = "collect" merges the identical fill legends into one
# ============================================================
p_panel <- (p_fi | p_dot) +
  plot_layout(widths = c(1.25, 1), guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom",
        plot.tag = element_text(size = BASE_SIZE + 1, face = "bold")) &
  guides(fill = guide_legend(
    title = "Dispersal rank",
    override.aes = list(shape = 21, size = 4, colour = "grey30")))
# 17 cm wide x 9 cm tall
w_cm <- 17; h_cm <- 9
png("connectivity/pci/fig_pci_panel.png",
    width = round(w_cm / 2.54 * 300), height = round(h_cm / 2.54 * 300), res = 300)
print(p_panel)
dev.off()
# PDF wants dimensions in INCHES (not pixels)
pdf("connectivity/pci/fig_pci_panel.pdf",
    width = w_cm / 2.54, height = h_cm / 2.54)
print(p_panel)
dev.off()
message("Figures written:")
message("  connectivity/pci/fig_fi_barplot.png")
message("  connectivity/pci/fig_pci_dotplot.png")
message("  connectivity/pci/fig_pci_panel.png / .pdf  (A4 half-page panel)")
