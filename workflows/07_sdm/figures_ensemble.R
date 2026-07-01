#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 07c_ensemble_prediction_map.R
# Workflow paper (Paper 1) — Module 7 figure
#
# Produces two figures:
#   Figure main  — 7-panel grid, one per species, reaches coloured by
#                  ensemble_mean probability (continuous, viridis)
#                  Grey reaches = below TSS threshold (unsuitable)
#   Figure supp  — same but binary (suitable/unsuitable) using TSS threshold
#
# READS:
#   sdm/ensemble/ensemble_<species>.csv   (subc_id, ensemble_mean, ...)
#   sdm/ensemble/ensemble_thresholds.csv  (species, threshold_tss)
#   spatial/subbasin_sarantaporos/stream_network_pruned.gpkg
#
# WRITES:
#   figures/sdm/fig_ensemble_continuous.png   (main, 173 mm wide)
#   figures/sdm/fig_ensemble_binary.png       (supplement)
#
# Species (7): Alburnoides_prespensis, Barbus_prespensis,
#              Chondrostoma_ohridanum, Anguilla_anguilla,
#              Salmo_farioides, Squalius_platyceps, Oxynoemacheilus_pindus
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(sf)
library(dplyr)
library(data.table)
library(ggplot2)
library(patchwork)
library(scales)      # label_number for legend

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("figures/sdm", recursive = TRUE, showWarnings = FALSE)

# ── parameters ───────────────────────────────────────────────────────────────
TARGET_SPECIES <- c(
  "Alburnoides_prespensis",
  "Barbus_prespensis",
  "Chondrostoma_ohridanum",
  "Anguilla_anguilla",
  "Salmo_farioides",
  "Squalius_platyceps",
  "Oxynoemacheilus_pindus"
)

# clean labels for panel titles (italics via expression() in ggplot)
SPECIES_LABELS <- c(
  "Alburnoides_prespensis"   = "A. prespensis",
  "Barbus_prespensis"        = "B. prespensis",
  "Chondrostoma_ohridanum"   = "C. ohridanum",
  "Anguilla_anguilla"        = "A. anguilla",
  "Salmo_farioides"          = "S. farioides",
  "Squalius_platyceps"       = "S. platyceps",
  "Oxynoemacheilus_pindus"   = "O. pindus"
)

FIG_W_MM <- 173
FIG_DPI  <- 300

COL_UNSUITABLE <- "grey88"
COL_NETWORK_BG <- "grey75"   # reaches not in prediction table (outside basin)

# ── load network geometry ────────────────────────────────────────────────────
message("Loading stream network...")
network_sf <- st_read("spatial/subbasin_sarantaporos/stream_network_pruned.gpkg",
                      quiet = TRUE) %>%
  st_transform(4326)
message("  ", nrow(network_sf), " reaches loaded.")

# ── load TSS thresholds ──────────────────────────────────────────────────────
thresholds <- fread("sdm/ensemble/ensemble_thresholds.csv")
# expects columns: species, threshold_tss

# ── shared map theme ─────────────────────────────────────────────────────────
theme_map_sdm <- function(base_size = 8) {
  theme_void(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      plot.title       = element_text(size = 7, face = "italic", hjust = 0.5,
                                      margin = margin(b = 2)),
      plot.margin      = margin(3, 3, 3, 3),
      legend.position  = "none"   # shared legend added via patchwork
    )
}

# ── build one panel per species ──────────────────────────────────────────────
make_panel_continuous <- function(sp) {
  f <- file.path("sdm/ensemble", paste0("ensemble_", sp, ".csv"))
  if (!file.exists(f)) {
    message("  WARNING: not found — ", f)
    return(NULL)
  }

  dt  <- fread(f)
  thr <- thresholds[species == sp, threshold_tss]
  if (length(thr) == 0 || is.na(thr)) thr <- 0

  # join predictions to network
  net <- network_sf %>%
    left_join(dt %>% select(subc_id, ensemble_mean), by = "subc_id") %>%
    mutate(
      suitable = !is.na(ensemble_mean) & ensemble_mean >= thr,
      prob     = if_else(suitable, ensemble_mean, NA_real_)
    )

  ggplot() +
    # background — all reaches in light grey
    geom_sf(data = net,
            colour = COL_UNSUITABLE, linewidth = 0.3) +
    # suitable reaches coloured by ensemble probability
    geom_sf(data = net %>% filter(suitable),
            aes(colour = prob), linewidth = 0.65) +
    scale_colour_viridis_c(
      name   = "Ensemble\nprobability",
      option = "viridis",
      limits = c(0, 1),
      breaks = c(0, 0.5, 1),
      labels = c("0", "0.5", "1"),
      na.value = COL_UNSUITABLE
    ) +
    labs(title = SPECIES_LABELS[sp]) +
    theme_map_sdm()
}

make_panel_binary <- function(sp) {
  f <- file.path("sdm/ensemble", paste0("ensemble_", sp, ".csv"))
  if (!file.exists(f)) return(NULL)

  dt  <- fread(f)
  thr <- thresholds[species == sp, threshold_tss]
  if (length(thr) == 0 || is.na(thr)) thr <- 0

  net <- network_sf %>%
    left_join(dt %>% select(subc_id, ensemble_mean), by = "subc_id") %>%
    mutate(suitable = !is.na(ensemble_mean) & ensemble_mean >= thr)

  n_suitable <- sum(net$suitable, na.rm = TRUE)
  pct <- round(100 * n_suitable / nrow(net), 1)

  ggplot() +
    geom_sf(data = net %>% filter(!suitable),
            colour = COL_UNSUITABLE, linewidth = 0.3) +
    geom_sf(data = net %>% filter(suitable),
            colour = "#1a7a3c", linewidth = 0.65) +
    labs(title = paste0(SPECIES_LABELS[sp], "  (", pct, "% suitable)")) +
    theme_map_sdm()
}

# ── build panel lists ────────────────────────────────────────────────────────
message("Building panels...")

panels_cont <- lapply(TARGET_SPECIES, make_panel_continuous)
names(panels_cont) <- TARGET_SPECIES
panels_cont <- Filter(Negate(is.null), panels_cont)

panels_bin  <- lapply(TARGET_SPECIES, make_panel_binary)
names(panels_bin) <- TARGET_SPECIES
panels_bin  <- Filter(Negate(is.null), panels_bin)

# ── shared colour legend ─────────────────────────────────────────────────────
# extract legend from one panel by temporarily turning it on
legend_panel <- panels_cont[[1]] +
  theme(
    legend.position  = "bottom",
    legend.title     = element_text(size = 6, face = "bold"),
    legend.text      = element_text(size = 6),
    legend.key.width = unit(12, "mm"),
    legend.key.height = unit(2.5, "mm")
  ) +
  guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))

get_legend <- function(p) {
  # extract just the legend grob from a ggplot
  gt   <- ggplot_gtable(ggplot_build(p))
  leg  <- which(sapply(gt$grobs, function(g) g$name) == "guide-box")
  if (length(leg) == 0) return(NULL)
  gt$grobs[[leg]]
}

leg_grob <- get_legend(legend_panel)

# ── assemble figure — continuous ─────────────────────────────────────────────
message("Assembling continuous figure...")

p_cont <- wrap_plots(panels_cont, ncol = 4) +
  plot_annotation(
    title    = "Ensemble SDM predictions — Sarantaporos sub-basin",
    subtitle = "Reaches coloured by ensemble probability (grey = below TSS threshold)",
    theme    = theme(
      plot.title    = element_text(size = 9,  face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 7.5, colour = "grey40", hjust = 0.5)
    )
  )

# add shared legend below
if (!is.null(leg_grob)) {
  p_cont_final <- p_cont /
    wrap_elements(leg_grob) +
    plot_layout(heights = c(20, 1))
} else {
  p_cont_final <- p_cont
}

ggsave("figures/sdm/fig_ensemble_continuous.png",
       p_cont_final,
       width  = FIG_W_MM,
       height = FIG_W_MM * 0.65,   # roughly 2 rows of 4 panels
       units  = "mm",
       dpi    = FIG_DPI,
       bg     = "white")
message("Saved: figures/sdm/fig_ensemble_continuous.png")

# ── assemble figure — binary ─────────────────────────────────────────────────
message("Assembling binary figure...")

p_bin <- wrap_plots(panels_bin, ncol = 4) +
  plot_annotation(
    title    = "Ensemble SDM predictions — suitable habitat (TSS threshold)",
    subtitle = "Green = suitable reaches · Grey = unsuitable",
    theme    = theme(
      plot.title    = element_text(size = 9,  face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 7.5, colour = "grey40", hjust = 0.5)
    )
  )

ggsave("figures/sdm/fig_ensemble_binary.png",
       p_bin,
       width  = FIG_W_MM,
       height = FIG_W_MM * 0.65,
       units  = "mm",
       dpi    = FIG_DPI,
       bg     = "white")
message("Saved: figures/sdm/fig_ensemble_binary.png")

message("\n=== Done ===")
message("figures/sdm/fig_ensemble_continuous.png")
message("figures/sdm/fig_ensemble_binary.png")

# ============================================================
# BONUS FIGURE — Barbus prespensis continuous probability (single panel)
# ============================================================

message("\nBuilding Barbus prespensis standalone figure...")

sp_barbus <- "Barbus_prespensis"
dt_barbus <- fread(file.path("sdm/ensemble",
                             paste0("ensemble_", sp_barbus, ".csv")))
thr_barbus <- thresholds[species == sp_barbus, threshold_tss]
if (length(thr_barbus) == 0 || is.na(thr_barbus)) thr_barbus <- 0

net_barbus <- network_sf %>%
  left_join(dt_barbus %>% select(subc_id, ensemble_mean), by = "subc_id") %>%
  mutate(
    suitable = !is.na(ensemble_mean) & ensemble_mean >= thr_barbus,
    prob     = if_else(suitable, ensemble_mean, NA_real_)
  )

n_suit <- sum(net_barbus$suitable, na.rm = TRUE)
pct_suit <- round(100 * n_suit / nrow(net_barbus), 1)

p_barbus <- ggplot() +
  # unsuitable reaches as grey background
  geom_sf(data = net_barbus %>% filter(!suitable),
          colour = COL_UNSUITABLE, linewidth = 0.35) +
  # suitable reaches coloured by continuous probability
  geom_sf(data = net_barbus %>% filter(suitable),
          aes(colour = prob), linewidth = 0.9) +
  scale_colour_viridis_c(
    name   = "Ensemble probability",
    option = "viridis",
    limits = c(thr_barbus, 1),
    breaks = scales::breaks_pretty(n = 4),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    title    = expression(italic("Barbus prespensis") ~ "— suitable habitat"),
    subtitle = paste0("Sarantaporos sub-basin · ", n_suit, " reaches suitable (",
                      pct_suit, "%) · grey = below TSS threshold")
  ) +
  theme_void(base_size = 9) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    plot.title       = element_text(size = 9, hjust = 0.5,
                                    margin = margin(b = 2)),
    plot.subtitle    = element_text(size = 7, colour = "grey40", hjust = 0.5,
                                    margin = margin(b = 4)),
    plot.margin      = margin(5, 5, 5, 5),
    legend.position  = "bottom",
    legend.title     = element_text(size = 7, face = "bold"),
    legend.text      = element_text(size = 6.5),
    legend.key.width  = unit(18, "mm"),
    legend.key.height = unit(2.5, "mm")
  ) +
  guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))

ggsave("figures/sdm/fig_barbus_prespensis_habitat.png",
       p_barbus,
       width  = FIG_W_MM,
       height = FIG_W_MM * 0.85,
       units  = "mm",
       dpi    = FIG_DPI,
       bg     = "white")
message("Saved: figures/sdm/fig_barbus_prespensis_habitat.png")
