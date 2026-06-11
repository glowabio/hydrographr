#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_habitat_fragmentation_figures.R
#
# Produce all figures for the habitat fragmentation analysis.
# Reads CSV outputs from 07a_habitat_fragmentation_metrics.R and the
# Sarantaporos stream network for spatial context.
#
# Figures:
#
#   Histograms (figures/patch_metrics/):
#     hist_dist_point_{species}.png   — inter-point distances, all species
#     hist_dist_patch_{species}.png   — inter-patch distances, all species
#     hist_dist_barbus_salmo_main.png — combined panel, main text figure
#
#   Maps (figures/maps/):
#     Map 1 — Habitat patches (no dams)
#       map1_patches_{species}_main.png          — Barbus + Salmo
#       map1_patches_all_species_supplementary.png
#
#     Map 2 — Dam buffer overlap
#       map2_buffer_{species}_main.png            — Barbus + Salmo
#       map2_buffer_all_species_supplementary.png
#
#     Map 3 — Habitat fragments, current vs future scenario
#       map3_fragments_{species}_main.png         — Barbus + Salmo, two-panel
#       map3_fragments_all_species_supplementary.png
#
# Inputs (from 01_habitat_fragmentation_metrics.R):
#   sdm/patch_metrics/patch_membership_{species}.csv
#   sdm/patch_metrics/dist_point_{species}.csv
#   sdm/patch_metrics/dist_patch_{species}.csv
#   sdm/patch_metrics/dam_buffer_reaches.csv
#   sdm/patch_metrics/fragments_{scenario}_{species}.csv
#   sdm/patch_metrics/species_impact_ranking.csv
#
# Network for maps:
#   spatial/subbasin_sarantaporos/stream_network_pruned.gpkg
#
# Dam points:
#   points_snapped/dams/dams_snapped_points.csv
#
# LOCATION: workflows/08_habitat_fragmentation/02_habitat_fragmentation_figures.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(sf)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

select <- dplyr::select

source("~/Documents/Postdoc/code/workflow_paper/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

TARGET_SPECIES <- c(
  "Alburnoides_prespensis",
  "Anguilla_anguilla",
  "Barbus_prespensis",
  "Chondrostoma_ohridanum",
  "Oxynoemacheilus_pindus",
  "Salmo_farioides",
  "Squalius_platyceps"
)

# Species shown in main text (others go to supplementary)
MAIN_TEXT_SPECIES <- c("Barbus_prespensis", "Salmo_farioides")

# Map colour constants
COL_UNSUITABLE <- "grey85"   # non-suitable reaches
COL_DAM_EXIST  <- "black"
COL_DAM_PLAN   <- "#E41A1C"

# ============================================================
# SETUP
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("HABITAT FRAGMENTATION FIGURES")
message(paste(rep("=", 80), collapse = ""))

dir.create("figures/patch_metrics", recursive = TRUE, showWarnings = FALSE)
dir.create("figures/maps",          recursive = TRUE, showWarnings = FALSE)

# ============================================================
# STEP 1: Load spatial data for maps
# ============================================================

message("\n=== Step 1: Loading spatial data ===")

# Sarantaporos pruned network — background for all maps
network_sf <- st_read(
  "spatial/subbasin_sarantaporos/stream_network_pruned.gpkg",
  quiet = TRUE
)
message("  Network reaches: ", nrow(network_sf))

# Dam points
dams <- fread("points_snapped/dams/dams_snapped_points.csv") %>%
  filter(subc_id %in% network_sf$subc_id)

dams_current_sf <- dams %>%
  filter(status == "existing") %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

dams_future_sf  <- dams %>%
  filter(status %in% c("existing", "planned")) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

dams_planned_sf <- dams %>%
  filter(status == "planned") %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

message("  Dams current: ", nrow(dams_current_sf),
        " | future: ", nrow(dams_future_sf))

# Fish occurrence points — shown on all fragment maps
fish_pts_sf <- fread("points_snapped/basin/fish_sdm_basin.csv") %>%
  filter(subc_id %in% network_sf$subc_id,
         !is.na(longitude_snapped), !is.na(latitude_snapped)) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

message("  Fish occurrence points in Sarantaporos: ", nrow(fish_pts_sf))

# Buffer reaches (for Map 2)
buffer_dt <- tryCatch(
  fread("sdm/patch_metrics/dam_buffer_reaches.csv"),
  error = function(e) { message("  dam_buffer_reaches.csv not found — Map 2 will be skipped"); NULL }
)
BUF_SUBC_COL <- "subc_id"

# ============================================================
# STEP 2: Shared map theme
# ============================================================

theme_map <- function(base_size = 10) {
  theme_void(base_size = base_size) +
    theme(
      plot.title      = element_text(face = "italic", size = base_size,
                                     hjust = 0.5, margin = margin(b = 4)),
      plot.subtitle   = element_text(size = base_size - 1, hjust = 0.5,
                                     colour = "grey40", margin = margin(b = 4)),
      legend.position = "bottom",
      legend.title    = element_text(size = base_size - 1),
      legend.text     = element_text(size = base_size - 2),
      legend.key.size = unit(0.4, "cm"),
      plot.margin     = margin(4, 4, 4, 4)
    )
}

# ============================================================
# STEP 3: Histograms — all species (supplementary)
# ============================================================

message("\n=== Step 3: Histograms — all species ===")

# Using png() directly to avoid ragg graphics API version mismatch
plot_dist_histograms <- function(sp,
                                 out_dir   = "figures/patch_metrics",
                                 width_in  = 7,
                                 height_in = 4) {
  sp_label <- gsub("_", " ", sp)
  f_point  <- paste0("sdm/patch_metrics/dist_point_", sp, ".csv")
  f_patch  <- paste0("sdm/patch_metrics/dist_patch_", sp, ".csv")

  if (file.exists(f_point)) {
    dp  <- fread(f_point) %>% filter(!is.na(dist_km))
    p   <- ggplot(dp, aes(x = dist_km)) +
      geom_histogram(bins = 20, fill = "#2C7BB6",
                     colour = "white", linewidth = 0.3) +
      labs(title    = paste0(sp_label, " — point-based distances"),
           subtitle = paste0("n = ", nrow(dp), " pairwise distances"),
           x = "Along-network distance (km)", y = "Count") +
      theme_bw(base_size = 11) +
      theme(plot.title = element_text(face = "italic"))
    out <- file.path(out_dir, paste0("hist_dist_point_", sp, ".png"))
    png(out, width = width_in, height = height_in, units = "in", res = 200)
    print(p); dev.off()
    message("  Saved: ", out)
  }

  if (file.exists(f_patch)) {
    dp2 <- fread(f_patch) %>% filter(!is.na(dist_km))
    p2  <- ggplot(dp2, aes(x = dist_km)) +
      geom_histogram(bins = 20, fill = "#D7191C",
                     colour = "white", linewidth = 0.3) +
      labs(title    = paste0(sp_label, " — patch-based distances"),
           subtitle = paste0("n = ", nrow(dp2), " patch pairs"),
           x = "Along-network distance between patches (km)",
           y = "Count") +
      theme_bw(base_size = 11) +
      theme(plot.title = element_text(face = "italic"))
    out2 <- file.path(out_dir, paste0("hist_dist_patch_", sp, ".png"))
    png(out2, width = width_in, height = height_in, units = "in", res = 200)
    print(p2); dev.off()
    message("  Saved: ", out2)
  }
}

for (sp in TARGET_SPECIES) plot_dist_histograms(sp)

# ============================================================
# STEP 4: Main text histogram — Barbus + Salmo, both approaches
# ============================================================

message("\n=== Step 4: Main text histogram ===")

combined_rows <- lapply(MAIN_TEXT_SPECIES, function(sp) {
  sp_label <- gsub("_", " ", sp)
  rows     <- list()
  f_point  <- paste0("sdm/patch_metrics/dist_point_", sp, ".csv")
  f_patch  <- paste0("sdm/patch_metrics/dist_patch_", sp, ".csv")
  if (file.exists(f_point))
    rows[["point"]] <- fread(f_point) %>%
    filter(!is.na(dist_km)) %>%
    mutate(species = sp_label, approach = "Point-based") %>%
    select(species, approach, dist_km)
  if (file.exists(f_patch))
    rows[["patch"]] <- fread(f_patch) %>%
    filter(!is.na(dist_km)) %>%
    mutate(species = sp_label, approach = "Patch-based") %>%
    select(species, approach, dist_km)
  rbindlist(rows, fill = TRUE)
}) %>%
  rbindlist(fill = TRUE)

if (nrow(combined_rows) > 0) {

  combined_rows <- combined_rows %>%
    mutate(approach = factor(approach,
                             levels = c("Point-based", "Patch-based")))

  p_main <- ggplot(combined_rows, aes(x = dist_km, fill = approach)) +
    geom_histogram(bins = 20, colour = "white", linewidth = 0.3,
                   position = "identity", alpha = 0.85) +
    scale_fill_manual(values = c("Point-based" = "#2C7BB6",
                                 "Patch-based"  = "#D7191C")) +
    facet_grid(approach ~ species, scales = "free_y") +
    labs(
      x        = "Along-network distance (km)",
      y        = "Count",
      title    = "Inter-reach distances: point-based vs patch-based",
      subtitle = paste0(
        "Patch-based distances span the full contiguous suitable habitat;\n",
        "point-based distances reflect only sampled occurrence locations."
      )
    ) +
    theme_bw(base_size = 11) +
    theme(
      strip.text.x    = element_text(face = "italic"),
      legend.position = "none",
      plot.subtitle   = element_text(size = 9, colour = "grey40")
    )

  out_main <- "figures/patch_metrics/hist_dist_barbus_salmo_main.png"
  png(out_main, width = 8, height = 5, units = "in", res = 200)
  print(p_main); dev.off()
  message("  Saved: ", out_main)
}

# ============================================================
# STEP 5: Map 1 — Habitat patches (no dams)
# ============================================================

message("\n=== Step 5: Map 1 — Habitat patches ===")

# Each patch gets a distinct colour from RColorBrewer Set3.
# All reaches shown in grey as background — shows full network extent.
# Existing dams = filled circles; planned dams = triangles.

make_patch_map <- function(sp) {
  sp_label <- gsub("_", " ", sp)
  pm_file  <- paste0("sdm/patch_metrics/patch_membership_", sp, ".csv")
  if (!file.exists(pm_file)) return(NULL)

  pm    <- fread(pm_file)
  n_p   <- length(unique(pm$patch_id))
  # Set3 has max 12 colours — recycle if more patches
  pal   <- rep(brewer.pal(min(max(3, n_p), 12), "Set3"), length.out = n_p)
  names(pal) <- as.character(sort(unique(pm$patch_id)))

  net_map <- network_sf %>%
    left_join(pm %>% select(subc_id, patch_id), by = "subc_id") %>%
    mutate(patch_id = as.factor(patch_id))

  ggplot() +
    geom_sf(data = network_sf,
            colour = COL_UNSUITABLE, linewidth = 0.3, show.legend = FALSE) +
    geom_sf(data = net_map %>% filter(!is.na(patch_id)),
            aes(colour = patch_id), linewidth = 0.7) +
    geom_sf(data = dams_current_sf, shape = 21,
            fill = COL_DAM_EXIST, colour = "white", size = 2, stroke = 0.4) +
    geom_sf(data = dams_planned_sf, shape = 24,
            fill = COL_DAM_PLAN,  colour = "white", size = 2, stroke = 0.4) +
    scale_colour_manual(
      values = pal, name = "Patch",
      guide  = guide_legend(nrow = 2, override.aes = list(linewidth = 2))
    ) +
    labs(title    = bquote(italic(.(sp_label))),
         subtitle = paste0(n_p, " habitat patch",
                           ifelse(n_p != 1, "es", ""))) +
    theme_map()
}

for (sp in MAIN_TEXT_SPECIES) {
  p <- make_patch_map(sp); if (is.null(p)) next
  out <- paste0("figures/maps/map1_patches_", sp, "_main.png")
  png(out, width = 6, height = 5, units = "in", res = 300)
  print(p); dev.off()
  message("  Saved: ", out)
}

patch_plots_all <- Filter(
  Negate(is.null),
  lapply(setNames(TARGET_SPECIES, TARGET_SPECIES), make_patch_map)
)
if (length(patch_plots_all) > 0) {
  n_col  <- 3
  p_supp <- wrap_plots(patch_plots_all, ncol = n_col) +
    plot_annotation(
      title   = "Habitat patches per species",
      caption = paste0("Grey: unsuitable. Coloured: patches. ",
                       "\u25cf Existing  \u25b2 Planned dams."),
      theme   = theme(
        plot.title   = element_text(face = "bold", size = 11, hjust = 0.5),
        plot.caption = element_text(size = 8, hjust = 0)
      )
    )
  out_supp <- "figures/maps/map1_patches_all_species_supplementary.png"
  png(out_supp,
      width  = n_col * 4,
      height = ceiling(length(patch_plots_all) / n_col) * 4,
      units  = "in", res = 300)
  print(p_supp); dev.off()
  message("  Saved: ", out_supp)
}

# ============================================================
# STEP 6: Map 2 — Fragments + dam buffers (combined, two-panel)
# ============================================================

message("\n=== Step 6: Map 2 — Fragment + buffer maps ===")

# Combined map: fragments and dam buffer zones shown together.
# Visual hierarchy (each layer drawn on top of the previous):
#   1. Full network — plain grey (unsuitable, outside buffer)
#   2. Buffer reaches — grey with black outline (dam-affected zone)
#   3. Fragment reaches — fragment colour with black outline (suitable habitat)
#   4. Dam points
#
# The black outline is the unifying device: it flags both buffer reaches
# and fragment reaches as "notable", while colour distinguishes fragments
# from buffer-only reaches. Plain grey reaches are neither suitable nor
# within a buffer zone.
#
# Outline effect: draw each notable layer twice —
#   first pass: slightly thicker, in black (the outline)
#   second pass: slightly thinner, in the fill colour (the line itself)

# Pre-compute buffer reach ids once (same for all species)
if (!is.null(buffer_dt)) {
  buffer_ids_chr <- buffer_dt %>%
    pull(!!BUF_SUBC_COL) %>%
    as.character() %>%
    unique()
  network_sf_buffer <- network_sf %>%
    filter(as.character(subc_id) %in% buffer_ids_chr)
} else {
  buffer_ids_chr    <- character(0)
  network_sf_buffer <- network_sf[0, ]
  message("  dam_buffer_reaches.csv not found — buffer layer omitted from maps")
}

make_fragment_panel <- function(sp, scenario_name, dams_sf_panel, title_label) {

  memb_file <- paste0("sdm/patch_metrics/fragment_membership_",
                      scenario_name, "_", sp, ".csv")
  if (!file.exists(memb_file)) return(NULL)

  frag_memb <- fread(memb_file)
  n_frags   <- max(frag_memb$fragment_id, na.rm = TRUE)

  # Set2 palette — suppressWarnings because Set2 max is 8 colours;
  # rep() handles recycling for species with more fragments
  pal <- rep(
    suppressWarnings(brewer.pal(min(max(3, n_frags), 12), "Set2")),
    length.out = n_frags
  )
  names(pal) <- as.character(seq_len(n_frags))

  # Buffer zones only shown in future scenario — the buffer was computed
  # from future dams (existing + planned). Showing it in the current
  # scenario panel would be misleading since planned dams don't exist yet.
  active_buffer_ids <- if (scenario_name == "future") buffer_ids_chr else character(0)
  active_buffer_sf  <- if (scenario_name == "future") network_sf_buffer else network_sf[0, ]

  # Fragment reaches joined to network geometry
  net_frags <- network_sf %>%
    left_join(
      frag_memb %>%
        select(subc_id, fragment_id) %>%
        mutate(fragment_id = as.factor(fragment_id)),
      by = "subc_id"
    )

  frag_ids_chr <- as.character(frag_memb$subc_id)

  # Buffer reaches that are NOT fragment reaches — grey with black outline
  network_sf_buffer_only <- active_buffer_sf %>%
    filter(!as.character(subc_id) %in% frag_ids_chr)

  # Fragment reaches inside the buffer zone — get the outline
  net_frags_in_buffer <- net_frags %>%
    filter(!is.na(fragment_id),
           as.character(subc_id) %in% active_buffer_ids)

  # Fragment reaches outside the buffer zone — no outline
  net_frags_out_buffer <- net_frags %>%
    filter(!is.na(fragment_id),
           !as.character(subc_id) %in% active_buffer_ids)

  ggplot() +

    # Layer 1: full network — grey background
    geom_sf(data = network_sf,
            colour = COL_UNSUITABLE, linewidth = 0.3, show.legend = FALSE) +

    # Layer 2a: buffer-only reaches — black outline
    geom_sf(data = network_sf_buffer_only,
            colour = "black", linewidth = 1.3, show.legend = FALSE) +
    # Layer 2b: buffer-only reaches — grey fill on top of outline
    geom_sf(data = network_sf_buffer_only,
            colour = "grey60", linewidth = 0.65, show.legend = FALSE) +

    # Layer 3a: fragment reaches outside buffer — fragment colour, no outline
    geom_sf(data = net_frags_out_buffer,
            aes(colour = fragment_id), linewidth = 0.8) +

    # Layer 3b: fragment reaches inside buffer — black outline first,
    # then fragment colour on top (outline flags dam impact on suitable habitat)
    geom_sf(data = net_frags_in_buffer,
            colour = "black", linewidth = 1.3, show.legend = FALSE) +
    geom_sf(data = net_frags_in_buffer,
            aes(colour = fragment_id), linewidth = 0.65) +

    # Layer 4: fish occurrence points — black dots
    geom_sf(data = fish_pts_sf %>% filter(species == sp),
            colour = "black", size = 1.2, shape = 19, alpha = 0.8) +

    # Layer 5: dam points — red filled triangles
    geom_sf(data = dams_sf_panel, shape = 24,
            fill = "#E41A1C", colour = "white", size = 2.5, stroke = 0.5) +

    scale_colour_manual(
      values = pal, name = "Fragment",
      guide  = guide_legend(nrow = 2, override.aes = list(linewidth = 2))
    ) +
    labs(subtitle = paste0(title_label, " — ", n_frags, " fragment",
                           ifelse(n_frags != 1, "s", ""))) +
    theme_map()
}

make_fragment_map <- function(sp) {
  sp_label  <- gsub("_", " ", sp)
  p_current <- make_fragment_panel(sp, "current", dams_current_sf, "Current")
  p_future  <- make_fragment_panel(sp, "future",  dams_future_sf,  "Future")
  if (is.null(p_current) || is.null(p_future)) return(NULL)
  (p_current | p_future) +
    plot_annotation(
      title   = bquote(italic(.(sp_label))),
      caption = paste0(
        "Coloured lines: suitable habitat fragments (isolated by dams). ",
        "Grey with outline: dam buffer zone (not suitable).\n",
        "Plain grey: unsuitable, outside buffer. ",
        "\u25b2 Dams  \u25cf Fish occurrences."
      ),
      theme = theme(
        plot.title   = element_text(face = "italic", size = 11, hjust = 0.5),
        plot.caption = element_text(size = 8, hjust = 0)
      )
    )
}

for (sp in MAIN_TEXT_SPECIES) {
  p <- make_fragment_map(sp); if (is.null(p)) next
  out <- paste0("figures/maps/map2_fragments_buffer_", sp, "_main.png")
  png(out, width = 10, height = 5, units = "in", res = 300)
  print(p); dev.off()
  message("  Saved: ", out)
}

# Supplementary: rows = species, columns = scenario (current | future)
frag_plots_supp <- list()
for (sp in TARGET_SPECIES) {
  p_cur <- make_fragment_panel(
    sp, "current", dams_current_sf,
    paste0(gsub("_", " ", sp), " — current")
  )
  p_fut <- make_fragment_panel(
    sp, "future",  dams_future_sf,
    paste0(gsub("_", " ", sp), " — future")
  )
  if (!is.null(p_cur)) frag_plots_supp[[paste0(sp, "_current")]] <- p_cur
  if (!is.null(p_fut)) frag_plots_supp[[paste0(sp, "_future")]]  <- p_fut
}

if (length(frag_plots_supp) > 0) {
  p_supp <- wrap_plots(frag_plots_supp, ncol = 2, byrow = TRUE) +
    plot_annotation(
      title   = "Habitat fragments + dam buffers per species — current vs future",
      caption = paste0(
        "Coloured: fragments. Grey with outline: dam buffer zone (future only). ",
        "Plain grey: unsuitable.\n\u25b2 Dams  \u25cf Fish occurrences."
      ),
      theme = theme(
        plot.title   = element_text(face = "bold", size = 11, hjust = 0.5),
        plot.caption = element_text(size = 8, hjust = 0)
      )
    )
  out_supp <- "figures/maps/map2_fragments_buffer_all_species_supplementary.png"
  png(out_supp,
      width  = 10,
      height = length(TARGET_SPECIES) * 4,
      units  = "in", res = 300)
  print(p_supp); dev.off()
  message("  Saved: ", out_supp)
}

# ============================================================
# STEP 7: Species impact ranking barplot
# ============================================================

message("\n=== Step 7: Species impact ranking barplot ===")

# Grouped barplot: n_fragments in current vs future scenario per species,
# ordered by delta_fragments (most impacted species first).
# Shows both the baseline fragmentation and the additional impact of
# planned dams side by side.

ranking <- fread("sdm/patch_metrics/species_impact_ranking.csv")

# Reshape to long format for grouped bars
ranking_long <- ranking %>%
  select(species, n_frag_current, n_frag_future, impact_rank) %>%
  tidyr::pivot_longer(
    cols      = c(n_frag_current, n_frag_future),
    names_to  = "scenario",
    values_to = "n_fragments"
  ) %>%
  mutate(
    scenario    = if_else(scenario == "n_frag_current", "Current", "Future"),
    scenario    = factor(scenario, levels = c("Current", "Future")),
    # Order species by impact rank (most impacted first)
    species_label = factor(
      gsub("_", " ", species),
      levels = gsub("_", " ", ranking$species[order(ranking$impact_rank)])
    )
  )

p_ranking <- ggplot(ranking_long,
                    aes(x = species_label, y = n_fragments, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  # Annotate delta_fragments above the future bar
  geom_text(
    data = ranking %>%
      mutate(species_label = factor(gsub("_", " ", species),
                                    levels = gsub("_", " ", ranking$species[order(ranking$impact_rank)]))),
    aes(x = species_label, y = n_frag_future + 0.8,
        label = paste0("+", delta_fragments)),
    inherit.aes = FALSE,
    size = 3, colour = "grey30"
  ) +
  scale_fill_manual(
    values = c("Current" = "#74add1", "Future" = "#d73027"),
    name   = "Scenario"
  ) +
  scale_x_discrete(labels = function(x) expression(italic(x))) +
  labs(
    x        = NULL,
    y        = "Number of habitat fragments",
    title    = "Habitat fragmentation per species — current vs future dam scenario",
    subtitle = "Values above bars show additional fragments created by planned dams"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x  = element_text(face = "italic", angle = 35, hjust = 1),
    legend.position = "top",
    plot.subtitle   = element_text(size = 9, colour = "grey40")
  )

out_ranking <- "figures/patch_metrics/barplot_species_impact_ranking.png"
png(out_ranking, width = 8, height = 5, units = "in", res = 200)
print(p_ranking); dev.off()
message("  Saved: ", out_ranking)

# ============================================================
# STEP 8: Patch size stacked barplot
# ============================================================

message("\n=== Step 8: Patch size stacked barplot ===")

# Stacked barplot: total suitable habitat length per species,
# each bar segment = one patch, ordered by patch size (largest at bottom).
# Shows both total habitat extent and how it is distributed across patches.

patch_summary_all <- fread("sdm/patch_metrics/patch_summary_all.csv")

# Load per-patch summaries for all species
patch_per_species <- lapply(TARGET_SPECIES, function(sp) {
  f <- paste0("sdm/patch_metrics/patch_membership_", sp, ".csv")
  if (!file.exists(f)) return(NULL)
  fread(f) %>%
    group_by(species, patch_id) %>%
    summarise(length_km = round(sum(reach_length_m, na.rm = TRUE) / 1000, 3),
              .groups = "drop") %>%
    as.data.table()
}) %>%
  rbindlist(fill = TRUE)

# Order species by total suitable length (longest first)
sp_order <- patch_summary_all %>%
  arrange(desc(total_length_km)) %>%
  pull(species) %>%
  gsub("_", " ", .)

patch_per_species <- patch_per_species %>%
  mutate(
    species_label = factor(gsub("_", " ", species), levels = sp_order),
    # Within each species, order patches largest to smallest
    patch_id      = as.character(patch_id)
  )

p_patches <- ggplot(patch_per_species,
                    aes(x = species_label, y = length_km,
                        fill = reorder(patch_id, -length_km))) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = rep(brewer.pal(12, "Set3"), length.out = nrow(patch_per_species)),
    guide  = "none"   # too many patches to show a useful legend
  ) +
  labs(
    x        = NULL,
    y        = "Suitable habitat length (km)",
    title    = "Suitable habitat per species — length and patch structure",
    subtitle = "Each bar segment = one habitat patch; ordered largest to smallest"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x  = element_text(face = "italic", angle = 35, hjust = 1),
    plot.subtitle = element_text(size = 9, colour = "grey40")
  )

out_patches <- "figures/patch_metrics/barplot_patch_sizes.png"
png(out_patches, width = 7, height = 5, units = "in", res = 200)
print(p_patches); dev.off()
message("  Saved: ", out_patches)

# ============================================================
# STEP 9: Dam proximity dotplot
# ============================================================

message("\n=== Step 9: Dam proximity dotplot ===")

# Dotplot: one dot per patch per species, x = minimum along-network
# distance to nearest dam, y = patch length (km).
# Point size = patch length; colour = scenario.
# Shows which patches are immediately threatened (dist ~ 0) vs remote.

dam_prox <- fread("sdm/patch_metrics/dam_patch_proximity.csv") %>%
  mutate(
    species_label = factor(gsub("_", " ", species),
                           levels = gsub("_", " ", TARGET_SPECIES)),
    scenario      = factor(scenario, levels = c("current", "future"),
                           labels = c("Current", "Future"))
  )

p_proximity <- ggplot(dam_prox,
                      aes(x = min_dist_dam_km, y = species_label,
                          colour = scenario, size = patch_length_km)) +
  geom_point(alpha = 0.75, position = position_jitter(height = 0.15, seed = 42)) +
  scale_colour_manual(
    values = c("Current" = "#74add1", "Future" = "#d73027"),
    name   = "Scenario"
  ) +
  scale_size_continuous(
    name   = "Patch length (km)",
    range  = c(1.5, 7),
    breaks = c(5, 20, 50, 100)
  ) +
  # Flag patches that directly overlap a dam
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  labs(
    x        = "Minimum along-network distance to nearest dam (km)",
    y        = NULL,
    title    = "Distance of habitat patches from nearest dam",
    subtitle = "Point size = patch length (km). Dashed line = patch overlaps dam reach."
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.y  = element_text(face = "italic"),
    legend.position = "right",
    plot.subtitle   = element_text(size = 9, colour = "grey40")
  )

out_proximity <- "figures/patch_metrics/dotplot_dam_proximity.png"
png(out_proximity, width = 8, height = 5, units = "in", res = 200)
print(p_proximity); dev.off()
message("  Saved: ", out_proximity)

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("HABITAT FRAGMENTATION FIGURES COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("\nHistograms:")
message("  figures/patch_metrics/hist_dist_point_{species}.png")
message("  figures/patch_metrics/hist_dist_patch_{species}.png")
message("  figures/patch_metrics/hist_dist_barbus_salmo_main.png")
message("\nSummary charts:")
message("  figures/patch_metrics/barplot_species_impact_ranking.png")
message("  figures/patch_metrics/barplot_patch_sizes.png")
message("  figures/patch_metrics/dotplot_dam_proximity.png")
message("\nMaps:")
message("  figures/maps/map1_patches_{species}_main.png")
message("  figures/maps/map1_patches_all_species_supplementary.png")
message("  figures/maps/map2_fragments_buffer_{species}_main.png")
message("  figures/maps/map2_fragments_buffer_all_species_supplementary.png")
message("\nNext: 08_pci.R")
