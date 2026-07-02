#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 07_functional_trait_analysis.R   (Module 10 -- Traits)
#
# Group the focal Sarantaporos fish species by their functional traits and
# map the resulting composition and diversity across the sub-basin. Three
# related analyses share one trait table and one grouping:
#
#   Chapter 1  Functional trait dendrogram (Gower + Ward), with a side panel
#              showing each group's trait composition.
#   Chapter 2  Functional-group pie map: each site as a pie of group shares,
#              over basin + stream network, pie size = richness.
#   Chapter 3  Functional diversity map: each site coloured by Rao's quadratic
#              entropy (continuous viridis scale), plus a sorted table.
#
# Target species come from the Sarantaporos checklist; trait values are read
# from the "Traits" sheet of the HCMR Excel file. Two species absent from the
# table take surrogate values (see SUBSTITUTE_* below). Discrete group colours
# use ColorBrewer "Set2", built dynamically from the number of groups produced
# by cutree(); the diversity map keeps a continuous viridis scale.
#
# INPUT:
#   - points_original/fish/species_list_sarantaporos.txt      (target species)
#   - points_original/fish/Fish distributional & traits data (1).xlsx
#       sheet "Traits" : functional traits per species
#   - points_snapped/fish/fish_hcmr_with_species_snapped.csv  (occurrences)
#   - spatial/subbasin_sarantaporos/subbasin_polygon.gpkg     (basin outline)
#   - spatial/subbasin_sarantaporos/stream_network_pruned.gpkg (river network)
#
# OUTPUT:
#   - figures/traits/fish_trait_dendrogram.png
#   - figures/traits/fish_group_pie_map.png
#   - figures/traits/fish_fd_map.png
#   - figures/traits/fish_fd_map_table.csv
#
# LOCATION: workflows/10_traits/07_functional_trait_analysis.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(data.table)
library(readxl)
library(dplyr)
library(tibble)
library(purrr)
library(cluster)
library(sf)
library(ggplot2)
library(ggdendro)
library(ggforce)
library(patchwork)
library(scales)
library(RColorBrewer)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("figures/traits", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

TRAITS_XLSX  <- "points_original/fish/Fish distributional & traits data (1).xlsx"
SPECIES_LIST <- "points_original/fish/species_list_sarantaporos.txt"
OCC_CSV      <- "points_snapped/fish/fish_hcmr_with_species_snapped.csv"
BASIN_GPKG   <- "spatial/subbasin_sarantaporos/subbasin_polygon.gpkg"
STREAM_GPKG  <- "spatial/subbasin_sarantaporos/stream_network_pruned.gpkg"

# Documented trait surrogates for species absent from the trait table:
#   (b) congener surrogate: copy a named close relative's row.
#   (c) genus surrogate: mode (categorical) + median (numeric) over all
#       congeners present in the table.
SUBSTITUTE_CONGENER     <- c("Chondrostoma_ohridanum" = "Chondrostoma_vardarense")
SUBSTITUTE_GENUS_MEDIAN <- "Squalius_platyceps"

# Number of functional groups to cut the dendrogram into. Two is a
# descriptive choice: the largest vertical gap isolates Anguilla.
N_GROUPS <- 2

# Unordered categorical traits (Gower treats these as factors); the remaining
# traits (Vertical_position, Migration, max_TL) stay numeric/ordered.
nominal_traits <- c("Diet", "Habitat", "Repro", "Morph", "Mouth", "Caudal_fin")

RIVER <- "#7d9bb0"   # fixed neutral colour for the stream network

# Human-readable category labels for each categorical trait code.
labels_list <- list(
  Diet       = c("1"="Herbivorous","2"="Insectivorous","3"="Omnivorous","4"="Piscivorous","5"="Detritivorous"),
  Habitat    = c("1"="Limnophilic","2"="Rheophilic","3"="Eurytopic"),
  Repro      = c("1"="Phytophilic","2"="Lithophilic","3"="Ostracophilic","4"="Pelagophilic","5"="Viviparous","6"="Mouth-brooding"),
  Morph      = c("1"="Fusiform","2"="Dorsoventrally flat","3"="Compressed","4"="Elongated","5"="Anguilliform"),
  Mouth      = c("1"="Superior","2"="Terminal","3"="Inferior/subterminal"),
  Caudal_fin = c("1"="Rounded","2"="Truncated","3"="Emarginate","4"="Forked","5"="Heterocercal","6"="Pointed")
)

# ============================================================
# CHAPTER 0: Load species checklist + trait table, resolve traits
# ============================================================

message("\n=== Loading species checklist and trait table ===")

# Target species from the Sarantaporos checklist (underscored names; drop a
# known typo variant of Chondrostoma).
species_list <- fread(SPECIES_LIST, header = TRUE) %>%
  mutate(species = gsub(" ", "_", species)) %>%
  filter(species != "Chondrostoma_ohridana") %>%
  distinct(species) %>%
  arrange(species) %>%
  pull(species)

# Trait table (sheet "Traits"); standardise names.
traits_raw <- read_xlsx(TRAITS_XLSX, sheet = "Traits") %>%
  rename(species = Species, Vertical_position = `Vertical position`) %>%
  mutate(species = gsub(" ", "_", species))

# The nine functional trait columns (everything except the species name),
# split by type for the genus surrogate: categorical -> mode, numeric -> median.
trait_vars <- setdiff(names(traits_raw), "species")
cat_vars   <- intersect(nominal_traits, trait_vars)
num_vars   <- setdiff(trait_vars, cat_vars)        # Vertical_position, max_TL, Migration

# Mode for categorical codes (ties broken by smallest value).
mode_stat <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# (c) genus surrogate values, computed once over all congeners.
squalius_rows <- traits_raw %>% filter(grepl("^Squalius", species))
squalius_surrogate <- bind_cols(
  squalius_rows %>% summarise(across(all_of(cat_vars), mode_stat)),
  squalius_rows %>% summarise(across(all_of(num_vars), ~ median(.x, na.rm = TRUE)))
)

# Resolve the full trait row for one target species.
resolve_traits <- function(sp) {
  # (b) congener surrogate: donor's whole row, relabelled.
  if (sp %in% names(SUBSTITUTE_CONGENER)) {
    donor <- SUBSTITUTE_CONGENER[[sp]]
    row <- traits_raw %>% filter(species == donor)
    if (nrow(row) == 1) {
      row$species <- sp
      return(row %>% mutate(source = paste0("substitute: ", donor)))
    }
  }
  # (c) genus surrogate: mode (categorical) + median (numeric) over congeners.
  if (sp == SUBSTITUTE_GENUS_MEDIAN) {
    return(tibble(species = sp) %>%
             bind_cols(squalius_surrogate) %>%
             mutate(source = "substitute: Squalius genus mode/median"))
  }
  # (a) direct match: the species' own row.
  row <- traits_raw %>% filter(species == sp)
  if (nrow(row) == 1) return(row %>% mutate(source = "trait table"))
  # not found.
  tibble(species = sp) %>%
    bind_cols(setNames(as.list(rep(NA_real_, length(trait_vars))), trait_vars)) %>%
    mutate(source = "MISSING")
}

traits <- map_dfr(species_list, resolve_traits) %>%
  mutate(across(all_of(trait_vars), ~ as.numeric(as.character(.x)))) %>%
  as.data.frame()
rownames(traits) <- traits$species

message(sprintf("  %d species (%d direct, %d substituted, %d missing)",
                nrow(traits),
                sum(traits$source == "trait table"),
                sum(grepl("^substitute", traits$source)),
                sum(traits$source == "MISSING")))
if (any(traits$source == "MISSING"))
  warning("Species with no trait data: ",
          paste(traits$species[traits$source == "MISSING"], collapse = ", "))

# ============================================================
# CHAPTER 1: Functional trait dendrogram
# ============================================================
# Gower distance handles the mix of unordered categories, ordered categories,
# and one real measurement (max_TL); Ward.D2 builds the tree.

message("\n=== Chapter 1: trait dendrogram ===")

# Factors for nominal traits; ordered/numeric traits stay numeric so
# neighbouring categories count as more similar than distant ones.
dist_vars <- c(nominal_traits, "Vertical_position", "Migration", "max_TL")
trait_for_dist <- traits[, dist_vars]
trait_for_dist[nominal_traits] <- lapply(trait_for_dist[nominal_traits], factor)

gower_dist <- daisy(trait_for_dist, metric = "gower")
hc     <- hclust(gower_dist, method = "ward.D2")
groups <- cutree(hc, k = N_GROUPS)

# --- Dynamic Set2 palette, one colour per group (recycled past 8 groups) ---
make_group_palette <- function(groups) {
  n_groups <- length(unique(groups))
  pal <- rep(
    suppressWarnings(brewer.pal(min(max(3, n_groups), 8), "Set2")),
    length.out = n_groups
  )
  setNames(pal, sort(unique(groups)))
}
group_pal    <- make_group_palette(groups)
group_colour <- setNames(group_pal[as.character(groups)], names(groups))

# Smallest group (Anguilla singleton at k = 2) for labelling.
group_sizes  <- table(groups)
singleton_id <- as.integer(names(group_sizes)[which.min(group_sizes)])
group_ids    <- sort(unique(groups))

# Meaningful labels for the 2-group case; generic otherwise.
group_labels <- if (N_GROUPS == 2) {
  setNames(ifelse(group_ids == singleton_id,
                  "Anguillids", "Resident freshwater fishes"), group_ids)
} else {
  setNames(paste("Group", group_ids), group_ids)
}

message("  Group membership (k = ", N_GROUPS, "):")
print(groups)

# --- Tree segments, coloured by group where a branch sits within one group ---
dd      <- dendro_data(as.dendrogram(hc), type = "rectangle")
seg     <- segment(dd)
tip_lab <- label(dd)
tip_lab$group_col <- group_colour[as.character(tip_lab$label)]

leaf_x <- setNames(tip_lab$x, as.character(tip_lab$label))
seg_group_colour <- apply(seg, 1, function(row) {
  xs   <- as.numeric(c(row["x"], row["xend"]))
  near <- names(leaf_x)[leaf_x >= min(xs) - 0.5 & leaf_x <= max(xs) + 0.5]
  gs   <- unique(groups[near])
  if (length(gs) == 1) group_pal[[as.character(gs)]] else "#9a9a95"
})

p_tree <- ggplot() +
  geom_segment(data = seg, aes(x = x, y = y, xend = xend, yend = yend),
               colour = seg_group_colour, linewidth = 1.3) +
  geom_text(data = tip_lab, aes(x = x, y = y - 0.01, label = gsub("_", "\n", label),
                                colour = group_col),
            hjust = 1, angle = 90, size = 4, lineheight = 0.85,
            fontface = "bold.italic", show.legend = FALSE) +
  scale_colour_identity() +
  # Genus and species sit on two stacked lines, so each label is shorter and
  # needs less room; the lower expansion still leaves a margin so none clip.
  scale_y_continuous(expand = expansion(mult = c(0.40, 0.05))) +
  labs(title = "Functional trait groups of Sarantaporos fish",
       y = "Gower dissimilarity (Ward.D2 linkage)", x = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.background = element_rect(fill = "#fafaf8", colour = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(colour = "#14323a", face = "bold"),
        plot.title  = element_text(face = "bold", colour = "#14323a"))

# --- Side panel: 100% stacked composition bars per categorical trait/group ---
comp_rows <- list()
for (g in group_ids) {
  members <- names(groups)[groups == g]
  gname   <- paste0(group_labels[[as.character(g)]], " (", length(members), " spp.)")
  for (tr in nominal_traits) {
    tab   <- table(factor(traits[members, tr]))
    tab   <- tab[tab > 0]
    props <- as.numeric(tab) / length(members)
    cats  <- labels_list[[tr]][names(tab)]
    o     <- order(-props)                          # most common category first
    comp_rows[[length(comp_rows) + 1]] <- data.frame(
      group = gname, trait = tr, category = cats[o], prop = props[o],
      rank = seq_along(o), base = group_pal[[as.character(g)]],
      stringsAsFactors = FALSE)
  }
}
comp <- do.call(rbind, comp_rows)
comp$trait <- factor(comp$trait, levels = rev(nominal_traits))

# Lighten the group base colour for lower-ranked categories.
lighten <- function(hex, f) {
  v <- col2rgb(hex) / 255
  grDevices::rgb(v[1] + (1 - v[1]) * f, v[2] + (1 - v[2]) * f, v[3] + (1 - v[3]) * f)
}
comp$fill <- mapply(function(base, rank) lighten(base, min(0.28 * (rank - 1), 0.78)),
                    comp$base, comp$rank)

p_bars <- ggplot(comp, aes(x = prop, y = trait, fill = I(fill))) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(prop >= 0.18, paste0(category, " ", round(prop * 100), "%"), "")),
            position = position_stack(vjust = 0.5), size = 4, colour = "white",
            fontface = "bold") +
  facet_wrap(~ group, ncol = 1, scales = "free_y") +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  labs(title = "Trait composition per group", x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.background = element_rect(fill = "#fafaf8", colour = NA),
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold", colour = "#14323a", size = 14),
        plot.title = element_text(face = "bold", colour = "#14323a", size = 15),
        axis.text.y = element_text(size = 13, colour = "#14323a", face = "bold"),
        axis.text.x = element_text(size = 10, colour = "#5d7479"))

dendro_fig <- patchwork::wrap_plots(p_tree, p_bars, widths = c(1.4, 1))
ggsave("figures/traits/fish_trait_dendrogram.png", dendro_fig,
       width = 11, height = 7, dpi = 300, bg = "#fafaf8")
message("  Saved: figures/traits/fish_trait_dendrogram.png")

# ============================================================
# CHAPTER 2: Functional-group pie map
# ============================================================

message("\n=== Chapter 2: functional-group pie map ===")

focal_species <- traits$species                     # resolved focal set
grp_lab <- setNames(as.character(groups), names(groups))   # species -> group id

# --- Spatial layers (basin + streams), both WGS84. Read here because the
#     basin polygon is also used to spatially filter occurrences below. ---
basin   <- st_read(BASIN_GPKG,  quiet = TRUE)
streams <- st_read(STREAM_GPKG, quiet = TRUE)
if (is.na(st_crs(basin)))   st_crs(basin)   <- 4326
if (is.na(st_crs(streams))) st_crs(streams) <- 4326
streams <- st_transform(streams, st_crs(basin))

# --- Occurrences: tidy names, merge duplicate VOID site, focal species,
#     snapped coordinates so points sit on the river network ---
occ <- fread(OCC_CSV, encoding = "UTF-8")
setnames(occ, old = names(occ), new = trimws(names(occ)))
occ[, species := gsub(" ", "_", trimws(species))]   # normalise to underscores
occ[grepl("VOID", Sites, ignore.case = TRUE), Sites := "VOID-AOOS"]
occ <- occ[species %in% focal_species & Sites != ""]
occ[, `:=`(lon = longitude_snapped, lat = latitude_snapped)]
occ[, grp := grp_lab[species]]

# --- Spatial filter: keep only occurrences inside the Sarantaporos basin
#     polygon. The occurrence file spans the whole Vjosa/Aoos basin, so sites
#     in neighbouring subbasins (e.g. Voidomatis) are dropped here. ---
occ_sf  <- st_as_sf(occ, coords = c("lon", "lat"), crs = st_crs(basin), remove = FALSE)
inside  <- lengths(st_intersects(occ_sf, st_union(basin))) > 0
n_before <- nrow(occ)
occ <- occ[inside]
message(sprintf("  Spatial filter: kept %d of %d occurrences inside Sarantaporos (%d dropped)",
                nrow(occ), n_before, n_before - nrow(occ)))

# --- Per-site counts: richness and one count column per group (gN) ---
site <- occ[, .(lon = first(lon), lat = first(lat),
                richness = uniqueN(species)), by = Sites]
for (g in group_ids) {
  counts_g <- occ[grp == as.character(g), .(n = uniqueN(species)), by = Sites]
  site[counts_g, on = "Sites", paste0("g", g) := i.n]
}
gcols <- paste0("g", group_ids)
for (col in gcols) site[is.na(get(col)), (col) := 0]

# --- Pie radius scaled by richness; nudge overlapping pies apart, recording
#     which moved so a leader line can point back to the true location ---
rmin <- min(site$richness); rmax <- max(site$richness)
# Pie radius scaled by richness; the additive term sets the smallest pie and
# the multiplier the extra size for richer sites (kept modest so the largest
# pies don't dominate the map).
site[, r := 0.010 + 0.014 * (richness - rmin) / (rmax - rmin)]
site[, `:=`(plot_lon = lon, plot_lat = lat, moved = FALSE)]
ord <- order(-site$richness)
for (ii in seq_along(ord)) for (jj in seq_along(ord)) {
  if (jj <= ii) next
  a <- ord[ii]; b <- ord[jj]
  dx <- site$plot_lon[b] - site$plot_lon[a]; dy <- site$plot_lat[b] - site$plot_lat[a]
  dist <- sqrt(dx^2 + dy^2); mind <- (site$r[a] + site$r[b]) * 1.15
  if (dist < mind) {
    ang <- if (dist == 0) runif(1, 0, 2 * pi) else atan2(dy, dx)
    site$plot_lon[b] <- site$plot_lon[a] + cos(ang) * mind
    site$plot_lat[b] <- site$plot_lat[a] + sin(ang) * mind
    site$moved[b] <- TRUE
  }
}

# --- Pie wedges (start/end angles) from each site's per-group counts ---
make_wedges <- function(s) {
  rows <- list()
  for (i in seq_len(nrow(s))) {
    counts <- setNames(as.numeric(s[i, gcols]), as.character(group_ids))
    counts <- counts[counts > 0]
    tot <- sum(counts); a0 <- 0
    for (g in names(counts)) {
      a1 <- a0 + 2 * pi * counts[[g]] / tot
      rows[[length(rows) + 1]] <- data.frame(
        x = s$plot_lon[i], y = s$plot_lat[i], r = s$r[i],
        start = a0, end = a1, group = g, stringsAsFactors = FALSE)
      a0 <- a1
    }
  }
  do.call(rbind, rows)
}
wedges <- make_wedges(as.data.frame(site))

bb <- st_bbox(basin); site_df <- as.data.frame(site)

p_pie <- ggplot() +
  geom_sf(data = basin, fill = "#eef3f1", colour = "#7a8c8a", linewidth = 0.5) +
  geom_sf(data = streams, aes(linewidth = strahler), colour = RIVER, alpha = 0.9,
          lineend = "round", show.legend = FALSE) +
  scale_linewidth(range = c(0.15, 1.1)) +
  geom_segment(data = site_df[site_df$moved, ],
               aes(x = lon, y = lat, xend = plot_lon, yend = plot_lat),
               colour = "#6b7b79", linewidth = 0.3) +
  geom_point(data = site_df[site_df$moved, ], aes(lon, lat), colour = "#6b7b79", size = 0.6) +
  geom_arc_bar(data = wedges, aes(x0 = x, y0 = y, r0 = 0, r = r, start = start, end = end, fill = group),
               colour = "white", linewidth = 0.25) +
  # Ghost layer: points mapped to richness, placed far off-canvas (not
  # alpha=0, which can blank the legend key too) so they never show on the
  # map but still generate a size legend for the pies. shape 21 = filled
  # circle, matching the pie look.
  geom_point(data = site_df, aes(size = richness),
             x = bb[["xmin"]] - 100, y = bb[["ymin"]] - 100,
             shape = 21, fill = "grey70", colour = "grey30") +
  scale_size_continuous(range = c(2.5, 7), name = "Species richness",
                        breaks = c(1, 4, 7)) +
  scale_fill_manual(values = group_pal,
                    labels = group_labels[as.character(group_ids)],
                    name = "Functional group") +
  coord_sf(xlim = c(bb["xmin"] - 0.03, bb["xmax"] + 0.03),
           ylim = c(bb["ymin"] - 0.03, bb["ymax"] + 0.03)) +
  labs(title = "Sarantaporos fish assemblages \u2014 functional-group composition",
       subtitle = "Stream network beneath (width = Strahler order); pies = group composition; size = richness",
       x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(order = 1), size = guide_legend(order = 2)) +
  theme_bw(base_size = 12) +
  theme(plot.background = element_rect(fill = "#fafaf8", colour = NA),
        panel.background = element_rect(fill = "#fafaf8", colour = "#b8c2c1"),
        panel.grid = element_line(colour = "#e3e9e8"),
        plot.title = element_text(face = "bold", colour = "#14323a",
                                  margin = margin(b = 2)),
        plot.subtitle = element_text(colour = "#5d7479", size = 9,
                                     margin = margin(b = 2)),
        plot.title.position = "plot",
        legend.position = "right",
        legend.box = "vertical",
        legend.key = element_rect(fill = "#fafaf8", colour = NA))

pie_fig <- p_pie
ggsave("figures/traits/fish_group_pie_map.png", pie_fig,
       width = 12, height = 10, dpi = 300, bg = "#fafaf8")
message("  Saved: figures/traits/fish_group_pie_map.png")

# ============================================================
# CHAPTER 3: Functional diversity map (Rao's Q, viridis)
# ============================================================

message("\n=== Chapter 3: functional diversity map ===")

Dm <- as.matrix(gower_dist)

# Rao's Q: mean pairwise Gower distance among species present (0 if < 2 spp).
raoQ <- function(spp) {
  spp <- unique(spp)
  if (length(spp) < 2) return(0)
  sub <- Dm[spp, spp, drop = FALSE]
  mean(sub[upper.tri(sub)])
}

site_fd <- occ[, .(lon = first(lon), lat = first(lat),
                   richness = uniqueN(species), FD = raoQ(species),
                   anguilla = ifelse("Anguilla_anguilla" %in% species, "yes", "no"),
                   species_list = paste(sort(unique(species)), collapse = "; ")), by = Sites]

p_fd <- ggplot() +
  geom_sf(data = basin, fill = "#eef3f1", colour = "#7a8c8a", linewidth = 0.5) +
  geom_sf(data = streams, aes(linewidth = strahler), colour = RIVER, alpha = 0.9,
          lineend = "round", show.legend = FALSE) +
  scale_linewidth(range = c(0.15, 1.1)) +
  geom_point(data = site_fd, aes(lon, lat, fill = FD, size = richness),
             shape = 21, colour = "#2a2a2a", stroke = 0.4) +
  scale_fill_viridis_c(option = "plasma", name = "Functional\ndiversity (Rao's Q)",
                       limits = c(0, max(site_fd$FD))) +
  scale_size_continuous(range = c(3, 11), name = "Species richness", breaks = c(1, 4, 7)) +
  coord_sf(xlim = c(bb["xmin"] - 0.03, bb["xmax"] + 0.03),
           ylim = c(bb["ymin"] - 0.03, bb["ymax"] + 0.03)) +
  labs(title = "Sarantaporos fish \u2014 functional diversity across sampling sites",
       subtitle = "Stream network beneath (width = Strahler order); colour = Rao's Q; size = species richness",
       x = "Longitude", y = "Latitude") +
  guides(fill = guide_colourbar(order = 1), size = guide_legend(order = 2)) +
  theme_bw(base_size = 12) +
  theme(plot.background = element_rect(fill = "#fafaf8", colour = NA),
        panel.background = element_rect(fill = "#fafaf8", colour = "#b8c2c1"),
        panel.grid = element_line(colour = "#e3e9e8"),
        plot.title = element_text(face = "bold", colour = "#14323a"),
        plot.subtitle = element_text(colour = "#5d7479", size = 9),
        legend.position = "right", legend.box = "vertical",
        legend.key = element_rect(fill = "#fafaf8", colour = NA))

ggsave("figures/traits/fish_fd_map.png", p_fd, width = 11, height = 10, dpi = 300, bg = "#fafaf8")
message("  Saved: figures/traits/fish_fd_map.png")

# Functional-diversity table (sorted high to low) + Anguilla comparison.
fd_table <- site_fd[order(-FD), .(
  Site = Sites, Richness = richness, FD_RaoQ = round(FD, 3), Anguilla = anguilla,
  Longitude = round(lon, 5), Latitude = round(lat, 5), Species = species_list)]
fwrite(fd_table, "figures/traits/fish_fd_map_table.csv")
message("  Saved: figures/traits/fish_fd_map_table.csv")

message(sprintf("\n  Mean FD with Anguilla:    %.3f (n=%d sites)",
                site_fd[anguilla == "yes", mean(FD)], site_fd[anguilla == "yes", .N]))
message(sprintf("  Mean FD without Anguilla: %.3f (n=%d multi-species sites)",
                site_fd[anguilla == "no" & richness > 1, mean(FD)],
                site_fd[anguilla == "no" & richness > 1, .N]))

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("FUNCTIONAL TRAIT ANALYSIS COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("  figures/traits/fish_trait_dendrogram.png")
message("  figures/traits/fish_group_pie_map.png")
message("  figures/traits/fish_fd_map.png")
message("  figures/traits/fish_fd_map_table.csv")
