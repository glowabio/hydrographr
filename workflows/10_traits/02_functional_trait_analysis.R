#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_functional_trait_analysis.R   (Module 10 -- Traits)
#
# Group the focal Sarantaporos fish species by their functional traits and
# map the resulting composition and diversity across the sub-basin. Three
# related analyses share one trait table and one grouping:
#
#   Chapter 1  Functional trait dendrogram (Gower + Ward) cut at CUT_HEIGHT,
#              over a trait matrix with one column per species, aligned to
#              its tip.
#   Chapter 2  Functional-group pie map: each site as a pie of group shares,
#              over basin + stream network, pie size = richness.
#   Chapter 3  Functional diversity map: each site coloured by Rao's quadratic
#              entropy (continuous viridis scale), plus a sorted table.
#
# Target species come from the Sarantaporos checklist; trait values are read
# from the "Traits" sheet of the HCMR Excel file. Two species absent from the
# table take surrogate values (see SUBSTITUTE_* below). Discrete group colours
# use the colour-blind-safe Okabe-Ito palette, built dynamically from the
# number of groups produced by cutting the tree at CUT_HEIGHT; the diversity
# map keeps a continuous viridis scale.
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
# LOCATION: workflows/10_traits/02_functional_trait_analysis.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(data.table)
library(readxl)
library(dplyr)
library(tibble)
library(purrr)
library(cluster)
library(sf)
library(ggplot2)
library(ggforce)
library(patchwork)
library(scales)

select <- dplyr::select

if (!exists("WORKFLOWS_DIR"))
  WORKFLOWS_DIR <- Sys.getenv("WORKFLOWS_CODE", "/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows")
source(file.path(WORKFLOWS_DIR, "helpers", "config.R"))
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

# Height at which the dendrogram is cut into functional groups. The cut is
# drawn on the figure as a dashed line, and the groups are derived from the
# same value (cutree(h = ...)) so line and colours can never disagree.
# At 0.4 the tree falls into four groups (merge heights 0.31 and 0.48 sit
# either side of it).
CUT_HEIGHT <- 0.4

# Unordered categorical traits (Gower treats these as factors); the remaining
# traits (Vertical_position, Migration, max_TL) stay numeric/ordered.
nominal_traits <- c("Diet", "Habitat", "Repro", "Morph", "Mouth", "Caudal_fin")

RIVER <- "grey70"   # fixed neutral colour for the stream network

# Human-readable category labels for each trait code, following the "legend"
# sheet of the trait workbook. A few are shortened to fit a matrix cell
# ("Inferior" for "Inferior or subterminal", "Flattened" for
# "Dorsoventrally flattened").
labels_list <- list(
  Diet              = c("1"="Herbivorous","2"="Insectivorous","3"="Omnivorous","4"="Piscivorous","5"="Detritivorous"),
  Habitat           = c("1"="Limnophilic","2"="Rheophilic","3"="Eurytopic"),
  Repro             = c("1"="Phytophilic","2"="Lithophilic","3"="Ostracophilic","4"="Pelagophilic","5"="Viviparous","6"="Mouth-brooding"),
  Morph             = c("1"="Fusiform","2"="Flattened","3"="Compressed","4"="Elongated","5"="Anguilliform"),
  Mouth             = c("1"="Superior","2"="Terminal","3"="Inferior"),
  Caudal_fin        = c("1"="Rounded","2"="Truncated","3"="Emarginate","4"="Forked","5"="Heterocercal","6"="Pointed"),
  Vertical_position = c("1"="Demersal","2"="Benthopelagic","3"="Pelagic"),
  Migration         = c("0"="Non-migratory","1"="Potamodromous","2"="Long-distance")
)

# Trait rows of the matrix panel, top to bottom, with the row labels used in
# the figure. max_TL is numeric and printed as a value, the rest are codes
# translated through labels_list above.
matrix_traits <- c(Diet              = "Diet",
                   Habitat           = "Habitat",
                   Repro             = "Reproduction",
                   Morph             = "Body shape",
                   Mouth             = "Mouth position",
                   Caudal_fin        = "Caudal fin",
                   Vertical_position = "Vertical position",
                   Migration         = "Migration",
                   max_TL            = "Max. length (cm)")

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
groups <- cutree(hc, h = CUT_HEIGHT)
N_GROUPS <- length(unique(groups))

# --- Colour-blind-safe palette (Okabe & Ito 2008), one colour per group.
#     Hues are assigned in this fixed order and recycled past eight groups.
#     Checked under deuteranopia, protanopia and tritanopia simulation: the
#     closest pair stays ~16 CIE Lab units apart, so all four groups remain
#     distinguishable for every common form of colour vision deficiency. ---
OKABE_ITO <- c("#0072B2",  # blue
               "#E69F00",  # orange
               "#009E73",  # bluish green
               "#CC79A7",  # reddish purple
               "#56B4E9",  # sky blue
               "#D55E00",  # vermillion
               "#F0E442",  # yellow
               "#000000")  # black
make_group_palette <- function(groups) {
  n_groups <- length(unique(groups))
  pal <- rep(OKABE_ITO, length.out = n_groups)
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

message("  Group membership (cut at ", CUT_HEIGHT, " -> k = ", N_GROUPS, "):")
print(groups)

# --- Tree segments, walked straight off the hclust merge matrix ---
# Each segment is coloured by the cluster it actually represents: a branch
# whose leaves all fall in one group takes that group's colour, a branch
# spanning groups is grey. (Deriving colour from the merge structure rather
# than from leaf positions matters for the tall risers, which sit at an x
# midway between their children and so cannot be identified positionally.)

# Tip positions, left to right; the matrix panel below reuses them so every
# column sits under its own tip.
tip_lab  <- data.frame(label = hc$labels[hc$order],
                       x     = seq_along(hc$order),
                       stringsAsFactors = FALSE)
tip_lab$group_col <- group_colour[tip_lab$label]
leaf_x   <- setNames(tip_lab$x, tip_lab$label)
n_tips   <- nrow(tip_lab)
x_limits <- c(0.5, n_tips + 0.5)

# Colour for a set of leaves (given as row indices of the original data).
branch_colour <- function(leaves) {
  gs <- unique(groups[hc$labels[leaves]])
  if (length(gs) == 1) group_pal[[as.character(gs)]] else "#9a9a95"
}

# Walk the merges bottom-up, recording each node's leaf set and x position
# (the midpoint of its two children, the standard dendrogram layout).
node_leaves <- vector("list", n_tips - 1)
node_x      <- numeric(n_tips - 1)
seg_rows    <- list()

for (k in seq_len(n_tips - 1)) {
  child   <- hc$merge[k, ]
  child_x <- numeric(2); child_y <- numeric(2); child_col <- character(2)
  leaves  <- list()
  for (j in 1:2) {
    if (child[j] < 0) {                       # a leaf
      leaf <- -child[j]
      leaves[[j]]  <- leaf
      child_x[j]   <- leaf_x[[hc$labels[leaf]]]
      child_y[j]   <- 0
    } else {                                  # an earlier merge
      leaves[[j]]  <- node_leaves[[child[j]]]
      child_x[j]   <- node_x[child[j]]
      child_y[j]   <- hc$height[child[j]]
    }
    child_col[j] <- branch_colour(leaves[[j]])
  }
  node_leaves[[k]] <- unlist(leaves)
  node_x[k]        <- mean(child_x)
  h                <- hc$height[k]

  # Two risers (each in its own child's colour) plus the crossbar joining
  # them (in the merged cluster's colour).
  seg_rows[[length(seg_rows) + 1]] <- data.frame(
    x      = c(child_x[1], child_x[2], child_x[1]),
    y      = c(child_y[1], child_y[2], h),
    xend   = c(child_x[1], child_x[2], child_x[2]),
    yend   = c(h, h, h),
    colour = c(child_col[1], child_col[2], branch_colour(node_leaves[[k]])),
    stringsAsFactors = FALSE)
}
seg <- do.call(rbind, seg_rows)
seg_group_colour <- seg$colour

p_tree <- ggplot() +
  # The cut that defines the groups, drawn behind the tree.
  geom_hline(yintercept = CUT_HEIGHT, linetype = "dashed",
             colour = "#5d7479", linewidth = 0.7) +
  annotate("text", x = n_tips + 0.45, y = CUT_HEIGHT,
           label = paste0("cut at ", CUT_HEIGHT),
           hjust = 1, vjust = -0.6, size = 3.8, colour = "#5d7479") +
  geom_segment(data = seg, aes(x = x, y = y, xend = xend, yend = yend),
               colour = seg_group_colour, linewidth = 1.3) +
  # Species names live on the matrix panel below, so the tree keeps only a
  # small margin under the tips.
  scale_x_continuous(limits = x_limits, expand = expansion(add = 0)) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(title = "Functional trait groups of Sarantaporos fish",
       y = "Gower dissimilarity (Ward.D2 linkage)", x = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(colour = "#14323a", face = "bold"),
        plot.title  = element_text(face = "bold", colour = "#14323a"),
        plot.margin = margin(t = 5, r = 5, b = 0, l = 5))

# --- Lower panel: trait matrix, one column per species aligned to its tip ---
# Cells carry the trait state as text, so the panel needs no colour key; the
# only colour is a light wash of the species' group, which ties each column
# back to the branch above it. Every species is shown, so the panel states
# what the groups share and where they differ without any percentages.

# Lighten a colour towards white by fraction f (1 = white).
lighten <- function(hex, f) {
  v <- col2rgb(hex) / 255
  grDevices::rgb(v[1] + (1 - v[1]) * f, v[2] + (1 - v[2]) * f, v[3] + (1 - v[3]) * f)
}

cell_label <- function(trait, value) {
  if (is.na(value)) return("–")
  if (trait == "max_TL") return(formatC(value, format = "fg", digits = 3))
  lab <- labels_list[[trait]][as.character(value)]
  if (is.na(lab)) as.character(value) else lab
}

mat <- expand.grid(species = tip_lab$label, trait = names(matrix_traits),
                   stringsAsFactors = FALSE)
mat$x     <- leaf_x[mat$species]
mat$row   <- match(mat$trait, names(matrix_traits))   # 1 = top row
mat$label <- mapply(function(sp, tr) cell_label(tr, traits[sp, tr]),
                    mat$species, mat$trait)
mat$group <- groups[mat$species]
mat$fill  <- vapply(mat$group, function(g) lighten(group_pal[[as.character(g)]], 0.86), "")

# Species names, two lines, in their group's colour; used as the top axis.
tip_names   <- gsub("_", "\n", tip_lab$label)
tip_colours <- unname(group_colour[tip_lab$label])

p_matrix <- ggplot(mat, aes(x = x, y = -row)) +
  geom_tile(aes(fill = I(fill)), width = 0.94, height = 0.9) +
  geom_text(aes(label = label), size = 3.3, colour = "#14323a") +
  scale_x_continuous(limits = x_limits, expand = expansion(add = 0),
                     breaks = tip_lab$x, labels = tip_names, position = "top") +
  scale_y_continuous(breaks = -seq_along(matrix_traits),
                     labels = unname(matrix_traits),
                     expand = expansion(add = 0.15)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid   = element_blank(),
        axis.text.x.top = element_text(size = 9.5, face = "bold.italic",
                                       colour = tip_colours, lineheight = 0.9,
                                       margin = margin(b = 4)),
        axis.text.y  = element_text(size = 10.5, face = "bold", colour = "#14323a",
                                    hjust = 1),
        plot.margin  = margin(t = 0, r = 5, b = 5, l = 5))

dendro_fig <- p_tree / p_matrix + patchwork::plot_layout(heights = c(1, 1.15))
png("figures/traits/fish_trait_dendrogram.png",
    width = 3000, height = 2400, res = 300, bg = "transparent")
print(dendro_fig)
dev.off()
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
  # Sub-basin boundary \u2014 thin outline, no fill, for spatial context
  geom_sf(data = basin, fill = NA, colour = "grey30", linewidth = 0.3) +
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
  guides(fill = guide_legend(order = 1), size = guide_legend(order = 2)) +
  theme_void(base_size = 20) +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = "#b8c2c1"),
        legend.position = "right",
        legend.box = "vertical",
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text  = element_text(size = 16),
        legend.key.size = unit(1.1, "cm"))

pie_fig <- p_pie
png("figures/traits/fish_group_pie_map.png",
    width = 3600, height = 3000, res = 300, bg = "transparent")
print(pie_fig)
dev.off()
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
  # Sub-basin boundary \u2014 thin outline, no fill, for spatial context
  geom_sf(data = basin, fill = NA, colour = "grey30", linewidth = 0.3) +
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
  guides(fill = guide_colourbar(order = 1), size = guide_legend(order = 2)) +
  theme_void(base_size = 20) +
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = "#b8c2c1"),
        legend.position = "right", legend.box = "vertical",
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text  = element_text(size = 16),
        legend.key.size = unit(1.1, "cm"))

png("figures/traits/fish_fd_map.png",
    width = 3300, height = 3000, res = 300, bg = "transparent")
print(p_fd)
dev.off()
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
