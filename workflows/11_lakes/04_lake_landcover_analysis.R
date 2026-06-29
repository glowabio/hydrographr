#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 04_lake_landcover_analysis.R   (Module 11 -- Lake analysis)
#
# Build a land cover area time series (1992-2020) for the lake catchment.
# For each year, per-sub-catchment ESA CCI class proportions are weighted by
# sub-catchment area, summed across the catchment, and plotted by class.
#
# Workflow:
#   1. get_predict_table() -> per-sub-catchment land cover proportions
#   2. Compute sub-catchment areas, weight proportions to km^2
#   3. Sum per class per year, reshape to long
#   4. Plot the land cover time series
#   5. Rank classes by area for the most recent year
#   6. Plot individual land cover classes, colored by group
#
# INPUT:
#   - data/subc_IDs.txt                                  (from 03_)
#   - data/spatial/subc_id_lake_catchment.tif             (from 03_)
#   - data/env90m/esa_cci_landcover_v2_1_1/*.txt          (from 01_)
#
# OUTPUT:
#   - data/env90m/predictTB.csv                           (land cover table)
#   - figures/lake_landcover_timeseries.png
#   - figures/lake_landcover_classes_grouped_colors.png
#
# LOCATION: workflows/XX_lakes/04_lake_landcover_analysis.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(terra)
library(data.table)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("figures", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

TILE_ID <- "h20v04"
YEARS   <- 1992:2020

# 22 ESA CCI land cover classes.
LC_CLASSES <- c("c10", "c20", "c30", "c40", "c50", "c60", "c70", "c80", "c90",
                "c100", "c110", "c120", "c130", "c140", "c150", "c160", "c170",
                "c180", "c190", "c200", "c210", "c220")

# One hardcoded color per class, grouped by hue (all tree cover greens
# together, all cropland yellows together, etc.) so they read as a family
# in the legend, but every color is explicit and can be adjusted
# individually. Used for the per-class plot in step 6.
CLASS_COLORS <- c(
  # Cropland (yellow)
  c10  = "#F9D71C",
  c20  = "#CDAD00",
  # Cropland/natural vegetation
  c30  = "#DAA520",
  # Natural vegetation/cropland
  c40  = "#9ACD32",
  # Tree cover (green)
  c50  = "#1B5E20",
  c60  = "#2E7D32",
  c70  = "#388E3C",
  c80  = "#43A047",
  c90  = "#66BB6A",
  # Tree and shrub
  c100 = "#2E8B57",
  # Herbaceous/tree and shrub
  c110 = "#66CDAA",
  # Shrubland
  c120 = "#8B4513",
  # Grassland
  c130 = "#2fff2f",
  # Lichens, mosses
  c140 = "#708090",
  # Sparse vegetation
  c150 = "#D2B48C",
  # Tree cover, flooded (teal)
  c160 = "#00798C",
  c170 = "#005F6B",
  # Shrub or herbaceous, flooded
  c180 = "#20B2AA",
  # Urban areas
  c190 = "#B22222",
  # Bare areas
  c200 = "#A0522D",
  # Water bodies
  c210 = "#1E90FF",
  # Snow and ice
  c220 = "#4DD0E1"
)

# Alternative palette: 22 distinct, ungrouped colors (Sasha Trubetskoy's
# "maximally distinct" set). Swap in for CLASS_COLORS below if every class
# should stand out on its own rather than read as a family.
CLASS_COLORS_DISTINCT <- c(
  c10  = "#E6194B", c20  = "#3CB44B", c30  = "#FFE119", c40  = "#4363D8",
  c50  = "#F58231", c60  = "#911EB4", c70  = "#46F0F0", c80  = "#F032E6",
  c90  = "#BFEF45", c100 = "#FABED4", c110 = "#469990", c120 = "#DCBEFF",
  c130 = "#9A6324", c140 = "#FFFAC8", c150 = "#800000", c160 = "#AAFFC3",
  c170 = "#808000", c180 = "#FFD8B1", c190 = "#000075", c200 = "#A9A9A9",
  c210 = "#000000", c220 = "#E6BEFF"
)

# Human-readable ESA CCI class names for the step 6 legend.
LC_CLASS_LABELS <- c(
  c10  = "Cropland, rainfed",
  c20  = "Cropland, irrigated or post-flooding",
  c30  = "Mosaic cropland / natural vegetation",
  c40  = "Mosaic natural vegetation / cropland",
  c50  = "Tree cover, broadleaved, evergreen",
  c60  = "Tree cover, broadleaved, deciduous",
  c70  = "Tree cover, needleleaved, evergreen",
  c80  = "Tree cover, needleleaved, deciduous",
  c90  = "Tree cover, mixed leaf type",
  c100 = "Mosaic tree and shrub / herbaceous cover",
  c110 = "Mosaic herbaceous cover / tree and shrub",
  c120 = "Shrubland",
  c130 = "Grassland",
  c140 = "Lichens and mosses",
  c150 = "Sparse vegetation",
  c160 = "Tree cover, flooded, fresh or brackish water",
  c170 = "Tree cover, flooded, saline water",
  c180 = "Shrub or herbaceous cover, flooded",
  c190 = "Urban areas",
  c200 = "Bare areas",
  c210 = "Water bodies",
  c220 = "Snow and ice"
)

# ============================================================
# STEP 1: Extract land cover proportions per sub-catchment
# ============================================================

message("\n=== Building land cover predict table ===")

# one variable per class-year combination, e.g. c10_1992 ... c220_2020
var_names <- as.vector(outer(LC_CLASSES, YEARS, paste, sep = "_"))

get_predict_table(
  variable       = var_names,
  statistics     = "mean",
  tile_id        = TILE_ID,
  input_var_path = "data/env90m/esa_cci_landcover_v2_1_1/",
  subcatch_id    = "data/subc_IDs.txt",
  out_file_path  = "data/env90m/predictTB.csv",
  read           = FALSE,
  overwrite      = TRUE,
  n_cores        = 6)

landcover <- fread("data/env90m/predictTB.csv")
head(landcover)

# ============================================================
# STEP 2: Weight proportions by sub-catchment area
# ============================================================

message("\n=== Weighting proportions by sub-catchment area ===")

# sub-catchment areas (km^2) from the lake-catchment raster
subc_raster <- rast("data/spatial/subc_id_lake_catchment.tif")
subc_areas  <- terra::expanse(subc_raster, unit = "km", zones = subc_raster)
names(subc_areas) <- c("layer", "subc_id", "area_km2")

# align id column name and join areas
names(landcover)[names(landcover) == "subcID"] <- "subc_id"
landcover <- merge(landcover, subc_areas[, c("subc_id", "area_km2")],
                   by = "subc_id")

# proportion * area -> km^2 per class-year, per sub-catchment
lc_cols        <- grep("^c[0-9]+_", names(landcover))
landcover_area <- landcover[, ..lc_cols] * landcover$area_km2

# ============================================================
# STEP 3: Sum per class per year, reshape to long
# ============================================================

col_sums <- colSums(landcover_area)

lake_land_cover <- enframe(col_sums, name = "id", value = "area_km2") %>%
  separate(id, into = c("variable", "year"), sep = "_") %>%
  mutate(year = as.numeric(year))

# explicit class order c10 -> c220
lake_land_cover$variable <- factor(lake_land_cover$variable, levels = LC_CLASSES)

# ============================================================
# STEP 4: Plot the land cover time series
# ============================================================

message("\n=== Plotting land cover time series ===")

p <- ggplot(lake_land_cover, aes(x = year, y = area_km2, color = variable)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = CLASS_COLORS) +
  scale_x_continuous(
    breaks = seq(min(lake_land_cover$year), max(lake_land_cover$year), by = 2)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Year", y = "Landcover area (km\u00b2)", color = "") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.text     = element_text(size = 9)) +
  guides(color = guide_legend(nrow = 2))

# png() over ggsave() to avoid the ragg device conflict
png("figures/lake_landcover_timeseries.png",
    width = 2400, height = 1500, res = 300)
print(p)
dev.off()

# ============================================================
# STEP 5: Rank classes by area for the most recent year
# ============================================================

lake_land_cover_2020 <- lake_land_cover[lake_land_cover$year == max(YEARS), ]
lake_land_cover_2020 <- lake_land_cover_2020[
  order(lake_land_cover_2020$area_km2, decreasing = TRUE), ]
print(lake_land_cover_2020, n = length(LC_CLASSES))

# ============================================================
# STEP 6: Individual land cover classes, colored by group
# ============================================================

message("\n=== Plotting individual land cover classes ===")

# choose which palette to use below: CLASS_COLORS (grouped by land cover
# family) or CLASS_COLORS_DISTINCT (all classes visually distinct)
selected_colors <- CLASS_COLORS

# order the legend to match the vertical stacking order of the lines at
# their final-year endpoint, so reading down the legend mirrors reading
# down the right edge of the plot
legend_order <- lake_land_cover %>%
  filter(year == max(year)) %>%
  arrange(desc(area_km2)) %>%
  pull(variable) %>%
  as.character()

p3 <- ggplot(lake_land_cover, aes(x = year, y = area_km2, color = variable)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = selected_colors, labels = LC_CLASS_LABELS,
                     breaks = legend_order) +
  scale_x_continuous(
    breaks = seq(min(lake_land_cover$year), max(lake_land_cover$year), by = 2)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Year", y = "Landcover area (km²)", color = "") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.text     = element_text(size = 9)) +
  guides(color = guide_legend(ncol = 1))

# png() over ggsave() to avoid the ragg device conflict
png("figures/lake_landcover_classes_grouped_colors.png",
    width = 3000, height = 2100, res = 300)
print(p3)
dev.off()

message("\nLand cover analysis complete.")
