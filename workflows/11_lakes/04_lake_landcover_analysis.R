#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 04_lake_landcover_analysis.R   (Module X -- Lake analysis)
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
#
# INPUT:
#   - data/subc_IDs.txt                                  (from 03_)
#   - data/spatial/subc_id_lake_catchment.tif             (from 03_)
#   - data/env90m/esa_cci_landcover_v2_1_1/*.txt          (from 01_)
#
# OUTPUT:
#   - data/env90m/predictTB.csv                           (land cover table)
#   - figures/lake_landcover_timeseries.png
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

# Kelly's (1965) 22 colours of maximum contrast, designed for 22 categories.
KELLY_COLORS <- setNames(
  c("#E68FAC", "#875692", "#F38400", "#A1CAF1", "#BE0032",
    "#008856", "#848482", "#C2B280", "#F3C300", "#0067A5",
    "#F99379", "#604E97", "#F6A600", "#B3446C", "#DCD300",
    "#882D17", "#8DB600", "#654522", "#E25822", "#2B3D26",
    "#222222", "#7B4173"),
  LC_CLASSES)

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
  scale_color_manual(values = KELLY_COLORS) +
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

message("\nLand cover analysis complete.")
