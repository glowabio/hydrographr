#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_extract_lake_intersection.R   (Module 11 -- Lake analysis)
#
# Find lakes inside the bounding box of the fish occurrences, subset to the
# target lake (Aoos reservoir), and compute where that lake intersects the
# stream network. The intersection points are the inlets/outlets used later
# to delineate the lake catchment.
#
# Kept separate because get_lake_intersection() depends on the external
# GuidosToolbox Workbench (GWB) MSPA tool, which must be installed locally:
#   https://forest.jrc.ec.europa.eu/en/activities/lpa/gtb/
#   https://glowabio.github.io/hydrographr/articles/case_study_lake_workflow.html
#
# Workflow:
#   1. Load stream network, fish occurrences, SWOT lake database
#   2. extract_lake_ids() over the occurrence bounding box
#   3. Subset the lake-id table to the target lake
#   4. get_lake_intersection() -> lake/stream intersection points
#
# INPUT:
#   - data/spatial/vjosa_partial.gpkg                    (stream network)
#   - data/species/all_snapped_fish_points_from_sp_list.csv
#   - data/spatial/swot_lakes.gpkg                       (SWOT prior lake DB)
#   - data/spatial/{segment,accumulation,basin}.tif      (from 01_)
#
# OUTPUT:
#   - data/lakes/lake_id.txt                             (all lake IDs in bbox)
#   - data/lakes/lake_intersections/coord_lake_<id>.txt  (intersection points)
#   - data/lakes/lake_intersections/lake_<id>.tif
#
# REQUIRES: GuidosToolbox Workbench (GWB) installed locally.
#
# LOCATION: workflows/XX_lakes/02_extract_lake_intersection.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(sf)
library(data.table)

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("data/lakes/lake_intersections", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Target lake: the Aoos reservoir, one of the lake IDs returned within the
# occurrence bounding box.
LAKE_ID <- 2130004632

# Tile covering the study area.
TILE_ID <- "h20v04"

# SWOT prior lake database, downloaded via https://hydroweb.next.theia-land.fr/
SWOT_LAKES <- "data/spatial/swot_lakes.gpkg"

# Check the layer name of the lake dataset geopackage
sf::st_layers(data/spatial/swot_lakes.gpkg)

# Internal SWOT layer name carried by get_lake_intersection().
SWOT_LAKE_NAME <- "lake"

# Full paths to the local GWB install (virtual-machine Linux paths).
EDGE <- "/home/USER/"

# ============================================================
# STEP 1: Load inputs
# ============================================================

message("\n=== Loading inputs ===")

# stream network
gpkg_data <- st_read("data/spatial/vjosa_partial.gpkg")

# fish occurrences used to define the search bounding box
species <- fread("data/species/all_snapped_fish_points_from_sp_list.csv")

# raster layers for the intersection call
stream <- sprintf("data/spatial/segment_%s.tif", TILE_ID)
flow   <- sprintf("data/spatial/accumulation_%s.tif", TILE_ID)
basins <- sprintf("data/spatial/basin_%s.tif", TILE_ID)

# quick look at the SWOT lake database
lakes <- st_read(SWOT_LAKES)
head(lakes)

# ============================================================
# STEP 2: Extract lake IDs within the occurrence bounding box
# ============================================================

message("\n=== Extracting lake IDs in occurrence bbox ===")

extract_lake_ids(data          = species,
                 lon           = "longitude_snapped",
                 lat           = "latitude_snapped",
                 bbox          = TRUE,
                 var_name      = "lake_id",
                 lake_shape    = SWOT_LAKES,
                 lake_id_table = "data/lakes",
                 quiet         = TRUE)

# ============================================================
# STEP 3: Subset to the target lake
# ============================================================

lake_ids <- fread("data/lakes/lake_id.txt", header = TRUE)
lake_ids <- lake_ids[lake_ids$lake_id == LAKE_ID, ]

# ============================================================
# STEP 4: Lake / stream-network intersection
# ============================================================

message("\n=== Computing lake / stream intersections ===")

lake_dat <- "data/lakes/lake_intersections/"

lake_intersect_table <- get_lake_intersection(
  lake_ids,
  SWOT_LAKES,
  lake_name = SWOT_LAKE_NAME,
  lake_id   = "lake_id",
  buffer    = FALSE,
  EDGE, stream, flow, basins, lake_dat,
  n_cores   = 1,
  quiet     = TRUE)

message("\nIntersection complete.")
message("Next: XX_lakes/03_delineate_lake_catchment.R")
