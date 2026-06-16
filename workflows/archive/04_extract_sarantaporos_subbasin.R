#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01b_extract_sarantaporos_subbasin.R
#
# Extract the Sarantaporos-only subbasin network for connectivity analysis.
# This is a subset of the Sarantaporos + Voidomatis subbasin used for SDM
# (01_extract_subbasin.R). The outlet point here is the confluence of
# Sarantaporos into Voidomatis, rather than the combined confluence into Aoos.
#
# Why a separate subbasin?
#   The SDM prediction extent (Sarantaporos + Voidomatis) is appropriate for
#   habitat modelling because both tributaries share similar environmental
#   conditions and species assemblages. However, for connectivity and
#   fragmentation analysis we focus on Sarantaporos only, which is the
#   sub-basin with the highest planned hydropower development pressure and
#   where field data are concentrated.
#
# Prerequisite: 01_extract_subbasin.R must have been run first. This script
#   reads the already-snapped fish and dam files from that run.
#
# Workflow:
#   1. Load already-snapped fish + dam points (from 01_extract_subbasin.R)
#   2. Extract Sarantaporos polygon via api_get_upstream_catchment()
#   3. Download full Sarantaporos stream network via api_get_stream_segments()
#   4. Prune network retaining reaches with fish/dam observations + buffer
#   5. Filter fish + dams to Sarantaporos subbasin
#   6. Visualise (leaflet map)
#
# Inputs:
#   points_snapped/fish/fish_all_species_snapped.csv
#   points_snapped/dams/dams_snapped_points.csv
#
# Outputs:
#   spatial/subbasin_sarantaporos/subbasin_polygon.gpkg
#   spatial/subbasin_sarantaporos/stream_network.gpkg
#   spatial/subbasin_sarantaporos/stream_network_pruned.gpkg
#   spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv
#   points_snapped/subbasin_sarantaporos/fish_sarantaporos.csv
#   points_snapped/subbasin_sarantaporos/dams_sarantaporos.csv
#
# LOCATION: workflows/03_snapping/04_extract_sarantaporos_subbasin.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(sf)
library(data.table)
library(dplyr)
library(leaflet)

select <- dplyr::select

source("~/Documents/Postdoc/code/workflow_paper/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

# Outlet point = confluence of Sarantaporos into Voidomatis
# Obtained from https://aqua.igb-berlin.de/upstream-dev/
# This gives only the Sarantaporos drainage, excluding Voidomatis
OUTLET_LON <- 20.592
OUTLET_LAT <- 40.071


# Network pruning parameters — same as 01_extract_subbasin.R for consistency
MIN_STRAHLER    <- 4
UPSTREAM_BUFFER <- 3

# ============================================================
# SETUP
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("SARANTAPOROS SUBBASIN EXTRACTION")
message(paste(rep("=", 80), collapse = ""))

dir.create("spatial/subbasin_sarantaporos",        recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/subbasin_sarantaporos", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# STEP 1: Load snapped fish + dam points
# ============================================================

message("\n=== Step 1: Loading snapped points ===")

# Use the basin-wide snapped files from 01_extract_subbasin.R —
# we will filter these down to Sarantaporos subc_ids in Step 4
fish_snapped <- fread("points_snapped/fish/fish_all_species_snapped.csv")
message("  Fish records loaded: ", nrow(fish_snapped))

dams_snapped <- fread("points_snapped/dams/dams_snapped_points.csv")
if ("id1" %in% names(dams_snapped)) dams_snapped <- dams_snapped %>% rename(site_id = id1)
message("  Dam records loaded: ", nrow(dams_snapped))

# Deduplicate to unique site locations
# fish file has multiple rows per site (one row per species)
fish_unique <- fish_snapped %>%
  distinct(site_id, longitude_snapped, latitude_snapped, subc_id, source)

dams_unique <- dams_snapped %>%
  distinct(site_id, longitude_snapped, latitude_snapped, subc_id, source)

message("  Unique fish sites: ", nrow(fish_unique))
message("  Unique dam sites:  ", nrow(dams_unique))

# ============================================================
# STEP 2: Extract Sarantaporos subbasin polygon
# ============================================================

message("\n=== Step 2: Extracting Sarantaporos polygon ===")
message("  Outlet: lon = ", OUTLET_LON, " | lat = ", OUTLET_LAT)
message("  (Confluence of Sarantaporos into Voidomatis)")

# api_get_upstream_catchment() delineates the entire catchment upstream of
# the given point — here that is the Sarantaporos drainage only, since the
# outlet is placed at its confluence with Voidomatis (not further downstream)
subbasin_polygon <- api_get_upstream_catchment(
  lon = OUTLET_LON,
  lat = OUTLET_LAT
)

st_write(subbasin_polygon,
         "spatial/subbasin_sarantaporos/subbasin_polygon.gpkg",
         delete_dsn = TRUE)
# save_to_nimbus(subbasin_polygon,
#                "spatial/subbasin_sarantaporos/subbasin_polygon.gpkg")
message("  Polygon saved | Area: ",
        round(as.numeric(st_area(subbasin_polygon)) / 1e6, 1), " km²")

# ============================================================
# STEP 3: Download Sarantaporos stream network
# ============================================================

message("\n=== Step 3: Downloading Sarantaporos stream network ===")

# api_get_stream_segments() with upstream = TRUE returns all stream reaches
# upstream of the given point. min_strahler = 2 gives us all reaches that
# may be relevant for pruning — lower-order streams needed as candidates for
# reaches with fish/dam observations before the Strahler filter is applied.
subbasin_streams <- api_get_stream_segments(
  lon          = OUTLET_LON,
  lat          = OUTLET_LAT,
  upstream     = TRUE,
  geometry_only = FALSE,
  min_strahler = 2
)

st_write(subbasin_streams,
         "spatial/subbasin_sarantaporos/stream_network.gpkg",
         delete_dsn = TRUE)
# save_to_nimbus(subbasin_streams,
#                "spatial/subbasin_sarantaporos/stream_network.gpkg")

subbasin_subc_ids <- unique(subbasin_streams$subc_id)

# ============================================================
# STEP 4: Prune stream network
# ============================================================

message("\n=== Step 4: Pruning network ===")
message("  Retaining Strahler >= ", MIN_STRAHLER,
        " + upstream buffer of ", UPSTREAM_BUFFER, " reaches")
message("  for reaches with fish or dam observations")

# extract_partial_stream_network() retains:
#   (a) all reaches with Strahler order >= MIN_STRAHLER
#   (b) reaches of any order that contain a fish or dam observation,
#       plus UPSTREAM_BUFFER reaches upstream of each such point
# This ensures that lower-order reaches relevant to our observations are kept
# while excluding the vast majority of ephemeral / irrelevant streams
subbasin_streams_pruned <- extract_partial_stream_network(
  stream                    = subbasin_streams,
  snapped_subcs             = c(fish_unique$subc_id, dams_unique$subc_id),
  strahler_retain_threshold = MIN_STRAHLER,
  upstream_buffer           = UPSTREAM_BUFFER
)

# Deduplicate by subc_id — keep first occurrence
# (extract_partial_stream_network() can return duplicate edges in some cases)
n_before <- nrow(subbasin_streams_pruned)
subbasin_streams_pruned <- subbasin_streams_pruned %>%
  distinct(subc_id, .keep_all = TRUE)
n_after <- nrow(subbasin_streams_pruned)

if (n_before > n_after)
  message("  Removed ", n_before - n_after, " duplicate edges")

st_write(subbasin_streams_pruned,
         "spatial/subbasin_sarantaporos/stream_network_pruned.gpkg",
         delete_dsn = TRUE)
# save_to_nimbus(subbasin_streams_pruned,
#                "spatial/subbasin_sarantaporos/stream_network_pruned.gpkg")

subbasin_subc_ids_pruned <- unique(subbasin_streams_pruned$subc_id)

fwrite(
  data.table(subc_id = subbasin_subc_ids_pruned),
  "spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv"
)
message("  Saved: spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv")

# ============================================================
# STEP 5: Filter fish + dams to Sarantaporos subbasin
# ============================================================

message("\n=== Step 5: Filtering points to Sarantaporos ===")

# Filter using the full (unpruned) subbasin subc_ids — we want all fish/dam
# records that fall anywhere in the Sarantaporos drainage, not just on the
# pruned network. The pruned network is used for analysis; the full filter
# is used for reporting and any future re-snapping needs.
fish_sarantaporos <- fish_snapped %>%
  filter(subc_id %in% subbasin_subc_ids)

dams_sarantaporos <- dams_snapped %>%
  filter(subc_id %in% subbasin_subc_ids)

message("  Fish records in Sarantaporos: ", nrow(fish_sarantaporos),
        " (", n_distinct(fish_sarantaporos$species), " species)")
message("  Dams in Sarantaporos: ", nrow(dams_sarantaporos))

if (nrow(dams_sarantaporos) > 0) {
  message("  Dams by status:")
  print(table(dams_sarantaporos$status))
}

fwrite(fish_sarantaporos,
       "points_snapped/subbasin_sarantaporos/fish_sarantaporos.csv")
fwrite(dams_sarantaporos,
       "points_snapped/subbasin_sarantaporos/dams_sarantaporos.csv")

message("  Saved: points_snapped/subbasin_sarantaporos/fish_sarantaporos.csv")
message("  Saved: points_snapped/subbasin_sarantaporos/dams_sarantaporos.csv")

# Check how many fish fall on the pruned network (relevant for SDM/connectivity)
fish_on_pruned <- fish_sarantaporos %>%
  filter(subc_id %in% subbasin_subc_ids_pruned)
message("  Fish records on pruned network: ", nrow(fish_on_pruned),
        " (", n_distinct(fish_on_pruned$species), " species)")

