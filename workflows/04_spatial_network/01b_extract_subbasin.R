#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01b_extract_subbasin.R
#
# Extract the Sarantaporos subbasin from the Vjosa/Aoos basin for
# focused connectivity and SDM analysis.
#
# Two outputs:
#   1. Greece-wide occurrences for Vjosa species (for SDM training)
#   2. Sarantaporos-only network, points, and polygon (for prediction
#      and connectivity analysis)
#
# Requires placeholder functions:
#   - api_get_upstream_catchment()
#   - api_get_upstream_stream_segments()
#
# Input:
#   - Snapped fish and dam points (Greece-wide)
#   - IUCN species checklist for Vjosa basin
#   - Vjosa basin stream network (from 01a)
#
# Output:
#   - spatial/sarantaporos/subbasin_polygon.gpkg
#   - spatial/sarantaporos/stream_network.gpkg
#   - points_snapped/sarantaporos/fish_vjosa_species_greece.csv
#     (all Greece occurrences of Vjosa species — for SDM training)
#   - points_snapped/sarantaporos/fish_sarantaporos.csv
#     (only occurrences within Sarantaporos — for connectivity)
#   - points_snapped/sarantaporos/dams_sarantaporos.csv
#     (only dams within Sarantaporos — for connectivity)
#
# LOCATION: workflows/04_spatial_network/01b_extract_subbasin.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(sf)
library(data.table)
library(dplyr)

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select

# Load helper function
source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

# Sarantaporos outlet point (where it joins the Aoos/Vjosa mainstem)
# We obtained the coordinates on https://aqua.igb-berlin.de/upstream-dev/
OUTLET_LON <- 20.538704
OUTLET_LAT <- 40.113735

# Parent basin
VJOSA_BASIN_ID <- 1292502

# ============================================================
# STEP 1: Get Sarantaporos subbasin polygon and stream network
# ============================================================

message("\n=== Step 1: Extracting Sarantaporos subbasin ===")

dir.create("spatial/sarantaporos", recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/sarantaporos", recursive = TRUE, showWarnings = FALSE)

# Get upstream catchment polygon from the outlet point
subbasin_polygon <- api_get_upstream_catchment(
  lon = OUTLET_LON,
  lat = OUTLET_LAT
)

st_write(subbasin_polygon, "spatial/sarantaporos/subbasin_polygon.gpkg")
save_to_nimbus(subbasin_polygon, "spatial/sarantaporos/subbasin_polygon.gpkg")
message("  Subbasin polygon saved")

# Get upstream stream segments from the outlet point
subbasin_streams <- api_get_upstream_stream_segments(
  lon = OUTLET_LON,
  lat = OUTLET_LAT,
  min_strahler = 2
)

st_write(subbasin_streams, "spatial/sarantaporos/sarantaporos_stream_network.gpkg",
         delete_dsn = TRUE)
save_to_nimbus(subbasin_streams, "spatial/sarantaporos/sarantaporos_stream_network.gpkg")
message("  Stream network saved: ", nrow(subbasin_streams), " segments")


# Get subcatchment IDs within the subbasin
sarantaporos_subc_ids <- unique(subbasin_streams$subc_id)
message("  Subcatchments in Sarantaporos: ", length(sarantaporos_subc_ids))

# ============================================================
# STEP 2: Load IUCN species list for Vjosa basin
# ============================================================

message("\n=== Step 2: Loading Vjosa species checklist ===")

vjosa_species <- fread("range_maps/vjosa_species_checklist_iucn.csv") %>%
  filter(basin_id == VJOSA_BASIN_ID) %>%
  pull(species) %>%
  unique()

message("  Species expected in Vjosa (IUCN): ", length(vjosa_species))

# ============================================================
# STEP 3: Filter fish occurrences — two outputs
# ============================================================

message("\n=== Step 3: Filtering fish occurrences ===")

fish_snapped <- fread("points_snapped/fish/fish_all_species_snapped.csv")

# 3a. ALL Greece occurrences for Vjosa species (SDM training)
fish_vjosa_species_greece <- fish_snapped %>%
  filter(species %in% vjosa_species)

fwrite(fish_vjosa_species_greece,
       "points_snapped/sarantaporos/fish_vjosa_species_greece.csv")

message("  Greece-wide occurrences for Vjosa species: ", nrow(fish_vjosa_species_greece))
message("  Species with occurrences: ",
        length(unique(fish_vjosa_species_greece$species)), " / ", length(vjosa_species))

# 3b. Only occurrences within Sarantaporos (connectivity analysis)
fish_sarantaporos <- fish_snapped %>%
  filter(subc_id %in% sarantaporos_subc_ids)

fwrite(fish_sarantaporos,
       "points_snapped/sarantaporos/fish_sarantaporos.csv")

message("  Occurrences within Sarantaporos: ", nrow(fish_sarantaporos))
message("  Species in Sarantaporos: ", length(unique(fish_sarantaporos$species)))

# ============================================================
# STEP 4: Filter dams to Sarantaporos
# ============================================================

message("\n=== Step 4: Filtering dams ===")

dams_snapped <- fread("points_snapped/dams/dams_snapped_points.csv")

dams_sarantaporos <- dams_snapped %>%
  filter(subc_id %in% sarantaporos_subc_ids) %>%
  filter(phase!="R")

fwrite(dams_sarantaporos,
       "points_snapped/sarantaporos/dams_sarantaporos.csv")

message("  Dams in Sarantaporos: ", nrow(dams_sarantaporos))
if (nrow(dams_sarantaporos) > 0) {
  message("  By phase:")
  print(table(dams_sarantaporos$phase))
}

# ============================================================
# STEP 5: Species coverage summary
# ============================================================

message("\n=== Step 5: Species coverage summary ===")

# Compare: IUCN expected vs observed in Sarantaporos vs observed in Greece
species_summary <- data.frame(
  species = vjosa_species
) %>%
  mutate(
    in_sarantaporos = species %in% unique(fish_sarantaporos$species),
    in_greece = species %in% unique(fish_vjosa_species_greece$species),
    n_occ_sarantaporos = sapply(species, function(sp)
      sum(fish_sarantaporos$species == sp)),
    n_occ_greece = sapply(species, function(sp)
      sum(fish_vjosa_species_greece$species == sp))
  )

fwrite(species_summary, "points_snapped/sarantaporos/species_coverage_summary.csv")

cat("\nSpecies coverage:\n")
cat("  IUCN expected in Vjosa:          ", length(vjosa_species), "\n")
cat("  With occurrences in Greece:      ", sum(species_summary$in_greece), "\n")
cat("  With occurrences in Sarantaporos:", sum(species_summary$in_sarantaporos), "\n")
cat("  IUCN-only (no occurrences):      ",
    sum(!species_summary$in_greece), "\n")

# Species in Sarantaporos but not in IUCN list (unexpected)
unexpected <- unique(fish_sarantaporos$species)
unexpected <- unexpected[!unexpected %in% vjosa_species]
if (length(unexpected) > 0) {
  cat("\n  Unexpected species in Sarantaporos (not in IUCN Vjosa list):\n")
  cat("  ", paste(unexpected, collapse = ", "), "\n")
}


### Prune network
subbasin_streams_filtered <- extract_partial_stream_network(
  subbasin_streams,
  all_snapped$subc_id,
  strahler_retain_threshold = 4,
  upstream_buffer = 3      # number of upstream segments to include
)

st_write(all_streams_filtered, "spatial/stream_networks/partial_stream_network.gpkg")




# ============================================================
# SUMMARY
# ============================================================

message("\n=== Sarantaporos Subbasin Extraction Complete ===")
message("\nOutputs:")
message("  spatial/sarantaporos/subbasin_polygon.gpkg")
message("  spatial/sarantaporos/stream_network.gpkg")
message("  points_snapped/sarantaporos/fish_vjosa_species_greece.csv  (SDM training)")
message("  points_snapped/sarantaporos/fish_sarantaporos.csv          (connectivity)")
message("  points_snapped/sarantaporos/dams_sarantaporos.csv          (connectivity)")
message("  points_snapped/sarantaporos/species_coverage_summary.csv")
message("\nNext steps:")
message("  - SDM: use fish_vjosa_species_greece.csv for training,")
message("         predict on Sarantaporos subcatchments")
message("  - Connectivity: use fish_sarantaporos.csv + dams_sarantaporos.csv")
message("         with 02_generate_network_graph.R")
