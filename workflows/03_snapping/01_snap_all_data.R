#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_snap_all_data.R   (Module 3 -- Snapping)
#
# Snap fish (HCMR + GBIF) and dam points to the stream network using the
# GeoFRESH cascade-snapping API. Each dataset is snapped, a check map is
# written, failed points are saved separately, and the two fish sources are
# combined into one snapped dataset.
#
# Cascade snapping tries each Strahler order in turn (4 -> 3 -> 2), so a
# point snaps to the largest nearby stream first and only falls back to
# smaller streams if needed, within the distance threshold.
#
# Workflow:
#   1. Load cleaned fish + dam points
#   2. Snap HCMR fish, GBIF fish, dams (one call each)
#   3. Write a snapping-check map and failed-points list per dataset
#   4. Combine HCMR + GBIF into all_snapped_fish_points.csv
#
# INPUT:
#   - points_cleaned/fish/fish_points_to_snap_hcmr.csv   (from 01_clean_hcmr_fish.R)
#   - points_cleaned/fish/fish_gbif_clean_to_snap.csv     (from 03_clean_gbif_fish.R)
#   - points_cleaned/dams/dams_sarantaporos_clean.csv     (from 01_clean_dam_data.R)
#
# OUTPUT:
#   - points_snapped/fish/all_snapped_fish_points.csv     (combined HCMR + GBIF)
#   - points_snapped/dams/dams_snapped_points.csv
#   - points_snapped/maps/{hcmr,gbif,dams}_snapping_check.html
#   - points_cleaned/{fish,dams}/*_failed_to_snap.csv      (if any fail)
#
# REQUIRES: internet access (GeoFRESH snapping API).
#
# LOCATION: workflows/03_snapping/01_snap_all_data.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(data.table)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(sf)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("points_snapped/fish", recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/dams", recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/maps", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Fish snap with a looser threshold than dams: survey/GBIF coordinates are
# coarser, while dams need tighter snapping to land on the correct reach.
STRAHLER_SEQ        <- c(4, 3, 2)
FISH_DIST_THRESHOLD <- 400   # metres
DAM_DIST_THRESHOLD  <- 150   # metres

# ============================================================
# HELPERS
# ============================================================

# Build a snapping-check leaflet map: original points coloured by whether
# they snapped, the snapped locations, and grey lines linking the two.
# lon_col/lat_col are the ORIGINAL coordinate column names (these differ
# between HCMR/dams "longitude" and GBIF "decimalLongitude").
make_snap_map <- function(original, snapped, id_col, lon_col, lat_col, title) {
  # original gained a logical `snapped` column before being passed in
  ok   <- original[original$snapped, ]
  fail <- original[!original$snapped, ]

  # Snapped-coordinate column names follow the API convention: <orig>_snapped
  lon_snapped <- paste0(lon_col, "_snapped")
  lat_snapped <- paste0(lat_col, "_snapped")
  lon_orig    <- paste0(lon_col, "_original")
  lat_orig    <- paste0(lat_col, "_original")

  link_lines <- lapply(seq_len(nrow(snapped)), function(i) {
    st_linestring(matrix(c(
      snapped[[lon_orig]][i],    snapped[[lat_orig]][i],
      snapped[[lon_snapped]][i], snapped[[lat_snapped]][i]
    ), ncol = 2, byrow = TRUE))
  }) %>% st_sfc(crs = 4326)

  leaflet() %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addCircleMarkers(lng = ok[[lon_col]], lat = ok[[lat_col]],
                     color = "blue", radius = 5, fillOpacity = 0.7, weight = 2,
                     group = "Original (Snapped)") %>%
    addCircleMarkers(lng = fail[[lon_col]], lat = fail[[lat_col]],
                     color = "orange", radius = 6, fillOpacity = 0.9, weight = 3,
                     group = "Original (Failed)") %>%
    addCircleMarkers(lng = snapped[[lon_snapped]], lat = snapped[[lat_snapped]],
                     color = "red", radius = 5, fillOpacity = 0.7, weight = 2,
                     group = "Snapped Points") %>%
    addPolylines(data = link_lines, color = "gray", weight = 1, opacity = 0.5,
                 group = "Snap Lines") %>%
    addLayersControl(
      baseGroups    = c("OpenStreetMap", "Satellite"),
      overlayGroups = c("Original (Snapped)", "Original (Failed)",
                        "Snapped Points", "Snap Lines"),
      options = layersControlOptions(collapsed = FALSE)) %>%
    addLegend(position = "bottomright",
              colors = c("blue", "orange", "red", "gray"),
              labels = c("Original (Snapped)", "Original (Failed)",
                         "Snapped Points", "Snap Lines"),
              title = title, opacity = 0.7)
}

# Print basic snapping stats (n snapped/failed, mean distance).
report_snap <- function(label, original, snapped, id_col) {
  original$snapped <- original[[id_col]] %in% snapped[[id_col]]
  n_ok   <- sum(original$snapped)
  n_fail <- sum(!original$snapped)
  message(sprintf("\n%s snapping: %d snapped, %d failed (mean dist %.1f m)",
                  label, n_ok, n_fail, mean(snapped$distance_metres)))
  original  # return with `snapped` column added
}

# ============================================================
# STEP 1: Load cleaned data
# ============================================================

message("\n=== Loading cleaned data ===")

hcmr_original <- fread("points_cleaned/fish/fish_points_to_snap_hcmr.csv")
gbif_original <- fread("points_cleaned/fish/fish_gbif_clean_to_snap.csv")
dams_original <- fread("points_cleaned/dams/dams_sarantaporos_clean.csv")

message(sprintf("  HCMR: %d  GBIF: %d  Dams: %d points",
                nrow(hcmr_original), nrow(gbif_original), nrow(dams_original)))

# ============================================================
# STEP 2a: Snap HCMR fish
# ============================================================
# Survey data with known sampling locations, so we snap directly without
# screening for sea/lake points first.

hcmr_snap <- api_get_snapped_points_cascade(
  data = hcmr_original, colname_lon = "longitude", colname_lat = "latitude",
  colname_site_id = "site_id",
  strahler_seq = STRAHLER_SEQ, distance_threshold = FISH_DIST_THRESHOLD
)
fwrite(hcmr_snap, "points_snapped/fish/hcmr_snapped_points.csv")

hcmr_original <- report_snap("HCMR", hcmr_original, hcmr_snap, "site_id")
saveWidget(make_snap_map(hcmr_original, hcmr_snap, "site_id",
                         "longitude", "latitude", "HCMR Data"),
           "points_snapped/maps/hcmr_snapping_check.html")

if (sum(!hcmr_original$snapped) > 0) {
  fwrite(hcmr_original[!hcmr_original$snapped, ],
         "points_cleaned/fish/hcmr_failed_to_snap.csv")
}

# ============================================================
# STEP 2b: Snap GBIF fish
# ============================================================
# Some points that fail here are in lakes (off the stream network).

gbif_snap <- api_get_snapped_points_cascade(
  data = gbif_original, colname_lon = "decimalLongitude", colname_lat = "decimalLatitude",
  colname_site_id = "gbifID",
  strahler_seq = STRAHLER_SEQ, distance_threshold = FISH_DIST_THRESHOLD
)
fwrite(gbif_snap, sprintf(
  "points_snapped/fish/gbif_snapped_points_min_strahler%d_dist_thresh_%d.csv",
  min(STRAHLER_SEQ), FISH_DIST_THRESHOLD))

gbif_original <- report_snap("GBIF", gbif_original, gbif_snap, "gbifID")
saveWidget(make_snap_map(gbif_original, gbif_snap, "gbifID",
                         "decimalLongitude", "decimalLatitude", "GBIF Data"),
           "points_snapped/maps/gbif_snapping_check.html")

if (sum(!gbif_original$snapped) > 0) {
  fwrite(gbif_original[!gbif_original$snapped, ],
         "points_cleaned/fish/gbif_failed_to_snap.csv")
}

# ============================================================
# STEP 3: Combine HCMR + GBIF
# ============================================================

message("\n=== Combining fish datasets ===")

hcmr_snap$source <- "HCMR"
gbif_snap$source <- "GBIF"

# Rename GBIF columns to the HCMR convention so the two can be row-bound.
gbif_snap <- gbif_snap %>%
  rename(longitude_original = decimalLongitude_original,
         latitude_original  = decimalLatitude_original,
         longitude_snapped  = decimalLongitude_snapped,
         latitude_snapped   = decimalLatitude_snapped,
         site_id            = gbifID)

common_cols <- intersect(names(hcmr_snap), names(gbif_snap))
all_snapped <- rbind(hcmr_snap[, ..common_cols], gbif_snap[, ..common_cols])

fwrite(all_snapped, "points_snapped/fish/all_snapped_fish_points.csv")
message(sprintf("  Combined: %d points (HCMR %d, GBIF %d)",
                nrow(all_snapped),
                sum(all_snapped$source == "HCMR"),
                sum(all_snapped$source == "GBIF")))

# ============================================================
# STEP 4: Snap dams
# ============================================================

dams_snap <- api_get_snapped_points_cascade(
  data = dams_original, colname_lon = "longitude", colname_lat = "latitude",
  colname_site_id = "site_id",
  strahler_seq = STRAHLER_SEQ, distance_threshold = DAM_DIST_THRESHOLD
)

# Re-attach the dam attributes dropped by the snapping call.
dams_snap <- dams_snap %>%
  left_join(dams_original) %>%
  select(-longitude, -latitude)

fwrite(dams_snap, "points_snapped/dams/dams_snapped_points.csv")

dams_original <- report_snap("Dams", dams_original, dams_snap, "site_id")
saveWidget(make_snap_map(dams_original, dams_snap, "site_id",
                         "longitude", "latitude", "Dam Source"),
           "points_snapped/maps/dams_snapping_check.html")

if (sum(!dams_original$snapped) > 0) {
  fwrite(dams_original[!dams_original$snapped, ],
         "points_cleaned/dams/dams_failed_to_snap.csv")
}

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("SNAPPING COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message(sprintf("  Fish (combined): %d points", nrow(all_snapped)))
message(sprintf("  Dams:            %d points", nrow(dams_snap)))
message("\nNext: 03_snapping/02_join_spnames_with_locations.R")
