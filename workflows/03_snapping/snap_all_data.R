# ============================================================================
# SNAP ALL DATA TO STREAM NETWORK
# ============================================================================
# Purpose: Snap both HCMR and GBIF data to stream network using async API
# Input: Cleaned CSV files (from step 02)
# Output: Snapped points with network coordinates
# ============================================================================
library(hydrographr)
library(data.table)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(sf)
# Load helper function
source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")

# Set path to nimbus data folder
wdir <- "/run/user/1000/gvfs/dav:host=nimbus.igb-berlin.de,ssl=true,user=grigoropoulou,prefix=%2Fremote.php%2Fwebdav/workflow_paper/data"
nimbus_path <- wdir

# delete
wdir <- "~/Documents/Postdoc/projects/workflow_paper/data"

setwd(wdir)


# Create snapped points directories
dir.create("points_snapped/fish", recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/dams", recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/maps", recursive = TRUE, showWarnings = FALSE)


# Load cleaned data URLs (adjust these to your actual URLs)
hcmr_url <- "https://nimbus.igb-berlin.de/index.php/s/6wGQqJAe6p3FTJg/download/fish_points_to_snap_hcmr.csv"
gbif_url <- "https://nimbus.igb-berlin.de/index.php/s/jfSDnfZPwARpbYH/download/fish_gbif_clean_to_snap.csv"



# ============================================================================
# LOAD ORIGINAL DATA
# ============================================================================

message("\n=== Loading Original Data ===")

# Download original HCMR data
hcmr_original <- fread(hcmr_url)
message(sprintf("Original HCMR points: %d", nrow(hcmr_original)))

# Download original GBIF data
gbif_original <- fread(gbif_url)
message(sprintf("Original GBIF points: %d", nrow(gbif_original)))


# Snapping parameters
STRAHLER_SEQ <- c(4, 3, 2)
DISTANCE_THRESHOLD <- 400  # meters


# ============================================================================
# SNAP HCMR DATA
# ============================================================================

# We have survey data of freshwater fish. In this case we can try to snap directly without cross-checking
# if the points are located at the sea, because we know where they were sampled

message("\n=== Snapping HCMR Data ===")

hcmr_snap_result <- api_get_snapped_points_cascade_async(
  csv_url = hcmr_url,
  colname_lon = "longitude",
  colname_lat = "latitude",
  colname_site_id = "Sites",
  strahler_seq = STRAHLER_SEQ,
  distance_threshold = DISTANCE_THRESHOLD
)

print(hcmr_snap_result)

# Save
fwrite(hcmr_snap_result, "points_snapped/fish/hcmr_snapped_points.csv")
message(sprintf("HCMR: Snapped %d points", nrow(hcmr_snap_result)))

hcmr_snap_result <- fread("points_snapped/fish/hcmr_snapped_points.csv")

# ============================================================================
# VISUALIZE HCMR SNAPPING
# ============================================================================

message("\n--- Creating HCMR visualization ---")

# Identify which points were successfully snapped
hcmr_snapped_ids <- hcmr_snap_result$Sites
hcmr_original$snapped <- hcmr_original$Sites %in% hcmr_snapped_ids

# Summary
n_snapped <- sum(hcmr_original$snapped)
n_failed <- sum(!hcmr_original$snapped)
message(sprintf("  Successfully snapped: %d", n_snapped))
message(sprintf("  Failed to snap: %d", n_failed))

# Create map
hcmr_map <- leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

  # Original points that were successfully snapped (blue)
  addCircleMarkers(
    data = hcmr_original[hcmr_original$snapped, ],
    lng = ~longitude,
    lat = ~latitude,
    color = "blue",
    fillColor = "blue",
    radius = 5,
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 2,
    group = "Original (Snapped)",
    popup = ~paste0(
      "<b>Original Point (Successfully Snapped)</b><br>",
      "Site ID: ", Sites, "<br>",
      "Lon: ", round(longitude, 5), "<br>",
      "Lat: ", round(latitude, 5)
    )
  ) %>%

  # Original points that FAILED to snap (orange)
  addCircleMarkers(
    data = hcmr_original[!hcmr_original$snapped, ],
    lng = ~longitude,
    lat = ~latitude,
    color = "orange",
    fillColor = "orange",
    radius = 6,
    fillOpacity = 0.9,
    stroke = TRUE,
    weight = 3,
    group = "Original (Failed)",
    popup = ~paste0(
      "<b>⚠️ Original Point (FAILED TO SNAP)</b><br>",
      "Site ID: ", Sites, "<br>",
      "Lon: ", round(longitude, 5), "<br>",
      "Lat: ", round(latitude, 5), "<br>",
      "<b style='color:red;'>This point was not successfully snapped</b>"
    )
  ) %>%

  # Snapped points (red)
  addCircleMarkers(
    data = hcmr_snap_result,
    lng = ~longitude_snapped,
    lat = ~latitude_snapped,
    color = "red",
    fillColor = "red",
    radius = 5,
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 2,
    group = "Snapped Points",
    popup = ~paste0(
      "<b>Snapped Point</b><br>",
      "Site ID: ", Sites, "<br>",
      "Lon: ", round(longitude_snapped, 5), "<br>",
      "Lat: ", round(latitude_snapped, 5), "<br>",
      "Distance: ", round(distance_metres, 1), " m<br>",
      "Strahler: ", strahler
    )
  ) %>%

  # Add lines connecting original to snapped (only for successfully snapped)
  addPolylines(
    data = lapply(1:nrow(hcmr_snap_result), function(i) {
      st_linestring(matrix(c(
        hcmr_snap_result$longitude_original[i], hcmr_snap_result$latitude_original[i],
        hcmr_snap_result$longitude_snapped[i], hcmr_snap_result$latitude_snapped[i]
      ), ncol = 2, byrow = TRUE))
    }) %>% st_sfc(crs = 4326),
    color = "gray",
    weight = 1,
    opacity = 0.5,
    group = "Snap Lines"
  ) %>%

  # Layer controls
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Satellite"),
    overlayGroups = c("Original (Snapped)", "Original (Failed)", "Snapped Points", "Snap Lines"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # Legend
  addLegend(
    position = "bottomright",
    colors = c("blue", "orange", "red", "gray"),
    labels = c("Original (Snapped)", "Original (Failed)", "Snapped Points", "Snap Lines"),
    title = "HCMR Data",
    opacity = 0.7
  )

hcmr_map

# Save map
save_to_nimbus(hcmr_map, "points_snapped/maps/hcmr_snapping_check.html")

# Save map locally
saveWidget(hcmr_map, "points_snapped/maps/hcmr_snapping_check.html")

# Print snapping statistics
message(sprintf("\nHCMR Snapping Statistics:"))
message(sprintf("  Total points: %d", nrow(hcmr_snap_result)))
message(sprintf("  Mean distance: %.1f m", mean(hcmr_snap_result$distance_metres)))
message(sprintf("  Median distance: %.1f m", median(hcmr_snap_result$distance_metres)))
message(sprintf("  Max distance: %.1f m", max(hcmr_snap_result$distance_metres)))
message(sprintf("  Points within %d m: %d (%.1f%%)",
                DISTANCE_THRESHOLD,
                sum(hcmr_snap_result$distance_metres <= DISTANCE_THRESHOLD),
                100 * sum(hcmr_snap_result$distance_metres <= DISTANCE_THRESHOLD) / nrow(hcmr_snap_result)))

# Save list of failed points
if (n_failed > 0) {
  hcmr_failed <- hcmr_original[!hcmr_original$snapped, ]
  fwrite(hcmr_failed, "points_cleaned/fish/hcmr_failed_to_snap.csv")
  message(sprintf("\nFailed points saved to: points_cleaned/fish/hcmr_failed_to_snap.csv"))
}


# ============================================================================
# SNAP GBIF DATA
# ============================================================================

message("\n=== Snapping GBIF Data ===")

ti <- system.time(gbif_snap_result <- api_get_snapped_points_cascade_async(
  csv_url = gbif_url,
  colname_lon = "decimalLongitude",
  colname_lat = "decimalLatitude",
  colname_site_id = "gbifID",
  strahler_seq = STRAHLER_SEQ,
  distance_threshold = DISTANCE_THRESHOLD
))

# 218.761 seconds

print(gbif_snap_result)

# Save
min_strahler <- min(STRAHLER_SEQ)
snap_out_path <- paste0("points_snapped/fish/gbif_snapped_points_min_strahler",min_strahler, "_dist_thresh_",DISTANCE_THRESHOLD, ".csv")
fwrite(gbif_snap_result, snap_out_path)

message(sprintf("GBIF: Snapped %d points", nrow(gbif_snap_result)))




# ============================================================================
# VISUALIZE GBIF SNAPPING
# ============================================================================

message("\n--- Creating GBIF visualization ---")

# Identify which points were successfully snapped
gbif_snapped_ids <- gbif_snap_result$gbifID
gbif_original$snapped <- gbif_original$gbifID %in% gbif_snapped_ids

# Summary
n_snapped_gbif <- sum(gbif_original$snapped)
n_failed_gbif <- sum(!gbif_original$snapped)
message(sprintf("  Successfully snapped: %d", n_snapped_gbif))
message(sprintf("  Failed to snap: %d", n_failed_gbif))

# Create map
gbif_map <- leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

  # Original points that were successfully snapped (blue)
  addCircleMarkers(
    data = gbif_original[gbif_original$snapped, ],
    lng = ~decimalLongitude,
    lat = ~decimalLatitude,
    color = "blue",
    fillColor = "blue",
    radius = 4,
    fillOpacity = 0.6,
    stroke = TRUE,
    weight = 1,
    group = "Original (Snapped)",
    popup = ~paste0(
      "<b>Original Point (Successfully Snapped)</b><br>",
      "GBIF ID: ", gbifID, "<br>",
      "Lon: ", round(decimalLongitude, 5), "<br>",
      "Lat: ", round(decimalLatitude, 5)
    )
  ) %>%

  # Original points that FAILED to snap (orange)
  addCircleMarkers(
    data = gbif_original[!gbif_original$snapped, ],
    lng = ~decimalLongitude,
    lat = ~decimalLatitude,
    color = "orange",
    fillColor = "orange",
    radius = 5,
    fillOpacity = 0.9,
    stroke = TRUE,
    weight = 3,
    group = "Original (Failed)",
    popup = ~paste0(
      "<b>⚠️ Original Point (FAILED TO SNAP)</b><br>",
      "GBIF ID: ", gbifID, "<br>",
      "Lon: ", round(decimalLongitude, 5), "<br>",
      "Lat: ", round(decimalLatitude, 5), "<br>",
      "<b style='color:red;'>This point was not successfully snapped</b>"
    )
  ) %>%

  # Snapped points (red)
  addCircleMarkers(
    data = gbif_snap_result,
    lng = ~decimalLongitude_snapped,
    lat = ~decimalLatitude_snapped,
    color = "red",
    fillColor = "red",
    radius = 4,
    fillOpacity = 0.6,
    stroke = TRUE,
    weight = 1,
    group = "Snapped Points",
    popup = ~paste0(
      "<b>Snapped Point</b><br>",
      "GBIF ID: ", gbifID, "<br>",
      "Lon: ", round(decimalLongitude_snapped, 5), "<br>",
      "Lat: ", round(decimalLatitude_snapped, 5), "<br>",
      "Distance: ", round(distance_metres, 1), " m<br>",
      "Strahler: ", strahler
    )
  ) %>%

  # Add lines connecting original to snapped
  addPolylines(
    data = lapply(1:nrow(gbif_snap_result), function(i) {
      st_linestring(matrix(c(
        gbif_snap_result$decimalLongitude_original[i], gbif_snap_result$decimalLatitude_original[i],
        gbif_snap_result$decimalLongitude_snapped[i], gbif_snap_result$decimalLatitude_snapped[i]
      ), ncol = 2, byrow = TRUE))
    }) %>% st_sfc(crs = 4326),
    color = "gray",
    weight = 1,
    opacity = 0.3,
    group = "Snap Lines"
  ) %>%

  # Layer controls
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Satellite"),
    overlayGroups = c("Original (Snapped)", "Original (Failed)", "Snapped Points", "Snap Lines"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # Legend
  addLegend(
    position = "bottomright",
    colors = c("blue", "orange", "red", "gray"),
    labels = c("Original (Snapped)", "Original (Failed)", "Snapped Points", "Snap Lines"),
    title = "GBIF Data",
    opacity = 0.7
  )

gbif_map

# Save map locally
saveWidget(gbif_map, "points_snapped/maps/gbif_snapping_check.html")

# Save map to nimbus
save_to_nimbus(gbif_map, "points_snapped/maps/gbif_snapping_check.html")

# Print snapping statistics
message(sprintf("\nGBIF Snapping Statistics:"))
message(sprintf("  Original points: %d", nrow(gbif_original)))
message(sprintf("  Successfully snapped: %d (%.1f%%)", n_snapped_gbif, 100 * n_snapped_gbif / nrow(gbif_original)))
message(sprintf("  Failed to snap: %d (%.1f%%)", n_failed_gbif, 100 * n_failed_gbif / nrow(gbif_original)))
if (n_snapped_gbif > 0) {
  message(sprintf("  Mean snap distance: %.1f m", mean(gbif_snap_result$distance_metres)))
  message(sprintf("  Median snap distance: %.1f m", median(gbif_snap_result$distance_metres)))
  message(sprintf("  Max snap distance: %.1f m", max(gbif_snap_result$distance_metres)))
}

# Save list of failed points
if (n_failed_gbif > 0) {
  gbif_failed <- gbif_original[!gbif_original$snapped, ]
  fwrite(gbif_failed, "points_cleaned/fish/gbif_failed_to_snap.csv")
  message(sprintf("\nFailed points saved to: points_cleaned/fish/gbif_failed_to_snap.csv"))
}





# ============================================================================
# COMBINE DATASETS
# ============================================================================

message("\n=== Combining Datasets ===")

hcmr_snap_result$source <- "HCMR"
gbif_snap_result$source <- "GBIF"

# Rename lon, lat, site_id columns of gbif dataset
gbif_snap_result <- gbif_snap_result %>%
  rename(longitude_original = decimalLongitude_original,
         latitude_original = decimalLatitude_original,
         longitude_snapped = decimalLongitude_snapped,
         latitude_snapped = decimalLatitude_snapped,
         site_id = gbifID)

# Rename site_id column of HCMR dataset
hcmr_snap_result <- hcmr_snap_result %>%
  rename(site_id = Sites)

# Get common columns
common_cols <- intersect(names(hcmr_snap_result), names(gbif_snap_result))
all_snapped <- rbind(
  hcmr_snap_result[, ..common_cols],
  gbif_snap_result[, ..common_cols]
)

fwrite(all_snapped, "points_snapped/fish/all_snapped_fish_points.csv")
message(sprintf("Combined: %d total points", nrow(all_snapped)))

# all_snapped <- fread("points_snapped/fish/all_snapped_fish_points.csv")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

message("\n=== Snapping Complete ===")
message("\nFinal Statistics:")
message(sprintf("  Total HCMR points: %d", sum(all_snapped$source == "HCMR")))
message(sprintf("  Total GBIF points: %d", sum(all_snapped$source == "GBIF")))
message(sprintf("  Grand total: %d points", nrow(all_snapped)))
message(sprintf("\n  Overall mean snap distance: %.1f m", mean(all_snapped$distance_metres)))
message(sprintf("  Overall median snap distance: %.1f m", median(all_snapped$distance_metres)))

message("\nMaps created:")
message("  - maps/hcmr_snapping_check.html")
message("  - maps/gbif_snapping_check.html")
message("  - maps/combined_snapping_check.html")



# ============================================================================
# SNAP DAMS DATA
# ============================================================================
message("\n=== Snapping Dams Data ===")

# Snapping parameters
STRAHLER_SEQ <- c(4, 3, 2)
DISTANCE_THRESHOLD <- 150  # meters

# Load cleaned dams data URL
dams_url <- "https://nimbus.igb-berlin.de/index.php/s/tagEWdtxTpbRE9g/download/dams_all_clean.csv"

# Load original dams data
dams_original <- fread(dams_url)
message(sprintf("Original dams points: %d", nrow(dams_original)))

# Snap dams data
DISTANCE_THRESHOLD <- 150
dams_snap_result <- api_get_snapped_points_cascade_async(
  csv_url = dams_url,
  colname_lon = "longitude",
  colname_lat = "latitude",
  colname_site_id = "id1",
  strahler_seq = STRAHLER_SEQ,
  distance_threshold = DISTANCE_THRESHOLD
)

print(dams_snap_result)


# dams_snap_result <- fread("points_snapped/dams/dams_snapped_points.csv")
dams_snap_result <- dams_snap_result %>%
  left_join(dams_original, by = "id1") %>%
  select(-longitude, -latitude)

# Save
fwrite(dams_snap_result, "points_snapped/dams/dams_snapped_points.csv")
message(sprintf("Dams: Snapped %d points", nrow(dams_snap_result)))

# ============================================================================
# VISUALIZE DAMS SNAPPING
# ============================================================================

message("\n--- Creating Dams visualization ---")

# Identify which points were successfully snapped
dams_snapped_ids <- dams_snap_result$id1
dams_original$snapped <- dams_original$id1 %in% dams_snapped_ids

# Summary
n_snapped_dams <- sum(dams_original$snapped)
n_failed_dams <- sum(!dams_original$snapped)
message(sprintf("  Successfully snapped: %d", n_snapped_dams))
message(sprintf("  Failed to snap: %d", n_failed_dams))

# Color palette for sources
dams_source_colors <- colorFactor(
  palette = c("RAAY" = "blue", "AMBER" = "green"),
  domain = c("RAAY", "AMBER")
)

# Create map
dams_map <- leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

  # Original points that were successfully snapped (colored by source)
  addCircleMarkers(
    data = dams_original[dams_original$snapped, ],
    lng = ~longitude,
    lat = ~latitude,
    color = ~dams_source_colors(source),
    fillColor = ~dams_source_colors(source),
    radius = 5,
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 2,
    group = "Original (Snapped)",
    popup = ~paste0(
      "<b>Original Dam (Successfully Snapped)</b><br>",
      "Dam ID: ", id1, "<br>",
      "Source: ", source, "<br>",
      "Phase: ", phase, "<br>",
      "Status: ", status, "<br>",
      "Type: ", type, "<br>",
      "Lon: ", round(longitude, 5), "<br>",
      "Lat: ", round(latitude, 5)
    )
  ) %>%

  # Original points that FAILED to snap (orange)
  addCircleMarkers(
    data = dams_original[!dams_original$snapped, ],
    lng = ~longitude,
    lat = ~latitude,
    color = "orange",
    fillColor = "orange",
    radius = 6,
    fillOpacity = 0.9,
    stroke = TRUE,
    weight = 3,
    group = "Original (Failed)",
    popup = ~paste0(
      "<b>⚠️ Original Dam (FAILED TO SNAP)</b><br>",
      "Dam ID: ", id1, "<br>",
      "Source: ", source, "<br>",
      "Phase: ", phase, "<br>",
      "Status: ", status, "<br>",
      "Lon: ", round(longitude, 5), "<br>",
      "Lat: ", round(latitude, 5), "<br>",
      "<b style='color:red;'>This point was not successfully snapped</b>"
    )
  ) %>%

  # Snapped points (red)
  addCircleMarkers(
    data = dams_snap_result,
    lng = ~longitude_snapped,
    lat = ~latitude_snapped,
    color = "red",
    fillColor = "red",
    radius = 5,
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 2,
    group = "Snapped Points",
    popup = ~paste0(
      "<b>Snapped Dam</b><br>",
      "Dam ID: ", id1, "<br>",
      "Lon: ", round(longitude_snapped, 5), "<br>",
      "Lat: ", round(latitude_snapped, 5), "<br>",
      "Distance: ", round(distance_metres, 1), " m<br>",
      "Strahler: ", strahler
    )
  ) %>%

  # Add lines connecting original to snapped
  addPolylines(
    data = lapply(1:nrow(dams_snap_result), function(i) {
      st_linestring(matrix(c(
        dams_snap_result$longitude_original[i], dams_snap_result$latitude_original[i],
        dams_snap_result$longitude_snapped[i], dams_snap_result$latitude_snapped[i]
      ), ncol = 2, byrow = TRUE))
    }) %>% st_sfc(crs = 4326),
    color = "gray",
    weight = 1,
    opacity = 0.5,
    group = "Snap Lines"
  ) %>%

  # Layer controls
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Satellite"),
    overlayGroups = c("Original (Snapped)", "Original (Failed)", "Snapped Points", "Snap Lines"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # Legend for sources
  addLegend(
    position = "bottomright",
    pal = dams_source_colors,
    values = dams_original$source,
    title = "Dam Source",
    opacity = 0.7
  )

# Save map locally
saveWidget(dams_map, "points_snapped/maps/dams_snapping_check.html")

# Save map to nimbus
save_to_nimbus(dams_map, "points_snapped/maps/dams_snapping_check.html")

# Print snapping statistics
message(sprintf("\nDams Snapping Statistics:"))
message(sprintf("  Original points: %d", nrow(dams_original)))
message(sprintf("  Successfully snapped: %d (%.1f%%)", n_snapped_dams, 100 * n_snapped_dams / nrow(dams_original)))
message(sprintf("  Failed to snap: %d (%.1f%%)", n_failed_dams, 100 * n_failed_dams / nrow(dams_original)))

if (n_snapped_dams > 0) {
  message(sprintf("  Mean snap distance: %.1f m", mean(dams_snap_result$distance_metres)))
  message(sprintf("  Median snap distance: %.1f m", median(dams_snap_result$distance_metres)))
  message(sprintf("  Max snap distance: %.1f m", max(dams_snap_result$distance_metres)))

  # Statistics by source
  message("\n  By source:")
  for (src in unique(dams_original$source)) {
    snapped_from_source <- sum(dams_snap_result$source == src, na.rm = TRUE)
    total_from_source <- sum(dams_original$source == src)
    message(sprintf("    %s: %d/%d snapped (%.1f%%)",
                    src, snapped_from_source, total_from_source,
                    100 * snapped_from_source / total_from_source))
  }

  # Statistics by status
  message("\n  By status:")
  for (stat in unique(dams_original$status)) {
    snapped_from_status <- sum(dams_snap_result$status == stat, na.rm = TRUE)
    total_from_status <- sum(dams_original$status == stat)
    message(sprintf("    %s: %d/%d snapped (%.1f%%)",
                    stat, snapped_from_status, total_from_status,
                    100 * snapped_from_status / total_from_status))
  }
}

# Save list of failed points
if (n_failed_dams > 0) {
  dams_failed <- dams_original[!dams_original$snapped, ]
  fwrite(dams_failed, "points_cleaned/dams/dams_failed_to_snap.csv")
  message(sprintf("\nFailed points saved to: points_cleaned/dams/dams_failed_to_snap.csv"))
}










