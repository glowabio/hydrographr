# ============================================================================
# CLEAN ALL DAM DATA
# ============================================================================
# Purpose: Process and combine dam data from multiple sources
# Sources:
#   1. RAAY Small Hydropower Plants (CSV)
#   2. AMBER Dams Database (CSV)
# Output: Combined cleaned dam dataset ready for snapping
# ============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(data.table)
library(sf)
library(leaflet)
library(htmlwidgets)
# Load helper function
source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")

# ============================================================================
# SETUP PATHS
# ============================================================================

# Set nimbus path
wdir <- "/run/user/1000/gvfs/dav:host=nimbus.igb-berlin.de,ssl=true,user=grigoropoulou,prefix=%2Fremote.php%2Fwebdav/workflow_paper/data"

# delete
wdir <- "~/Documents/Postdoc/projects/workflow_paper/data"


# Set local working directory
setwd(wdir)

# Create directory structure if needed
dir.create("points_original/dams", recursive = TRUE, showWarnings = FALSE)
dir.create("points_original/maps", recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/dams", recursive = TRUE, showWarnings = FALSE)


# ============================================================================
# 1. PROCESS RAAY SMALL HYDROPOWER PLANTS
# ============================================================================

message("\n=== Processing RAAY Hydropower Data ===")
# Define input directory and file mapping
csv_dir <- "points_original/dams"

file_map <- c(
  "V_SDI_R_HYDRO12_Installation_Licence.csv" = "IL",  # Installation Permit
  "V_SDI_R_HYDRO13_Operational_Licence.csv"  = "OL",  # Operational Permit
  "V_SDI_R_HYDRO11_Production_Licence.csv"   = "PL",  # Production Permit
  "V_SDI_R_HYDRO7_Evaluation.csv"            = "E",   # Evaluation
  "V_SDI_R_HYDRO_OTHER_VALUES_Rejected.csv"  = "R"    # Rejected
)

# Function to process each CSV file
read_and_process_raay <- function(file_name, phase_code) {
  file_path <- file.path(csv_dir, file_name)

  # Check if file exists
  if (!file.exists(file_path)) {
    warning(sprintf("File not found: %s", file_path))
    return(NULL)
  }

  df <- fread(file_path)

  # Rename status column if needed
  if ("katastash_code" %in% names(df)) {
    df <- rename(df, status_code = katastash_code)
  }

  # Process geometry column
  if ("geometry" %in% names(df)) {
    df <- df %>%
      mutate(
        geometry = str_remove_all(geometry, "MULTIPOINT|\\(|\\)"),
        geometry = str_trim(geometry)
      ) %>%
      separate_rows(geometry, sep = ",") %>%
      group_by(across(-geometry)) %>%
      slice(1) %>%  # Keep only first coordinate pair per row
      ungroup() %>%
      separate(geometry, into = c("longitude", "latitude"), sep = " ", convert = TRUE)
  }

  # Filter for Y/L part if column exists
  if ("part" %in% names(df)) {
    df <- df %>% filter(part == "Y/L")
  }

  # Add phase and type
  df <- mutate(df,
               phase = phase_code,
               type = "shp",  # small hydropower plant
               source = "RAAY",
               status = if_else(phase_code == "OL", "existing", "planned"))

  # keep only the most recent record per location
    df <- df %>%
      group_by(longitude, latitude) %>%
      arrange(desc(imerominia)) %>%
      slice(1) %>%
      ungroup()

  return(df)
}

# Process all CSV files
raay_dams <- imap_dfr(names(file_map), ~ read_and_process_raay(.x, file_map[[.x]]))

# keep only the most recent record per location
raay_dams <- raay_dams %>%
  group_by(longitude, latitude) %>%
  arrange(desc(imerominia)) %>%
  slice(1) %>%
  ungroup()


# Check if any data was loaded
if (nrow(raay_dams) == 0) {
  stop("No data loaded from CSV files. Check file paths and names.")
}

# Select relevant columns
raay_dams <- raay_dams %>%
  select(id1, phase, status_code, longitude, latitude,
         type, source, status, imerominia) %>%
  rename(date = imerominia)

message(sprintf("RAAY: Processed %d hydropower plants", nrow(raay_dams)))
message(sprintf("  Phases: %s", paste(unique(raay_dams$phase), collapse = ", ")))

# Save individual file
fwrite(raay_dams, "points_cleaned/dams/dams_raay_clean.csv")

# Convert to sf object and save as GPKG
raay_dams_sf <- st_as_sf(raay_dams,
                         coords = c("longitude", "latitude"),
                         crs = 4326,  # WGS84
                         remove = FALSE)  # Keep lon/lat columns
# Save to nimbus
save_to_nimbus(raay_dams_sf, "points_cleaned/dams/dams_raay_clean.gpkg")

# Save locally
st_write(raay_dams_sf, "points_cleaned/dams/dams_raay_clean.gpkg")

message("GPKG file saved: points_cleaned/dams/dams_raay_clean.gpkg")

# ============================================================================
# 2. PROCESS AMBER DAMS
# ============================================================================

message("\n=== Processing AMBER Dams Data ===")

amber_file <- "points_original/dams/atlas-country-Greece.csv"

amber_dams <- read.csv(amber_file) %>%
  select(GUID, Longitude_WGS84, Latitude_WGS84, type) %>%
  rename(
    longitude = Longitude_WGS84,
    latitude = Latitude_WGS84
  ) %>%
  mutate(
    type = "dam",
    phase = "amber",
    status_code = "existing",
    source = "AMBER",
    status = "existing"
  ) %>%
  mutate(GUID = gsub("\\{|\\}", "", GUID)) %>%  # Remove { or } from GUID
  mutate(id1 = sapply(strsplit(as.character(GUID), "-"), `[`, 5), .before = GUID) %>%
  select(id1, phase, status_code, longitude, latitude, type, source, status) %>%
  mutate(date = NA)

message(sprintf("AMBER: Processed %d dams", nrow(amber_dams)))

# Save individual file locally
fwrite(amber_dams, "points_cleaned/dams/dams_amber_clean.csv")

# Convert to sf object and save as GPKG
amber_dams_sf <- st_as_sf(amber_dams,
                          coords = c("longitude", "latitude"),
                          crs = 4326,  # WGS84
                          remove = FALSE)  # Keep lon/lat columns
save_to_nimbus(amber_dams_sf, "points_cleaned/dams/amber_dams_clean.gpkg")

# Save locally
st_write(amber_dams_sf, "points_cleaned/dams/amber_dams_clean.gpkg")


# ============================================================================
# 3. COMBINE ALL DAM DATASETS
# ============================================================================

message("\n=== Combining All Dam Datasets ===")

dams_all <- rbind(
  raay_dams,
  amber_dams
)

# Summary statistics
message(sprintf("\nCombined Dataset Summary:"))
message(sprintf("  Total dams: %d", nrow(dams_all)))
message(sprintf("  By source:"))
for (src in unique(dams_all$source)) {
  n <- sum(dams_all$source == src)
  pct <- round(100 * n / nrow(dams_all), 1)
  message(sprintf("    %s: %d (%.1f%%)", src, n, pct))
}

message(sprintf("\n  RAAY by phase:"))
raay_summary <- dams_all %>%
  filter(source == "RAAY") %>%
  group_by(phase) %>%
  summarise(n = n(), .groups = "drop")
for (i in 1:nrow(raay_summary)) {
  message(sprintf("    %s: %d", raay_summary$phase[i], raay_summary$n[i]))
}

# Save combined file
fwrite(dams_all, "points_cleaned/dams/dams_all_clean.csv")

message("\n✓ All dam data cleaned and saved")

# ============================================================================
# 4. VISUALIZATION: ALL DAM SOURCES
# ============================================================================

message("\n=== Creating Visualization ===")

# Color palette for sources
source_colors <- colorFactor(
  palette = c("RAAY" = "blue", "AMBER" = "green"),
  domain = c("RAAY", "AMBER")
)

# Color palette for RAAY phases
phase_colors <- colorFactor(
  palette = c("IL" = "lightblue", "OL" = "darkblue", "PL" = "purple",
              "E" = "orange", "R" = "gray"),
  domain = c("IL", "OL", "PL", "E", "R")
)

# Create overview map
dams_map <- leaflet(dams_all) %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

  # All dams colored by source
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    color = ~source_colors(source),
    fillColor = ~source_colors(source),
    radius = 4,
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 1,
    group = "By Source",
    popup = ~paste0(
      "<b>Dam ID: </b>", id1, "<br>",
      "<b>Source: </b>", source, "<br>",
      "<b>Phase: </b>", phase, "<br>",
      "<b>Status: </b>", status_code, "<br>",
      "<b>Type: </b>", type, "<br>",
      "<b>Lon: </b>", round(longitude, 5), "<br>",
      "<b>Lat: </b>", round(latitude, 5)
    )
  ) %>%

  # RAAY dams colored by phase
  addCircleMarkers(
    data = dams_all %>% filter(source == "RAAY"),
    lng = ~longitude,
    lat = ~latitude,
    color = ~phase_colors(phase),
    fillColor = ~phase_colors(phase),
    radius = 4,
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 1,
    group = "RAAY by Phase",
    popup = ~paste0(
      "<b>Dam ID: </b>", id1, "<br>",
      "<b>Phase: </b>", phase, "<br>",
      "<b>Status: </b>", status_code, "<br>",
      "<b>Lon: </b>", round(longitude, 5), "<br>",
      "<b>Lat: </b>", round(latitude, 5)
    )
  ) %>%

  # Layer controls
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Satellite"),
    overlayGroups = c("By Source", "RAAY by Phase"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # Legend for sources
  addLegend(
    position = "bottomright",
    pal = source_colors,
    values = ~source,
    title = "Dam Source",
    opacity = 0.7,
    group = "By Source"
  ) %>%

  # Legend for RAAY phases
  addLegend(
    position = "bottomleft",
    pal = phase_colors,
    values = dams_all %>% filter(source == "RAAY") %>% pull(phase),
    title = "RAAY Phase",
    opacity = 0.7,
    group = "RAAY by Phase"
  ) %>%

  # Hide phase legend initially
  hideGroup("RAAY by Phase")

# Save map
save_to_nimbus(dams_map, "points_cleaned/maps/dams_all_sources_overview.html")

# Save map locally
saveWidget(dams_map, "points_cleaned/maps/dams_all_sources_overview.html")


message("\n✓ Visualization complete")

# ============================================================================
# 5. DATA QUALITY CHECKS
# ============================================================================

message("\n=== Data Quality Checks ===")

# Check for missing coordinates
missing_coords <- dams_all %>%
  filter(is.na(longitude) | is.na(latitude))

if (nrow(missing_coords) > 0) {
  message(sprintf("⚠️  WARNING: %d dams have missing coordinates", nrow(missing_coords)))
  message("  Sources affected:")
  print(table(missing_coords$source))
} else {
  message("✓ No missing coordinates")
}

# Check for duplicates
duplicates <- dams_all %>%
  group_by(longitude, latitude) %>%
  filter(n() > 1) %>%
  arrange(longitude, latitude)

if (nrow(duplicates) > 0) {
  message(sprintf("⚠️  WARNING: %d records at duplicate locations", nrow(duplicates)))
  message("  These may be the same dam in multiple databases")
} else {
  message("✓ No duplicate locations")
}

# Check coordinate ranges (Greece roughly: lon 19-28, lat 34-42)
out_of_bounds <- dams_all %>%
  filter(longitude < 19 | longitude > 28 | latitude < 34 | latitude > 42)

if (nrow(out_of_bounds) > 0) {
  message(sprintf("⚠️  WARNING: %d dams outside Greece bounds", nrow(out_of_bounds)))
  message("  Check these manually")
} else {
  message("✓ All coordinates within Greece bounds")
}




# ============================================================================
# 6. DUPLICATE ANALYSIS AND VISUALIZATION
# ============================================================================

message("\n=== Analyzing Duplicates ===")

# Find duplicates with detailed information
duplicates_detailed <- dams_all %>%
  group_by(longitude, latitude) %>%
  mutate(
    n_at_location = n(),
    is_duplicate = n() > 1
  ) %>%
  ungroup() %>%
  filter(is_duplicate) %>%
  arrange(longitude, latitude, source)

if (nrow(duplicates_detailed) > 0) {
  message(sprintf("Found %d records at %d duplicate locations",
                  nrow(duplicates_detailed),
                  n_distinct(duplicates_detailed[, c("longitude", "latitude")])))

  # Statistics by source combination
  duplicate_combos <- duplicates_detailed %>%
    group_by(longitude, latitude) %>%
    summarise(
      sources = paste(sort(unique(source)), collapse = " + "),
      n_sources = n_distinct(source),
      total_records = n(),
      .groups = "drop"
    )

  message("\nDuplicate location breakdown:")
  message(sprintf("  Locations with records from multiple sources: %d",
                  sum(duplicate_combos$n_sources > 1)))
  message(sprintf("  Locations with records from same source: %d",
                  sum(duplicate_combos$n_sources == 1)))

  message("\nBy source combination:")
  combo_summary <- duplicate_combos %>%
    group_by(sources) %>%
    summarise(
      n_locations = n(),
      total_records = sum(total_records),
      .groups = "drop"
    ) %>%
    arrange(desc(n_locations))

  for (i in 1:nrow(combo_summary)) {
    message(sprintf("  %s: %d locations (%d records)",
                    combo_summary$sources[i],
                    combo_summary$n_locations[i],
                    combo_summary$total_records[i]))
  }

  # Save duplicates to CSV
  fwrite(duplicates_detailed, "points_cleaned/dams/dams_duplicates.csv")
  message("\n✓ Duplicates saved to: points_cleaned/dams/dams_duplicates.csv")

  # Create visualization of duplicates
  # Color palette for duplicate visualization
  dup_source_colors <- colorFactor(
    palette = c("RAAY" = "red", "AMBER" = "orange"),
    domain = c("RAAY", "AMBER")
  )

  duplicates_map <- leaflet() %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

    # Add duplicate points
    addCircleMarkers(
      data = duplicates_detailed,
      lng = ~longitude,
      lat = ~latitude,
      color = ~dup_source_colors(source),
      fillColor = ~dup_source_colors(source),
      radius = 6,
      fillOpacity = 0.8,
      stroke = TRUE,
      weight = 2,
      popup = ~paste0(
        "<b style='color:red;'>DUPLICATE LOCATION</b><br>",
        "<b>Records at this location: </b>", n_at_location, "<br>",
        "<b>Dam ID: </b>", id1, "<br>",
        "<b>Source: </b>", source, "<br>",
        "<b>Phase: </b>", phase, "<br>",
        "<b>Status: </b>", status_code, "<br>",
        "<b>Type: </b>", type, "<br>",
        "<b>Lon: </b>", round(longitude, 6), "<br>",
        "<b>Lat: </b>", round(latitude, 6)
      ),
      label = ~paste0("Duplicate: ", n_at_location, " records (", source, ")")
    ) %>%

    # Add layer controls
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Satellite"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%

    # Add legend
    addLegend(
      position = "bottomright",
      pal = dup_source_colors,
      values = duplicates_detailed$source,
      title = "Duplicate Source",
      opacity = 0.8
    )

  # Save duplicates map to nimbus
  save_to_nimbus(duplicates_map, "points_cleaned/maps/dams_duplicates_map.html")
  message("✓ Duplicate visualization saved: points_cleaned/maps/dams_duplicates_map.html")

  # Save duplicates map locally
  saveWidget(duplicates_map, "points_cleaned/maps/dams_duplicates_map.html")
  message("✓ Duplicate visualization saved: points_cleaned/maps/dams_duplicates_map.html")



  # Create comparison table for cross-source duplicates
  cross_source_dups <- duplicates_detailed %>%
    group_by(longitude, latitude) %>%
    filter(n_distinct(source) > 1) %>%
    arrange(longitude, latitude, source) %>%
    select(longitude, latitude, source, id1, phase, status_code, type) %>%
    ungroup()

  if (nrow(cross_source_dups) > 0) {
    message(sprintf("\n⚠️  Found %d records at locations appearing in multiple databases",
                    nrow(cross_source_dups)))
    fwrite(cross_source_dups, "points_cleaned/dams/dams_cross_source_duplicates.csv")
    message("✓ Cross-source duplicates saved to: points_cleaned/dams_cross_source_duplicates.csv")
  }

} else {
  message("✓ No duplicate locations found")
}


# ============================================================================
# SUMMARY
# ============================================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("DAM DATA CLEANING COMPLETE")
message(paste(rep("=", 80), collapse = ""))
message(sprintf("\nFiles created:"))
message(sprintf("  - points_cleaned/dams_raay_clean.csv (%d dams)", nrow(raay_dams)))
message(sprintf("  - points_cleaned/dams_amber_clean.csv (%d dams)", nrow(amber_dams)))
message(sprintf("  - points_cleaned/dams_all_clean.csv (%d dams)", nrow(dams_all)))
message("\nMap created:")
message("  - points_original/maps/dams_all_sources_overview.html")
message("\nAll files copied to Nimbus")
message("\n✓ Ready for snapping!")



