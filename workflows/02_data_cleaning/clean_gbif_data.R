# ============================================================================
# CLEAN GBIF FISH OCCURRENCE DATA
# ============================================================================
# Purpose: Clean, spatially validate, and taxonomically check freshwater fish
#          occurrence records from GBIF for Greece
# Input: Raw GBIF CSV (may have malformed rows)
# Output: Cleaned GBIF fish occurrences ready for snapping
# ============================================================================

library(data.table)
library(dplyr)
library(tidyr)
library(CoordinateCleaner)
library(specleanr)
library(rfishbase)
library(readr)
library(leaflet)
library(htmlwidgets)

# Install danubeoccurR if needed
# devtools::install_github("ytorres-cambas/danubeoccurR")
library(danubeoccurR)

# Load helper function
source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")

# ============================================================================
# SETUP PATHS
# ============================================================================

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# Create directory structure
dir.create("points_original/fish", recursive = TRUE, showWarnings = FALSE)
dir.create("points_original/maps", recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/fish", recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/maps", recursive = TRUE, showWarnings = FALSE)


# ============================================================================
# 1. READ AND FIX MALFORMED CSV
# ============================================================================

message("\n=== Step 1: Reading and Fixing Raw GBIF Data ===")

file_raw <- file.path(BASE_DIR, "points_original/fish/combined_greece_fish_occurrences_from_sp_list.csv")

# Read with fread (handles large files efficiently)
gbif <- fread(file_raw,
              quote = "\"",
              fill = Inf)

message(sprintf("Initial read: %d rows, %d columns", nrow(gbif), ncol(gbif)))

# ============================================================================
# 1a. Remove rows with extra V columns (malformed rows)
# ============================================================================

# Identify V columns (overflow columns from malformed rows)
v_cols <- grep("^V[0-9]+$", names(gbif), value = TRUE)

if (length(v_cols) > 0) {
  message(sprintf("Found %d overflow columns (V*) - removing malformed rows", length(v_cols)))

  # Keep only rows where all V columns are empty
  gbif_tmp <- gbif[rowSums(gbif[, ..v_cols] != "", na.rm = TRUE) == 0]
  gbif_tmp <- gbif_tmp[, !..v_cols]  # Drop V columns

  message(sprintf("Removed %d malformed rows", nrow(gbif) - nrow(gbif_tmp)))
} else {
  gbif_tmp <- gbif
  message("No overflow columns found")
}

# ============================================================================
# 1b. Fix problematic longitude values
# ============================================================================

# Check for unrealistic longitude values
if ("decimalLongitude" %in% names(gbif_tmp)) {
  bad_lons <- gbif_tmp %>%
    filter(!is.na(decimalLongitude), decimalLongitude > 1000)

  if (nrow(bad_lons) > 0) {
    message(sprintf("Found %d rows with invalid longitude (>1000)", nrow(bad_lons)))
    gbif_tmp <- gbif_tmp %>% filter(decimalLongitude < 1000 | is.na(decimalLongitude))
  }
}

# ============================================================================
# 1c. Use readr to catch remaining parsing issues
# ============================================================================

# Save temporary file
temp_file <- tempfile(fileext = ".csv")
fwrite(gbif_tmp, temp_file)

message(sprintf("Saved %d rows to temp file for validation", nrow(gbif_tmp)))

# Read with readr to detect remaining problems
gbif_tmp <- read_csv(temp_file, show_col_types = FALSE)

# Get problematic row numbers
prob_rows <- problems(gbif_tmp)$row

if (length(prob_rows) > 0) {
  message(sprintf("Found %d rows with parsing issues - removing", length(prob_rows)))
  gbif_clean <- gbif_tmp[-prob_rows, ]
} else {
  message("No parsing issues found")
  gbif_clean <- gbif_tmp
}

# Clean up temp file
unlink(temp_file)

# Save the fixed raw file
save_to_nimbus(gbif_clean, "points_original/fish/combined_greece_fish_occurrences_fixed.csv")

# delete?
fwrite(gbif_clean, "points_original/fish/combined_greece_fish_occurrences_from_sp_list_fixed.csv")

message(sprintf("\n✓ CSV fixing complete: %d clean rows retained", nrow(gbif_clean)))

# ============================================================================
# 2. INITIAL EXPLORATION
# ============================================================================

message("\n=== Step 2: Initial Exploration ===")

gbif_raw <- fread("points_original/fish/combined_greece_fish_occurrences_from_sp_list_fixed.csv")
gbif_raw <- gbif_clean

message(sprintf("Dataset dimensions: %d rows × %d columns", nrow(gbif_raw), ncol(gbif_raw)))
message("\nColumn names:")
print(names(gbif_raw))

# Check for duplicates
duplicates <- gbif_raw %>%
  count(decimalLongitude, decimalLatitude, speciesKey, datasetKey, year) %>%
  filter(n > 1)

if (nrow(duplicates) > 0) {
  message(sprintf("\n⚠️  Found %d duplicate coordinate-species-year combinations", nrow(duplicates)))
} else {
  message("\n✓ No exact duplicates found")
}

# ============================================================================
# 3. COORDINATE TYPE CONVERSION
# ============================================================================


## First filter occurrences only from Greece
gbif_raw <- gbif_raw %>% filter(countryCode=="GR")

message("\n=== Step 3: Converting Coordinates to Numeric ===")

gbif_raw <- gbif_raw %>%
  mutate(
    decimalLatitude  = as.numeric(decimalLatitude),
    decimalLongitude = as.numeric(decimalLongitude)
  )

# Check coordinate ranges (Greece: lon ~19-28, lat ~34-42)
coord_summary <- gbif_raw %>%
  filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) %>%
  summarise(
    min_lon = min(decimalLongitude),
    max_lon = max(decimalLongitude),
    min_lat = min(decimalLatitude),
    max_lat = max(decimalLatitude)
  )

message(sprintf("Longitude range: %.2f to %.2f", coord_summary$min_lon, coord_summary$max_lon))
message(sprintf("Latitude range: %.2f to %.2f", coord_summary$min_lat, coord_summary$max_lat))

# ============================================================================
# 4. METADATA-BASED FILTERING
# ============================================================================

message("\n=== Step 4: Metadata Filtering ===")

n_before <- nrow(gbif_raw)

gbif_metadata_filtered <- gbif_raw %>%
  filter(!is.na(year)) %>%
  filter(coordinatePrecision < 0.01 | is.na(coordinatePrecision)) %>%
  filter(coordinateUncertaintyInMeters <= 1000 | is.na(coordinateUncertaintyInMeters)) %>%
  filter(!coordinateUncertaintyInMeters %in% c(301, 3036, 999, 9999))

n_after <- nrow(gbif_metadata_filtered)
message(sprintf("Removed %d rows (%.1f%%) based on metadata filters",
                n_before - n_after,
                100 * (n_before - n_after) / n_before))

# ============================================================================
# 5. SPATIAL FILTERS (CoordinateCleaner)
# ============================================================================

message("\n=== Step 5: Spatial Filtering with CoordinateCleaner ===")

n_before <- nrow(gbif_metadata_filtered)

gbif_spatial_cleaned <- gbif_metadata_filtered %>%
  cc_cen(buffer = 1000, verbose = TRUE) %>%
  cc_cap(buffer = 1000, verbose = TRUE) %>%
  cc_inst(buffer = 1000, verbose = TRUE) %>%
  distinct(
    decimalLongitude,
    decimalLatitude,
    speciesKey,
    datasetKey,
    year,
    .keep_all = TRUE
  ) %>%
  filter(!is.na(species), species != "")

n_after <- nrow(gbif_spatial_cleaned)
message(sprintf("Removed %d rows (%.1f%%) with spatial filters",
                n_before - n_after,
                100 * (n_before - n_after) / n_before))

# ============================================================================
# 6. TAXONOMIC VALIDATION
# ============================================================================

message("\n=== Step 6: Taxonomic Name Checking ===")
message("This may take several minutes...")

gbif_taxa_checked <- check_species_name(
  data = gbif_spatial_cleaned,
  col_species_name = "species",
  target_accuracy = 90,
  accuracy_decrement = NULL,
  verbose = TRUE,
  manual = TRUE
)

message(sprintf("Taxonomic validation complete: %d species validated",
                length(unique(gbif_taxa_checked$species))))

# ============================================================================
# 7. MANUAL CURATION
# ============================================================================

message("\n=== Step 7: Manual Curation ===")

# Remove specific problematic rows if identified during manual review
# Adjust row numbers based on your manual inspection
# gbif_taxa_curated <- gbif_taxa_checked[-c(5836:5840), ]

# For now, keep all
gbif_taxa_curated <- gbif_taxa_checked

message(sprintf("After manual curation: %d rows", nrow(gbif_taxa_curated)))

# ============================================================================
# 8. FINAL CLEANING AND COLUMN SELECTION
# ============================================================================

message("\n=== Step 8: Final Cleaning ===")

gbif_cleaned <- gbif_taxa_curated %>%
  mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
  select(
    gbifID,
    datasetKey,
    class,
    order,
    family,
    genus,
    species,
    year,
    month,
    day,
    decimalLongitude,
    decimalLatitude
  ) %>%
  filter(
    !is.na(decimalLongitude),
    !is.na(decimalLatitude)
  )

message(sprintf("\nFinal cleaned dataset: %d rows", nrow(gbif_cleaned)))

# ============================================================================
# 9. VISUALIZATION
# ============================================================================

message("\n=== Step 9: Creating Visualization ===")

# Create overview map
gbif_map <- leaflet(gbif_cleaned) %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addCircleMarkers(
    lng = ~decimalLongitude,
    lat = ~decimalLatitude,
    radius = 3,
    color = "blue",
    fillColor = "blue",
    fillOpacity = 0.5,
    stroke = FALSE,
    popup = ~paste0(
      "<b>Species:</b> ", species, "<br>",
      "<b>Family:</b> ", family, "<br>",
      "<b>Year:</b> ", year, "<br>",
      "<b>GBIF ID:</b> ", gbifID
    )
  ) %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Save map
save_to_nimbus(gbif_map, "points_cleaned/maps/gbif_fish_cleaned_overview.html")

# Save locally
saveWidget(gbif_map, "points_cleaned/maps/gbif_fish__from_sp_list_cleaned_overview.html")

# ============================================================================
# 10. SAVE CLEANED DATA
# ============================================================================

message("\n=== Step 10: Saving Cleaned Data ===")

# Save to Nimbus
save_to_nimbus(gbif_cleaned, "points_cleaned/fish/fish_gbif_clean.csv")

# Save locally
fwrite(gbif_cleaned, "points_cleaned/fish/fish_gbif_from_sp_list_clean.csv")

# gbif_cleaned <- fread("points_cleaned/fish/fish_gbif_clean.csv")

# Remove duplicates, keep only coordinates and gbifID to snap
gbif_cleaned_unique <- gbif_cleaned %>%
  distinct(decimalLongitude,decimalLatitude, .keep_all = TRUE)

fwrite(gbif_cleaned_unique, "points_cleaned/fish/fish_gbif_from_sp_list_clean_unique.csv")

# GBIF data of fish might include records in the sea that we do not need
# We can test if this is the case, trying to obtain the regional unit ID of the
# occurrences. In case any occurrences are not assigned an ID, it means they fall in the sea
# and we can discard them from further steps. We chose regional unit id because
# it is a higher level of organisation than basin or subcatchment id,
# and thus it is faster to obtain it from the database

gbif_cleaned_unique <- fread("points_cleaned/fish/fish_gbif_from_sp_list_clean_unique.csv")


system.time(reg_unit_ids <- api_get_local_ids(data = gbif_cleaned_unique,
                                              colname_lon = "decimalLongitude",
                                              colname_lat = "decimalLatitude",
                                              colname_site_id = "gbifID"))
# CSV version
# Get the URL of the above file to obtain the ids
# gbif_cleaned_unique_url <- "https://nimbus.igb-berlin.de/index.php/s/kq7pLJrtdC7WsFW/download/fish_gbif_from_sp_list_clean_unique.csv"

# system.time(reg_unit_ids2 <- api_get_local_ids(csv_url = gbif_cleaned_unique_url,
#                    colname_lon = "decimalLongitude",
#                    colname_lat = "decimalLatitude",
#                    colname_site_id = "gbifID",
#                    which_ids = "reg_id"))

# Keep only occurrences that were assigned regional unit id
# Get site_id of these occurrences
site_ids_keep <- reg_unit_ids %>% filter(!is.na(reg_id)) %>%
  pull(gbifID)

# filter data to keep these site_ids
gbif_cleaned_to_snap <- gbif_cleaned_unique %>%
  filter(gbifID %in% site_ids_keep)

fwrite(gbif_cleaned_to_snap, "points_cleaned/fish/fish_gbif_from_sp_list_clean_to_snap.csv")


# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("GBIF DATA CLEANING COMPLETE")
message(paste(rep("=", 80), collapse = ""))

message(sprintf("\nCleaning Summary:"))
message(sprintf("  Original rows: %d", nrow(gbif)))
message(sprintf("  After CSV fixing: %d (%.1f%% retained)",
                nrow(gbif_clean),
                100 * nrow(gbif_clean) / nrow(gbif)))
message(sprintf("  After metadata filters: %d", nrow(gbif_metadata_filtered)))
message(sprintf("  After spatial filters: %d", nrow(gbif_spatial_cleaned)))
message(sprintf("  After taxonomic validation: %d", nrow(gbif_taxa_checked)))
message(sprintf("  Final cleaned: %d (%.1f%% of original)",
                nrow(gbif_cleaned),
                100 * nrow(gbif_cleaned) / nrow(gbif)))

message(sprintf("\nTaxonomic Summary:"))
message(sprintf("  Unique species: %d", length(unique(gbif_cleaned$species))))
message(sprintf("  Unique families: %d", length(unique(gbif_cleaned$family))))
message(sprintf("  Unique genera: %d", length(unique(gbif_cleaned$genus))))

message(sprintf("\nTemporal Coverage:"))
year_range <- range(gbif_cleaned$year, na.rm = TRUE)
message(sprintf("  Year range: %d - %d", year_range[1], year_range[2]))

message(sprintf("\nSpatial Coverage:"))
message(sprintf("  Longitude: %.2f to %.2f",
                min(gbif_cleaned$decimalLongitude),
                max(gbif_cleaned$decimalLongitude)))
message(sprintf("  Latitude: %.2f to %.2f",
                min(gbif_cleaned$decimalLatitude),
                max(gbif_cleaned$decimalLatitude)))

message("\nFiles created:")
message("  - points_original/fish/combined_greece_fish_occurrences_fixed.csv")
message("  - points_cleaned/fish/fish_gbif_clean.csv")
message("  - points_original/maps/gbif_fish_cleaned_overview.html")
message("\n✓ Ready for snapping!")

