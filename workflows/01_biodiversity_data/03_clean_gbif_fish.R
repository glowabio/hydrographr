#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03_clean_gbif_fish.R   (Module 1 -- Biodiversity Data)
#
# Clean, spatially validate, and taxonomically check the freshwater fish
# occurrence records downloaded from GBIF (script 02). Repairs malformed
# CSV rows, applies metadata and spatial filters, validates taxonomy against
# the GBIF backbone, filters to the study basin, and exports occurrences
# ready for snapping.
#
# Extent: full Vjosa/Aoos basin (basin filter in Step 8b), matching the SDM
# training extent. The Sarantaporos subbasin trim happens later, in Module 3.
#
# Workflow:
#   1.  Read raw GBIF CSV and repair malformed rows
#   2.  Initial exploration / duplicate check
#   3.  Convert coordinates to numeric
#   4.  Metadata-based filtering (year, precision, uncertainty)
#   5.  Spatial filtering (CoordinateCleaner: centroids, capitals, institutions)
#   6.  Taxonomic name validation against GBIF backbone (interactive)
#   7.  Manual curation hook
#   8.  Final column selection + filter; 8b filter to target basin
#   9.  Visualise
#   10. Save cleaned + to-snap datasets
#
# INPUT:
#   - points_original/fish/fish_data_gbif.csv   (raw download, from script 02)
#   - config/study_area_params.csv              (BASIN_ID, from script 01)
#
# OUTPUT:
#   - points_original/fish/fish_data_gbif_fixed.csv  (CSV-repaired raw)
#   - points_cleaned/fish/fish_gbif_clean.csv        (cleaned, basin only)
#   - points_cleaned/fish/fish_gbif_clean_to_snap.csv (unique locs for snapping)
#   - points_cleaned/maps/gbif_fish_cleaned_overview.html
#
# REQUIRES: internet access (api_get_ids for basin assignment).
#
# LOCATION: workflows/01_biodiversity_data/03_clean_gbif_fish.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(data.table)
library(dplyr)
library(tidyr)
library(CoordinateCleaner)
library(specleanr)
library(rfishbase)
library(readr)
library(leaflet)
library(htmlwidgets)

# danubeoccurR provides check_species_name() for Step 6 (GBIF taxonomy
# validation). Install from GitHub only if missing, so the script does not
# hit GitHub/token on every run.
if (!requireNamespace("danubeoccurR", quietly = TRUE)) {
  pak::pak("ytorres-cambas/danubeoccurR")
}
library(danubeoccurR)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("points_original/fish", recursive = TRUE, showWarnings = FALSE)
dir.create("points_original/maps", recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/fish",  recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/maps",  recursive = TRUE, showWarnings = FALSE)

# ============================================================
# STEP 1: Read and fix malformed CSV
# ============================================================

message("\n=== Step 1: Reading and Fixing Raw GBIF Data ===")

file_raw <- file.path(BASE_DIR, "points_original/fish/fish_data_gbif.csv")

# fread handles large files; fill = Inf tolerates ragged rows so the repair
# steps below can detect and drop them rather than failing on read.
gbif <- fread(file_raw,
              quote = "\"",
              fill  = Inf)

message(sprintf("Initial read: %d rows, %d columns", nrow(gbif), ncol(gbif)))

# --- 1a. Remove rows with overflow (V*) columns from malformed records ---
# Ragged rows push extra fields into auto-named V1, V2... columns. A row is
# malformed if any V column is non-empty; keep only rows where all are empty.
v_cols <- grep("^V[0-9]+$", names(gbif), value = TRUE)

if (length(v_cols) > 0) {
  message(sprintf("Found %d overflow columns (V*) - removing malformed rows", length(v_cols)))
  gbif_tmp <- gbif[rowSums(gbif[, ..v_cols] != "", na.rm = TRUE) == 0]
  gbif_tmp <- gbif_tmp[, !..v_cols]   # drop the now-empty V columns
  message(sprintf("Removed %d malformed rows", nrow(gbif) - nrow(gbif_tmp)))
} else {
  gbif_tmp <- gbif
  message("No overflow columns found")
}

# --- 1b. Drop unrealistic longitude values (field-shift artifacts) ---
if ("decimalLongitude" %in% names(gbif_tmp)) {
  bad_lons <- gbif_tmp %>%
    filter(!is.na(decimalLongitude), decimalLongitude > 1000)
  if (nrow(bad_lons) > 0) {
    message(sprintf("Found %d rows with invalid longitude (>1000)", nrow(bad_lons)))
    gbif_tmp <- gbif_tmp %>% filter(decimalLongitude < 1000 | is.na(decimalLongitude))
  }
}

# --- 1c. Round-trip through readr to catch any remaining parse problems ---
# readr reports row-level parsing issues that fread silently tolerates, so
# we write out, re-read, and drop flagged rows.
temp_file <- tempfile(fileext = ".csv")
fwrite(gbif_tmp, temp_file)
message(sprintf("Saved %d rows to temp file for validation", nrow(gbif_tmp)))

gbif_tmp <- read_csv(temp_file, show_col_types = FALSE)
prob_rows <- problems(gbif_tmp)$row

if (length(prob_rows) > 0) {
  message(sprintf("Found %d rows with parsing issues - removing", length(prob_rows)))
  gbif_clean <- gbif_tmp[-prob_rows, ]
} else {
  message("No parsing issues found")
  gbif_clean <- gbif_tmp
}
unlink(temp_file)

fwrite(gbif_clean, "points_original/fish/fish_data_gbif_fixed.csv")
message(sprintf("\nCSV fixing complete: %d clean rows retained", nrow(gbif_clean)))

# ============================================================
# STEP 2: Initial exploration
# ============================================================

message("\n=== Step 2: Initial Exploration ===")

gbif_raw <- gbif_clean

message(sprintf("Dataset dimensions: %d rows x %d columns", nrow(gbif_raw), ncol(gbif_raw)))
message("\nColumn names:")
print(names(gbif_raw))

# Flag exact duplicate coordinate-species-year-dataset combinations.
duplicates <- gbif_raw %>%
  count(decimalLongitude, decimalLatitude, speciesKey, datasetKey, year) %>%
  filter(n > 1)

if (nrow(duplicates) > 0) {
  message(sprintf("\nFound %d duplicate coordinate-species-year combinations", nrow(duplicates)))
} else {
  message("\nNo exact duplicates found")
}

# ============================================================
# STEP 3: Coordinate type conversion
# ============================================================

message("\n=== Step 3: Converting Coordinates to Numeric ===")

# Restrict to Greek records first (country code), then coerce coordinates.
gbif_raw <- gbif_raw %>% filter(countryCode == "GR")

gbif_raw <- gbif_raw %>%
  mutate(
    decimalLatitude  = as.numeric(decimalLatitude),
    decimalLongitude = as.numeric(decimalLongitude)
  )

# Report coordinate ranges as a sanity check (Greece: lon ~19-28, lat ~34-42).
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

# ============================================================
# STEP 4: Metadata-based filtering
# ============================================================

message("\n=== Step 4: Metadata Filtering ===")

n_before <- nrow(gbif_raw)

# Keep records with a year, fine coordinate precision, and low spatial
# uncertainty. The named uncertainty values (301, 3036, 999, 9999) are known
# GBIF placeholder/default codes, not real measurements, so they are dropped.
gbif_metadata_filtered <- gbif_raw %>%
  filter(!is.na(year)) %>%
  filter(coordinatePrecision < 0.01 | is.na(coordinatePrecision)) %>%
  filter(coordinateUncertaintyInMeters <= 1000 | is.na(coordinateUncertaintyInMeters)) %>%
  filter(!coordinateUncertaintyInMeters %in% c(301, 3036, 999, 9999))

n_after <- nrow(gbif_metadata_filtered)
message(sprintf("Removed %d rows (%.1f%%) based on metadata filters",
                n_before - n_after,
                100 * (n_before - n_after) / n_before))

# ============================================================
# STEP 5: Spatial filters (CoordinateCleaner)
# ============================================================

message("\n=== Step 5: Spatial Filtering with CoordinateCleaner ===")

n_before <- nrow(gbif_metadata_filtered)

# Remove records at country centroids, capitals, and biodiversity
# institutions (common geocoding artifacts), then de-duplicate.
gbif_spatial_cleaned <- gbif_metadata_filtered %>%
  cc_cen(buffer = 10, verbose = TRUE) %>%
  cc_cap(buffer = 10, verbose = TRUE) %>%
  cc_inst(buffer = 10, verbose = TRUE) %>%
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

# ============================================================
# STEP 6: Taxonomic validation
# ============================================================

message("\n=== Step 6: Taxonomic Name Checking ===")
message("This may take several minutes...")

# manual = TRUE triggers interactive review for ambiguous matches, so this
# step is run interactively, not in batch.
gbif_taxa_checked <- check_species_name(
  data               = gbif_spatial_cleaned,
  col_species_name   = "species",
  target_accuracy    = 90,
  accuracy_decrement = NULL,
  verbose            = TRUE,
  manual             = TRUE
)

message(sprintf("Taxonomic validation complete: %d species validated",
                length(unique(gbif_taxa_checked$species))))

# ============================================================
# STEP 7: Manual curation
# ============================================================

message("\n=== Step 7: Manual Curation ===")

# Hook for removing problematic rows identified during manual review.
# Example: gbif_taxa_curated <- gbif_taxa_checked[-c(5836:5840), ]
gbif_taxa_curated <- gbif_taxa_checked

message(sprintf("After manual curation: %d rows", nrow(gbif_taxa_curated)))

# ============================================================
# STEP 8: Final cleaning and column selection
# ============================================================

message("\n=== Step 8: Final Cleaning ===")

# Empty strings -> NA, keep the columns needed downstream, require
# coordinates, and restrict to post-1980 records (older records are sparse
# and less reliable for SDM).
gbif_cleaned <- gbif_taxa_curated %>%
  mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
  select(
    gbifID, datasetKey, class, order, family, genus, species,
    year, month, day, decimalLongitude, decimalLatitude
  ) %>%
  filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) %>%
  filter(year > 1980)

# --- 8b. Filter to target basin ---
message("\n=== Step 8b: Filtering to target basin ===")

# BASIN_ID is derived once in 01_clean_hcmr_fish.R and shared via config.
study_params <- fread("config/study_area_params.csv")
BASIN_ID <- study_params[param == "BASIN_ID", as.integer(value)]
message("  Target basin ID: ", BASIN_ID)

# Assign basin IDs by unique location (gbifID as site id) and keep only the
# records that fall inside the study basin.
gbif_unique_locs <- gbif_cleaned %>%
  distinct(gbifID, decimalLongitude, decimalLatitude)
message("  Unique locations to query: ", nrow(gbif_unique_locs))

basin_ids_gbif <- api_get_ids(
  points          = gbif_unique_locs,
  colname_lon     = "decimalLongitude",
  colname_lat     = "decimalLatitude",
  colname_site_id = "gbifID",
  mode            = "local"
)

gbif_cleaned <- gbif_cleaned %>%
  left_join(basin_ids_gbif) %>%
  filter(basin_id == BASIN_ID) %>%
  select(-subc_id, -basin_id, -reg_id)

message("  Records in target basin: ", nrow(gbif_cleaned))
message("  Species in target basin: ", n_distinct(gbif_cleaned$species))

# ============================================================
# STEP 9: Visualise
# ============================================================

message("\n=== Step 9: Creating Visualization ===")

gbif_map <- leaflet(gbif_cleaned) %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addCircleMarkers(
    lng         = ~decimalLongitude,
    lat         = ~decimalLatitude,
    radius      = 3,
    color       = "blue",
    fillColor   = "blue",
    fillOpacity = 0.5,
    stroke      = FALSE,
    popup       = ~paste0(
      "<b>Species:</b> ", species, "<br>",
      "<b>Family:</b> ", family, "<br>",
      "<b>Year:</b> ", year, "<br>",
      "<b>GBIF ID:</b> ", gbifID
    )
  ) %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Satellite"),
    options    = layersControlOptions(collapsed = FALSE)
  )

saveWidget(gbif_map, "points_cleaned/maps/gbif_fish_cleaned_overview.html")
message("  Saved: points_cleaned/maps/gbif_fish_cleaned_overview.html")

# ============================================================
# STEP 10: Save cleaned data
# ============================================================

message("\n=== Step 10: Saving Cleaned Data ===")

fwrite(gbif_cleaned, "points_cleaned/fish/fish_gbif_clean.csv")
message("  Saved: points_cleaned/fish/fish_gbif_clean.csv")

# Unique locations for snapping (one row per coordinate, not per occurrence).
gbif_cleaned_to_snap <- gbif_cleaned %>%
  distinct(decimalLongitude, decimalLatitude, .keep_all = TRUE)

fwrite(gbif_cleaned_to_snap, "points_cleaned/fish/fish_gbif_clean_to_snap.csv")
message("  Saved: points_cleaned/fish/fish_gbif_clean_to_snap.csv")

# ============================================================
# SUMMARY
# ============================================================

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
message("  - points_original/fish/fish_data_gbif_fixed.csv")
message("  - points_cleaned/fish/fish_gbif_clean.csv")
message("  - points_cleaned/fish/fish_gbif_clean_to_snap.csv")
message("  - points_cleaned/maps/gbif_fish_cleaned_overview.html")
message("\nNext: 03_snapping/01_snap_all_data.R")
