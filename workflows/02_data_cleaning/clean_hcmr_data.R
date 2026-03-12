# ============================================================================
# CLEAN HCMR FISH OCCURRENCE DATA
# ============================================================================
# Purpose: Clean and format freshwater fish occurrence records from HCMR
#          Excel file for Greece (includes Barbus abundance data)
# Input: Raw HCMR Excel file with species presence/absence matrix + Barbus sheet
# Output: Single cleaned HCMR fish occurrence file ready for snapping
# ============================================================================

library(data.table)
library(readxl)
library(dplyr)
library(tidyr)
library(leaflet)
library(htmlwidgets)

# Load helper function
source("~/Documents/Postdoc/code/workflow_paper/helpers/save_to_nimbus.R")

# ============================================================================
# SETUP PATHS
# ============================================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("HCMR FISH DATA CLEANING")
message(paste(rep("=", 80), collapse = ""))

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)


# Create directory structure
dir.create("points_cleaned/fish", recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/maps", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# PART 1: PROCESS MAIN DATASET (ALL SPECIES)
# ============================================================================

message("\n=== Step 1: Processing Main Dataset (All Species) ===")

# Clean fish data
sp_raw <- read_xlsx("points_original/fish/Fish distributional & traits data (1).xlsx") %>%
  rename(longitude = Longtitude,
         latitude  = Latitude)

message(sprintf("Loaded %d rows and %d columns", nrow(sp_raw), ncol(sp_raw)))

# Check for inverted lat/lon
sp <- sp_raw %>%
  mutate(
    inverted_lat_lon = latitude >= 19 & latitude <= 29 &
      longitude >= 34 & longitude <= 42
  )

n_inverted <- sum(sp$inverted_lat_lon, na.rm = TRUE)
message(sprintf("Found %d rows with inverted lat/lon", n_inverted))

# Fix inverted lat/lon
sp <- sp %>%
  mutate(
    latitude_fixed = ifelse(inverted_lat_lon, longitude, latitude),
    longitude_fixed = ifelse(inverted_lat_lon, latitude, longitude)
  ) %>%
  select(-latitude, -longitude, -inverted_lat_lon) %>%
  rename(
    latitude = latitude_fixed,
    longitude = longitude_fixed
  )

message("✓ Coordinates fixed")

# keep the dry or fishless locations separated
dry_fishless <- sp %>% filter(!is.na(DRY) | !is.na(FISHLESS))

message(sprintf("Found %d dry or fishless sites", nrow(dry_fishless)))

if (nrow(dry_fishless) > 0) {
  fwrite(dry_fishless, file.path(BASE_DIR, "points_original/fish/fish_hcmr_dry_fishless_sites.csv"))
  message("✓ Saved dry/fishless sites separately")
}

# keep locations with fish presence
sp <- sp %>% filter(is.na(DRY) | is.na(FISHLESS)) %>%
  select(-DRY, -FISHLESS)

message(sprintf("Retained %d sites with fish presence", length(unique(sp$Sites))))

# flip to longer format
sp <- sp %>% pivot_longer(
  cols = -c(Sites, latitude, longitude),  # keep site info
  names_to = "species",
  values_to = "value"
) %>%
  filter(!is.na(value)) %>%
  select(-value)

message(sprintf("Created %d occurrence records", nrow(sp)))

# check species names
unique_species <- sp %>% select(species) %>% arrange(species) %>% distinct() %>% pull()

message(sprintf("Found %d unique species in main dataset", length(unique_species)))

# Remove invalid latitudes
n_before <- nrow(sp)
sp <- sp %>% filter(latitude > 10)
n_after <- nrow(sp)

if (n_before > n_after) {
  message(sprintf("Removed %d records with latitude <= 10", n_before - n_after))
}

# ============================================================================
# PART 2: PROCESS BARBUS ABUNDANCE DATA (SARANTAPOROS)
# ============================================================================

message("\n=== Step 2: Processing Barbus Abundance Data (Sarantaporos) ===")

# Check if Sarantaporos Barbus file exists
barbus_file <- "points_original/fish/Sarantaporos_Barbus.xlsx"

if (file.exists(barbus_file)) {
  message(sprintf("Found Barbus file: %s", barbus_file))

  # Clean fish data (using your exact logic)
  spdata <- read_xlsx(barbus_file) %>%
    rename(Rel_Abund = "Barbus prespensis") %>%
    filter(Rel_Abund > 0) %>%
    mutate(site_id = paste0("Sarant_", row_number())) %>%
    rename(longitude = "Latitude",    # Note: these are swapped in original file
           latitude  = "Longtitude") %>%
    select(site_id, longitude, latitude, Rel_Abund)

  message(sprintf("Loaded %d Barbus records with abundance > 0", nrow(spdata)))

  # Convert to long format matching main dataset structure
  barbus_long <- spdata %>%
    mutate(
      Sites = site_id,
      species = "Barbus_prespensis"
    ) %>%
    select(Sites, latitude, longitude, species)

  message(sprintf("Converted to %d Barbus presence records", nrow(barbus_long)))

  # Spatial validation
  n_before_barbus <- nrow(barbus_long)
  barbus_long <- barbus_long %>%
    filter(
      !is.na(latitude), !is.na(longitude),
      latitude > 10,
      longitude >= 19, longitude <= 29,
      latitude >= 34, latitude <= 42
    )
  n_after_barbus <- nrow(barbus_long)

  if (n_before_barbus > n_after_barbus) {
    message(sprintf("Removed %d Barbus records with invalid coordinates",
                    n_before_barbus - n_after_barbus))
  }

  # Save standalone Barbus file (for reference)
  fwrite(spdata, file.path(BASE_DIR, "points_cleaned/fish/spdata_barbus_vjosa_clean.csv"))
  message("✓ Saved standalone Barbus file: spdata_barbus_vjosa_clean.csv")

  # ============================================================================
  # COMBINE MAIN + BARBUS
  # ============================================================================

  message("\n=== Step 3: Combining Main and Barbus Datasets ===")
  sp <- sp %>% mutate(latitude= as.numeric(latitude),
                      longitude = as.numeric(longitude))
  sp_combined <- bind_rows(sp, barbus_long)

  message(sprintf("Combined dataset: %d total records", nrow(sp_combined)))
  message(sprintf("  - Main dataset: %d records", nrow(sp)))
  message(sprintf("  - Barbus data: %d records", nrow(barbus_long)))

} else {
  message(sprintf("⚠️  Barbus file not found: %s", barbus_file))
  message("Using only main dataset")
  sp_combined <- sp
}

# ============================================================================
# FINAL STEPS: SAVE AND VISUALIZE
# ============================================================================

message("\n=== Step 4: Saving Combined Dataset ===")

# check final species names
unique_species_final <- sp_combined %>%
  select(species) %>%
  arrange(species) %>%
  distinct() %>%
  pull()

message(sprintf("Final dataset contains %d unique species:", length(unique_species_final)))
for (sp_name in unique_species_final) {
  message(sprintf("  - %s", sp_name))
}

# Write clean version
fwrite(sp_combined, file.path(BASE_DIR, "points_cleaned/fish/fish_greece_hcmr.csv"))
message("✓ Saved: points_cleaned/fish/fish_greece_hcmr.csv")

# Re-read to verify
sp_combined <- fread(file.path(BASE_DIR, "points_cleaned/fish/fish_greece_hcmr.csv"))
message(sprintf("Verified: %d rows read back", nrow(sp_combined)))

# Get unique coordinates to snap
sp_to_snap <- sp_combined %>% distinct(Sites, longitude, latitude)

message(sprintf("Unique sites to snap: %d", nrow(sp_to_snap)))

# write points to snap
fwrite(sp_to_snap, file.path(BASE_DIR, "points_cleaned/fish/fish_points_to_snap_hcmr.csv"))
message("✓ Saved: points_cleaned/fish/fish_points_to_snap_hcmr.csv")

# ============================================================================
# VISUALIZATION
# ============================================================================

message("\n=== Step 5: Creating Visualization ===")

# plot coordinates to check
hcmr_map <- leaflet(sp_combined) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~as.numeric(longitude),
    lat = ~as.numeric(latitude),
    popup = ~paste0("<b>Site:</b> ", Sites, "<br><b>Species:</b> ", species),
    radius = 4,
    color = "red",
    fillOpacity = 0.6
  )

# Save map
saveWidget(hcmr_map,
           file.path(BASE_DIR, "points_cleaned/maps/hcmr_fish_overview.html"),
           selfcontained = TRUE)
message("✓ Created interactive map")

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("HCMR DATA CLEANING COMPLETE")
message(paste(rep("=", 80), collapse = ""))

message("\nSummary Statistics:")
message(sprintf("  Total occurrence records: %d", nrow(sp_combined)))
message(sprintf("  Unique sites: %d", nrow(sp_to_snap)))
message(sprintf("  Unique species: %d", length(unique_species_final)))
message(sprintf("  Dry/fishless sites excluded: %d", nrow(dry_fishless)))

# Species frequency
species_counts <- sp_combined %>% count(species) %>% arrange(desc(n))
message("\nMost common species:")
for (i in 1:min(10, nrow(species_counts))) {
  message(sprintf("  %d. %s: %d occurrences",
                  i,
                  species_counts$species[i],
                  species_counts$n[i]))
}

# Sites summary
sites_summary <- sp_combined %>%
  count(Sites) %>%
  summarise(
    mean_species = mean(n),
    median_species = median(n),
    max_species = max(n)
  )

message("\nSpecies per site:")
message(sprintf("  Mean: %.1f", sites_summary$mean_species))
message(sprintf("  Median: %.0f", sites_summary$median_species))
message(sprintf("  Maximum: %d", sites_summary$max_species))

# Spatial extent
message("\nSpatial Coverage:")
message(sprintf("  Longitude: %.4f to %.4f",
                min(sp_combined$longitude), max(sp_combined$longitude)))
message(sprintf("  Latitude: %.4f to %.4f",
                min(sp_combined$latitude), max(sp_combined$latitude)))

message("\nFiles Created:")
message("  - points_cleaned/fish/fish_greece_hcmr.csv (all species including Barbus)")
message("  - points_cleaned/fish/fish_points_to_snap_hcmr.csv (unique sites)")
message("  - points_cleaned/fish/spdata_barbus_clean.csv (standalone Barbus reference)")
message("  - points_cleaned/maps/hcmr_fish_overview.html")
if (nrow(dry_fishless) > 0) {
  message("  - points_original/fish/fish_hcmr_dry_fishless_sites.csv")
}

message("\n✓ Ready for snapping!")
