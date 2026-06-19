#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_clean_hcmr_fish.R   (Module 1 -- Biodiversity Data)
#
# Clean and format freshwater fish occurrence records from the HCMR Excel
# file. Combines the Greece-wide HCMR dataset with the Sarantaporos basin
# field dataset, assigns basin IDs via api_get_ids(), derives the study
# BASIN_ID from a known anchor point, and filters all occurrences to that
# basin. The basin ID is saved to config for every downstream script.
#
# Extent: full Vjosa/Aoos basin (not Sarantaporos-only). SDM training in
# Module 7 needs the wider basin occurrence set; the Sarantaporos subbasin
# trim happens later, in Module 3 (03_extract_subbasin.R).
#
# Workflow:
#   1.  Load and clean main HCMR dataset (Greece-wide)
#   2.  Load and clean basin field dataset (Sarantaporos.xlsx)
#   3.  Combine datasets
#   4.  Assign basin IDs via api_get_ids()
#   5.  Derive BASIN_ID from anchor point and filter to target basin
#   5b. Remove invalid/undetermined taxa
#   5c. Validate taxonomic names against GBIF backbone (interactive)
#   6.  Save outputs + basin ID config
#   7.  Visualise
#
# INPUT:
#   - points_original/fish/Fish distributional & traits data (1).xlsx
#   - points_original/fish/Sarantaporos.xlsx            (basin field data)
#   - points_original/fish/species_list_sarantaporos.txt
#
# OUTPUT:
#   - points_cleaned/fish/fish_basin_hcmr.csv           (all species, basin only)
#   - points_cleaned/fish/fish_points_to_snap_hcmr.csv  (unique sites for snapping)
#   - points_cleaned/fish/fish_basin_field_data_clean.csv
#   - config/study_area_params.csv                      (BASIN_ID for downstream)
#   - points_cleaned/maps/hcmr_fish_basin_overview.html
#
# LOCATION: workflows/01_biodiversity_data/01_clean_hcmr_fish.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(data.table)
library(readxl)
library(dplyr)
library(tidyr)
library(leaflet)
library(htmlwidgets)

# danubeoccurR provides check_species_name() for Step 5c (taxonomy
# validation). Install from GitHub only if missing
if (!requireNamespace("danubeoccurR", quietly = TRUE)) {
  pak::pak("ytorres-cambas/danubeoccurR")
}
library(danubeoccurR)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("points_cleaned/fish", recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/maps", recursive = TRUE, showWarnings = FALSE)
dir.create("config",              recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Known anchor point confirmed to be within the target basin (Aoos/Vjosa).
# Used to derive BASIN_ID from the api_get_ids() output: whatever basin this
# point falls in is the study basin, so we never hard-code the basin number.
ANCHOR_SITE_ID  <- "40POROS_DW"
ANCHOR_LON      <- 20.72211
ANCHOR_LAT      <- 40.11128

message("\n", paste(rep("=", 80), collapse = ""))
message("HCMR FISH DATA CLEANING")
message(paste(rep("=", 80), collapse = ""))

# ============================================================
# STEP 1: Load and clean main HCMR dataset (Greece-wide)
# ============================================================

message("\n=== Step 1: Processing main HCMR dataset ===")

sp_raw <- read_xlsx("points_original/fish/Fish distributional & traits data (1).xlsx") %>%
  rename(longitude = Longtitude,
         latitude  = Latitude)

message("  Loaded: ", nrow(sp_raw), " rows, ", ncol(sp_raw), " columns")

# Some rows have lat/lon swapped: detect by checking whether "latitude"
# actually falls in the longitude range for Greece, then swap those rows.
sp <- sp_raw %>%
  mutate(
    inverted_lat_lon = latitude >= 19 & latitude <= 29 &
      longitude >= 34 & longitude <= 42
  )

n_inverted <- sum(sp$inverted_lat_lon, na.rm = TRUE)
message("  Inverted lat/lon rows: ", n_inverted)

sp <- sp %>%
  mutate(
    latitude_fixed  = ifelse(inverted_lat_lon, longitude, latitude),
    longitude_fixed = ifelse(inverted_lat_lon, latitude, longitude)
  ) %>%
  select(-latitude, -longitude, -inverted_lat_lon) %>%
  rename(latitude = latitude_fixed, longitude = longitude_fixed)

# Separate dry/fishless sites: these are true-absence survey sites, not
# occurrence records, so they must not enter the presence dataset.
dry_fishless <- sp %>% filter(!is.na(DRY) | !is.na(FISHLESS))
message("  Dry/fishless sites: ", nrow(dry_fishless))

if (nrow(dry_fishless) > 0) {
  fwrite(dry_fishless,
         "points_original/fish/fish_hcmr_dry_fishless_sites.csv")
}

sp <- sp %>%
  filter(is.na(DRY) | is.na(FISHLESS)) %>%
  select(-DRY, -FISHLESS)

message("  Sites with fish presence: ", length(unique(sp$Sites)))

# Pivot wide species columns to long format; keep only presences (non-NA).
sp <- sp %>%
  pivot_longer(
    cols      = -c(Sites, latitude, longitude),
    names_to  = "species",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  select(-value)

message("  Occurrence records: ", nrow(sp))

# Drop records with clearly invalid latitude (placeholder/garbage values).
n_before <- nrow(sp)
sp <- sp %>% filter(latitude > 10)
message("  Removed invalid latitude records: ", n_before - nrow(sp))

# ============================================================
# STEP 2: Load and clean basin field dataset
# ============================================================

message("\n=== Step 2: Processing basin field dataset ===")

basin_file <- "points_original/fish/Sarantaporos.xlsx"

if (file.exists(basin_file)) {

  # Lat/lon column headers are swapped in the source file, so rename them
  # crossed here to read the coordinates correctly.
  raw <- read_xlsx(basin_file) %>%
    rename(
      longitude = "Latitude",
      latitude  = "Longtitude"
    ) %>%
    mutate(site_id = paste0("Basin_", row_number()))

  coord_cols   <- c("site_id", "longitude", "latitude")
  species_cols <- setdiff(names(raw), coord_cols)[-1]

  message("  Species columns found: ", length(species_cols))

  # Pivot to long format, keep only presences (abundance > 0).
  basin_long <- raw %>%
    select(all_of(c(coord_cols, species_cols))) %>%
    pivot_longer(
      cols      = all_of(species_cols),
      names_to  = "species",
      values_to = "abundance"
    ) %>%
    filter(!is.na(abundance), abundance > 0) %>%
    mutate(
      Sites   = site_id,
      species = gsub(" ", "_", species)
    ) %>%
    select(Sites, latitude, longitude, species)

  message("  Presence records: ", nrow(basin_long),
          " (", n_distinct(basin_long$species), " species)")

  # Coordinate validation: keep only points inside the Greece bounding box.
  n_before <- nrow(basin_long)
  basin_long <- basin_long %>%
    filter(
      !is.na(latitude), !is.na(longitude),
      longitude >= 19, longitude <= 29,
      latitude  >= 37, latitude  <= 42
    )
  message("  Removed invalid coordinate records: ", n_before - nrow(basin_long))

  fwrite(basin_long, "points_cleaned/fish/fish_basin_field_data_clean.csv")
  message("  Saved: points_cleaned/fish/fish_basin_field_data_clean.csv")

} else {
  message("  Basin field data file not found: ", basin_file)
  message("  Using main dataset only")
  basin_long <- NULL
}

# ============================================================
# STEP 3: Combine datasets
# ============================================================

message("\n=== Step 3: Combining datasets ===")

sp <- sp %>%
  mutate(latitude  = as.numeric(latitude),
         longitude = as.numeric(longitude))

if (!is.null(basin_long)) {
  sp_combined <- bind_rows(sp, basin_long)
  message("  Main dataset records:        ", nrow(sp))
  message("  Basin field data records:    ", nrow(basin_long))
} else {
  sp_combined <- sp
}

message("  Combined total records:      ", nrow(sp_combined))
message("  Unique species:              ", n_distinct(sp_combined$species))
message("  Unique sites:                ", n_distinct(sp_combined$Sites))

# ============================================================
# STEP 4: Assign basin IDs via api_get_ids()
# ============================================================

message("\n=== Step 4: Assigning basin IDs ===")

# One API call per unique site coordinate (not per occurrence record).
sites_to_query <- sp_combined %>%
  distinct(Sites, longitude, latitude) %>%
  rename(site_id = Sites)

# Add the anchor point so BASIN_ID can always be derived in Step 5, even if
# the anchor site is not itself present in the HCMR data.
anchor_row <- data.frame(
  site_id   = ANCHOR_SITE_ID,
  longitude = ANCHOR_LON,
  latitude  = ANCHOR_LAT
)
sites_to_query <- bind_rows(sites_to_query, anchor_row) %>%
  distinct(site_id, .keep_all = TRUE)

message("  Unique sites to query: ", nrow(sites_to_query))

fwrite(sites_to_query, "points_cleaned/fish/sites_for_basin_id_query.csv")

basin_ids <- api_get_ids(
  points          = sites_to_query,
  colname_lon     = "longitude",
  colname_lat     = "latitude",
  colname_site_id = "site_id",
  mode            = "local"
)

# Join basin IDs back to every occurrence record.
sp_with_basins <- sp_combined %>%
  left_join(basin_ids %>% rename(Sites = site_id))

cat("  Sites missing basin_id:", sum(is.na(sp_with_basins$basin_id)), "\n")

# ============================================================
# STEP 5: Derive BASIN_ID from anchor point + filter
# ============================================================

message("\n=== Step 5: Deriving basin ID and filtering ===")

# The study basin is whichever basin the anchor point falls in. Deriving it
# this way (rather than hard-coding) keeps the script reusable if the
# hydrography version or basin numbering changes.
BASIN_ID <- basin_ids %>%
  filter(site_id == ANCHOR_SITE_ID) %>%
  pull(basin_id)

if (length(BASIN_ID) == 0 || is.na(BASIN_ID)) {
  stop("Could not derive BASIN_ID from anchor point ", ANCHOR_SITE_ID,
       ". Check that the anchor coordinates are correct.")
}

message("  BASIN_ID derived from anchor point (", ANCHOR_SITE_ID, "): ", BASIN_ID)

# Persist BASIN_ID so all downstream scripts read the same study area.
fwrite(
  data.table(param = "BASIN_ID", value = BASIN_ID),
  "config/study_area_params.csv"
)
message("  Saved: config/study_area_params.csv")

# Keep only occurrences inside the target basin.
n_before <- nrow(sp_with_basins)
sp_basin <- sp_with_basins %>%
  filter(basin_id == BASIN_ID)

message("  Records before basin filter: ", n_before)
message("  Records in target basin:     ", nrow(sp_basin))
message("  Records excluded:            ", n_before - nrow(sp_basin))
message("  Species in basin:            ", n_distinct(sp_basin$species))
message("  Sites in basin:              ", n_distinct(sp_basin$Sites))

# ============================================================
# STEP 5b: Remove invalid taxa
# ============================================================

message("\n=== Step 5b: Removing invalid taxa ===")

# Squalius_sp._Aoos is an undetermined species-level identification and is
# not suitable for species distribution modelling, so drop it.
invalid_taxa <- c("Squalius_sp._Aoos")

n_before <- nrow(sp_basin)
sp_basin <- sp_basin %>%
  filter(!species %in% invalid_taxa)

message("  Removed taxa: ", paste(invalid_taxa, collapse = ", "))
message("  Records removed: ", n_before - nrow(sp_basin))
message("  Records remaining: ", nrow(sp_basin))

# ============================================================
# STEP 5c: Taxonomic name validation
# ============================================================
# danubeoccurR::check_species_name() validates names against the GBIF
# backbone taxonomy. manual = TRUE triggers interactive review for ambiguous
# matches, so this step is run interactively, not in batch.

message("\n=== Step 5c: Taxonomic name validation ===")
message("  This may take several minutes...")

sp_basin_validated <- check_species_name(
  data               = sp_basin %>% mutate(species = gsub("_", " ", species)),
  col_species_name   = "species",
  target_accuracy    = 90,
  accuracy_decrement = NULL,
  verbose            = TRUE,
  manual             = TRUE
)

# During manual review we changed "Chondrostoma ohridana" to the updated
# accepted name "Chondrostoma ohridanum".
message("  Validated species: ", n_distinct(sp_basin_validated$species))

# Adopt the validated names and drop the helper columns added by
# check_species_name(). Add manual removals here after reviewing output.
sp_basin <- sp_basin_validated %>%
  mutate(species = speciescheck) %>%
  select(-manually_updated, -speciescheck, -subc_id, -basin_id, -reg_id)

message("  Records after taxonomic validation: ", nrow(sp_basin))

# ============================================================
# STEP 6: Save outputs
# ============================================================

message("\n=== Step 6: Saving outputs ===")

# All species, basin only.
fwrite(sp_basin, "points_cleaned/fish/fish_basin_hcmr.csv")
message("  Saved: points_cleaned/fish/fish_basin_hcmr.csv")

# Unique sites for the snapping step (one row per site, not per occurrence).
sp_to_snap <- sp_basin %>%
  distinct(Sites, longitude, latitude) %>%
  rename(site_id = Sites)

fwrite(sp_to_snap, "points_cleaned/fish/fish_points_to_snap_hcmr.csv")
message("  Saved: points_cleaned/fish/fish_points_to_snap_hcmr.csv")
message("  Unique sites to snap: ", nrow(sp_to_snap))

# ============================================================
# STEP 7: Visualise
# ============================================================

message("\n=== Step 7: Creating visualisation ===")

hcmr_map <- leaflet(sp_basin) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lng         = ~as.numeric(longitude),
    lat         = ~as.numeric(latitude),
    popup       = ~paste0("<b>Site:</b> ", Sites,
                          "<br><b>Species:</b> ", species,
                          "<br><b>Basin ID:</b> ", basin_id),
    radius      = 4,
    color       = "#2166ac",
    fillOpacity = 0.7,
    stroke      = FALSE
  ) %>%
  addMarkers(
    lng   = ANCHOR_LON,
    lat   = ANCHOR_LAT,
    label = paste0("Anchor point: ", ANCHOR_SITE_ID, " (basin ", BASIN_ID, ")")
  )

saveWidget(
  hcmr_map,
  "points_cleaned/maps/hcmr_fish_basin_overview.html",
  selfcontained = TRUE
)
message("  Saved: points_cleaned/maps/hcmr_fish_basin_overview.html")

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("HCMR DATA CLEANING COMPLETE")
message(paste(rep("=", 80), collapse = ""))
message("\nTarget basin ID: ", BASIN_ID)
message("\nRecords:")
message("  Total occurrence records: ", nrow(sp_basin))
message("  Unique sites:             ", nrow(sp_to_snap))
if (!is.null(basin_long)) {
  message("  Of which basin field data sites: ",
          n_distinct(sp_basin$Sites[grepl("^Basin_", sp_basin$Sites)]))
}
if (nrow(dry_fishless) > 0) {
  message("  Dry/fishless sites excluded: ", nrow(dry_fishless))
}

message("\nSpecies in basin (", n_distinct(sp_basin$species), " total):")
species_counts <- sp_basin %>% count(species) %>% arrange(desc(n))
for (i in seq_len(nrow(species_counts))) {
  message("  ", i, ". ", species_counts$species[i],
          ": ", species_counts$n[i], " occurrences")
}

message("\nFiles created:")
message("  points_cleaned/fish/fish_basin_hcmr.csv")
message("  points_cleaned/fish/fish_points_to_snap_hcmr.csv")
message("  points_cleaned/fish/fish_basin_field_data_clean.csv")
message("  config/study_area_params.csv  <- BASIN_ID = ", BASIN_ID)
message("  points_cleaned/maps/hcmr_fish_basin_overview.html")
message("\nNext: snapping script")
