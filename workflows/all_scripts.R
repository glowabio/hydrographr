#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_clean_hcmr_fish.R
#
# Clean and format freshwater fish occurrence records from HCMR Excel file.
# Derives the study basin ID from a known anchor point and filters all
# occurrences to the target basin. Saves basin ID for downstream scripts.
#
# Workflow:
#   1. Load and clean main HCMR dataset (Greece-wide)
#   2. Load and clean basin field dataset
#   3. Combine datasets
#   4. Assign basin IDs via api_get_ids()
#   5. Derive BASIN_ID from known anchor point and filter to target basin
#   6. Save outputs + basin ID config
#   7. Visualise
#
# Input:
#   - points_original/fish/Fish distributional & traits data (1).xlsx
#   - points_original/fish/Sarantaporos.xlsx  (basin field data)
#   - points_original/fish/species_list_sarantaporos.txt
#
# Output:
#   - points_cleaned/fish/fish_basin_hcmr.csv          (all species, basin only)
#   - points_cleaned/fish/fish_points_to_snap_hcmr.csv (unique sites for snapping)
#   - points_cleaned/fish/fish_hcmr_dry_fishless_sites.csv
#   - config/study_area_params.csv                     (BASIN_ID for downstream)
#   - points_cleaned/maps/hcmr_fish_overview.html
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

select <- dplyr::select

source("~/Documents/Postdoc/code/workflow_paper/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

# Known anchor point confirmed to be within the target basin (Aoos/Vjosa)
# Used to derive BASIN_ID from api_get_ids() output
ANCHOR_SITE_ID  <- "40POROS_DW"
ANCHOR_LON      <- 20.72211
ANCHOR_LAT      <- 40.11128

# ============================================================
# SETUP
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("HCMR FISH DATA CLEANING")
message(paste(rep("=", 80), collapse = ""))

dir.create("points_cleaned/fish", recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/maps", recursive = TRUE, showWarnings = FALSE)
dir.create("config",              recursive = TRUE, showWarnings = FALSE)

# ============================================================
# STEP 1: Load and clean main HCMR dataset (Greece-wide)
# ============================================================

message("\n=== Step 1: Processing main HCMR dataset ===")

sp_raw <- read_xlsx("points_original/fish/Fish distributional & traits data (1).xlsx") %>%
  rename(longitude = Longtitude,
         latitude  = Latitude)

message("  Loaded: ", nrow(sp_raw), " rows, ", ncol(sp_raw), " columns")

# Fix inverted lat/lon
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

# Separate dry/fishless sites
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

# Pivot to long format — keep only presences
sp <- sp %>%
  pivot_longer(
    cols      = -c(Sites, latitude, longitude),
    names_to  = "species",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  select(-value)

message("  Occurrence records: ", nrow(sp))

# Remove invalid latitudes
n_before <- nrow(sp)
sp <- sp %>% filter(latitude > 10)
message("  Removed invalid latitude records: ", n_before - nrow(sp))

# ============================================================
# STEP 2: Load and clean basin field dataset
# ============================================================

message("\n=== Step 2: Processing basin field dataset ===")

basin_file <- "points_original/fish/Sarantaporos.xlsx"

if (file.exists(basin_file)) {

  raw <- read_xlsx(basin_file) %>%
    rename(
      longitude = "Latitude",    # swapped in original file
      latitude  = "Longtitude"
    ) %>%
    mutate(site_id = paste0("Basin_", row_number()))

  coord_cols   <- c("site_id", "longitude", "latitude")
  species_cols <- setdiff(names(raw), coord_cols)[-1]

  message("  Species columns found: ", length(species_cols))

  # Pivot to long format, keep only presences
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

  # Coordinate validation
  n_before <- nrow(basin_long)
  basin_long <- basin_long %>%
    filter(
      !is.na(latitude), !is.na(longitude),
      longitude >= 19, longitude <= 29,
      latitude  >= 37, latitude  <= 42
    )
  message("  Removed invalid coordinate records: ", n_before - nrow(basin_long))

  # Save standalone basin field dataset
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

# Get unique site coordinates for API call
sites_to_query <- sp_combined %>%
  distinct(Sites, longitude, latitude) %>%
  rename(site_id = Sites)

# Add anchor point to ensure BASIN_ID can always be derived
# even if anchor site is not in the HCMR data
anchor_row <- data.frame(
  site_id   = ANCHOR_SITE_ID,
  longitude = ANCHOR_LON,
  latitude  = ANCHOR_LAT
)
sites_to_query <- bind_rows(sites_to_query, anchor_row) %>%
  distinct(site_id, .keep_all = TRUE)

message("  Unique sites to query: ", nrow(sites_to_query))

# Write to Nimbus for API call
fwrite(sites_to_query, "points_cleaned/fish/sites_for_basin_id_query.csv")
# sites_csv_url <- "https://nimbus.igb-berlin.de/index.php/s/PLACEHOLDER/download/sites_for_basin_id_query.csv"
# # NOTE: update the Nimbus URL after uploading
# sites_to_query <- fread(sites_csv_url)

basin_ids <- api_get_ids(
  points          = sites_to_query,
  colname_lon     = "longitude",
  colname_lat     = "latitude",
  colname_site_id = "site_id",
  mode            = "local"
)

# Join basin IDs back to combined data
sp_with_basins <- sp_combined %>%
  left_join(basin_ids %>% rename(Sites = site_id))

cat("  Sites missing basin_id:", sum(is.na(sp_with_basins$basin_id)), "\n")

# ============================================================
# STEP 5: Derive BASIN_ID from anchor point + filter
# ============================================================

message("\n=== Step 5: Deriving basin ID and filtering ===")

# Derive BASIN_ID from known anchor point
BASIN_ID <- basin_ids %>%
  filter(site_id == ANCHOR_SITE_ID) %>%
  pull(basin_id)

if (length(BASIN_ID) == 0 || is.na(BASIN_ID)) {
  stop("Could not derive BASIN_ID from anchor point ", ANCHOR_SITE_ID,
       ". Check that the anchor coordinates are correct.")
}

message("  BASIN_ID derived from anchor point (", ANCHOR_SITE_ID, "): ", BASIN_ID)

# Save BASIN_ID to config for all downstream scripts
fwrite(
  data.table(param = "BASIN_ID", value = BASIN_ID),
  "config/study_area_params.csv"
)
message("  Saved: config/study_area_params.csv")

# Filter to target basin only
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

# Remove hybrid/undetermined taxa not suitable for SDM
# Squalius_sp._Aoos: undetermined species-level identification
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
# Uses danubeoccurR::check_species_name() to validate species names
# against GBIF backbone taxonomy. Manual review is triggered for
# ambiguous matches. Run interactively — not suitable for batch execution.

message("\n=== Step 5c: Taxonomic name validation ===")
message("  This may take several minutes...")

# install.packages("danubeoccurR") if not available
library(danubeoccurR)

sp_basin_validated <- check_species_name(
  data             = sp_basin %>% mutate(species = gsub("_", " ", species)),
  col_species_name = "species",
  target_accuracy  = 90,
  accuracy_decrement = NULL,
  verbose          = TRUE,
  manual           = TRUE
)

# We manually changed "Chondrostoma ohridana"
# to the updated name "Chondrostoma ohridanum"

message("  Validated species: ", n_distinct(sp_basin_validated$species))

# Manual curation — add removals here after reviewing check_species_name output
# Example: sp_basin_validated <- sp_basin_validated %>% filter(species != "...")
# For now retain all validated records
sp_basin <- sp_basin_validated %>% mutate(species = speciescheck) %>%
  select(-manually_updated, -speciescheck, -subc_id, -basin_id, -reg_id)

message("  Records after taxonomic validation: ", nrow(sp_basin))


# ============================================================
# STEP 6: Save outputs
# ============================================================

message("\n=== Step 6: Saving outputs ===")

# All species, basin only
fwrite(sp_basin, "points_cleaned/fish/fish_basin_hcmr.csv")
message("  Saved: points_cleaned/fish/fish_basin_hcmr.csv")

# Unique sites for snapping
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
  # Anchor point marker
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
message("  config/study_area_params.csv  ← BASIN_ID = ", BASIN_ID)
message("  points_cleaned/maps/hcmr_fish_basin_overview.html")
message("\nNext: snapping script")
# GBIF fish downloader for Vjosa basin
library(rgbif)
library(data.table)
library(dplyr)
library(stringr)
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")

# ── Config ───────────────────────────────────────────────────────────────────

setwd(BASE_DIR)
out_dir      <- file.path(BASE_DIR, "points_original/fish")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

GBIF_USER  <- Sys.getenv("GBIF_USER")
GBIF_PWD   <- Sys.getenv("GBIF_PWD")
GBIF_EMAIL <- Sys.getenv("GBIF_EMAIL")

# Got bounding box from https://aqua.igb-berlin.de/upstream-dev/
BASIN_BBOX <- "POLYGON((19.311 39.783, 19.311 40.686, 21.23 40.686, 21.23 39.783, 19.311 39.783))"
MODE       <- "species"   # "order" | "species"

med_fish_orders <- c(
  "Anguilliformes", "Clupeiformes", "Siluriformes", "Acipenseriformes",
  "Salmoniformes", "Aulopiformes", "Myctophiformes", "Gadiformes",
  "Perciformes", "Scorpaeniformes", "Tetraodontiformes", "Gobiiformes",
  "Atheriniformes", "Beloniformes", "Beryciformes", "Synbranchiformes",
  "Cypriniformes", "Esociformes", "Pleuronectiformes"
)

species_list <- fread("points_cleaned/fish/fish_basin_hcmr.csv") %>%
  pull(species) %>%
  unique()

taxa <- if (MODE == "order") med_fish_orders else species_list
rank <- if (MODE == "order") "ORDER" else "SPECIES"

# ── Download ─────────────────────────────────────────────────────────────────

submitted <- list()

for (taxon in taxa) {
  cat("\n[", taxon, "]\n")

  # Resolve taxon key
  bb <- tryCatch(name_backbone(name = taxon, rank = rank), error = \(e) NULL)
  if (is.null(bb) || !"usageKey" %in% names(bb)) {
    cat("  no usageKey — skipping\n"); next
  }

  # Check occurrence count
  n <- tryCatch(
    occ_search(taxonKey = bb$usageKey, geometry = BASIN_BBOX, limit = 1)$meta$count,
    error = \(e) 0L
  )
  if (!length(n) || n == 0) { cat("  0 occurrences — skipping\n"); next }
  cat("  ", n, "occurrences found\n")

  # Submit (up to 5 retries)
  for (attempt in seq_len(5)) {
    key <- tryCatch(
      occ_download(
        pred("hasGeospatialIssue", FALSE),
        pred("hasCoordinate", TRUE),
        pred("occurrenceStatus", "PRESENT"),
        pred_not(pred_isnull("eventDate")),
        pred_not(pred_in("basisOfRecord", c("FOSSIL_SPECIMEN", "LIVING_SPECIMEN"))),
        pred_in("taxonKey", bb$usageKey),
        pred_within(BASIN_BBOX),
        format = "SIMPLE_CSV",
        user = GBIF_USER, pwd = GBIF_PWD, email = GBIF_EMAIL
      ),
      error = \(e) { message("  submit failed (attempt ", attempt, "): ", e$message); NULL }
    )
    if (!is.null(key)) { submitted[[taxon]] <- key; cat("  submitted:", key, "\n"); break }
    Sys.sleep(120)
  }

  # Throttle: pause every 5 submissions
  if (length(submitted) %% 5 == 0) { cat("  [batch pause 60s]\n"); Sys.sleep(60) }
}

# ── Wait, fetch, and combine ─────────────────────────────────────────────────

rows <- lapply(names(submitted), function(taxon) {
  key      <- submitted[[taxon]]
  zip_path <- file.path(out_dir, paste0(key, ".zip"))

  if (!file.exists(zip_path)) {
    cat("\nwaiting for", taxon, "(", key, ")...\n")
    occ_download_wait(key)
    occ_download_get(key, path = out_dir)
  }

  csv_name <- unzip(zip_path, list = TRUE)$Name
  csv_name <- csv_name[grepl("\\.csv$", csv_name)][1]
  if (is.na(csv_name)) { cat("  no CSV in ZIP for", taxon, "\n"); return(NULL) }

  taxon_dir <- file.path(out_dir, gsub("[^A-Za-z0-9_-]", "_", taxon))
  dir.create(taxon_dir, showWarnings = FALSE, recursive = TRUE)

  dt <- tryCatch(
    fread(unzip(zip_path, files = csv_name, exdir = taxon_dir), fill = TRUE, showProgress = FALSE),
    error = \(e) { message("  read error for ", taxon, ": ", e$message); NULL }
  )
  if (is.null(dt)) return(NULL)

  cat("  ", nrow(dt), "rows —", taxon, "\n")
  dt[, taxon_downloaded := taxon][]
})

combined <- rbindlist(rows, fill = TRUE)
out_csv  <- file.path(out_dir, "fish_data_gbif.csv")
fwrite(combined, out_csv)

# GBIF citation
citation <- occ_download_meta(key)$doi

# write out
write.table(citation, "points_original/fish/gbif_citation.txt")

# ── Summary ──────────────────────────────────────────────────────────────────

message(
  "\n", strrep("=", 60), "\nGBIF DOWNLOAD COMPLETE\n", strrep("=", 60),
  "\n  Mode:            ", MODE,
  "\n  Taxa downloaded: ", uniqueN(combined$taxon_downloaded),
  "\n  Total rows:      ", nrow(combined),
  "\n  Saved to:        ", out_csv,
  "\n\nNext: 02_clean_gbif_data.R"
)
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
# pak::pak("ytorres-cambas/danubeoccurR")
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

file_raw <- file.path(BASE_DIR, "points_original/fish/fish_data_gbif.csv")

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
save_to_nimbus(gbif_clean, "points_original/fish/fish_data_gbif_fixed.csv")

# delete?
fwrite(gbif_clean, "points_original/fish/fish_data_gbif_fixed.csv")

message(sprintf("\n✓ CSV fixing complete: %d clean rows retained", nrow(gbif_clean)))

# ============================================================================
# 2. INITIAL EXPLORATION
# ============================================================================

message("\n=== Step 2: Initial Exploration ===")

gbif_raw <- fread("points_original/fish/fish_data_gbif_fixed.csv")
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
  ) %>%
  filter(year > 1980)


# ============================================================================
# 8b. FILTER TO TARGET BASIN
# ============================================================================

message("\n=== Step 8b: Filtering to target basin ===")

# Load basin ID derived in clean_hcmr_fish.R
study_params <- fread("config/study_area_params.csv")
BASIN_ID <- study_params[param == "BASIN_ID", as.integer(value)]
message("  Target basin ID: ", BASIN_ID)

# Get unique locations for API call
# Use gbifID as site identifier
gbif_unique_locs <- gbif_cleaned %>%
  distinct(gbifID, decimalLongitude, decimalLatitude)

message("  Unique locations to query: ", nrow(gbif_unique_locs))

# Assign basin IDs
basin_ids_gbif <- api_get_ids(
  points          = gbif_unique_locs,
  colname_lon     = "decimalLongitude",
  colname_lat     = "decimalLatitude",
  colname_site_id = "gbifID",
  mode            = "local"
)

# Join back and filter
gbif_cleaned <- gbif_cleaned %>%
  left_join(basin_ids_gbif) %>%
  filter(basin_id == BASIN_ID) %>%
  select(-subc_id, - basin_id, -reg_id)

message("  Records in target basin: ", nrow(gbif_cleaned))
message("  Species in target basin: ", n_distinct(gbif_cleaned$species))


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
saveWidget(gbif_map, "points_cleaned/maps/gbif_fish_cleaned_overview.html")

# ============================================================================
# 10. SAVE CLEANED DATA
# ============================================================================

message("\n=== Step 10: Saving Cleaned Data ===")

# Save to Nimbus
save_to_nimbus(gbif_cleaned, "points_cleaned/fish/fish_gbif_clean.csv")

# Save locally
fwrite(gbif_cleaned, "points_cleaned/fish/fish_gbif_clean.csv")

gbif_cleaned <- fread("points_cleaned/fish/fish_gbif_clean.csv")

# Remove duplicates, keep only coordinates and gbifID to snap
gbif_cleaned_to_snap <- gbif_cleaned %>%
  distinct(decimalLongitude,decimalLatitude, .keep_all = TRUE)

fwrite(gbif_cleaned_to_snap, "points_cleaned/fish/fish_gbif_clean_to_snap.csv")
save_to_nimbus(gbif_cleaned_to_snap, "points_cleaned/fish/fish_gbif_clean_to_snap.csv")


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
message("  - points_original/fish/fish_data_gbif_fixed.csv")
message("  - points_cleaned/fish/fish_gbif_clean.csv")
message("  - points_original/maps/gbif_fish_cleaned_overview.html")
message("\n✓ Ready for snapping!")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 04_clean_dam_data.R   (Module 1 -- Barrier data preparation)
#
# Reproducible cleaning of small-hydropower (SHP) barrier data for the
# Sarantaporos sub-basin, starting from the RAE (RAAEY) licence registers.
#
# PROVENANCE (important for Methods):
#   The barrier data are the RAE small-hydropower registers, downloaded
#   January 2026 as five CSVs, one per licensing stage:
#     - Operational  (V_SDI_R_HYDRO13)  -> existing
#     - Installation (V_SDI_R_HYDRO12)  -> planned
#     - Production    (V_SDI_R_HYDRO11) -> planned
#     - Evaluation    (V_SDI_R_HYDRO7)  -> planned
#     - Rejected      (..._OTHER_VALUES)-> EXCLUDED (will not be built)
#   Each licence ('aa' = RAE plant code) has multiple geometry "parts":
#   Y/L = water-intake / dam locations, Y/S = powerhouse. A single licence
#   can therefore have several points along the river (intakes + station),
#   sometimes kilometres apart, sharing one 'aa' and one capacity (power_mw).
#
# CLASSIFICATION (dam / factory / dry / exclude):
#   Each point was classified by an expert (co-author) into DAM (a real
#   in-stream barrier), FACTORY (powerhouse), DRY (stream verified dry on
#   the ground / satellite) or EXCLUDE. This expert judgement is NOT
#   reproducible from the registers; it is supplied as a documented input
#   table (dams_sarantaporos_table.csv) and joined here BY COORDINATE.
#
# POWER OF MULTI-PART PLANTS:
#   A licence's capacity (power_mw) is the plant total, repeated on every
#   part row. Where one licence contributes several DAM points to the
#   network, its capacity is SPLIT equally across those DAM points so the
#   plant's energy is not counted multiple times in the per-MW dam ranking
#   (Module 9). Connectivity/fragmentation still treats each DAM as a cut.
#
# SCENARIOS:
#   existing = Operational licence dams within Sarantaporos
#   planned  = Installation + Production + Evaluation dams within Sarantaporos
#   (Rejected dams are excluded from both.)
#
# Input:
#   - points_original/dams/V_SDI_R_HYDRO13_Operational_Licence.csv
#   - points_original/dams/V_SDI_R_HYDRO12_Installation_Licence.csv
#   - points_original/dams/V_SDI_R_HYDRO11_Production_Licence.csv
#   - points_original/dams/V_SDI_R_HYDRO7_Evaluation.csv
#   - points_original/dams/dams_sarantaporos_table.csv   (expert classification + site_id)
#
# Note: no basin-polygon filter is applied here because the expert
# classification table is already restricted to the study set. To enforce
# a strict Sarantaporos-only cut, intersect with api_get_upstream_catchment().
#
# Output:
#   - points_cleaned/dams/dams_sarantaporos_clean.csv    (DAMs, existing+planned, with power)
#   - points_cleaned/dams/dams_sarantaporos_clean.gpkg
#   - points_cleaned/dams/dams_classification_full.csv   (all parts + type, audit trail)
#   - points_cleaned/maps/dams_sarantaporos_clean.html
#
# LOCATION: workflows/02_barrier_data/01_clean_dam_data.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(data.table)
library(sf)
library(leaflet)
library(htmlwidgets)

select <- dplyr::select

source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("points_cleaned/dams", recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/maps", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

DAM_DIR <- "points_original/dams"

# RAE register CSVs and the status each implies.
# Rejected is read only to be reported/excluded, not used in scenarios.
RAE_FILES <- tribble(
  ~file,                                          ~stage,         ~status,
  "V_SDI_R_HYDRO13_Operational_Licence.csv",      "operational",  "existing",
  "V_SDI_R_HYDRO12_Installation_Licence.csv",     "installation", "planned",
  "V_SDI_R_HYDRO11_Production_Licence.csv",        "production",  "planned",
  "V_SDI_R_HYDRO7_Evaluation.csv",                "evaluation",   "planned"
)
# Rejected handled separately (reported, excluded)
REJECTED_FILE <- "V_SDI_R_HYDRO_OTHER_VALUES_Rejected.csv"

# expert classification table (co-author): coordinates + type
CLASS_FILE <- file.path(DAM_DIR, "dams_sarantaporos_table.csv")

# only these types are kept as network barriers
DAM_TYPES <- "DAM"

# coordinate rounding for the classification join (~10 m at 4 dp)
JOIN_DP <- 4

# ============================================================
# Helper: first lon/lat from a WKT (MULTI)POINT string
# ============================================================
parse_lonlat <- function(wkt) {
  m <- str_match(wkt, "(-?\\d+\\.\\d+)\\s+(-?\\d+\\.\\d+)")
  data.table(longitude = as.numeric(m[, 2]),
             latitude  = as.numeric(m[, 3]))
}

# ============================================================
# STEP 1: Read & stack the RAE registers (OL + IL + PL + Evaluation)
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("DAM DATA CLEANING (RAE registers -> Sarantaporos barriers)")
message(paste(rep("=", 80), collapse = ""))

message("\n=== Step 1: Reading RAE registers ===")

read_rae <- function(file, stage, status) {
  path <- file.path(DAM_DIR, file)
  if (!file.exists(path)) stop("RAE file not found: ", path)
  d <- fread(path, encoding = "UTF-8")
  xy <- parse_lonlat(d$geometry)
  d <- cbind(d, xy)
  d %>%
    transmute(
      aa, a_m, part, company,
      thesh,
      power_mw    = as.numeric(power_mw),
      max_power   = as.numeric(max_power),
      longitude, latitude,
      stage       = stage,
      status      = status
    )
}

rae <- pmap_dfr(RAE_FILES, function(file, stage, status)
  read_rae(file, stage, status))

message("  RAE rows (OL+IL+PL+Evaluation): ", nrow(rae))
message("  by stage: ",
        paste(names(table(rae$stage)), table(rae$stage), sep = "=", collapse = ", "))

# Some plants are registered in MORE THAN ONE sheet (e.g. a project listed
# under both Production and Evaluation). That produces duplicate points for
# the same physical location (same aa + part + coordinates) with different
# stage/status. Keep ONE row per (aa, part, location), with priority:
#   operational > production > installation > evaluation
# (existing wins; among planned, the more advanced licensing stage wins).
STAGE_PRIORITY <- c(operational = 1, production = 2, installation = 3, evaluation = 4)

n_before <- nrow(rae)
rae <- rae %>%
  mutate(.lonr = round(longitude, JOIN_DP),
         .latr = round(latitude,  JOIN_DP),
         .prio = STAGE_PRIORITY[stage]) %>%
  arrange(.prio) %>%
  distinct(aa, part, .lonr, .latr, .keep_all = TRUE) %>%
  select(-.lonr, -.latr, -.prio)
message("  De-duplicated across sheets: ", n_before, " -> ", nrow(rae),
        " (", n_before - nrow(rae), " duplicate cross-sheet rows removed)")

# Report rejected count (for Methods / transparency), then leave them out
rej <- read_rae(REJECTED_FILE, "rejected", "rejected")
message("  Rejected rows (EXCLUDED from scenarios): ", nrow(rej))

# ============================================================
# STEP 2: Join expert classification (BY COORDINATE)
# ============================================================
# The co-author table classifies each point as DAM / FACTORY / DRY /
# EXCLUDE. We join it to the RAE points on rounded coordinates (the two
# share the same source geometry, so coordinates match to ~10 m).

message("\n=== Step 2: Joining expert classification (by coordinate) ===")

classif <- fread(CLASS_FILE, sep = ";", encoding = "UTF-8") %>%
  transmute(
    site_id  = site_id,                       # keep the existing C- site_id
    type     = toupper(trimws(type)),
    lon_key  = round(as.numeric(longitude), JOIN_DP),
    lat_key  = round(as.numeric(latitude),  JOIN_DP),
    Name_GR
  )

rae <- rae %>%
  mutate(lon_key = round(longitude, JOIN_DP),
         lat_key = round(latitude,  JOIN_DP)) %>%
  left_join(classif, by = c("lon_key", "lat_key"))

n_unclassified <- sum(is.na(rae$type))
message("  RAE points classified: ", sum(!is.na(rae$type)),
        " | unclassified: ", n_unclassified,
        " (unclassified are outside the Sarantaporos study set)")

# audit trail: every part with its classification
fwrite(rae, "points_cleaned/dams/dams_classification_full.csv")

# ============================================================
# STEP 3: Keep DAM points only
# ============================================================

message("\n=== Step 3: Keeping DAM-type points ===")

dams <- rae %>%
  filter(type %in% DAM_TYPES) %>%
  filter(!is.na(longitude), !is.na(latitude))

message("  DAM points (pre-basin-filter): ", nrow(dams),
        " | existing: ", sum(dams$status == "existing"),
        " | planned: ",  sum(dams$status == "planned"))

# ============================================================
# STEP 4: Split plant capacity across co-licence DAM points
# ============================================================
# A licence ('aa') reports its TOTAL capacity on every part. Where one
# licence contributes several DAM points, dividing its power equally
# avoids counting the plant's energy multiple times in the per-MW ranking.
# Connectivity (Module 8/9 boundary) still treats each DAM as a cut.

message("\n=== Step 4: Splitting plant capacity across co-licence DAMs ===")

dams <- dams %>%
  group_by(aa) %>%
  mutate(n_dam_in_licence = n(),
         power_mw_split   = power_mw / n_dam_in_licence) %>%
  ungroup()

multi <- dams %>% filter(n_dam_in_licence > 1) %>% distinct(aa, n_dam_in_licence, power_mw)
if (nrow(multi) > 0) {
  message("  Licences with >1 DAM point (capacity split):")
  for (i in seq_len(nrow(multi)))
    message("    ", multi$aa[i], ": ", multi$power_mw[i], " MW / ",
            multi$n_dam_in_licence[i], " = ",
            round(multi$power_mw[i] / multi$n_dam_in_licence[i], 3), " MW each")
} else {
  message("  (no multi-DAM licences)")
}

# ============================================================
# STEP 5: Final table + outputs
# ============================================================
# (No basin polygon filter: the expert classification table is already
#  restricted to the study set. If a strict Sarantaporos-only cut is
#  wanted later, intersect dams_sf with api_get_upstream_catchment().)

message("\n=== Step 5: Writing outputs ===")

dams_out <- dams %>%
  transmute(
    site_id,
    aa, a_m, part, company, thesh,
    type, status, stage,
    power_mw_licence = power_mw,
    n_dam_in_licence,
    power_mw         = round(power_mw_split, 4),   # split capacity (for ranking)
    longitude, latitude
  )

fwrite(dams_out, "points_cleaned/dams/dams_sarantaporos_clean.csv")
message("  Saved: points_cleaned/dams/dams_sarantaporos_clean.csv  (",
        nrow(dams_out), " DAMs)")

dams_sf <- st_as_sf(dams_out, coords = c("longitude", "latitude"),
                    crs = 4326, remove = FALSE)
st_write(dams_sf %>%
           select(site_id, aa, a_m, part, type, status, power_mw),
         "points_cleaned/dams/dams_sarantaporos_clean.gpkg",
         delete_dsn = TRUE, quiet = TRUE)
message("  Saved: points_cleaned/dams/dams_sarantaporos_clean.gpkg")

# ============================================================
# STEP 6: Map
# ============================================================

message("\n=== Step 6: Map ===")

status_colors <- colorFactor(c("existing" = "darkblue", "planned" = "orange"),
                             domain = c("existing", "planned"))

m <- leaflet(dams_out) %>%
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addCircleMarkers(
    ~longitude, ~latitude,
    color = ~status_colors(status), fillColor = ~status_colors(status),
    radius = 5, fillOpacity = 0.8, stroke = TRUE, weight = 1,
    popup = ~paste0("<b>site_id:</b> ", site_id,
                    "<br><b>aa:</b> ", aa, "<br><b>licence:</b> ", a_m,
                    "<br><b>status:</b> ", status,
                    "<br><b>power (split):</b> ", power_mw, " MW",
                    "<br><b>licence total:</b> ", power_mw_licence, " MW")
  ) %>%
  addLayersControl(baseGroups = c("CartoDB", "Satellite"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright", pal = status_colors, values = ~status,
            title = "Dam status", opacity = 0.8)

saveWidget(m, "points_cleaned/maps/dams_sarantaporos_clean.html", selfcontained = TRUE)
save_to_nimbus(m, "points_cleaned/maps/dams_sarantaporos_clean.html")
message("  Saved: points_cleaned/maps/dams_sarantaporos_clean.html")

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("DAM CLEANING COMPLETE")
message(paste(rep("=", 80), collapse = ""))
message("Source: RAE registers (Jan 2026), stages OL+IL+PL+Evaluation; Rejected excluded.")
message("Sarantaporos DAMs: ", nrow(dams_out),
        " (existing: ", sum(dams_out$status == "existing"),
        ", planned: ",  sum(dams_out$status == "planned"), ")")
message("Multi-DAM licences (capacity split): ", nrow(multi))
message("\nNext: snapping script (api_get_snapped_points_cascade)")
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

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)


# Create snapped points directories
dir.create("points_snapped/fish", recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/dams", recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/maps", recursive = TRUE, showWarnings = FALSE)


# Load cleaned data URLs (adjust these to your actual URLs)
hcmr_url <- "https://nimbus.igb-berlin.de/index.php/s/2Q8AJGHB6GqsD4m/download/fish_points_to_snap_hcmr.csv"
gbif_url <- "https://nimbus.igb-berlin.de/index.php/s/3WnoTAT7J7M46aP/download/fish_gbif_clean_to_snap.csv"


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

hcmr_snap_result <- api_get_snapped_points_cascade(
  data = hcmr_original,
  colname_lon = "longitude",
  colname_lat = "latitude",
  colname_site_id = "site_id",
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
hcmr_snapped_ids <- hcmr_snap_result$site_id
hcmr_original$snapped <- hcmr_original$site_id %in% hcmr_snapped_ids

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
      "Site ID: ", site_id, "<br>",
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
      "Site ID: ", site_id, "<br>",
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
      "Site ID: ", site_id, "<br>",
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

gbif_snap_result <- api_get_snapped_points_cascade(
  data = gbif_original,
  colname_lon = "decimalLongitude",
  colname_lat = "decimalLatitude",
  colname_site_id = "gbifID",
  strahler_seq = STRAHLER_SEQ,
  distance_threshold = DISTANCE_THRESHOLD
)



print(gbif_snap_result)

# Save
min_strahler <- min(STRAHLER_SEQ)
snap_out_path <- paste0("points_snapped/fish/gbif_snapped_points_min_strahler",min_strahler, "_dist_thresh_",DISTANCE_THRESHOLD, ".csv")
fwrite(gbif_snap_result, snap_out_path)

message(sprintf("GBIF: Snapped %d points", nrow(gbif_snap_result)))

gbif_snap_result <- fread(snap_out_path)

## some of the points that failed to snap are in lakes

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
dams_url <- "https://nimbus.igb-berlin.de/index.php/s/nzMQKArCD8SqNcK/download/dams_all_clean.csv"

# Load original dams data -- replace url with new dam data June 2026
# dams_original <- fread(dams_url)
# message(sprintf("Original dams points: %d", nrow(dams_original)))

dams_original <- fread("points_cleaned/dams/dams_sarantaporos_clean.csv")

# Snap dams data
DISTANCE_THRESHOLD <- 150
dams_snap_result <- api_get_snapped_points_cascade(
  data = dams_original,
  colname_lon = "longitude",
  colname_lat = "latitude",
  colname_site_id = "site_id",
  strahler_seq = STRAHLER_SEQ,
  distance_threshold = DISTANCE_THRESHOLD
)

print(dams_snap_result)


# dams_snap_result <- fread("points_snapped/dams/dams_snapped_points.csv")
dams_snap_result <- dams_snap_result %>%
  left_join(dams_original) %>%
  select(-longitude, -latitude)


# Save
fwrite(dams_snap_result, "points_snapped/dams/dams_snapped_points.csv")
message(sprintf("Dams: Snapped %d points", nrow(dams_snap_result)))

# ============================================================================
# VISUALIZE DAMS SNAPPING
# ============================================================================

message("\n--- Creating Dams visualization ---")

# Identify which points were successfully snapped
dams_snapped_ids <- dams_snap_result$site_id
dams_original$snapped <- dams_original$site_id %in% dams_snapped_ids

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
      "Dam ID: ", site_id, "<br>",
      # "Source: ", source, "<br>",
      # "Status: ", status, "<br>",
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
      "Dam ID: ", site_id, "<br>",
      # "Source: ", source, "<br>",
      # "Phase: ", phase, "<br>",
      # "Status: ", status, "<br>",
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
      "Dam ID: ", site_id, "<br>",
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










# ============================================================================
# MERGE SNAPPED COORDINATES WITH SPECIES DATA
# ============================================================================
library(hydrographr)
library(data.table)
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(sf)

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

message("\n=== Merging Snapped Data with Species Information ===")

# Load original data with species
fish_hcmr <- fread("points_cleaned/fish/fish_basin_hcmr.csv")
fish_gbif <- fread("points_cleaned/fish/fish_gbif_clean.csv")

# Load snapped data
all_snapped <- fread("points_snapped/fish/all_snapped_fish_points.csv")

# Separate HCMR and GBIF from snapped data
hcmr_snapped <- all_snapped %>% filter(source == "HCMR")
gbif_snapped <- all_snapped %>% filter(source == "GBIF")

# Merge HCMR
fish_hcmr_snapped <- fish_hcmr %>%
  left_join(hcmr_snapped, by = c("Sites" = "site_id")) %>%
  # rename(
  #   longitude_original_hcmr = longitude,
  #   latitude_original_hcmr = latitude
  # ) %>% # exclude the 2 points that were not snapped
  filter(!is.na(longitude_snapped))%>%
  mutate(species = gsub(" ", "_", species))


message(sprintf("HCMR: %d records with species", nrow(fish_hcmr_snapped)))
message(sprintf("  Successfully snapped: %d (%.1f%%)",
                sum(!is.na(fish_hcmr_snapped$subc_id)),
                100 * sum(!is.na(fish_hcmr_snapped$subc_id)) / nrow(fish_hcmr_snapped)))

# Merge GBIF
fish_gbif_snapped <- fish_gbif %>%
  mutate(gbifID=as.character(gbifID)) %>%
  left_join(gbif_snapped, by = c("gbifID" = "site_id")) %>%
  rename(
    decimalLongitude_original_gbif = decimalLongitude,
    decimalLatitude_original_gbif = decimalLatitude
  ) %>% # filter out the points that were not successfully snapped
  filter(!is.na(longitude_snapped)) %>%
  mutate(species = gsub(" ", "_", species))

message(sprintf("GBIF: %d records with species", nrow(fish_gbif_snapped)))
message(sprintf("  Successfully snapped: %d (%.1f%%)",
                sum(!is.na(fish_gbif_snapped$subc_id)),
                100 * sum(!is.na(fish_gbif_snapped$subc_id)) / nrow(fish_gbif_snapped)))

# Save merged files
write.csv(fish_hcmr_snapped, "points_snapped/fish/fish_hcmr_with_species_snapped.csv")
write.csv(fish_gbif_snapped, "points_snapped/fish/fish_gbif_with_species_snapped.csv")

# Optional: Combine both into one file with standardized columns
fish_all_combined <- rbind(
  fish_hcmr_snapped %>%
    select(site_id = Sites, species, longitude_original, latitude_original,
           longitude_snapped, latitude_snapped, subc_id, strahler,
           distance_metres, source) %>%
    mutate(dataset = "HCMR"),

  fish_gbif_snapped %>%
    select(site_id = gbifID, species, genus, family, order,
           longitude_original, latitude_original,
           longitude_snapped, latitude_snapped, subc_id, strahler,
           distance_metres, source, year, month, day) %>%
    mutate(dataset = "GBIF"),
  fill = TRUE
)

write.csv(fish_all_combined, "points_snapped/fish/fish_all_species_snapped.csv")

message("\n✓ Merged files created:")
message("  - points_snapped/fish_hcmr_with_species_snapped.csv")
message("  - points_snapped/fish_gbif_with_species_snapped.csv")
message("  - points_snapped/fish_all_species_snapped.csv")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_extract_subbasin.R
#
# Extract the target basin and subbasin networks for connectivity and
# SDM analysis.
#
# Training extent:   full Vjosa/Aoos basin (SDM model fitting + pseudoabsences)
# Prediction extent: subbasin upstream of outlet (Sarantaporos + Voidomatis)
#
# Workflow:
#   1. Load basin-filtered snapped fish + dams
#   2. Download basin polygon + stream network + prune  (SDM training extent)
#   3. Extract subbasin polygon + stream network + prune (SDM prediction extent)
#   4. Filter fish + dams to subbasin subcatchments
#   5. [VISUALISATION] Basin + subbasin networks + points
#   6. [VISUALISATION] Full vs pruned subbasin network
#   7. Filter fish to target species → SDM outputs
#
# Input:
#   - points_snapped/fish/fish_all_species_snapped.csv
#   - points_snapped/dams/dams_snapped_points.csv
#   - config/study_area_params.csv
#   - points_original/fish/species_list_sarantaporos.txt
#
# Output:
#   - spatial/basin/basin_polygon.gpkg
#   - spatial/basin/stream_network.gpkg
#   - spatial/basin/stream_network_pruned.gpkg
#   - spatial/basin/basin_subc_ids_pruned.csv
#   - spatial/subbasin/subbasin_polygon.gpkg
#   - spatial/subbasin/stream_network.gpkg
#   - spatial/subbasin/stream_network_pruned.gpkg
#   - spatial/subbasin/subbasin_subc_ids_pruned.csv
#   - points_snapped/subbasin/fish_subbasin.csv
#   - points_snapped/subbasin/dams_subbasin.csv
#   - points_snapped/subbasin/fish_sdm_basin.csv
#   - points_snapped/subbasin/fish_sdm_subbasin.csv
#
# LOCATION: workflows/03_snapping/03_extract_subbasin.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(sf)
library(data.table)
library(dplyr)
library(leaflet)

select <- dplyr::select

source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

# Load basin ID derived in 01_clean_hcmr_fish.R
study_params <- fread("config/study_area_params.csv")
BASIN_ID <- study_params[param == "BASIN_ID", as.integer(value)]
message("  Basin ID: ", BASIN_ID)

# Subbasin outlet point (confluence of Sarantaporos + Voidomatis → Aoos mainstem)
# Coordinates obtained from https://aqua.igb-berlin.de/upstream-dev/
OUTLET_LON <- 20.5870613
OUTLET_LAT <- 40.0728991

# Network pruning parameters — applied to both basin and subbasin
MIN_STRAHLER    <- 4
UPSTREAM_BUFFER <- 3

# Target species for SDM
target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>%
  unique()

message("  Target species: ", length(target_species))

# ============================================================
# STEP 1: Load basin-filtered snapped points
# ============================================================

message("\n=== Step 1: Loading snapped points ===")

# Both files are already basin-filtered by the cleaning scripts
fish_snapped <- fread("points_snapped/fish/fish_all_species_snapped.csv")
message("  Fish records loaded: ", nrow(fish_snapped))

dams_snapped <- fread("points_snapped/dams/dams_snapped_points.csv")
if ("id1" %in% names(dams_snapped)) dams_snapped <- dams_snapped %>% rename(site_id = id1)
message("  Dam records loaded: ", nrow(dams_snapped))

# Deduplicate to unique site locations
# fish file has multiple rows per site (one per species)
fish_unique <- fish_snapped %>%
  distinct(site_id, longitude_snapped, latitude_snapped, subc_id, source)

dams_unique <- dams_snapped %>%
  distinct(site_id, longitude_snapped, latitude_snapped, subc_id, source)

message("  Unique fish sites: ", nrow(fish_unique))
message("  Unique dam sites:  ", nrow(dams_unique))

# ============================================================
# STEP 2: Download basin polygon + stream network + prune
# (SDM training extent)
# ============================================================

message("\n=== Step 2: Downloading basin network (SDM training extent) ===")

dir.create("spatial/basin",        recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/basin", recursive = TRUE, showWarnings = FALSE)

# Basin polygon
basin_polygon <- api_get_basin_polygon(basin_id = BASIN_ID)
st_write(basin_polygon, "spatial/basin/basin_polygon.gpkg", delete_dsn = TRUE)
save_to_nimbus(basin_polygon, "spatial/basin/basin_polygon.gpkg")
message("  Basin polygon saved")

# Full basin stream network
basin_streams <- api_get_stream_segments(
  basin_id      = BASIN_ID,
  geometry_only = FALSE,
  min_strahler  = 2
)
basin_streams$basin_id <- BASIN_ID



st_write(basin_streams, "spatial/basin/stream_network.gpkg", delete_dsn = TRUE)
save_to_nimbus(basin_streams, "spatial/basin/stream_network.gpkg")

basin_subc_ids <- unique(basin_streams$subc_id)
message("  Basin stream segments: ", nrow(basin_streams),
        " | subcatchments: ", length(basin_subc_ids))

# Prune basin network based on all fish + dam points in basin
basin_streams_pruned <- extract_partial_stream_network(
  stream                    = basin_streams,
  snapped_subcs             = c(fish_unique$subc_id, dams_unique$subc_id),
  strahler_retain_threshold = MIN_STRAHLER,
  upstream_buffer           = UPSTREAM_BUFFER
)


# Deduplicate by subc_id — keep first occurrence
n_before <- nrow(basin_streams_pruned)
basin_streams_pruned <- basin_streams_pruned %>%
  distinct(subc_id, .keep_all = TRUE)
n_after <- nrow(basin_streams_pruned)

if (n_before > n_after) {
  message("  Removed ", n_before - n_after,
          " duplicate edges from pruned network")
}

st_write(basin_streams_pruned, "spatial/basin/stream_network_pruned.gpkg",
         delete_dsn = TRUE)
save_to_nimbus(basin_streams_pruned, "spatial/basin/stream_network_pruned.gpkg")

basin_subc_ids_pruned <- unique(basin_streams_pruned$subc_id)
message("  Pruned basin segments: ", nrow(basin_streams_pruned),
        " | subcatchments: ", length(basin_subc_ids_pruned))

fwrite(data.table(subc_id = basin_subc_ids_pruned),
       "spatial/basin/basin_subc_ids_pruned.csv")
message("  Saved: spatial/basin/basin_subc_ids_pruned.csv")

# ============================================================
# STEP 3: Extract subbasin polygon + stream network + prune
# (SDM prediction extent + connectivity analysis)
# ============================================================

message("\n=== Step 3: Extracting subbasin (SDM prediction extent) ===")

dir.create("spatial/subbasin",        recursive = TRUE, showWarnings = FALSE)
dir.create("points_snapped/subbasin", recursive = TRUE, showWarnings = FALSE)

# Subbasin polygon
subbasin_polygon <- api_get_upstream_catchment(lon = OUTLET_LON, lat = OUTLET_LAT)
st_write(subbasin_polygon, "spatial/subbasin/subbasin_polygon.gpkg", delete_dsn = TRUE)
save_to_nimbus(subbasin_polygon, "spatial/subbasin/subbasin_polygon.gpkg")
message("  Subbasin polygon saved")

# Full subbasin stream network
# add_target_streams = TRUE ensures target column is returned (needed for pruning)
subbasin_streams <- api_get_stream_segments(
  lon                = OUTLET_LON,
  lat                = OUTLET_LAT,
  upstream           = TRUE,
  geometry_only      = FALSE,
  min_strahler       = 2
)


st_write(subbasin_streams, "spatial/subbasin/stream_network.gpkg", delete_dsn = TRUE)
save_to_nimbus(subbasin_streams, "spatial/subbasin/stream_network.gpkg")

subbasin_subc_ids <- unique(subbasin_streams$subc_id)
message("  Subbasin stream segments: ", nrow(subbasin_streams),
        " | subcatchments: ", length(subbasin_subc_ids))

# Filter points to subbasin before pruning
# (pruning is based on which reaches have observations)
# fish_subbasin_unique <- fish_unique %>% filter(subc_id %in% subbasin_subc_ids)
# dams_subbasin_unique <- dams_unique %>% filter(subc_id %in% subbasin_subc_ids)

# Prune subbasin network
subbasin_streams_pruned <- extract_partial_stream_network(
  stream                    = subbasin_streams,
  snapped_subcs             = c(fish_unique$subc_id, dams_unique$subc_id),
  strahler_retain_threshold = MIN_STRAHLER,
  upstream_buffer           = UPSTREAM_BUFFER
)

# Deduplicate by subc_id — keep first occurrence
n_before <- nrow(subbasin_streams_pruned)
subbasin_streams_pruned <- subbasin_streams_pruned %>%
  distinct(subc_id, .keep_all = TRUE)
n_after <- nrow(subbasin_streams_pruned)

if (n_before > n_after) {
  message("  Removed ", n_before - n_after,
          " duplicate edges from pruned network")
}

# save the clean version
st_write(subbasin_streams_pruned,
         "spatial/subbasin/stream_network_pruned.gpkg",
         delete_dsn = TRUE)

save_to_nimbus(subbasin_streams_pruned, "spatial/subbasin/stream_network_pruned.gpkg")

subbasin_subc_ids_pruned <- unique(subbasin_streams_pruned$subc_id)
message("  Pruned subbasin segments: ", nrow(subbasin_streams_pruned),
        " | subcatchments: ", length(subbasin_subc_ids_pruned))

fwrite(data.table(subc_id = subbasin_subc_ids_pruned),
       "spatial/subbasin/subbasin_subc_ids_pruned.csv")
message("  Saved: spatial/subbasin/subbasin_subc_ids_pruned.csv")

# ============================================================
# STEP 4: Filter fish + dams to subbasin
# ============================================================

message("\n=== Step 4: Filtering points to subbasin ===")

# Use full species file (with species column) for subbasin fish
fish_subbasin <- fish_snapped %>%
  filter(subc_id %in% subbasin_subc_ids)

dams_subbasin <- dams_snapped %>%
  filter(subc_id %in% subbasin_subc_ids)

message("  Fish records in subbasin: ", nrow(fish_subbasin),
        " (", n_distinct(fish_subbasin$species), " species)")
message("  Dams in subbasin: ", nrow(dams_subbasin))
if (nrow(dams_subbasin) > 0) {
  message("  Dams by phase:")
  print(table(dams_subbasin$phase))
}

fwrite(fish_subbasin, "points_snapped/subbasin/fish_subbasin.csv")
fwrite(dams_subbasin, "points_snapped/subbasin/dams_subbasin.csv")

# sf versions for visualisation
fish_subbasin_sf <- fish_subbasin_unique %>%
  filter(!is.na(longitude_snapped), !is.na(latitude_snapped)) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

dams_subbasin_sf <- dams_subbasin_unique %>%
  filter(!is.na(longitude_snapped), !is.na(latitude_snapped)) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

# ============================================================
# STEP 5: Visualise basin + subbasin networks + points
#
# Grey = full basin network (training extent)
# Blue = subbasin network (prediction extent)
# All fish + dam points should fall on the blue subbasin network.
# ============================================================

message("\n=== Step 5: Visualising basin + subbasin ===")

map_basin_subbasin <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%

  addPolygons(
    data = subbasin_polygon, color = "#2166ac", weight = 2,
    fillOpacity = 0.05, group = "Subbasin boundary"
  ) %>%

  addPolylines(
    data = basin_streams, color = "#bdbdbd", weight = 1,
    opacity = 0.7, group = "Basin network"
  ) %>%

  addPolylines(
    data = subbasin_streams, color = "#2166ac", weight = 2,
    opacity = 0.9, group = "Subbasin network"
  ) %>%

  addCircleMarkers(
    data = fish_subbasin_sf, radius = 4,
    color = "#e6550d", fillColor = "#e6550d", fillOpacity = 0.8,
    stroke = FALSE, label = ~paste0(site_id, " | ", source),
    group = "Fish"
  ) %>%

  addCircleMarkers(
    data = dams_subbasin_sf, radius = 5,
    color = "#d73027", fillColor = "#d73027", fillOpacity = 0.8,
    stroke = FALSE, label = ~site_id, group = "Dams"
  ) %>%

  addMarkers(
    lng = OUTLET_LON, lat = OUTLET_LAT,
    label = "Subbasin outlet"
  ) %>%

  addLayersControl(
    overlayGroups = c("Subbasin boundary", "Basin network",
                      "Subbasin network", "Fish", "Dams"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  addLegend(
    position = "bottomright",
    colors   = c("#bdbdbd", "#2166ac", "#e6550d", "#d73027"),
    labels   = c("Basin network", "Subbasin network",
                 "Fish occurrences", "Dams"),
    title    = "Basin and subbasin"
  )

print(map_basin_subbasin)
# Verify: all points should fall on the blue subbasin network
# Grey-only reaches are basin segments outside the subbasin — expected

# ============================================================
# STEP 6: Visualise full vs pruned subbasin network
#
# Grey = full subbasin network
# Blue = pruned network
# All fish points must fall on the blue pruned network.
# Any point on grey only = pruning error.
# ============================================================

message("\n=== Step 6: Comparing full vs pruned subbasin network ===")

map_pruning <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%

  addPolygons(
    data = subbasin_polygon, color = "#525252", weight = 2,
    fillOpacity = 0.05, group = "Subbasin boundary"
  ) %>%

  addPolylines(
    data = subbasin_streams, color = "#bdbdbd", weight = 1,
    opacity = 0.8, group = "Full subbasin network"
  ) %>%

  addPolylines(
    data = subbasin_streams_pruned, color = "#2166ac", weight = 2,
    opacity = 0.9, group = "Pruned network"
  ) %>%

  addCircleMarkers(
    data = fish_subbasin_sf, radius = 4,
    color = "#e6550d", fillColor = "#e6550d", fillOpacity = 0.8,
    stroke = FALSE, label = ~paste0(site_id, " | ", source),
    group = "Fish"
  ) %>%

  addMarkers(
    lng = OUTLET_LON, lat = OUTLET_LAT,
    label = "Subbasin outlet"
  ) %>%

  addLayersControl(
    overlayGroups = c("Subbasin boundary", "Full subbasin network",
                      "Pruned network", "Fish"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  addLegend(
    position = "bottomright",
    colors   = c("#bdbdbd", "#2166ac", "#e6550d"),
    labels   = c("Full subbasin network", "Pruned network", "Fish occurrences"),
    title    = "Subbasin — full vs pruned"
  )

print(map_pruning)

# ============================================================
# STEP 7: Filter fish to target species → SDM outputs
# ============================================================

message("\n=== Step 7: Filtering to target species for SDM ===")

# SDM training: target species occurrences across full basin
fish_sdm_basin <- fish_snapped %>%
  filter(species %in% target_species) %>%
  select(-genus, -family, -order)

fish_sdm_basin <- fish_sdm_basin %>%
  mutate(year = ifelse(source == "HCMR", 2024, NA))

fwrite(fish_sdm_basin, "points_snapped/basin/fish_sdm_basin.csv")
message("  Basin SDM training records: ", nrow(fish_sdm_basin),
        " (", n_distinct(fish_sdm_basin$species), " species)")

# SDM prediction check: target species occurrences within subbasin
fish_sdm_subbasin <- fish_subbasin %>%
  filter(species %in% target_species)

fwrite(fish_sdm_subbasin, "points_snapped/subbasin/fish_sdm_subbasin.csv")
message("  Subbasin SDM records: ", nrow(fish_sdm_subbasin),
        " (", n_distinct(fish_sdm_subbasin$species), " species)")

# Coverage summary
species_coverage <- data.frame(species = target_species) %>%
  mutate(
    n_basin    = sapply(species, function(sp) sum(fish_sdm_basin$species == sp)),
    n_subbasin = sapply(species, function(sp) sum(fish_sdm_subbasin$species == sp)),
    in_basin    = n_basin > 0,
    in_subbasin = n_subbasin > 0
  ) %>%
  arrange(desc(n_basin))

fwrite(species_coverage, "points_snapped/subbasin/species_coverage_summary.csv")

cat("\nSpecies coverage:\n")
cat("  Target species:               ", length(target_species), "\n")
cat("  With records in basin:        ", sum(species_coverage$in_basin), "\n")
cat("  With records in subbasin:     ", sum(species_coverage$in_subbasin), "\n")
cat("  No records anywhere:          ",
    sum(!species_coverage$in_basin), "\n")
print(species_coverage)

# ============================================================
# SUMMARY
# ============================================================

message("\n=== Extraction Complete ===")
message("\nBasin (training extent):")
message("  spatial/basin/basin_polygon.gpkg")
message("  spatial/basin/stream_network.gpkg")
message("  spatial/basin/stream_network_pruned.gpkg")
message("  spatial/basin/basin_subc_ids_pruned.csv")
message("  points_snapped/basin/fish_sdm_basin.csv")
message("\nSubbasin (prediction extent + connectivity):")
message("  spatial/subbasin/subbasin_polygon.gpkg")
message("  spatial/subbasin/stream_network.gpkg")
message("  spatial/subbasin/stream_network_pruned.gpkg")
message("  spatial/subbasin/subbasin_subc_ids_pruned.csv")
message("  points_snapped/subbasin/fish_subbasin.csv")
message("  points_snapped/subbasin/dams_subbasin.csv")
message("  points_snapped/subbasin/fish_sdm_subbasin.csv")
message("  points_snapped/subbasin/species_coverage_summary.csv")
message("\nNext steps:")
message("  SDM: download env vars for basin_subc_ids_pruned,")
message("       predict on subbasin_subc_ids_pruned")
message("  Connectivity: use fish_subbasin.csv + dams_subbasin.csv")
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

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_generate_network_graph.R
# Build igraph river network from H90M subcatchments + barriers
# TWO SCENARIOS: current (existing dam only) vs future (existing + planned)
#
# NOTE ON PASSABILITY:
#   This script is species-agnostic. It carries the number of dams per
#   reach (n_shp) on nodes and edges, but does NOT assign a passability
#   value. Structural fragmentation (Module 5) treats any edge with
#   n_shp > 0 as a blocking barrier. Species-specific passability
#   (0.8 / 0.5 / 0) is applied later, at PCI computation time
#   (Module 10 / pci script), as pass = species_passability ^ n_shp.
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(sf)
library(igraph)
library(dplyr)
library(hydrographr)

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select   <- dplyr::select
rename   <- dplyr::rename
group_by <- dplyr::group_by

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# INPUT FILES
# ============================================================
subcatchments <- st_read("spatial/subbasin_sarantaporos/stream_network_pruned.gpkg")

# temporary fix until length is returned by api_getupstreamstreamsegments
basin_stream_length <- read_geopackage("spatial/basin/stream_network_pruned.gpkg",
                                       import_as = "data.table") %>%
  select(subc_id, length)

subcatchments <- subcatchments %>% left_join(basin_stream_length)

# New snapped dam inventory (status column: "existing" / "planned")
barriers <- read.csv("points_snapped/dams/dams_snapped_points.csv") %>%
  filter(subc_id %in% subcatchments$subc_id)   # keep dams on the Sarantaporos network

message("Barriers on network: ", nrow(barriers),
        "  (existing: ", sum(barriers$status == "existing"),
        ", planned: ",   sum(barriers$status == "planned"), ")")

# ============================================================
# BARRIER SCENARIOS  (driven by the 'status' column)
# ============================================================

# Current: only the existing operational dam
barrier_counts_current <- barriers %>%
  filter(status == "existing") %>%
  group_by(subc_id) %>%
  summarise(n_shp = n(), .groups = "drop")

# Future: existing + all planned (rejected/EXCLUDE already removed upstream)
barrier_counts_future <- barriers %>%
  group_by(subc_id) %>%
  summarise(n_shp = n(), .groups = "drop")

message("Current scenario: ", sum(barrier_counts_current$n_shp), " dams in ",
        nrow(barrier_counts_current), " subcatchments")
message("Future scenario:  ", sum(barrier_counts_future$n_shp), " dams in ",
        nrow(barrier_counts_future), " subcatchments")

# ============================================================
# FUNCTION: Build river graph from subcatchments + barrier counts
#   Carries n_shp (dam count) on nodes and edges. No passability.
# ============================================================
build_river_graph <- function(subcatchments_raw, barrier_counts) {

  # Step 1: Join barrier counts with subcatchments
  sc <- subcatchments_raw %>%
    left_join(barrier_counts, by = "subc_id") %>%
    mutate(n_shp = ifelse(is.na(n_shp), 0, n_shp))

  # Step 2: Deduplicate
  sc <- sc %>%
    distinct(subc_id, .keep_all = TRUE)

  # Step 3: Build edges
  edges_df <- sc %>%
    st_drop_geometry() %>%
    select(subc_id, target) %>%
    rename(from = subc_id, to = target) %>%
    mutate(from = as.character(from),
           to   = as.character(to))

  # Step 4: Build vertices (carry reach length, dam count, Strahler order)
  vertices_df <- sc %>%
    st_drop_geometry() %>%
    select(subc_id, length, strahler, n_shp) %>%
    rename(name = subc_id, length_reach = length) %>%
    mutate(name         = as.character(name),
           length_reach = ifelse(is.na(length_reach), 0, length_reach),
           n_shp        = ifelse(is.na(n_shp), 0, n_shp))

  # Step 5: Create igraph (first pass) and attach vertex attributes
  rg <- igraph::graph_from_data_frame(edges_df)

  rg_v_df <- igraph::as_data_frame(rg, "vertices") %>%
    left_join(vertices_df, by = "name") %>%
    mutate(
      length_reach = ifelse(name == "0" | is.na(length_reach), 1, length_reach),
      n_shp        = ifelse(name == "0" | is.na(n_shp),        0, n_shp)
    )

  rg_tmp <- igraph::graph_from_data_frame(edges_df, v = rg_v_df)

  # Step 6: Transfer dam count from nodes -> edges
  #   An edge is considered barriered if either of its connected nodes
  #   carries a dam. We keep the per-edge dam count (max of the two nodes)
  #   so the PCI step can compute species_passability ^ n_shp downstream.
  graph_v_df <- igraph::as_data_frame(rg_tmp, "vertices")

  graph_e_df <- igraph::as_data_frame(rg_tmp, "edges") %>%
    left_join(graph_v_df %>% select(name, n_shp) %>% rename(from = name),
              by = "from") %>%
    rename(n_shp_from = n_shp) %>%
    mutate(
      n_shp_from = ifelse(is.na(n_shp_from), 0, n_shp_from),
      n_shp_edge = n_shp_from          # barrier sits on the reach's DOWNSTREAM edge only
    ) %>%
    select(from, to, n_shp_edge)

  # Step 7: Final graph
  rg_final <- igraph::graph_from_data_frame(d = graph_e_df, vertices = graph_v_df)

  V(rg_final)$weight   <- 1
  E(rg_final)$n_shp    <- ifelse(is.na(E(rg_final)$n_shp_edge), 0,
                                 E(rg_final)$n_shp_edge)
  # convenience flag for structural fragmentation (Module 5)
  E(rg_final)$barrier  <- E(rg_final)$n_shp > 0

  return(rg_final)
}

# ============================================================
# BUILD BOTH GRAPHS
# ============================================================

message("\nBuilding current scenario graph...")
river_graph_current <- build_river_graph(subcatchments, barrier_counts_current)
message("Current: ", vcount(river_graph_current), " nodes, ",
        ecount(river_graph_current), " edges")

message("\nBuilding future scenario graph...")
river_graph_future <- build_river_graph(subcatchments, barrier_counts_future)
message("Future:  ", vcount(river_graph_future), " nodes, ",
        ecount(river_graph_future), " edges")

# ============================================================
# SAVE
# ============================================================
saveRDS(river_graph_current, "spatial/stream_networks/river_graph_current.RDS")
saveRDS(river_graph_future,  "spatial/stream_networks/river_graph_future.RDS")

message("\nBoth graphs saved!")

# ============================================================
# DIAGNOSTICS
# ============================================================
message("\n--- Current scenario ---")
message("Barriered edges: ", sum(E(river_graph_current)$barrier),
        " out of ", ecount(river_graph_current))

message("\n--- Future scenario ---")
message("Barriered edges: ", sum(E(river_graph_future)$barrier),
        " out of ", ecount(river_graph_future))

# How many additional edges are barriered in the future?
n_affected <- sum(E(river_graph_future)$barrier) -
  sum(E(river_graph_current)$barrier)
message("\nAdditional barriered edges in future scenario: ", n_affected)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_centrality.R   (Module 4 -- Network Analyses)
#
# Compute betweeness centrality on the bare Sarantaporos network graph
# and map it onto the stream network. Species-free, barrier-free:
# centrality describes the undisturbed network topology.
#
# High-betweeness reaches are network bottlenecks through which most
# shortest paths are routed; they flag locations where barrier placement
# would disconnect a disproportionate share of the network (linking to
# the fragmentation analysis in Module 5).
#
# INPUT:
#   - spatial/stream_networks/river_graph_current.RDS
#       (any scenario graph works -- centrality uses topology only, not dams;
#        we use the current graph, which carries the same 642 reaches)
#   - spatial/subbasin_sarantaporos/stream_network_pruned.gpkg
#       (for mapping centrality back onto reaches)
#
# OUTPUT:
#   - connectivity/centrality_table.csv
#   - spatial/stream_networks/stream_betweeness.gpkg   (stream network + bc)
#
# LOCATION: workflows/04_network_analyses/02_centrality.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(tidyverse)
library(igraph)
library(sf)
library(data.table)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("connectivity", showWarnings = FALSE)

# ============================================================
# LOAD NETWORK GRAPH
# ============================================================
# Centrality uses topology only; dams are irrelevant here. We load the
# current-scenario graph simply because it carries the full reach set.
message("Loading network graph...")
river_graph <- readRDS("spatial/stream_networks/river_graph_current.RDS")

# Drop the artificial root node "0" so it does not distort centrality
if ("0" %in% V(river_graph)$name) {
  river_graph <- delete_vertices(river_graph, "0")
}

message("  Network: ", vcount(river_graph), " nodes, ",
        ecount(river_graph), " edges")

# ============================================================
# COMPUTE betweeness CENTRALITY
# ============================================================
# get_centrality() (hydrographr) wraps igraph centrality on the directed
# stream graph. mode = "in" considers only upstream-connected segments.
message("\nComputing betweeness centrality...")

centrality <- get_centrality(river_graph, index = "betweenness", mode = "in")

# get_centrality returns a data.frame keyed by subc_id; keep it tidy
centrality <- centrality %>%
  mutate(subc_id = as.character(subc_id))

message("  Reaches with centrality values: ", nrow(centrality))
message("  betweeness range: ",
        round(min(centrality$betweeness, na.rm = TRUE), 1), " - ",
        round(max(centrality$betweeness, na.rm = TRUE), 1))

fwrite(centrality, "connectivity/centrality_table.csv")
message("  Saved: connectivity/centrality_table.csv")

# ============================================================
# MAP CENTRALITY ONTO THE STREAM NETWORK
# ============================================================
# Join betweeness back to the stream geometries so it can be plotted /
# exported. (Equivalent to reclass_raster() in the raster-based workflow,
# but on the vector stream network.)
message("\nMapping centrality onto stream network...")

streams <- st_read("spatial/subbasin_sarantaporos/stream_network_pruned.gpkg",
                   quiet = TRUE) %>%
  mutate(subc_id = as.character(subc_id))

streams_bc <- streams %>%
  left_join(centrality %>% select(subc_id, betweeness), by = "subc_id")

st_write(streams_bc,
         "spatial/stream_networks/stream_betweeness.gpkg",
         delete_dsn = TRUE, quiet = TRUE)
message("  Saved: spatial/stream_networks/stream_betweeness.gpkg")

# ============================================================
# QUICK MAP (ggplot) -- betweeness along the network
# ============================================================
message("\nDrawing centrality map...")

p <- ggplot(streams_bc) +
  geom_sf(aes(colour = betweeness, linewidth = betweeness)) +
  scale_colour_viridis_c(option = "magma", name = "betweenness") +
  scale_linewidth_continuous(range = c(0.2, 1.5), guide = "none") +
  theme_minimal() +
  labs(title = "Betweenness centrality of the Sarantaporos network")

png("connectivity/centrality_map.png", width = 1800, height = 1600, res = 200)
print(p)
dev.off()
message("  Saved: connectivity/centrality_map.png")

message("\nDone.")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_network_fragmentation.R   (Module 5 -- Network Fragmentation)
#
# Species-FREE structural fragmentation of the physical river network.
# Treats every dam as a fully blocking barrier, cuts the network at dam
# edges, and decomposes it into free-flowing fragments (weak components).
#
# Reports, per scenario (current / future):
#   - number of fragments
#   - fragment lengths (total / median / max free-flowing length)
#   - Strahler-order distribution of reaches across fragments
#
# This is the network-level counterpart to the species-level habitat
# fragmentation in Module 8. No fish data are used here.
#
# INPUT:
#   - spatial/stream_networks/river_graph_current.RDS
#   - spatial/stream_networks/river_graph_future.RDS
#     (built by 02_generate_network_graph.R; carry V()$length_reach,
#      V()$strahler, E()$barrier)
#
# OUTPUT:
#   - connectivity/network_fragmentation_summary.csv
#   - connectivity/fragment_strahler_distribution.csv
#
# LOCATION: workflows/05_network_fragmentation/01_network_fragmentation.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(data.table)

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("connectivity", showWarnings = FALSE)

# ============================================================
# LOAD SCENARIO GRAPHS
# ============================================================

message("Loading scenario graphs...")
river_graph_current <- readRDS("spatial/stream_networks/river_graph_current.RDS")
river_graph_future  <- readRDS("spatial/stream_networks/river_graph_future.RDS")

message("  Current: ", vcount(river_graph_current), " nodes, ",
        ecount(river_graph_current), " edges")
message("  Future:  ", vcount(river_graph_future), " nodes, ",
        ecount(river_graph_future), " edges")

# ============================================================
# FUNCTION: cut network at dams and decompose into fragments
# ============================================================
# A "fragment" = a free-flowing stretch: a weakly connected component
# of the network after removing all edges flagged as barriers.
fragment_network <- function(g, scenario_name) {

  # Remove barrier edges (dam locations). Treat mode = "weak" so a fragment
  # spans both upstream and downstream reaches, independent of flow direction.
  barrier_edges <- which(E(g)$barrier)
  g_cut <- delete_edges(g, barrier_edges)

  comp <- components(g_cut, mode = "weak")

  # Per-node table with fragment membership, reach length, Strahler order
  node_tbl <- tibble(
    name         = V(g_cut)$name,
    fragment     = comp$membership,
    length_reach = V(g_cut)$length_reach,
    strahler     = V(g_cut)$strahler
  ) %>%
    filter(name != "0")   # drop artificial root node

  # Fragment-level lengths (km)
  frag_lengths <- node_tbl %>%
    group_by(fragment) %>%
    summarise(length_km = sum(length_reach, na.rm = TRUE) / 1000,
              n_reaches = n(),
              .groups = "drop")

  summary_row <- tibble(
    scenario          = scenario_name,
    n_barrier_edges   = length(barrier_edges),
    n_fragments       = nrow(frag_lengths),
    total_length_km   = sum(frag_lengths$length_km),
    median_frag_km    = median(frag_lengths$length_km),
    max_frag_km       = max(frag_lengths$length_km),
    # free-flowing length = the same fragment lengths, reported as the
    # total km sitting in uninterrupted (barrier-free) stretches
    mean_freeflow_km  = mean(frag_lengths$length_km)
  )

  # Strahler-order distribution of reaches (length per order)
  strahler_dist <- node_tbl %>%
    filter(!is.na(strahler)) %>%
    group_by(strahler) %>%
    summarise(length_km = sum(length_reach, na.rm = TRUE) / 1000,
              n_reaches = n(),
              .groups = "drop") %>%
    mutate(scenario = scenario_name, .before = 1)

  list(summary = summary_row,
       fragments = frag_lengths %>% mutate(scenario = scenario_name, .before = 1),
       strahler = strahler_dist)
}

# ============================================================
# RUN BOTH SCENARIOS
# ============================================================

message("\nFragmenting current scenario...")
res_current <- fragment_network(river_graph_current, "current")

message("Fragmenting future scenario...")
res_future  <- fragment_network(river_graph_future, "future")

# ============================================================
# COMBINE + REPORT
# ============================================================

summary_df <- bind_rows(res_current$summary, res_future$summary)
strahler_df <- res_current$strahler
fragments_df <- bind_rows(res_current$fragments, res_future$fragments)

message("\n=== NETWORK FRAGMENTATION SUMMARY ===")
print(summary_df)

message("\nStrahler-order distribution (length km per order):")
print(strahler_df)

fwrite(summary_df,  "connectivity/network_fragmentation_summary.csv")
fwrite(strahler_df, "connectivity/fragment_strahler_distribution.csv")
fwrite(fragments_df, "connectivity/fragment_lengths.csv")

message("\nSaved:")
message("  connectivity/network_fragmentation_summary.csv")
message("  connectivity/fragment_strahler_distribution.csv")
message("  connectivity/fragment_lengths.csv")

# ============================================================
# DELTA (impact of planned dams)
# ============================================================
d_frag <- res_future$summary$n_fragments - res_current$summary$n_fragments
message("\nAdditional fragments from planned dams: +", d_frag)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_network_fragmentation_map.R   (Module 5 -- Network Fragmentation)
#
# Two-panel map: the Sarantaporos network under the current vs. future
# dam scenario, with reaches coloured by structural fragment membership.
# Species-free. Visualises the "isolate rather than destroy" result:
# one near-continuous network (current) shattered into many short
# fragments (future).
#
# INPUT:
#   - spatial/stream_networks/river_graph_current.RDS
#   - spatial/stream_networks/river_graph_future.RDS
#   - spatial/subbasin_sarantaporos/stream_network_pruned.gpkg
#   - points_snapped/dams/dams_snapped_points.csv   (to mark dam locations)
#
# OUTPUT:
#   - figures/fragmentation_network_map.png
#
# LOCATION: workflows/05_network_fragmentation/02_network_fragmentation_map.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(sf)
library(data.table)
library(patchwork)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("figures", showWarnings = FALSE)

# ============================================================
# LOAD
# ============================================================
river_graph_current <- readRDS("spatial/stream_networks/river_graph_current.RDS")
river_graph_future  <- readRDS("spatial/stream_networks/river_graph_future.RDS")

streams <- st_read("spatial/subbasin_sarantaporos/stream_network_pruned.gpkg",
                   quiet = TRUE) %>%
  mutate(subc_id = as.character(subc_id))

dams <- fread("points_snapped/dams/dams_snapped_points.csv") %>%
  mutate(subc_id = as.character(subc_id)) %>%
  filter(subc_id %in% streams$subc_id)

dams_sf <- st_as_sf(dams,
                    coords = c("longitude_snapped", "latitude_snapped"),
                    crs = st_crs(streams))

# ============================================================
# FRAGMENT MEMBERSHIP PER SCENARIO
# ============================================================
# Cut at barrier edges, weak components = fragments. Return a subc_id ->
# fragment lookup, with fragments ordered by length so colours are stable.
fragment_membership <- function(g) {
  g_cut <- delete_edges(g, which(E(g)$barrier))
  comp  <- components(g_cut, mode = "weak")

  tibble(
    subc_id      = V(g_cut)$name,
    fragment     = comp$membership,
    length_reach = V(g_cut)$length_reach
  ) %>%
    filter(subc_id != "0") %>%
    group_by(fragment) %>%
    mutate(frag_length = sum(length_reach, na.rm = TRUE)) %>%
    ungroup() %>%
    # relabel fragments largest -> smallest for a stable colour order
    mutate(fragment = dense_rank(desc(frag_length))) %>%
    select(subc_id, fragment)
}

memb_current <- fragment_membership(river_graph_current)
memb_future  <- fragment_membership(river_graph_future)

streams_cur <- streams %>% left_join(memb_current, by = "subc_id")
streams_fut <- streams %>% left_join(memb_future,  by = "subc_id")

n_cur <- max(memb_current$fragment, na.rm = TRUE)
n_fut <- max(memb_future$fragment,  na.rm = TRUE)

# ============================================================
# PLOT
# ============================================================
# Cycle a qualitative palette across fragments. With many fragments the
# legend is uninformative, so we drop it; the eye reads "few large blocks"
# vs "many small pieces", which is the point.
pal <- rep(RColorBrewer::brewer.pal(12, "Paired"), length.out = max(n_cur, n_fut))

make_panel <- function(net_sf, n_frag, title_label) {
  ggplot() +
    geom_sf(data = net_sf,
            aes(colour = factor(fragment)),
            linewidth = 0.6) +
    scale_colour_manual(values = pal, na.value = "grey80", guide = "none") +
    labs(title = title_label,
         subtitle = paste0(n_frag, " free-flowing fragment",
                           ifelse(n_frag == 1, "", "s"))) +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 10))
}

p_cur <- make_panel(streams_cur, n_cur, "Current") +
  geom_sf(data = dams_sf %>% filter(status == "existing"),
          shape = 21, fill = "black", colour = "white", size = 2.5)

p_fut <- make_panel(streams_fut, n_fut, "Future") +
  geom_sf(data = dams_sf, shape = 21, fill = "black",
          colour = "white", size = 2)

combined <- p_cur + p_fut +
  plot_annotation(
    title = "Structural fragmentation of the Sarantaporos network",
    theme = theme(plot.title = element_text(face = "bold", size = 14)))

png("figures/fragmentation_network_map.png",
    width = 2400, height = 1400, res = 200)
print(combined)
dev.off()

message("Saved: figures/fragmentation_network_map.png")
message("  Current: ", n_cur, " fragments | Future: ", n_fut, " fragments")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_env_space.R   (Module 6 -- Species in Environmental Space)
#
# Self-contained, descriptive module: shows how species occurrences are
# distributed across a small set of environmental gradients, BEFORE any
# modelling (Module 7). Runs independently of the SDM scripts.
#
# It builds its OWN environmental table for five descriptor variables
# (distinct filename from the SDM predict table to avoid confusion),
# subsets it to the sub-catchments of fish occurrences, and produces:
#   (a) per-species violin/boxplots across the five variables
#   (b) a PCA biplot of occurrences in environmental space
#
# Variables (5):
#   outlet_diff_dw_basin   elevation difference to basin outlet (topographic position)
#   bio01                  annual mean temperature (deg C)
#   bio15                  precipitation seasonality
#   order_strahler         Strahler stream order (network position)
#   slope_grad_dw_cel_mean downstream channel slope gradient
#
# Download + rescaling are repeated faithfully from the SDM data-prep step
# so values are identical to those the SDM used; only the variable subset
# and the output filename differ.
#
# INPUT:
#   env90m/ (downloaded H90m + CHELSA tables; same as SDM module)
#   env90m/subc_ids_basin.txt
#   points_snapped/fish/fish_all_species_snapped.csv   (occurrence subc_ids)
#
# OUTPUT:
#   env90m/env_space_table.csv                          (distinct from predict_table.csv)
#   figures/env_space/violin_{variable}.png
#   figures/env_space/env_space_violins_panel.png
#   figures/env_space/env_space_pca.png
#
# LOCATION: workflows/06_env_space/01_env_space.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("figures/env_space", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

TARGET_SPECIES <- c(
  "Alburnoides_prespensis", "Anguilla_anguilla", "Barbus_prespensis",
  "Chondrostoma_ohridanum", "Oxynoemacheilus_pindus", "Salmo_farioides",
  "Squalius_platyceps"
)

# Five descriptor variables (raw env90m names) for this module only
variables <- c(
  "bio01_1981-2010_observed",   # annual mean temperature
  "bio15_1981-2010_observed",   # precipitation seasonality
  "order_strahler",             # Strahler stream order
  "outlet_diff_dw_basin",       # elevation difference to outlet
  "slope_grad_dw_cel"           # downstream channel slope gradient
)

statistics <- c("mean")
tile_id    <- c("h18v04", "h20v04")     # same tiles as the SDM module
n_cores    <- max(1, parallel::detectCores() - 1)

# ============================================================
# STEP 0: Download the env90m tables for the 5 variables
# ============================================================
# NOTE ON THE DOWNLOAD DIRECTORY:
#   We download into the SAME directory the SDM module uses ("env90m/"),
#   with the SAME tiles and source tables, so the values are
#   guaranteed identical to those the SDM used. This module only requests
#   the subset of variables it needs (two of the three download functions;
#   no land cover). If the SDM module has already been run, these tables
#   are already present and the downloads simply confirm/skip them, so no
#   data is duplicated.
#   Running this module on its own (without the SDM) downloads only these
#   five variables, keeping Module 6 self-contained.

message("\n=== Step 0: Downloading env90m tables (5 variables) ===")

dir.create("env90m", showWarnings = FALSE, recursive = TRUE)

# Climate: bio01 (temperature) + bio15 (precipitation seasonality)
download_observed_climate_tables(
  subset       = c("bio01_1981-2010_observed", "bio15_1981-2010_observed"),
  tile_ids     = tile_id,
  download      = TRUE,
  download_dir = "env90m",
  file_format  = "txt",
  delete_zips  = TRUE,
  ignore_missing = FALSE,
  tempdir      = NULL,
  quiet        = FALSE
)

# Hydrography: Strahler order, elevation diff to outlet, downstream slope gradient
download_hydrography90m_tables(
  subset       = c("order_strahler", "outlet_diff_dw_basin", "slope_grad_dw_cel"),
  tile_ids     = tile_id,
  download      = TRUE,
  download_dir = "env90m",
  file_format  = "txt",
  delete_zips  = TRUE,
  ignore_missing = FALSE,
  tempdir      = NULL,
  quiet        = FALSE
)

# subc_ids_basin.txt is created by the SDM download step. If running this
# module independently, ensure it exists (it lists the basin sub-catchment
# IDs that get_predict_table extracts values for).
if (!file.exists("env90m/subc_ids_basin.txt"))
  stop("env90m/subc_ids_basin.txt not found. Create it (list of basin subc_ids) ",
       "or run the SDM download step first; it is shared between modules.")

# ============================================================
# STEP 1: Build the environmental table (this module's own copy)
# ============================================================

message("\n=== Step 1: Building env_space table (5 variables) ===")

env_space_file <- "env90m/env_space_table.csv"   # distinct from SDM predict_table.csv

env_tbl <- get_predict_table(
  variable      = variables,
  statistics    = statistics,
  tile_id       = tile_id,
  input_var_path = "env90m",
  subcatch_id   = "env90m/subc_ids_basin.txt",
  out_file_path = env_space_file,
  read          = TRUE,
  quiet         = FALSE,
  n_cores       = n_cores,
  overwrite     = TRUE
)

# Strip temporal suffix on climate columns: bio01_1981-2010_observed_mean -> bio01_mean
colnames(env_tbl) <- gsub("_1981-2010_observed", "", colnames(env_tbl))

message("  Columns: ", paste(names(env_tbl), collapse = ", "))

# ============================================================
# STEP 2: Rescale (identical scale factors to the SDM module)
# ============================================================

message("\n=== Step 2: Rescaling ===")

# slope gradient: /1e6
if ("slope_grad_dw_cel_mean" %in% names(env_tbl))
  env_tbl <- env_tbl %>% mutate(slope_grad_dw_cel_mean = slope_grad_dw_cel_mean / 1e6)

# bio01: temperature in Kelvin x10 -> /10 then -273.15 (deg C)
if ("bio01_mean" %in% names(env_tbl))
  env_tbl <- env_tbl %>% mutate(bio01_mean = (bio01_mean / 10) - 273.15)

# bio15: precipitation seasonality x10 -> /10
if ("bio15_mean" %in% names(env_tbl))
  env_tbl <- env_tbl %>% mutate(bio15_mean = bio15_mean / 10)

# outlet_diff_dw_basin and order_strahler: no scale factor (raw units)

fwrite(env_tbl, env_space_file)
message("  Saved rescaled table: ", env_space_file)

# ============================================================
# STEP 3: Subset to fish occurrence sub-catchments
# ============================================================

message("\n=== Step 3: Subsetting to occurrence sub-catchments ===")

fish <- fread("points_snapped/fish/fish_all_species_snapped.csv") %>%
  filter(species %in% gsub("_", " ", TARGET_SPECIES) | species %in% TARGET_SPECIES) %>%
  mutate(species = gsub(" ", "_", species)) %>%
  select(species, subc_id) %>%
  distinct()

message("  Occurrence records: ", nrow(fish))

# The five rescaled columns we will plot
plot_cols <- c("outlet_diff_dw_basin_mean", "bio01_mean", "bio15_mean",
               "stream_strahler", "slope_grad_dw_cel_mean")
# order_strahler may come back without _mean if it is not a zonal stat; handle both
if (!"stream_strahler" %in% names(env_tbl) && "order_strahler" %in% names(env_tbl)) {
  env_tbl <- env_tbl %>% rename(stream_strahler = order_strahler)
}
if (!"outlet_diff_dw_basin_mean" %in% names(env_tbl) && "outlet_diff_dw_basin" %in% names(env_tbl)) {
  env_tbl <- env_tbl %>% rename(outlet_diff_dw_basin_mean = outlet_diff_dw_basin)
}

occ_env <- fish %>%
  left_join(env_tbl, by = "subc_id") %>%
  filter(if_all(all_of(plot_cols), ~ !is.na(.)))

message("  Occurrences with complete env data: ", nrow(occ_env))

# Long format for plotting; nicer labels
var_labels <- c(
  outlet_diff_dw_basin_mean = "Elevation diff. to outlet (m)",
  bio01_mean                = "Mean annual temp. (\u00b0C)",
  bio15_mean                = "Precip. seasonality",
  stream_strahler       = "Strahler order",
  slope_grad_dw_cel_mean    = "Channel slope gradient"
)

occ_long <- occ_env %>%
  pivot_longer(cols = all_of(plot_cols), names_to = "variable", values_to = "value") %>%
  mutate(species_label = gsub("_", " ", species),
         variable_label = var_labels[variable])

# ============================================================
# STEP 4: Violin/boxplots per variable
# ============================================================

message("\n=== Step 4: Violin plots ===")

make_violin <- function(df, var_key) {
  d <- df %>% filter(variable == var_key)
  ggplot(d, aes(x = species_label, y = value, fill = species_label)) +
    geom_violin(alpha = 0.6, colour = NA, scale = "width") +
    geom_boxplot(width = 0.15, outlier.size = 0.5, alpha = 0.9) +
    labs(x = NULL, y = var_labels[[var_key]], title = var_labels[[var_key]]) +
    theme_bw(base_size = 11) +
    theme(legend.position = "none",
          axis.text.x = element_text(face = "italic", angle = 35, hjust = 1),
          plot.title  = element_text(size = 11))
}

violin_plots <- lapply(plot_cols, function(v) make_violin(occ_long, v))
names(violin_plots) <- plot_cols

# individual files
for (v in plot_cols) {
  out <- paste0("figures/env_space/violin_", v, ".png")
  png(out, width = 7, height = 4, units = "in", res = 200)
  print(violin_plots[[v]]); dev.off()
  message("  Saved: ", out)
}

# combined panel
panel <- wrap_plots(violin_plots, ncol = 2) +
  plot_annotation(
    title = "Environmental conditions at species occurrences",
    theme = theme(plot.title = element_text(face = "bold", size = 13))
  )
png("figures/env_space/env_space_violins_panel.png",
    width = 11, height = 12, units = "in", res = 200)
print(panel); dev.off()
message("  Saved: figures/env_space/env_space_violins_panel.png")

# ============================================================
# STEP 5: PCA of occurrences in environmental space
# ============================================================

message("\n=== Step 5: PCA ===")

# PCA uses the four CONTINUOUS predictors only. Strahler order is ordinal,
# so we exclude it from the PCA (it remains in the violin plots); treating
# an ordinal as continuous in a Euclidean PCA would be a stretch.
pca_cols <- c("outlet_diff_dw_basin_mean", "bio01_mean",
              "bio15_mean", "slope_grad_dw_cel_mean")

# Power-transform strongly right-skewed variables before PCA so it is not
# dominated by a few extreme reaches. (Adjust per the distributions above.)
pca_dat <- occ_env %>%
  mutate(
    slope_grad_dw_cel_mean    = log1p(pmax(slope_grad_dw_cel_mean, 0)),
    outlet_diff_dw_basin_mean = log1p(pmax(outlet_diff_dw_basin_mean, 0))
  )

pca_mat <- pca_dat %>% select(all_of(pca_cols)) %>% as.matrix()
pca     <- prcomp(pca_mat, center = TRUE, scale. = TRUE)

# scores + loadings
scores <- as.data.frame(pca$x[, 1:2]) %>%
  mutate(species_label = gsub("_", " ", pca_dat$species))

load_scale <- 3
loadings <- as.data.frame(pca$rotation[, 1:2]) %>%
  tibble::rownames_to_column("variable") %>%
  mutate(variable_label = var_labels[variable],
         PC1 = PC1 * load_scale, PC2 = PC2 * load_scale)

var_expl <- round(100 * (pca$sdev^2) / sum(pca$sdev^2), 1)

p_pca <- ggplot() +
  geom_point(data = scores, aes(PC1, PC2, colour = species_label),
             size = 2, alpha = 0.75) +
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")), colour = "grey25") +
  geom_text(data = loadings,
            aes(x = PC1 * 1.1, y = PC2 * 1.1, label = variable_label),
            size = 3, colour = "grey25") +
  labs(x = paste0("PC1 (", var_expl[1], "%)"),
       y = paste0("PC2 (", var_expl[2], "%)"),
       colour = "Species",
       title = "Occurrences in environmental space (PCA)") +
  theme_bw(base_size = 11) +
  theme(legend.text = element_text(face = "italic"))

png("figures/env_space/env_space_pca.png",
    width = 8, height = 6, units = "in", res = 200)
print(p_pca); dev.off()
message("  Saved: figures/env_space/env_space_pca.png")

# ============================================================
# SUMMARY
# ============================================================
message("\n", paste(rep("=", 60), collapse = ""))
message("ENV-SPACE FIGURES COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("  env90m/env_space_table.csv")
message("  figures/env_space/violin_{variable}.png")
message("  figures/env_space/env_space_violins_panel.png")
message("  figures/env_space/env_space_pca.png")
# ============================================================================
# DOWNLOAD ENVIRONMENTAL DATA FROM ENVIRONMENT90M
# ============================================================================
# Purpose: Download environmental predictor variables as TXT files from Env90m
# Input: Previously downloaded stream network (partial_stream_network.gpkg)
# Output: TXT files with environmental data organized by dataset
# ============================================================================
# Date: 2026-03-23
# ============================================================================

library(hydrographr)
library(data.table)
library(dplyr)

# ============================================================================
# SETUP
# ============================================================================

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)

# ============================================================================
# LOAD EXISTING STREAM NETWORK
# ============================================================================

message("\n=== Loading Existing Stream Network ===")

stream_network <- hydrographr::read_geopackage(
    "spatial/basin/stream_network_pruned.gpkg",
  import_as = "data.table"
)

message(sprintf("Loaded stream network: %d segments", nrow(stream_network)))

# Extract unique subcatchment IDs
subc_ids <- unique(stream_network$subc_id)
subc_ids <- subc_ids[!is.na(subc_ids)]

message(sprintf("Unique subcatchments: %d", length(subc_ids)))

# ============================================================================
# DETERMINE TILE IDs
# ============================================================================

message("\n=== Determining Tile IDs ===")

# Option 1: Manually specify tiles (if you know them)
tile_id <- c("h18v04", "h20v04")

# Option 2: Auto-detect from coordinates (uncomment if needed)
# coords <- st_coordinates(st_centroid(st_as_sf(stream_network)))
# tile_id <- get_tile_id(data = as.data.frame(coords), lon = "X", lat = "Y")
# tile_id <- unique(tile_id$tile_id)

message(sprintf("Tiles needed: %s", paste(tile_id, collapse = ", ")))

# ============================================================================
# CREATE DIRECTORY STRUCTURE
# ============================================================================

message("\n=== Creating Directory Structure ===")

dir.create("env90m", showWarnings = FALSE, recursive = TRUE)

message("✓ Created directory: env90m/")

# ============================================================================
# DOWNLOAD ENVIRONMENTAL DATA TABLES
# ============================================================================

message("\n=== Downloading Environmental Data Tables ===")
message("This may take several minutes depending on data size...")
# message("Total expected download: ~ 72.3 GB")

# --------------------------------------------------------------------------
# 1. OBSERVED CLIMATE VARIABLES
# --------------------------------------------------------------------------

message("\n--- Downloading Climate Variables ---")

download_observed_climate_tables(
  subset = c("bio01_1981-2010_observed", "bio04_1981-2010_observed",
             "bio05_1981-2010_observed", "bio06_1981-2010_observed",
             "bio15_1981-2010_observed", "bio17_1981-2010_observed",
             "bio18_1981-2010_observed"),
  tile_ids = tile_id,
  download = TRUE,
  download_dir = "env90m",
  file_format = "txt",
  delete_zips = TRUE,
  ignore_missing = FALSE,
  tempdir = NULL,
  quiet = FALSE
)

message("\n✓ Climate tables downloaded to: env90m/chelsa_bioclim_v2_1/")

# Verify download
climate_file <- list.files(
  path = "env90m/chelsa_bioclim_v2_1/1981-2010_observed/bio01",
  pattern = ".txt$",
  full.names = TRUE
)[1]

if (!is.na(climate_file) && file.exists(climate_file)) {
  file_size <- file.info(climate_file)$size / 1024 / 1024
  message(sprintf("  Sample file: %s (%.2f MB)", basename(climate_file), file_size))
  message("\n  First 10 rows:")
  system(paste("head", climate_file))
} else {
  warning("Climate file not found! Download may have failed.")
}

# --------------------------------------------------------------------------
# 2. LAND COVER VARIABLES
# --------------------------------------------------------------------------

message("\n--- Downloading Land Cover Variables ---")

download_landcover_tables(
  base_vars = c("c10", "c20", "c30", "c40", "c50", "c60", "c120", "c130",
                "c150", "c160", "c180", "c190", "c200", "c210"),
  years = c("1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000",
            "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010",
            "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020"),
  tile_ids = tile_id,
  download = TRUE,
  download_dir = "env90m",
  file_format = "txt",
  delete_zips = TRUE,
  ignore_missing = FALSE,
  tempdir = NULL,
  quiet = FALSE
)

message("\n✓ Land cover tables downloaded to: env90m/esa_cci_landcover_v2_1_1/")

# Verify download
landcover_file <- list.files(
  path = "env90m/esa_cci_landcover_v2_1_1/c60",
  pattern = ".txt$",
  full.names = TRUE
)[1]

if (!is.na(landcover_file) && file.exists(landcover_file)) {
  file_size <- file.info(landcover_file)$size / 1024 / 1024
  message(sprintf("  Sample file: %s (%.2f MB)", basename(landcover_file), file_size))
  message("\n  First 10 rows:")
  system(paste("head", landcover_file))
} else {
  warning("Land cover file not found! Download may have failed.")
}

# --------------------------------------------------------------------------
# 3. HYDROGRAPHY90M VARIABLES
# --------------------------------------------------------------------------

message("\n--- Downloading Hydrography Variables ---")

download_hydrography90m_tables(
  subset = c("cti", "order_strahler", "length", "cum_length", "gradient", "elev_drop",
             "accumulation", #"channel_grad_dw_seg", "channel_grad_up_seg",
             "channel_elv_dw_seg", "channel_elv_up_seg", "connections",
             "stream_dist_dw_near", "stream_dist_up_near", "slope_grad_dw_cel",
             "outlet_diff_dw_basin"),
  tile_ids = tile_id,
  download = TRUE,
  download_dir = "env90m",
  file_format = "txt",
  delete_zips = TRUE,
  ignore_missing = FALSE,
  tempdir = NULL,
  quiet = FALSE
)



message("\n✓ Hydrography tables downloaded to: env90m/hydrography90m_v1_0/")

# --------------------------------------------------------------------------
# 4. ADDITIONAL VARIABLES (OPTIONAL - UNCOMMENT IF NEEDED)
# --------------------------------------------------------------------------

# Soil variables
# download_soil_tables(
#   subset = c("clyppt", "sltppt", "sndppt"),
#   tile_ids = tile_id,
#   download = TRUE,
#   download_dir = "env90m",
#   file_format = "txt",
#   delete_zips = TRUE,
#   quiet = FALSE
# )

# Projected climate (future scenarios)
# download_projected_climate_tables(
#   subset = c("bio01_2041-2070_ssp370", "bio01_2041-2070_ssp585"),
#   tile_ids = tile_id,
#   download = TRUE,
#   download_dir = "env90m",
#   file_format = "txt",
#   delete_zips = TRUE,
#   quiet = FALSE
# )

# ============================================================================
# CREATE SUBCATCHMENT ID FILE
# ============================================================================

message("\n=== Creating Subcatchment ID Reference File ===")

subc_ids_dt <- data.table(subc_id = subc_ids)
fwrite(subc_ids_dt,
       file = "env90m/subc_ids_basin.txt",
       col.names = FALSE)

message(sprintf("✓ Saved %d subcatchment IDs to: env90m/subc_ids.txt",
                length(subc_ids)))

# ============================================================================
# VERIFICATION & SUMMARY
# ============================================================================

message("\n=== Download Verification ===")

n_climate  <- length(list.files("env90m/chelsa_bioclim_v2_1",      pattern = ".txt$", recursive = TRUE))
n_landcover <- length(list.files("env90m/esa_cci_landcover_v2_1_1", pattern = ".txt$", recursive = TRUE))
n_hydro    <- length(list.files("env90m/hydrography90m_v1_0",       pattern = ".txt$", recursive = TRUE))

message(sprintf("\nFiles downloaded:"))
message(sprintf("  Climate:    %d files", n_climate))
message(sprintf("  Land cover: %d files", n_landcover))
message(sprintf("  Hydrography:%d files", n_hydro))
message(sprintf("  Total:      %d files", n_climate + n_landcover + n_hydro))

total_size <- sum(
  file.info(list.files("env90m", pattern = ".txt$",
                       recursive = TRUE, full.names = TRUE))$size,
  na.rm = TRUE
) / 1024 / 1024 / 1024

message(sprintf("\nTotal disk space used: %.2f GB", total_size))

# ============================================================================
# FINAL SUMMARY
# ============================================================================

message("\n========================================")
message("=== DOWNLOAD COMPLETE ===")
message("========================================")

message("\nDirectory structure created:")
message("  env90m/")
message("  ├── chelsa_bioclim_v2_1/")
message("  ├── esa_cci_landcover_v2_1_1/")
message("  ├── hydrography90m_v1_0/")
message("  └── subc_ids.txt")

message("\nVariables downloaded:")
message("  Climate:     bio01, bio04, bio05, bio06, bio15, bio17, bio18 (1981-2010_observed)")
message("  Land cover:  c10, c20, c30, c40, c50, c60, c120, c130, c150, c160, c180, c190, c200, c210 (1992-2020)")
message("  Hydrography: order_strahler, length, cum_length, gradient, elev_drop, accumulation,")
message("               channel_grad_dw/up_seg, channel_elv_dw/up_seg, connections,")
message("               stream_dist_dw/up_near, slope_grad_dw_cel")

message("\nTiles downloaded:")
message(sprintf("  %s", paste(tile_id, collapse = ", ")))

message("\nNext steps:")
message("  1. Run script: 02_create_prediction_table.R")
message("  2. This will create the pred_tab.csv for SDM modeling")

message(sprintf("\nNOTE: Raw downloaded tables can be deleted after creating"))
message(sprintf("      the prediction table to save disk space (~%.1f GB)", total_size))

message("\n========================================\n")
# ============================================================================
# CREATE PREDICTION TABLE FOR SPECIES DISTRIBUTION MODELING
# ============================================================================
# Purpose: Create prediction table from downloaded Environment90m data
# Input: Downloaded environmental tables from script 01
# Output: predict_table.csv - ready for SDM modeling
# ============================================================================
# Date: 2026-04-23
# ============================================================================

library(hydrographr)
library(data.table)
library(dplyr)

# ============================================================================
# SETUP
# ============================================================================

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)

# ============================================================================
# VERIFY INPUT FILES EXIST
# ============================================================================

message("\n=== Verifying Input Files ===")

if (!file.exists("env90m/subc_ids_basin.txt")) {
  stop("ERROR: env90m/subc_ids_basin.txt not found!",
       "\n  Please run script 01_download_environmental_variables.R first.")
}

required_dirs <- c(
  "env90m/chelsa_bioclim_v2_1",
  "env90m/esa_cci_landcover_v2_1_1",
  "env90m/hydrography90m_v1_0"
)

for (dir in required_dirs) {
  if (!dir.exists(dir)) {
    stop("ERROR: Required directory not found: ", dir,
         "\n  Please run script 01_download_environmental_variables.R first.")
  }
}

message("✓ All required input files found")

# ============================================================================
# CONFIGURATION
# ============================================================================

message("\n=== Configuration ===")

# Define variables to include in prediction table
variables <- c(
  "bio01_1981-2010_observed", "bio04_1981-2010_observed", "bio05_1981-2010_observed",
  "bio06_1981-2010_observed", "bio15_1981-2010_observed", "bio17_1981-2010_observed",
  "bio18_1981-2010_observed",

  # Land cover 2020 (most recent; species observations up to 2024)
  "c10_2020", "c20_2020", "c30_2020", "c40_2020", "c50_2020", "c60_2020",
  "c120_2020", "c130_2020", "c150_2020", "c160_2020", "c180_2020",
  "c190_2020", "c200_2020", "c210_2020",

  "cti", "order_strahler", "length", "cum_length", "gradient", "elev_drop", "accumulation",
   "channel_elv_dw_seg", "channel_elv_up_seg", "outlet_diff_dw_basin",
   "stream_dist_dw_near", "stream_dist_up_near", "slope_grad_dw_cel"
)

# Define statistics to calculate
statistics <- c("mean")

# Define tile IDs (same as in script 01)
tile_id <- c("h18v04", "h20v04")

# Detect available cores for parallel processing
n_cores <- parallel::detectCores() - 1

message(sprintf("Variables: %d total", length(variables)))
message(sprintf("Statistics: %s", paste(statistics, collapse = ", ")))
message(sprintf("Tiles: %s", paste(tile_id, collapse = ", ")))
message(sprintf("CPU cores to use: %d", n_cores))

# ============================================================================
# CREATE PREDICTION TABLE
# ============================================================================

message("\n=== Creating Prediction Table ===")
message("This may take several minutes depending on data size...")

# Define output path
output_file <- "env90m/predict_table.csv"

# Run get_predict_table
predict_table <- get_predict_table(
  variable = variables,
  statistics = statistics,
  tile_id = tile_id,
  input_var_path = "env90m",
  subcatch_id = "env90m/subc_ids_basin.txt",
  out_file_path = output_file,
  read = TRUE,
  quiet = FALSE,
  n_cores = n_cores,
  overwrite = TRUE
)

message(sprintf("\n✓ Prediction table created: %d rows, %d columns",
                nrow(predict_table),
                ncol(predict_table)))

# Simplify bioclimatic variable names — remove temporal suffix
# bio01_1981-2010_observed_mean → bio01_mean
colnames(predict_table) <- gsub("_1981-2010_observed", "", colnames(predict_table))

message("  Column names after simplification:")
print(names(predict_table))

# ============================================================
# RESCALE VARIABLES — apply scale factors from Hydrography90m paper
# ============================================================

message("\n=== Rescaling variables ===")
# predict_table <- fread(output_file)

# CTI: raw values have scale factor of 10^8
# divide to get dimensionless wetness index (typical range 0-30)
predict_table <- predict_table %>%
  mutate(cti_mean = cti_mean / 1e8,
         slope_grad_dw_cel_mean   = slope_grad_dw_cel_mean / 1e6)


cat("CTI range after rescaling:",
    round(range(predict_table$cti_mean, na.rm=TRUE), 3), "\n")

# Bioclimatic variables: all raw values are × 10
# bio01, 05, 06, 08, 09, 10, 11: temperature variables in Kelvin × 10
#   → divide by 10 then subtract 273.15 to convert to °C
# bio04: temperature seasonality (SD × 100) × 10 → divide by 10 only
# bio02, 03, 07: temperature range/isothermality × 10 → divide by 10 only
# bio12-19: precipitation variables × 10 → divide by 10 only (mm)

# Variables that need /10 AND -273.15 (temperature in Kelvin)
bio_kelvin <- c("bio01", "bio05", "bio06", "bio08", "bio09", "bio10", "bio11")

# Variables that need /10 only
bio_scale_only <- c("bio02", "bio03", "bio04", "bio07",
                    "bio12", "bio13", "bio14", "bio15",
                    "bio16", "bio17", "bio18", "bio19")

# Apply to columns present in predict table
# Column names follow pattern bio01_mean, bio05_mean etc.
for (bio in bio_kelvin) {
  col <- paste0(bio, "_mean")
  if (col %in% names(predict_table)) {
    predict_table <- predict_table %>%
      mutate(!!col := (.data[[col]] / 10) - 273.15)
    cat(col, "range after rescaling:",
        round(range(predict_table[[col]], na.rm=TRUE), 2), "°C\n")
  }
}

for (bio in bio_scale_only) {
  col <- paste0(bio, "_mean")
  if (col %in% names(predict_table)) {
    predict_table <- predict_table %>%
      mutate(!!col := .data[[col]] / 10)
    cat(col, "range after rescaling:",
        round(range(predict_table[[col]], na.rm=TRUE), 2), "\n")
  }
}

# Save corrected predict table
fwrite(predict_table, "env90m/predict_table.csv")
message("Saved rescaled predict table: env90m/predict_table.csv")

# ============================================================================
# HANDLE MISSING DATA (IF ANY)
# ============================================================================

message("\n=== Checking for Missing Data ===")

# predict_table <- fread(output_file)

missing_counts <- predict_table[, lapply(.SD, function(x) sum(is.na(x)))]
has_missing <- any(missing_counts > 0)

if (has_missing) {
  message("\n⚠ Missing values detected:")

  for (col in names(missing_counts)) {
    if (col != "subc_id") {
      n_missing <- missing_counts[[col]]
      if (n_missing > 0) {
        pct_missing <- 100 * n_missing / nrow(predict_table)
        message(sprintf("  %s: %d (%.1f%%)", col, n_missing, pct_missing))
      }
    }
  }

  message("\nOptions for handling missing data:")
  message("  1. Keep all rows (NAs will need to be handled in modeling)")
  message("  2. Remove rows with any missing values")
  message("  3. Remove only specific problematic variables")

  message("\nSaving two versions:")

  fwrite(predict_table, "env90m/predict_table_full.csv")
  message("  ✓ Saved: env90m/predict_table_full.csv (all rows, including NAs)")

  predict_table_complete <- na.omit(predict_table)
  fwrite(predict_table_complete, "env90m/predict_table_complete.csv")
  message(sprintf("  ✓ Saved: env90m/predict_table_complete.csv (%d rows, NAs removed)",
                  nrow(predict_table_complete)))

  message(sprintf("\nRemoved %d rows (%.1f%%) with missing values",
                  nrow(predict_table) - nrow(predict_table_complete),
                  100 * (nrow(predict_table) - nrow(predict_table_complete)) / nrow(predict_table)))

  predict_table <- predict_table_complete

} else {
  message("✓ No missing values detected")
}

# ============================================================================
# DATA QUALITY CHECKS
# ============================================================================

message("\n=== Data Quality Summary ===")

message(sprintf("\nDimensions: %d rows × %d columns", nrow(predict_table), ncol(predict_table)))

message("\nColumn names:")
print(names(predict_table))

message("\nBasic statistics:")
print(summary(predict_table))

message("\nChecking for extreme values...")
for (col in names(predict_table)) {
  if (col != "subc_id" && is.numeric(predict_table[[col]])) {
    q <- quantile(predict_table[[col]], probs = c(0.01, 0.99), na.rm = TRUE)
    n_outliers <- sum(predict_table[[col]] < q[1] | predict_table[[col]] > q[2], na.rm = TRUE)
    if (n_outliers > 0) {
      pct_outliers <- 100 * n_outliers / nrow(predict_table)
      message(sprintf("  %s: %d values (%.1f%%) outside 1st-99th percentile",
                      col, n_outliers, pct_outliers))
    }
  }
}

# ============================================================================
# OPTIONAL: CLEAN UP DOWNLOADED TABLES
# ============================================================================

message("\n=== Disk Space Management ===")

downloaded_size <- sum(
  file.info(list.files("env90m", pattern = ".txt$",
                       recursive = TRUE, full.names = TRUE))$size,
  na.rm = TRUE
) / 1024 / 1024 / 1024

predict_table_size <- file.info("env90m/predict_table.csv")$size / 1024 / 1024

message(sprintf("Downloaded tables: %.2f GB", downloaded_size))
message(sprintf("Prediction table: %.2f MB", predict_table_size))
message(sprintf("Space savings if deleted: %.2f GB", downloaded_size))

message("\nTo free up disk space, you can delete the downloaded tables:")
message("  env90m/chelsa_bioclim_v2_1/")
message("  env90m/esa_cci_landcover_v2_1_1/")
message("  env90m/hydrography90m_v1_0/")

# Uncomment to automatically delete
# unlink("env90m/chelsa_bioclim_v2_1", recursive = TRUE)
# unlink("env90m/esa_cci_landcover_v2_1_1", recursive = TRUE)
# unlink("env90m/hydrography90m_v1_0", recursive = TRUE)
# message("\n✓ Deleted downloaded tables")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

message("\n========================================")
message("=== PREDICTION TABLE CREATION COMPLETE ===")
message("========================================")

message("\nFiles created:")
message("  ✓ env90m/predict_table.csv (main file)")
if (has_missing) {
  message("  ✓ env90m/predict_table_full.csv (with NAs)")
  message("  ✓ env90m/predict_table_complete.csv (NAs removed)")
}

message("\nPrediction table summary:")
message(sprintf("  Rows:      %d", nrow(predict_table)))
message(sprintf("  Columns:   %d", ncol(predict_table)))
message(sprintf("  Variables: %d", length(variables)))
message(sprintf("  File size: %.2f MB", predict_table_size))

message("\nNext steps:")
message("  1. Load predict_table.csv in your SDM workflow")
message("  2. Join with species occurrence data")
message("  3. Run species distribution models")
message("  4. Predict habitat suitability")

message("\nExample usage:")
message("  predict_table <- fread('env90m/predict_table.csv')")

message("\n========================================\n")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03b_prepare_sdm_data.R
#
# Prepare species-level presence/absence datasets for SSN and SDM modeling.
#
# Training + prediction extent: full Vjosa/Aoos basin
#
# For each target species:
#   - Presences: HCMR confirmed presences + GBIF records within basin
#   - True absences: HCMR surveyed sites where species was NOT recorded
#   - Pseudoabsences: randomly sampled from basin_subc_ids_pruned,
#     excluding subcatchments already covered by HCMR surveys,
#     number fixed to maintain prevalence = 0.1
#     (n_pseudoabs = n_presences * 9 - n_true_absences, min 0)
#
# Coordinates for SSN snapping:
#   - Presences/true absences: longitude_snapped/latitude_snapped from HCMR
#   - Pseudoabsences: stream reach centroids from basin network
#
# Input:
#   - points_snapped/fish/fish_all_species_snapped.csv
#   - spatial/basin/basin_subc_ids_pruned.csv
#   - spatial/basin/stream_network_pruned.gpkg
#   - env90m/predict_table.csv
#   - points_original/fish/species_list_sarantaporos.txt
#
# Output:
#   - sdm/input/occurr/{species}/occurr_{species}.csv  (per species)
#   - sdm/input/occurr/species_data_summary.csv
#
# LOCATION: workflows/07_sdm/03b_prepare_sdm_data.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(data.table)
library(dplyr)
library(sf)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

TARGET_PREVALENCE <- 0.1
set.seed(42)

# ============================================================
# SETUP
# ============================================================

dir.create("sdm/input/occurr", recursive = TRUE, showWarnings = FALSE)

target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>% unique()

message("Target species: ", length(target_species))

# ============================================================
# STEP 1: Load occurrence data
# ============================================================

message("\n=== Step 1: Loading occurrence data ===")

fish_all <- fread("points_snapped/fish/fish_all_species_snapped.csv") %>%
  filter(species %in% target_species)

message("  Total records for target species: ", nrow(fish_all))
message("  Sources: ", paste(unique(fish_all$source), collapse = ", "))

# ============================================================
# STEP 2: Load environmental predictors
# ============================================================

message("\n=== Step 2: Loading environmental predictors ===")

predict_table <- fread("env90m/predict_table.csv")
message("  Predict table: ", nrow(predict_table), " subcatchments, ",
        ncol(predict_table), " columns")

predictor_cols <- names(predict_table) %>% setdiff("subc_id")
message("  Predictor columns: ", length(predictor_cols))

# ============================================================
# STEP 3: Load basin subcatchment IDs + stream network centroids
# ============================================================

message("\n=== Step 3: Loading basin subcatchments + centroids ===")

basin_subc_ids_pruned <- fread("spatial/basin/basin_subc_ids_pruned.csv") %>%
  pull(subc_id)

message("  Basin pruned subcatchments: ", length(basin_subc_ids_pruned))

# Stream reach centroids for pseudoabsence coordinates
basin_centroids <- st_read("spatial/basin/stream_network_pruned.gpkg",
                           quiet = TRUE) %>%
  distinct(subc_id, .keep_all = TRUE) %>%
  st_centroid() %>%
  st_transform(crs = 4326) %>%
  mutate(
    longitude_snapped = st_coordinates(.)[, 1],
    latitude_snapped  = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(subc_id, longitude_snapped, latitude_snapped)

message("  Basin centroids available: ", nrow(basin_centroids))

# HCMR-surveyed subcatchments — excluded from pseudoabsence pool
hcmr_surveyed_subcs <- fish_all %>%
  filter(source == "HCMR") %>%
  pull(subc_id) %>%
  unique()

message("  HCMR surveyed subcatchments: ", length(hcmr_surveyed_subcs))

# Pseudoabsence pool: basin pruned network minus HCMR surveyed subcatchments
pseudoabs_pool <- setdiff(basin_subc_ids_pruned, hcmr_surveyed_subcs)
message("  Pseudoabsence pool size: ", length(pseudoabs_pool))

# ============================================================
# STEP 4: Build per-species datasets
# ============================================================

message("\n=== Step 4: Building per-species datasets ===")

species_summary <- list()

for (sp in target_species) {

  message("\n  --- ", sp, " ---")

  sp_dir <- file.path("sdm/input/occurr", sp)
  dir.create(sp_dir, recursive = TRUE, showWarnings = FALSE)

  # ---- Presences ----
  presences <- fish_all %>%
    filter(species == sp) %>%
    distinct(subc_id, source, longitude_snapped, latitude_snapped) %>%
    mutate(pres_abs = 1L)

  n_pres <- nrow(presences)
  message("    Presences: ", n_pres,
          " (HCMR: ", sum(presences$source == "HCMR"),
          ", GBIF: ", sum(presences$source == "GBIF"), ")")

  if (n_pres == 0) {
    message("    Skipping — no presences")
    next
  }

  # ---- True absences ----
  true_abs_subcs <- setdiff(hcmr_surveyed_subcs, presences$subc_id)

  true_abs_coords <- fish_all %>%
    filter(subc_id %in% true_abs_subcs) %>%
    distinct(subc_id, longitude_snapped, latitude_snapped)

  true_absences <- data.frame(
    subc_id  = true_abs_subcs,
    source   = "HCMR_true_absence",
    pres_abs = 0L
  ) %>%
    left_join(true_abs_coords, by = "subc_id")

  n_true_abs <- nrow(true_absences)
  message("    True absences: ", n_true_abs)

  # ---- Pseudoabsences ----
  n_abs_needed       <- round(n_pres * (1 - TARGET_PREVALENCE) / TARGET_PREVALENCE)
  n_pseudoabs_needed <- max(0L, n_abs_needed - n_true_abs)

  message("    Total absences needed (prevalence=", TARGET_PREVALENCE, "): ",
          n_abs_needed)
  message("    Pseudoabsences to sample: ", n_pseudoabs_needed)

  sp_pseudoabs_pool <- setdiff(pseudoabs_pool, presences$subc_id)

  if (n_pseudoabs_needed > length(sp_pseudoabs_pool)) {
    message("    WARNING: pool (", length(sp_pseudoabs_pool),
            ") smaller than needed (", n_pseudoabs_needed,
            ") — using all available")
    n_pseudoabs_needed <- length(sp_pseudoabs_pool)
  }

  if (n_pseudoabs_needed > 0) {
    pseudoabs_subcs <- sample(sp_pseudoabs_pool, n_pseudoabs_needed,
                              replace = FALSE)

    pseudoabsences <- data.frame(
      subc_id  = pseudoabs_subcs,
      source   = "pseudoabsence",
      pres_abs = 0L
    ) %>%
      left_join(basin_centroids, by = "subc_id")
  } else {
    pseudoabsences <- data.frame(
      subc_id           = integer(0),
      source            = character(0),
      pres_abs          = integer(0),
      longitude_snapped = numeric(0),
      latitude_snapped  = numeric(0)
    )
  }

  message("    Pseudoabsences sampled: ", nrow(pseudoabsences))

  # ---- Combine ----
  sp_data <- bind_rows(presences, true_absences, pseudoabsences)

  actual_prevalence <- mean(sp_data$pres_abs)
  message("    Actual prevalence: ", round(actual_prevalence, 3),
          " (target: ", TARGET_PREVALENCE, ")")

  # ---- Check coordinates ----
  n_missing_coords <- sum(is.na(sp_data$longitude_snapped) |
                            is.na(sp_data$latitude_snapped))
  if (n_missing_coords > 0) {
    message("    WARNING: ", n_missing_coords,
            " rows missing coordinates — removing")
    sp_data <- sp_data %>%
      filter(!is.na(longitude_snapped), !is.na(latitude_snapped))
  }

  # ---- Join environmental predictors ----
  sp_data <- sp_data %>%
    left_join(
      predict_table %>% select(subc_id, all_of(predictor_cols)),
      by = "subc_id"
    )

  n_missing_env <- sum(is.na(sp_data[[predictor_cols[1]]]))
  if (n_missing_env > 0) {
    message("    WARNING: ", n_missing_env,
            " rows missing environmental data — removing")
    sp_data <- sp_data %>%
      filter(if_all(all_of(predictor_cols), ~ !is.na(.)))
  }

  message("    Final dataset: ", nrow(sp_data), " rows (",
          sum(sp_data$pres_abs), " presences, ",
          sum(sp_data$pres_abs == 0), " absences)")

  # ---- Save ----
  fwrite(sp_data, file.path(sp_dir, paste0("occurr_", sp, ".csv")))
  message("    Saved: sdm/input/occurr/", sp, "/occurr_", sp, ".csv")

  species_summary[[sp]] <- data.frame(
    species          = sp,
    n_presences      = sum(sp_data$pres_abs == 1),
    n_hcmr_pres      = sum(sp_data$pres_abs == 1 & sp_data$source == "HCMR"),
    n_gbif_pres      = sum(sp_data$pres_abs == 1 & sp_data$source == "GBIF"),
    n_true_absences  = sum(sp_data$source == "HCMR_true_absence"),
    n_pseudoabsences = sum(sp_data$source == "pseudoabsence"),
    prevalence       = round(mean(sp_data$pres_abs), 3),
    n_env_predictors = length(predictor_cols),
    has_coords       = TRUE
  )
}

# ============================================================
# STEP 5: Save summary
# ============================================================

message("\n=== Step 5: Saving summary ===")

summary_df <- rbindlist(species_summary)
fwrite(summary_df, "sdm/input/occurr/species_data_summary.csv")

cat("\n")
print(summary_df)

message("\n", paste(rep("=", 80), collapse = ""))
message("SDM DATA PREPARATION COMPLETE")
message(paste(rep("=", 80), collapse = ""))
message("\nTraining extent:   full Vjosa/Aoos basin")
message("Pseudoabsence pool: basin_subc_ids_pruned (", length(basin_subc_ids_pruned), " subcatchments)")
message("Target prevalence: ", TARGET_PREVALENCE)
message("Species processed: ", nrow(summary_df))
message("\nOutputs:")
message("  sdm/input/occurr/{species}/occurr_{species}.csv")
message("  sdm/input/occurr/species_data_summary.csv")
message("\nNext: 04_ssn_models.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03_check_multicollinearity.R
#
# Quality check and collinearity assessment of the basin prediction table.
# Retains VIF-filtered predictor variables for use in SDM fitting.
#
# Workflow:
#   1. Load predict table + quality checks
#   2. Drop near-zero variance land cover classes
#   3. Correlation matrix visualisation
#   4. VIF-based variable selection
#   5. Save retained variables + filtered predict table
#
# Input:
#   - env90m/predict_table.csv
#   - spatial/basin/basin_subc_ids_pruned.csv
#
# Output:
#   - env90m/predict_table_vif.csv   (filtered predict table)
#   - env90m/selected_vars.csv             (retained variable names)
#
# LOCATION: workflows/07_sdm/03_check_multicollinearity.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(data.table)
library(dplyr)
library(corrplot)
library(usdm)
library(tidyr)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# STEP 1: Load predict table + quality checks
# ============================================================

message("\n=== Step 1: Loading and checking predict table ===")

predict_table <- fread("env90m/predict_table.csv")
message("  Rows: ", nrow(predict_table))
message("  Columns: ", ncol(predict_table))

basin_subc_ids_pruned <- fread("spatial/basin/basin_subc_ids_pruned.csv") %>%
  pull(subc_id)

# Check row count matches expected subcatchments
ifelse(
  nrow(predict_table) == length(basin_subc_ids_pruned),
  "Row count matches expected subcatchments.",
  paste0("MISMATCH: predict table has ", nrow(predict_table),
         " rows but expected ", length(basin_subc_ids_pruned))
)

# Check for missing values
missing_counts <- colSums(is.na(predict_table))
if (any(missing_counts > 0)) {
  message("  Columns with missing values:")
  print(missing_counts[missing_counts > 0])
} else {
  message("  No missing values")
}

# Simplify climate variable names for readability
colnames(predict_table) <- gsub("_1981-2010_observed", "", colnames(predict_table))
message("  Column names after simplification:")
print(names(predict_table))

# ============================================================
# STEP 2: Drop near-zero variance land cover classes
# ============================================================

message("\n=== Step 2: Dropping near-zero variance land cover classes ===")

# Check which land cover classes have very few non-zero subcatchments
lc_cols <- names(predict_table)[grepl("_y2020$", names(predict_table))]
lc_nonzero <- predict_table %>%
  select(all_of(lc_cols)) %>%
  summarise(across(everything(), ~ sum(. > 0, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "class", values_to = "n_nonzero") %>%
  arrange(n_nonzero)

message("  Land cover classes by non-zero subcatchment count:")
print(lc_nonzero)

# Drop land cover classes with near-zero variance
# Threshold: fewer than 100 non-zero subcatchments (<2% of network)
# Also drop ecologically irrelevant classes for a mountain river basin
lc_drop <- lc_nonzero %>%
  filter(n_nonzero < 100) %>%
  pull(class)

# Drop classes not expected in Greek freshwater subcatchments
# lc_drop <- c("c130_y2020", "c150_y2020", "c190_y2020", "c200_y2020")
predict_table <- predict_table %>%
  dplyr::select(-any_of(lc_drop))

message("  Dropped: ", paste(lc_drop, collapse = ", "))
message("  Remaining columns: ", ncol(predict_table))

# ============================================================
# STEP 3: Correlation matrix
# ============================================================

message("\n=== Step 3: Correlation matrix ===")

# Select numeric predictor variables (mean values + land cover)
numeric_vars <- predict_table %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(ends_with("_mean") | ends_with("2020"))

message("  Predictor columns for collinearity check: ", ncol(numeric_vars))

cor_matrix <- cor(numeric_vars, use = "complete.obs")

corrplot(cor_matrix,
         method  = "color",
         type    = "lower",
         tl.cex  = 0.6,
         diag    = FALSE,
         col     = COL2("RdBu", 200))

# ============================================================
# STEP 4: VIF-based variable selection
# ============================================================

message("\n=== Step 4: VIF-based variable selection (th = 10) ===")

vif_results <- vifstep(numeric_vars, th = 10)
vif_results@results

selected_vars <- vif_results@results$Variables
message("  Variables retained after VIF: ", length(selected_vars))
print(selected_vars)

# replace channel_elv_up_seg_mean with channel_elv_up_seg_mean for interpretability
selected_vars <- gsub("channel_elv_up_seg_mean",
                      "channel_elv_dw_seg_mean",
                      selected_vars)

# ============================================================
# STEP 5: Save outputs
# ============================================================

message("\n=== Step 5: Saving outputs ===")

# Filtered predict table — subc_id + retained predictors only
predict_table_vif <- predict_table %>%
  dplyr::select(any_of(c("subc_id", selected_vars)))

fwrite(predict_table_vif, "env90m/predict_table_vif.csv")
message("  Saved: env90m/predict_table_vif.csv")
message("  Dimensions: ", nrow(predict_table_vif), " x ", ncol(predict_table_vif))

# Retained variable names — used by 03b_prepare_sdm_data.R and 04_ssn_models.R
fwrite(
  data.table(variable = selected_vars),
  "env90m/selected_vars.csv"
)
message("  Saved: env90m/selected_vars.csv")

# ============================================================
# SUMMARY
# ============================================================

message("\n=== Collinearity Check Complete ===")
message("  Input predictors:    ", ncol(numeric_vars))
message("  Retained after VIF:  ", length(selected_vars))
message("  Removed:             ", ncol(numeric_vars) - length(selected_vars))
message("\nOutputs:")
message("  env90m/predict_table_vif.csv")
message("  env90m/selected_vars.csv")
message("\nNext: 03b_prepare_sdm_data.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 04_ssn_models.R
#
# Fit Spatial Stream Network (SSN) models for freshwater fish species
# distribution in the Vjosa/Aoos basin.
#
# Training + prediction extent: full basin (basin_subc_ids_pruned)
# This provides a larger environmental space for model fitting and
# more pseudoabsences distributed across the basin.
#
# Each species gets its own SSN object because observation sites differ
# (presences + true absences + pseudoabsences are species-specific).
# Prediction sites are shared across species and built once.
#
# Workflow (outside loop — once):
#   1. Load basin stream network + predict table
#   2. Build prediction sites sf object (basin centroids)
#   3. Build LSN edges
#   4. Calculate upstream distances for edges
#   5. Calculate AFVs for edges
#
# Workflow (inside loop — per species):
#   6.  Load species occurrence file from 03b
#   7.  Build obs_sites sf for this species
#   8.  Snap obs + pred sites to LSN
#   9.  Calculate upstream distances for obs + pred sites
#   10. Calculate AFVs for obs + pred sites
#   11. Assemble SSN object + distance matrices
#   12. Join + scale predictors
#   13. Torgegram
#   14. Fit covariance structure candidates, select best by AICc
#   15. Refit with REML, predict across basin, save
#   16. Join subbasin predictions to stream network gpkg + save summary
#
# Input:
#   - spatial/basin/stream_network_pruned.gpkg
#   - sdm/input/occurr/{species}/occurr_{species}.csv  (from 03b)
#   - spatial/basin/basin_subc_ids_pruned.csv
#   - points_snapped/subbasin/subbasin_subc_ids_pruned.csv
#   - env90m/predict_table.csv
#   - points_original/fish/species_list_sarantaporos.txt
#
# Output:
#   - spatial/basin/ssn/{species}.ssn
#   - sdm/ssn_models/ssn_{species}.rds
#   - sdm/ssn_models/torgegram_{species}.png
#   - sdm/ssn_models/model_summary.csv
#   - sdm/predictions/pred_{species}.csv       (full basin)
#   - spatial/subbasin/stream_network_predictions.gpkg  (subbasin only)
#
# LOCATION: workflows/07_sdm/04_ssn_models.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(SSNbler)
library(SSN2)
library(hydrographr)
library(sf)
library(data.table)
library(dplyr)
library(ggplot2)
library(broom)
library(dismo)

# Compute MCC across thresholds (same as 05_maxent.R and 06_random_forest.R)
compute_mcc_threshold <- function(pres_preds, abs_preds,
                                  thresh_seq = seq(0, 1, by = 0.01)) {
  mcc_vals <- sapply(thresh_seq, function(t) {
    TP <- as.numeric(sum(pres_preds >= t, na.rm = TRUE))
    FN <- as.numeric(sum(pres_preds <  t, na.rm = TRUE))
    FP <- as.numeric(sum(abs_preds  >= t, na.rm = TRUE))
    TN <- as.numeric(sum(abs_preds  <  t, na.rm = TRUE))
    denom <- sqrt((TP+FP) * (TP+FN) * (TN+FP) * (TN+FN))
    if (is.na(denom) || denom == 0) return(NA)
    (TP * TN - FP * FN) / denom
  })
  best_thresh <- thresh_seq[which.max(mcc_vals)]
  best_mcc    <- max(mcc_vals, na.rm = TRUE)
  return(list(threshold = best_thresh, mcc = best_mcc))
}

select <- dplyr::select

source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

dir.create("spatial/basin/ssn", recursive = TRUE, showWarnings = FALSE)
dir.create("sdm/ssn_models",    recursive = TRUE, showWarnings = FALSE)
dir.create("sdm/predictions",   recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Target CRS: EPSG 27704 WGS 84 / Equi7 Europe (metres)
# Pan-European projected CRS chosen for workflow generalisability
PROJ_CRS <- 27704

# Snap tolerance in metres for sites_to_lsn()
SNAP_TOL <- 100

# Fixed-effect predictors
# cum_length: longitudinal position in network
# bio01_mean: mean annual temperature (most interpretable bioclimatic variable)
# cti_mean: compound topographic index (habitat wetness proxy)
FORMULA_PREDICTORS <- c("cum_length", "bio01_mean", "cti_mean")

# Occurrence files directory (output of 03b_prepare_sdm_data.R)
OCCURR_DIR <- "sdm/input/occurr"

# Target species
target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>% unique()

message("Target species (", length(target_species), "):")
for (sp in target_species) message("  ", sp)

# ============================================================
# STEP 1: Load basin stream network + predict table
# ============================================================

message("\n=== Step 1: Loading inputs ===")

basin_streams <- st_read("spatial/basin/stream_network_pruned.gpkg") %>%
  st_transform(crs = PROJ_CRS)

# Rename existing length column to avoid conflict with SSNbler's Length
basin_streams <- basin_streams %>%
  rename(length_reach = length)

message("  Basin stream segments: ", nrow(basin_streams))

geom_types <- unique(as.character(st_geometry_type(basin_streams)))
if (any(geom_types == "MULTILINESTRING")) {
  message("  Converting MULTILINESTRING to LINESTRING...")
  basin_streams <- st_cast(basin_streams, "LINESTRING")
}
message("  Geometry type: ", paste(unique(as.character(
  st_geometry_type(basin_streams))), collapse = ", "))

predict_table <- fread("env90m/predict_table.csv")
message("  Predict table: ", nrow(predict_table), " subcatchments, ",
        ncol(predict_table), " columns")

missing_preds <- setdiff(FORMULA_PREDICTORS, names(predict_table))
if (length(missing_preds) > 0)
  stop("Missing predictor columns: ", paste(missing_preds, collapse = ", "))

# Basin subcatchment IDs — used for prediction sites
basin_subc_ids <- fread("spatial/basin/basin_subc_ids_pruned.csv") %>%
  pull(subc_id)

# Subbasin subcatchment IDs — for saving subbasin predictions to gpkg
subbasin_subc_ids <- fread("spatial/subbasin/subbasin_subc_ids_pruned.csv") %>%
  pull(subc_id)

# ============================================================
# STEP 2: Build prediction sites sf object (shared across species)
# ============================================================

message("\n=== Step 2: Building prediction sites ===")

pred_sites <- basin_streams %>%
  filter(subc_id %in% basin_subc_ids) %>%
  distinct(subc_id, .keep_all = TRUE) %>%
  st_point_on_surface() %>%
  select(subc_id)

message("  Prediction sites (full basin): ", nrow(pred_sites))

# ============================================================
# STEP 3: Build LSN edges (shared across species)
# ============================================================

message("\n=== Step 3: Building LSN edges ===")

lsn_path <- "spatial/basin/ssn/lsn"

edges <- lines_to_lsn(
  streams        = basin_streams,
  lsn_path       = lsn_path,
  check_topology = TRUE,
  snap_tolerance = 1,
  topo_tolerance = 20,
  overwrite      = TRUE
)

message("  LSN edges: ", nrow(edges))

# ============================================================
# STEP 4: Calculate upstream distances for edges (shared)
# ============================================================

message("\n=== Step 4: Calculating upstream distances for edges ===")

edges <- updist_edges(
  edges       = edges,
  save_local  = TRUE,
  lsn_path    = lsn_path,
  calc_length = TRUE
)

message("  Edge upstream distances calculated")

# ============================================================
# STEP 5: Calculate AFVs for edges (shared)
# ============================================================

message("\n=== Step 5: Calculating AFVs for edges ===")

if (!"accumulation_mean" %in% names(edges)) {
  edges <- edges %>%
    left_join(
      predict_table %>% select(subc_id, accumulation_mean),
      by = "subc_id"
    )
}

n_missing <- sum(is.na(edges$accumulation_mean))
n_zeros   <- sum(edges$accumulation_mean == 0, na.rm = TRUE)
if (n_missing > 0) {
  message("  WARNING: ", n_missing, " edges missing accumulation — replacing with 1")
  edges$accumulation_mean[is.na(edges$accumulation_mean)] <- 1
}
if (n_zeros > 0) {
  message("  WARNING: ", n_zeros, " edges with zero accumulation — replacing with 1")
  edges$accumulation_mean[edges$accumulation_mean == 0] <- 1
}

edges <- afv_edges(
  edges     = edges,
  infl_col  = "accumulation_mean",
  segpi_col = "areaPI",
  afv_col   = "afvArea",
  lsn_path  = lsn_path
)

message("  AFV range: ", round(min(edges$afvArea), 4),
        " — ", round(max(edges$afvArea), 4))
message("  Most downstream edge AFV (should be ~1.0): ",
        round(max(edges$afvArea), 4))

# ============================================================
# SPECIES LOOP
# ============================================================

ssn_results <- list()

for (sp in target_species) {

  message("\n", paste(rep("=", 60), collapse = ""))
  message("  SPECIES: ", sp)
  message(paste(rep("=", 60), collapse = ""))

  # ---- Step 6: Load species occurrence file from 03b ----

  occurr_file <- file.path(OCCURR_DIR, sp, paste0("occurr_", sp, ".csv"))

  if (!file.exists(occurr_file)) {
    message("  Skipping — occurrence file not found: ", occurr_file)
    next
  }

  sp_data <- fread(occurr_file)

  n_pres <- sum(sp_data$pres_abs == 1)
  n_abs  <- sum(sp_data$pres_abs == 0)

  message("  Presences: ", n_pres,
          " | True absences: ", sum(sp_data$source == "HCMR_true_absence"),
          " | Pseudoabsences: ", sum(sp_data$source == "pseudoabsence"))

  if (n_pres < 3) {
    message("  Skipping — fewer than 3 presences")
    next
  }

  # ---- Step 7: Build obs_sites sf for this species ----

  obs_sites_sp <- sp_data %>%
    filter(!is.na(longitude_snapped), !is.na(latitude_snapped)) %>%
    select(subc_id, pres_abs, source, longitude_snapped, latitude_snapped) %>%
    st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326) %>%
    st_transform(crs = PROJ_CRS)

  message("  Obs sites as sf: ", nrow(obs_sites_sp))

  # ---- Step 8: Snap obs + pred sites to LSN ----

  message("  Snapping sites to LSN...")

  sp_lsn_obs <- paste0("obs_", gsub(" ", "_", sp))

  obs_snapped_sp <- sites_to_lsn(
    sites          = obs_sites_sp,
    edges          = edges,
    lsn_path       = lsn_path,
    file_name      = sp_lsn_obs,
    snap_tolerance = SNAP_TOL,
    save_local     = TRUE,
    overwrite      = TRUE
  )

  message("  Obs snapped: ", nrow(obs_snapped_sp), " / ", nrow(obs_sites_sp))
  cat("  Max snap distance (m):", max(obs_snapped_sp$snapdist, na.rm = TRUE), "\n")

  pred_snapped_sp <- sites_to_lsn(
    sites          = pred_sites,
    edges          = edges,
    lsn_path       = lsn_path,
    file_name      = "pred_basin",
    snap_tolerance = SNAP_TOL,
    save_local     = TRUE,
    overwrite      = TRUE
  )

  message("  Pred snapped: ", nrow(pred_snapped_sp))

  # ---- Step 9: Calculate upstream distances for obs + pred ----

  message("  Calculating upstream distances for sites...")

  site_list_sp <- updist_sites(
    sites      = list(obs = obs_snapped_sp,
                      pred_basin = pred_snapped_sp),
    edges      = edges,
    length_col = "Length",
    save_local = TRUE,
    lsn_path   = lsn_path
  )

  # ---- Step 10: Calculate AFVs for obs + pred sites ----

  message("  Calculating AFVs for sites...")

  site_list_sp <- afv_sites(
    sites      = site_list_sp,
    edges      = edges,
    afv_col    = "afvArea",
    save_local = TRUE,
    lsn_path   = lsn_path
  )

  cat("  afvArea in obs:", "afvArea" %in% names(site_list_sp$obs), "\n")

  # ---- Step 11: Assemble SSN object + distance matrices ----

  message("  Assembling SSN object...")

  ssn_path_sp <- file.path("spatial/basin/ssn",
                           paste0(gsub(" ", "_", sp), ".ssn"))

  ssn_obj_sp <- ssn_assemble(
    edges      = edges,
    lsn_path   = lsn_path,
    obs_sites  = site_list_sp$obs,
    preds_list = list(pred_basin = site_list_sp$pred_basin),
    ssn_path   = ssn_path_sp,
    import     = TRUE,
    check      = TRUE,
    afv_col    = "afvArea",
    overwrite  = TRUE
  )

  message("  SSN object assembled")

  message("  Creating distance matrices...")
  ssn_create_distmat(
    ssn.object    = ssn_obj_sp,
    predpts       = "pred_basin",
    among_predpts = TRUE,
    overwrite     = TRUE
  )
  message("  Distance matrices created")

  # ---- Step 12: Join + scale predictors ----

  message("  Joining and scaling predictors...")

  predict_table_model <- predict_table %>%
    select(subc_id, all_of(FORMULA_PREDICTORS))

  # Join to obs
  obs_data <- ssn_get_data(ssn_obj_sp, "obs")

  if (!"subc_id" %in% names(obs_data))
    stop("subc_id not in obs sites for ", sp)

  obs_with_env <- obs_data %>%
    left_join(predict_table_model, by = "subc_id")

  n_na <- sum(is.na(obs_with_env[[FORMULA_PREDICTORS[1]]]))
  if (n_na > 0)
    message("  WARNING: ", n_na, " obs sites missing predictor values")

  # Scale using obs mean/SD
  obs_scaled <- obs_with_env %>%
    mutate(across(all_of(FORMULA_PREDICTORS),
                  ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)))

  ssn_obj_sp <- ssn_put_data(obs_scaled, ssn_obj_sp, "obs")

  # Join + scale pred sites using obs scaling params
  pred_data <- ssn_get_data(ssn_obj_sp, "pred_basin") %>%
    left_join(predict_table_model, by = "subc_id")

  for (pred in FORMULA_PREDICTORS) {
    obs_vals <- obs_with_env %>% st_drop_geometry() %>% pull(pred)
    obs_mean <- mean(obs_vals, na.rm = TRUE)
    obs_sd   <- sd(obs_vals, na.rm = TRUE)
    if (obs_sd > 0)
      pred_data[[pred]] <- (pred_data[[pred]] - obs_mean) / obs_sd
  }

  ssn_obj_sp <- ssn_put_data(pred_data, ssn_obj_sp, "pred_basin")

  # Verify condition number
  obs_env_check <- ssn_get_data(ssn_obj_sp, "obs") %>%
    st_drop_geometry() %>%
    select(all_of(FORMULA_PREDICTORS))
  X_check <- model.matrix(~ ., data = obs_env_check)
  cat("  Condition number:", round(kappa(X_check), 2), "\n")

  # ---- Step 13: Torgegram ----

  formula_rhs <- paste(FORMULA_PREDICTORS, collapse = " + ")
  formula_mod <- as.formula(paste("pres_abs ~", formula_rhs))

  tg <- tryCatch(
    Torgegram(
      formula    = formula_mod,
      ssn.object = ssn_obj_sp,
      type       = c("flowcon", "flowuncon", "euclid")
    ),
    error = function(e) { message("  Torgegram failed: ", e$message); NULL }
  )

  if (!is.null(tg)) {
    png(paste0("sdm/ssn_models/torgegram_", sp, ".png"),
        width = 1200, height = 400)
    plot(tg)
    dev.off()
    message("  Torgegram saved")
  }

  # ---- Step 14: Fit covariance structure candidates ----

  message("  Fitting models...")

  model_specs <- list(
    td_only  = list(taildown_type = "spherical"),
    tu_td    = list(tailup_type   = "exponential",
                    taildown_type = "spherical"),
    tu_td_eu = list(tailup_type   = "exponential",
                    taildown_type = "spherical",
                    euclid_type   = "gaussian")
  )

  fitted_models <- list()

  for (mod_name in names(model_specs)) {
    spec <- model_specs[[mod_name]]
    message("    Fitting: ", mod_name)

    fitted_models[[mod_name]] <- tryCatch(
      do.call(ssn_glm, c(
        list(formula    = formula_mod,
             family     = "binomial",
             ssn.object = ssn_obj_sp,
             additive   = "afvArea",
             estmethod  = "ml"),
        spec
      )),
      error = function(e) {
        message("      Failed: ", e$message)
        NULL
      }
    )
  }

  fitted_models <- Filter(Negate(is.null), fitted_models)

  if (length(fitted_models) == 0) {
    message("  All models failed for ", sp)
    next
  }

  # Model comparison
  if (length(fitted_models) == 1) {
    model_comparison <- glance(fitted_models[[1]])
  } else {
    model_comparison <- tryCatch(
      do.call(glances, fitted_models),
      error = function(e) {
        data.frame(
          model = names(fitted_models),
          AICc  = sapply(fitted_models, function(m) glance(m)$AICc)
        )
      }
    )
  }
  print(model_comparison)

  # Select best covariance structure by AICc
  best_name <- names(fitted_models)[which.min(
    sapply(fitted_models, function(m) glance(m)$AICc)
  )]
  best_model <- fitted_models[[best_name]]
  message("  Best covariance: ", best_name)

  # Check for separation
  se_max <- max(abs(tidy(best_model)$std.error), na.rm = TRUE)
  if (se_max > 1e6)
    message("  WARNING: Very large standard errors — possible separation")

  # loocv RMSPE
  rmspe <- tryCatch(loocv(best_model)$RMSPE, error = function(e) NA)
  message("  loocv RMSPE: ", round(rmspe, 4))

  # ---- Step 15: Refit with REML, predict, save ----

  best_spec <- model_specs[[best_name]]

  best_model_reml <- tryCatch(
    do.call(ssn_glm, c(
      list(formula    = formula_mod,
           family     = "binomial",
           ssn.object = ssn_obj_sp,
           additive   = "afvArea",
           estmethod  = "reml"),
      best_spec
    )),
    error = function(e) {
      message("  REML refit failed — keeping ML model: ", e$message)
      best_model
    }
  )

  saveRDS(best_model_reml,
          paste0("sdm/ssn_models/ssn_", sp, ".rds"))
  message("  Model saved")

  # Predict across full basin
  preds_out <- tryCatch(
    augment(best_model_reml, newdata = "pred_basin"),
    error = function(e) { message("  Prediction failed: ", e$message); NULL }
  )

  if (!is.null(preds_out)) {
    pred_df <- preds_out %>%
      st_drop_geometry() %>%
      select(subc_id, .fitted) %>%
      rename(suitability = .fitted) %>%
      mutate(
        probability = round(1 / (1 + exp(-suitability)), 4),
        species     = sp,
        best_cov    = best_name
      )

    fwrite(pred_df, paste0("sdm/predictions/pred_", sp, ".csv"))
    message("  Predictions saved: ", nrow(pred_df), " basin subcatchments")
    message("  Probability range: ", round(min(pred_df$probability), 3),
            " — ", round(max(pred_df$probability), 3))
  }


  # ---- Evaluate SSN: AUC + TSS + MCC (in-sample fitted values) ----
  # Note: in-sample evaluation — optimistic upper bound on performance
  # Cross-validation not feasible due to computational cost of SSN fitting
  fitted_probs <- 1 / (1 + exp(-fitted(best_model_reml)))
  obs_data_eval <- ssn_get_data(ssn_obj_sp, "obs") %>%
    st_drop_geometry()

  pres_probs <- fitted_probs[obs_data_eval$pres_abs == 1]
  abs_probs  <- fitted_probs[obs_data_eval$pres_abs == 0]

  # AUC + TSS threshold using dismo
  eval_obj       <- dismo::evaluate(p = pres_probs, a = abs_probs)
  auc_ssn        <- eval_obj@auc
  thresh_ssn_tss <- dismo::threshold(eval_obj)$spec_sens

  sensitivity_ssn <- mean(pres_probs >= thresh_ssn_tss)
  specificity_ssn <- mean(abs_probs  <  thresh_ssn_tss)
  tss_ssn         <- sensitivity_ssn + specificity_ssn - 1

  # MCC threshold
  mcc_result_ssn  <- compute_mcc_threshold(pres_probs, abs_probs)
  thresh_ssn_mcc  <- mcc_result_ssn$threshold
  mcc_ssn         <- mcc_result_ssn$mcc

  message("  AUC (in-sample):           ", round(auc_ssn, 3))
  message("  TSS (in-sample):           ", round(tss_ssn, 3),
          " | threshold: ",               round(thresh_ssn_tss, 3))
  message("  MCC threshold (in-sample): ", round(thresh_ssn_mcc, 3),
          " | MCC: ",                     round(mcc_ssn, 3))
  message("  loocv RMSPE:               ", round(rmspe, 4))


  ssn_results[[sp]] <- list(
    best_cov         = best_name,
    n_presences      = n_pres,
    n_true_absences  = sum(sp_data$source == "HCMR_true_absence"),
    n_pseudoabsences = sum(sp_data$source == "pseudoabsence"),
    AICc             = round(glance(best_model_reml)$AICc, 2),
    loocv_RMSPE      = round(rmspe, 4),
    AUC              = round(auc_ssn, 3),
    TSS              = round(tss_ssn, 3),
    best_threshold_tss = round(thresh_ssn_tss, 3),
    MCC              = round(mcc_ssn, 3),
    best_threshold_mcc = round(thresh_ssn_mcc, 3)
  )


  rm(ssn_obj_sp, fitted_models, best_model, best_model_reml,
     obs_sites_sp, obs_snapped_sp, site_list_sp)
  gc()
}

# ============================================================
# STEP 16: Join subbasin predictions to network gpkg + save summary
# and metrics
# ============================================================

message("\n=== Step 16: Saving outputs ===")

# Join predictions to subbasin stream network for QGIS visualisation
# Predictions cover full basin but we display subbasin only
network <- st_read("spatial/subbasin/stream_network_pruned.gpkg")

pred_files <- list.files("sdm/predictions",
                         pattern = "pred_.*\\.csv$",
                         full.names = TRUE)

for (f in pred_files) {
  pred   <- fread(f)
  sp     <- unique(pred$species)
  col    <- paste0("prob_", sp)

  pred_clean <- pred %>%
    filter(subc_id %in% subbasin_subc_ids) %>%
    distinct(subc_id, .keep_all = TRUE) %>%
    select(subc_id, probability) %>%
    rename(!!col := probability)

  network <- network %>%
    left_join(pred_clean, by = "subc_id")

  message("  Joined: ", col)
}

# Join predictor values for QGIS visualisation
network <- network %>%
  left_join(
    predict_table %>% select(subc_id, all_of(FORMULA_PREDICTORS)),
    by = "subc_id"
  )

st_write(network,
         "spatial/subbasin/stream_network_predictions.gpkg",
         delete_dsn = TRUE)
message("  Saved: spatial/subbasin/stream_network_predictions.gpkg")

# Model summary
results_summary <- lapply(names(ssn_results), function(sp) {
  r <- ssn_results[[sp]]
  data.frame(
    species            = sp,
    n_presences        = r$n_presences,
    n_true_absences    = r$n_true_absences,
    n_pseudoabsences   = r$n_pseudoabsences,
    best_cov           = r$best_cov,
    AICc               = r$AICc,
    loocv_RMSPE        = r$loocv_RMSPE,
    AUC                = r$AUC,
    TSS                = r$TSS,
    best_threshold_tss = r$best_threshold_tss,
    MCC                = r$MCC,
    best_threshold_mcc = r$best_threshold_mcc
  )
}) %>% rbindlist()

print(results_summary)
fwrite(results_summary, "sdm/ssn_models/model_summary.csv")
message("  Saved: sdm/ssn_models/model_summary.csv")

target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>% unique()

# Exclude Chondrostoma — SSN not fitted
ssn_species <- target_species[target_species != "Chondrostoma_ohridanum" &
                                target_species != "Chondrostoma_ohridana"]

coef_list  <- list()
fit_list   <- list()
cov_list   <- list()
cv_list    <- list()

for (sp in ssn_species) {

  message("Processing: ", sp)

  mod_file <- paste0("sdm/ssn_models/ssn_", sp, ".rds")
  if (!file.exists(mod_file)) {
    message("  Model file not found — skipping")
    next
  }

  mod <- readRDS(mod_file)

  # 1. Fixed-effect coefficients + SEs
  coef_list[[sp]] <- tidy(mod) %>%
    mutate(species = sp) %>%
    select(species, term, estimate, std.error, statistic, p.value)

  # 2. Model fit statistics
  fit_list[[sp]] <- glance(mod) %>%
    mutate(species = sp) %>%
    select(species, n, AIC, AICc, BIC, logLik, deviance, pseudo.r.squared)

  # 3. Covariance parameters
  cov_raw <- coef(mod, type = "ssn")

  cov_params <- rbindlist(lapply(names(cov_raw), function(component) {
    vals <- cov_raw[[component]]
    data.frame(
      species   = sp,
      component = component,
      parameter = names(vals),
      estimate  = as.numeric(vals)
    )
  }))

  cov_list[[sp]] <- cov_params

  # 4. loocv RMSPE
  cv <- tryCatch(
    loocv(mod),
    error = function(e) { message("  loocv failed: ", e$message); NULL }
  )

  if (!is.null(cv)) {
    cv_list[[sp]] <- data.frame(
      species     = sp,
      loocv_RMSPE = round(cv$RMSPE, 4)
    )
    message("  loocv RMSPE: ", round(cv$RMSPE, 4))
  }
}

# Combine and save
coef_df <- rbindlist(coef_list)
fit_df  <- rbindlist(fit_list)
cov_df  <- rbindlist(cov_list)
cv_df   <- rbindlist(cv_list)

fwrite(coef_df, "sdm/ssn_models/ssn_coefficients.csv")
fwrite(fit_df,  "sdm/ssn_models/ssn_model_fit.csv")
fwrite(cov_df,  "sdm/ssn_models/ssn_covariance_params.csv")
fwrite(cv_df,   "sdm/ssn_models/ssn_loocv.csv")



message("\n", paste(rep("=", 60), collapse = ""))
message("SSN MODELLING COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("\nTraining extent:   full Vjosa/Aoos basin")
message("Prediction extent: full basin (pred saved) + subbasin (gpkg)")
message("\nOutputs:")
message("  spatial/basin/ssn/{species}.ssn")
message("  sdm/ssn_models/ssn_{species}.rds")
message("  sdm/ssn_models/torgegram_{species}.png")
message("  sdm/ssn_models/model_summary.csv")
message("  sdm/predictions/pred_{species}.csv  (full basin)")
message("  spatial/subbasin/stream_network_predictions.gpkg  (subbasin)")
message("\nNext: 05_maxent.R, 06_random_forest.R, 07_ensemble.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 05_maxent.R
#
# Fit MaxEnt species distribution models for freshwater fish species
# in the Vjosa/Aoos basin using the maxnet package.
#
# Approach:
#   - Presence-only modeling (HCMR + GBIF presences from full basin)
#   - Background: all basin subcatchments
#   - Predictors: all VIF-filtered environmental variables
#   - Default regularization (beta = 1.0) and feature classes
#   - Evaluation: AUC + TSS + MCC
#     * Jackknife (leave-one-out) for species with < 10 presences
#     * Random 20% hold-out for species with >= 10 presences
#   - Variable importance: permutation-based AUC drop
#   - Prediction: full basin → subbasin extracted for gpkg
#
# Input:
#   - points_snapped/basin/fish_sdm_basin.csv
#   - spatial/basin/basin_subc_ids_pruned.csv  (background)
#   - env90m/predict_table_vif.csv
#   - points_original/fish/species_list_sarantaporos.txt
#   - spatial/subbasin/stream_network_pruned.gpkg  (for output gpkg)
#   - spatial/subbasin/subbasin_subc_ids_pruned.csv
#
# Output:
#   - sdm/maxent_models/maxent_{species}.rds
#   - sdm/maxent_models/maxent_evaluation.csv
#   - sdm/maxent_models/maxent_variable_importance.csv
#   - sdm/predictions/pred_maxent_{species}.csv  (full basin)
#   - spatial/subbasin/stream_network_predictions_maxent.gpkg
#
# LOCATION: workflows/07_sdm/05_maxent.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(maxnet)
library(data.table)
library(dplyr)
library(sf)

select <- dplyr::select

# ============================================================
# HELPER FUNCTIONS
# ============================================================

# Compute MCC across thresholds
# pres_preds: predicted probabilities at presence/test sites
# abs_preds:  predicted probabilities at absence/background sites
compute_mcc_threshold <- function(pres_preds, abs_preds,
                                  thresh_seq = seq(0, 1, by = 0.01)) {
  mcc_vals <- sapply(thresh_seq, function(t) {
    TP <- as.numeric(sum(pres_preds >= t, na.rm = TRUE))
    FN <- as.numeric(sum(pres_preds <  t, na.rm = TRUE))
    FP <- as.numeric(sum(abs_preds  >= t, na.rm = TRUE))
    TN <- as.numeric(sum(abs_preds  <  t, na.rm = TRUE))
    denom <- sqrt((TP+FP) * (TP+FN) * (TN+FP) * (TN+FN))
    if (is.na(denom) || denom == 0) return(NA)
    (TP * TN - FP * FN) / denom
  })
  best_thresh <- thresh_seq[which.max(mcc_vals)]
  best_mcc    <- max(mcc_vals, na.rm = TRUE)
  return(list(threshold = best_thresh, mcc = best_mcc))
}

# Compute MaxEnt variable importance via permutation
# Permutes each predictor in background data and measures AUC drop
# Positive importance: predictor improves discrimination
# Negative importance: permuting predictor improves AUC (predictor adds noise)
compute_maxent_varimp <- function(mod, pres_data, bg_data,
                                  predictor_cols, n_permut = 10) {
  pres_preds <- predict(mod,
                        newdata = as.data.frame(pres_data),
                        type    = "cloglog")
  bg_preds   <- predict(mod,
                        newdata = as.data.frame(bg_data),
                        type    = "cloglog")
  auc_base   <- mean(sapply(pres_preds, function(p) mean(p > bg_preds)))

  imp_list <- list()
  for (pred in predictor_cols) {
    auc_permut <- numeric(n_permut)
    for (k in seq_len(n_permut)) {
      bg_permut          <- as.data.frame(bg_data)
      bg_permut[[pred]]  <- sample(bg_permut[[pred]])
      bg_preds_p         <- predict(mod,
                                    newdata = bg_permut,
                                    type    = "cloglog")
      auc_permut[k]      <- mean(sapply(pres_preds,
                                        function(p) mean(p > bg_preds_p)))
    }
    imp_list[[pred]] <- data.frame(
      predictor  = pred,
      importance = round(auc_base - mean(auc_permut), 4)
    )
  }
  rbindlist(imp_list) %>% arrange(desc(importance))
}

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

dir.create("sdm/maxent_models", recursive = TRUE, showWarnings = FALSE)
dir.create("sdm/predictions",   recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Minimum presences threshold for hold-out vs jackknife evaluation
# Species with < MIN_PRES_HOLDOUT presences use jackknife (leave-one-out)
# Species with >= MIN_PRES_HOLDOUT presences use 20% random hold-out
MIN_PRES_HOLDOUT <- 10

# Hold-out proportion for species with sufficient presences
HOLDOUT_PROP <- 0.2

# Regularization multiplier (beta) — default 1.0
# Higher values = smoother, less complex response curves
BETA <- 1.0

# Number of permutations for variable importance
# Higher = more stable estimates but slower
N_PERMUT <- 10

set.seed(42)

# Target species
target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>% unique()

message("Target species (", length(target_species), "):")
for (sp in target_species) message("  ", sp)

# ============================================================
# STEP 1: Load data
# ============================================================

message("\n=== Step 1: Loading data ===")

# Presences — HCMR + GBIF from full basin
fish_basin <- fread("points_snapped/basin/fish_sdm_basin.csv") %>%
  filter(species %in% target_species)

message("  Fish records (full basin): ", nrow(fish_basin),
        " (", n_distinct(fish_basin$species), " species)")

# Environmental predictors
predict_table <- fread("env90m/predict_table_vif.csv")
message("  Predict table: ", nrow(predict_table), " subcatchments, ",
        ncol(predict_table), " columns")

# Predictor columns — all VIF-filtered variables
predictor_cols <- names(predict_table) %>% setdiff("subc_id")
message("  Predictor columns: ", length(predictor_cols))

# Basin subcatchment IDs — used as background
basin_subc_ids <- fread("spatial/basin/basin_subc_ids_pruned.csv") %>%
  pull(subc_id)

message("  Background subcatchments (full basin): ", length(basin_subc_ids))

# Subbasin subcatchment IDs — for output gpkg
subbasin_subc_ids <- fread("spatial/subbasin/subbasin_subc_ids_pruned.csv") %>%
  pull(subc_id)

# ============================================================
# STEP 2: Prepare background environmental data
# ============================================================

message("\n=== Step 2: Preparing background data ===")

# Background = all basin subcatchments with environmental data
background_env <- predict_table %>%
  filter(subc_id %in% basin_subc_ids) %>%
  select(subc_id, all_of(predictor_cols))

# Remove rows with NAs
background_env <- background_env %>%
  filter(if_all(all_of(predictor_cols), ~ !is.na(.)))

message("  Background rows after NA removal: ", nrow(background_env))

# ============================================================
# STEP 3: Species loop
# ============================================================

message("\n=== Step 3: Fitting MaxEnt models per species ===")

evaluation_list <- list()
importance_list <- list()

for (sp in target_species) {

  message("\n", paste(rep("=", 60), collapse = ""))
  message("  SPECIES: ", sp)
  message(paste(rep("=", 60), collapse = ""))

  # ---- Get presences ----
  sp_pres <- fish_basin %>%
    filter(species == sp) %>%
    distinct(subc_id) %>%
    pull(subc_id)

  n_pres <- length(sp_pres)
  message("  Presences: ", n_pres)

  if (n_pres < 3) {
    message("  Skipping — fewer than 3 presences")
    next
  }

  # ---- Join environmental data to presences ----
  pres_env <- predict_table %>%
    filter(subc_id %in% sp_pres) %>%
    select(subc_id, all_of(predictor_cols)) %>%
    filter(if_all(all_of(predictor_cols), ~ !is.na(.)))

  n_pres_env <- nrow(pres_env)
  message("  Presences with env data: ", n_pres_env)

  if (n_pres_env < 3) {
    message("  Skipping — fewer than 3 presences with environmental data")
    next
  }

  # ---- Evaluation strategy ----
  # Jackknife for species with < MIN_PRES_HOLDOUT presences
  # 20% hold-out for species with >= MIN_PRES_HOLDOUT presences
  use_jackknife <- n_pres_env < MIN_PRES_HOLDOUT
  message("  Evaluation strategy: ",
          ifelse(use_jackknife, "jackknife (leave-one-out)", "20% hold-out"))

  if (use_jackknife) {

    # ---- Jackknife evaluation ----
    # For each presence, leave it out, fit model on rest, predict on held-out
    jack_preds <- numeric(n_pres_env)

    for (i in seq_len(n_pres_env)) {

      train_pres <- pres_env[-i, ] %>% select(all_of(predictor_cols))
      test_pres  <- pres_env[i, ]  %>% select(all_of(predictor_cols))
      train_bg   <- background_env %>% select(all_of(predictor_cols))

      p_vec <- c(rep(1, nrow(train_pres)), rep(0, nrow(train_bg)))
      x_mat <- bind_rows(train_pres, train_bg) %>% as.data.frame()

      mod_i <- tryCatch(
        maxnet(p = p_vec, data = x_mat, regmult = BETA),
        error = function(e) { NULL }
      )

      if (!is.null(mod_i)) {
        jack_preds[i] <- predict(mod_i,
                                 newdata = as.data.frame(test_pres),
                                 type    = "cloglog")
      } else {
        jack_preds[i] <- NA
      }
    }

    # Full model fitted on all presences (for background predictions + importance)
    p_full <- c(rep(1, n_pres_env), rep(0, nrow(background_env)))
    x_full <- bind_rows(
      pres_env %>% select(all_of(predictor_cols)),
      background_env %>% select(all_of(predictor_cols))
    ) %>% as.data.frame()

    mod_full <- tryCatch(
      maxnet(p = p_full, data = x_full, regmult = BETA),
      error = function(e) { message("  Model fitting failed"); NULL }
    )

    if (is.null(mod_full)) next

    bg_preds <- predict(mod_full,
                        newdata = as.data.frame(
                          background_env %>% select(all_of(predictor_cols))),
                        type = "cloglog")

    # AUC — compare jackknife presence predictions vs background predictions
    auc <- mean(sapply(jack_preds[!is.na(jack_preds)], function(p) {
      mean(p > bg_preds)
    }))

    # TSS at threshold maximising sensitivity + specificity
    thresholds  <- seq(0, 1, by = 0.01)
    sensitivity <- sapply(thresholds, function(t) mean(jack_preds >= t, na.rm=TRUE))
    specificity <- sapply(thresholds, function(t) mean(bg_preds < t))
    tss_vals    <- sensitivity + specificity - 1
    best_thresh <- thresholds[which.max(tss_vals)]
    tss         <- max(tss_vals)

    message("  AUC (jackknife): ", round(auc, 3))
    message("  TSS (jackknife): ", round(tss, 3),
            " at threshold: ", round(best_thresh, 3))

    # MCC threshold — using jackknife preds vs background preds
    mcc_result <- compute_mcc_threshold(jack_preds[!is.na(jack_preds)], bg_preds)
    mcc_thresh <- mcc_result$threshold
    mcc_val    <- mcc_result$mcc
    message("  MCC threshold: ", round(mcc_thresh, 3),
            " | MCC: ", round(mcc_val, 3))

  } else {

    # ---- 20% hold-out evaluation ----
    hold_idx  <- sample(seq_len(n_pres_env),
                        size    = floor(n_pres_env * HOLDOUT_PROP),
                        replace = FALSE)
    train_idx <- setdiff(seq_len(n_pres_env), hold_idx)

    train_pres <- pres_env[train_idx, ] %>% select(all_of(predictor_cols))
    test_pres  <- pres_env[hold_idx, ]  %>% select(all_of(predictor_cols))
    train_bg   <- background_env %>% select(all_of(predictor_cols))

    p_vec <- c(rep(1, nrow(train_pres)), rep(0, nrow(train_bg)))
    x_mat <- bind_rows(train_pres, train_bg) %>% as.data.frame()

    mod_full <- tryCatch(
      maxnet(p = p_vec, data = x_mat, regmult = BETA),
      error = function(e) { message("  Model fitting failed"); NULL }
    )

    if (is.null(mod_full)) next

    # Predict on test presences + background
    test_preds <- predict(mod_full,
                          newdata = as.data.frame(test_pres),
                          type    = "cloglog")

    bg_preds <- predict(mod_full,
                        newdata = as.data.frame(train_bg),
                        type    = "cloglog")

    # AUC
    auc <- mean(sapply(test_preds, function(p) mean(p > bg_preds)))

    # TSS at threshold maximising sensitivity + specificity
    thresholds  <- seq(0, 1, by = 0.01)
    sensitivity <- sapply(thresholds, function(t) mean(test_preds >= t))
    specificity <- sapply(thresholds, function(t) mean(bg_preds < t))
    tss_vals    <- sensitivity + specificity - 1
    best_thresh <- thresholds[which.max(tss_vals)]
    tss         <- max(tss_vals)

    message("  AUC (20% hold-out): ", round(auc, 3))
    message("  TSS (20% hold-out): ", round(tss, 3),
            " at threshold: ", round(best_thresh, 3))

    # MCC threshold
    mcc_result <- compute_mcc_threshold(test_preds, bg_preds)
    mcc_thresh <- mcc_result$threshold
    mcc_val    <- mcc_result$mcc
    message("  MCC threshold: ", round(mcc_thresh, 3),
            " | MCC: ", round(mcc_val, 3))

    # Refit on all presences for final prediction + importance
    p_full <- c(rep(1, n_pres_env), rep(0, nrow(background_env)))
    x_full <- bind_rows(
      pres_env %>% select(all_of(predictor_cols)),
      background_env %>% select(all_of(predictor_cols))
    ) %>% as.data.frame()

    mod_full <- tryCatch(
      maxnet(p = p_full, data = x_full, regmult = BETA),
      error = function(e) { message("  Full model refit failed"); NULL }
    )

    if (is.null(mod_full)) next
  }

  # ---- Save model ----
  saveRDS(mod_full, paste0("sdm/maxent_models/maxent_", sp, ".rds"))
  message("  Model saved")

  # ---- Variable importance via permutation ----
  message("  Computing variable importance (", N_PERMUT, " permutations)...")

  imp_sp <- tryCatch(
    compute_maxent_varimp(
      mod            = mod_full,
      pres_data      = pres_env %>% select(all_of(predictor_cols)),
      bg_data        = background_env %>% select(all_of(predictor_cols)),
      predictor_cols = predictor_cols,
      n_permut       = N_PERMUT
    ),
    error = function(e) {
      message("  Variable importance failed: ", e$message)
      NULL
    }
  )

  if (!is.null(imp_sp)) {
    imp_sp$species <- sp
    importance_list[[sp]] <- imp_sp %>%
      select(species, predictor, importance)
    message("  Top predictor: ", imp_sp$predictor[1],
            " (", round(imp_sp$importance[1], 4), ")")
  }

  # ---- Predict across full basin ----
  basin_pred_data <- background_env %>%
    select(subc_id, all_of(predictor_cols))

  basin_preds <- predict(mod_full,
                         newdata = as.data.frame(
                           basin_pred_data %>% select(all_of(predictor_cols))),
                         type = "cloglog")

  pred_df <- data.frame(
    subc_id     = basin_pred_data$subc_id,
    probability = round(as.numeric(basin_preds), 4),
    species     = sp
  )

  fwrite(pred_df, paste0("sdm/predictions/pred_maxent_", sp, ".csv"))
  message("  Predictions saved: ", nrow(pred_df), " basin subcatchments")
  message("  Probability range: ", round(min(pred_df$probability), 3),
          " — ", round(max(pred_df$probability), 3))

  # ---- Store evaluation ----
  evaluation_list[[sp]] <- data.frame(
    species            = sp,
    n_presences        = n_pres_env,
    eval_strategy      = ifelse(use_jackknife, "jackknife", "20pct_holdout"),
    AUC                = round(auc, 3),
    TSS                = round(tss, 3),
    best_threshold_tss = round(best_thresh, 3),
    MCC                = round(mcc_val, 3),
    best_threshold_mcc = round(mcc_thresh, 3)
  )
}

# ============================================================
# STEP 4: Save evaluation table + variable importance
# ============================================================

message("\n=== Step 4: Saving evaluation table and variable importance ===")

eval_df <- rbindlist(evaluation_list)
print(eval_df)
fwrite(eval_df, "sdm/maxent_models/maxent_evaluation.csv")
message("  Saved: sdm/maxent_models/maxent_evaluation.csv")

if (length(importance_list) > 0) {
  imp_df_all <- rbindlist(importance_list)
  fwrite(imp_df_all, "sdm/maxent_models/maxent_variable_importance.csv")
  message("  Saved: sdm/maxent_models/maxent_variable_importance.csv")
}

# ============================================================
# STEP 5: Join subbasin predictions to network gpkg
# ============================================================

message("\n=== Step 5: Joining predictions to subbasin network ===")

network <- st_read("spatial/subbasin/stream_network_pruned.gpkg")

pred_files <- list.files("sdm/predictions",
                         pattern = "pred_maxent_.*\\.csv$",
                         full.names = TRUE)

for (f in pred_files) {
  pred <- fread(f)
  sp   <- unique(pred$species)
  col  <- paste0("prob_", sp)

  pred_clean <- pred %>%
    filter(subc_id %in% subbasin_subc_ids) %>%
    distinct(subc_id, .keep_all = TRUE) %>%
    select(subc_id, probability) %>%
    rename(!!col := probability)

  network <- network %>%
    left_join(pred_clean, by = "subc_id")

  message("  Joined: ", col)
}

st_write(network,
         "spatial/subbasin/stream_network_predictions_maxent.gpkg",
         delete_dsn = TRUE)
message("  Saved: spatial/subbasin/stream_network_predictions_maxent.gpkg")

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("MAXENT MODELLING COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("\nOutputs:")
message("  sdm/maxent_models/maxent_{species}.rds")
message("  sdm/maxent_models/maxent_evaluation.csv")
message("  sdm/maxent_models/maxent_variable_importance.csv")
message("  sdm/predictions/pred_maxent_{species}.csv  (full basin)")
message("  spatial/subbasin/stream_network_predictions_maxent.gpkg")
message("\nNext: 06_random_forest.R, 07_ensemble.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 06_random_forest.R
#
# Fit Random Forest species distribution models for freshwater fish species
# in the Vjosa/Aoos basin using the ranger package.
#
# Approach:
#   - Presence/absence modeling (HCMR + GBIF presences + pseudoabsences)
#   - Training extent: full basin
#   - Predictors: all VIF-filtered environmental variables
#   - Class balancing: bootstrap sample with equal presences/absences per tree
#     following Valavi et al. (2021) for low prevalence data
#   - Evaluation: AUC + TSS
#     * Jackknife (leave-one-out) for species with < 10 presences
#     * Random 20% hold-out for species with >= 10 presences
#   - Prediction: full basin → subbasin extracted for gpkg
#
# Input:
#   - sdm/input/occurr/{species}/occurr_{species}.csv  (from 03b)
#   - spatial/basin/basin_subc_ids_pruned.csv
#   - env90m/predict_table_vif.csv
#   - points_original/fish/species_list_sarantaporos.txt
#   - spatial/subbasin/stream_network_pruned.gpkg
#   - spatial/subbasin/subbasin_subc_ids_pruned.csv
#
# Output:
#   - sdm/rf_models/rf_{species}.rds
#   - sdm/rf_models/rf_evaluation.csv
#   - sdm/rf_models/rf_variable_importance.csv
#   - sdm/predictions/pred_rf_{species}.csv  (full basin)
#   - spatial/subbasin/stream_network_predictions_rf.gpkg
#
# References:
#   Valavi et al. (2021) Modelling species presence-only data with random forests
#   Ecography 44: 1731-1742
#
# LOCATION: workflows/07_sdm/06_random_forest.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(ranger)
library(data.table)
library(dplyr)
library(sf)
library(parallel)

select <- dplyr::select

# Compute MCC across thresholds
# pres_preds: predicted probabilities at presence/test sites
# abs_preds:  predicted probabilities at absence/background sites
compute_mcc_threshold <- function(pres_preds, abs_preds,
                                  thresh_seq = seq(0, 1, by = 0.01)) {
  mcc_vals <- sapply(thresh_seq, function(t) {
    TP <- as.numeric(sum(pres_preds >= t, na.rm = TRUE))
    FN <- as.numeric(sum(pres_preds <  t, na.rm = TRUE))
    FP <- as.numeric(sum(abs_preds  >= t, na.rm = TRUE))
    TN <- as.numeric(sum(abs_preds  <  t, na.rm = TRUE))
    denom <- sqrt((TP+FP) * (TP+FN) * (TN+FP) * (TN+FN))
    if (is.na(denom) || denom == 0) return(NA)
    (TP * TN - FP * FN) / denom
  })
  best_thresh <- thresh_seq[which.max(mcc_vals)]
  best_mcc    <- max(mcc_vals, na.rm = TRUE)
  return(list(threshold = best_thresh, mcc = best_mcc))
}


source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

dir.create("sdm/rf_models",  recursive = TRUE, showWarnings = FALSE)
dir.create("sdm/predictions", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Minimum presences threshold for hold-out vs jackknife evaluation
MIN_PRES_HOLDOUT <- 10

# Hold-out proportion
HOLDOUT_PROP <- 0.2

# Number of trees
N_TREES <- 1000

# Number of cores
N_CORES <- detectCores() - 6

# TSS threshold sequence
THRESH_SEQ <- seq(0, 1, by = 0.01)

set.seed(42)

# Occurrence files directory
OCCURR_DIR <- "sdm/input/occurr"

# Target species
target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>% unique()

message("Target species (", length(target_species), "):")
for (sp in target_species) message("  ", sp)

# ============================================================
# STEP 1: Load data
# ============================================================

message("\n=== Step 1: Loading data ===")

# Environmental predictors — VIF-filtered + rescaled
predict_table <- fread("env90m/predict_table_vif.csv")
message("  Predict table: ", nrow(predict_table), " subcatchments, ",
        ncol(predict_table), " columns")

predictor_cols <- names(predict_table) %>% setdiff("subc_id")
message("  Predictor columns: ", length(predictor_cols))

# Basin subcatchment IDs — for prediction
basin_subc_ids <- fread("spatial/basin/basin_subc_ids_pruned.csv") %>%
  pull(subc_id)

# Subbasin subcatchment IDs — for output gpkg
subbasin_subc_ids <- fread("spatial/subbasin/subbasin_subc_ids_pruned.csv") %>%
  pull(subc_id)

# Full basin prediction data
basin_pred_data <- predict_table %>%
  filter(subc_id %in% basin_subc_ids) %>%
  filter(if_all(all_of(predictor_cols), ~ !is.na(.)))

message("  Basin prediction rows: ", nrow(basin_pred_data))

# ============================================================
# STEP 2: Species loop
# ============================================================

message("\n=== Step 2: Fitting Random Forest models per species ===")

evaluation_list   <- list()
importance_list   <- list()

for (sp in target_species) {

  message("\n", paste(rep("=", 60), collapse = ""))
  message("  SPECIES: ", sp)
  message(paste(rep("=", 60), collapse = ""))

  # ---- Load occurrence data from 03b ----
  occurr_file <- file.path(OCCURR_DIR, sp, paste0("occurr_", sp, ".csv"))

  if (!file.exists(occurr_file)) {
    message("  Skipping — occurrence file not found: ", occurr_file)
    next
  }

  sp_data <- fread(occurr_file) %>%
    filter(if_all(all_of(predictor_cols), ~ !is.na(.))) %>%
    mutate(pres_abs = as.factor(pres_abs))

  n_pres <- sum(sp_data$pres_abs == 1)
  n_abs  <- sum(sp_data$pres_abs == 0)

  message("  Presences: ", n_pres, " | Absences: ", n_abs)

  if (n_pres < 3) {
    message("  Skipping — fewer than 3 presences")
    next
  }

  # ---- Evaluation strategy ----
  use_jackknife <- n_pres < MIN_PRES_HOLDOUT
  message("  Evaluation strategy: ",
          ifelse(use_jackknife, "jackknife (leave-one-out)", "20% hold-out"))

  # ---- Formula ----
  formula_rf <- as.formula(paste("pres_abs ~",
                                 paste(predictor_cols, collapse = " + ")))

  if (use_jackknife) {

    # ---- Jackknife evaluation ----
    # Leave one presence out, train on rest + all absences, predict on held-out
    pres_rows <- which(sp_data$pres_abs == 1)
    abs_rows  <- which(sp_data$pres_abs == 0)
    jack_preds <- numeric(length(pres_rows))

    for (i in seq_along(pres_rows)) {

      train_idx <- c(pres_rows[-i], abs_rows)
      test_row  <- pres_rows[i]

      train_data <- sp_data[train_idx, ]
      test_data  <- sp_data[test_row, ]

      n_pres_train <- sum(train_data$pres_abs == 1)

      mod_i <- tryCatch(
        ranger(
          formula        = formula_rf,
          data           = train_data %>% select(pres_abs, all_of(predictor_cols)),
          num.trees      = N_TREES,
          probability    = TRUE,
          num.threads    = N_CORES,
          case.weights   = ifelse(train_data$pres_abs == 1,
                                  1,
                                  n_pres_train / sum(train_data$pres_abs == 0)),
          seed           = 42
        ),
        error = function(e) { message("    Fold ", i, " failed: ", e$message); NULL }
      )

      if (!is.null(mod_i)) {
        jack_preds[i] <- predict(mod_i,
                                 data = test_data %>%
                                   select(all_of(predictor_cols)))$predictions[, "1"]
      } else {
        jack_preds[i] <- NA
      }
    }

    # Fit full model on all data for predictions + variable importance
    mod_full <- tryCatch(
      ranger(
        formula      = formula_rf,
        data         = sp_data %>% select(pres_abs, all_of(predictor_cols)),
        num.trees    = N_TREES,
        probability  = TRUE,
        importance   = "permutation",
        num.threads  = N_CORES,
        case.weights = ifelse(sp_data$pres_abs == 1,
                              1,
                              n_pres / n_abs),
        seed         = 42
      ),
      error = function(e) { message("  Full model failed: ", e$message); NULL }
    )

    if (is.null(mod_full)) next

    # Predictions on all absences for AUC/TSS calculation
    abs_preds <- predict(mod_full,
                         data = sp_data[abs_rows, ] %>%
                           select(all_of(predictor_cols)))$predictions[, "1"]

    # AUC
    auc <- mean(sapply(jack_preds[!is.na(jack_preds)], function(p) {
      mean(p > abs_preds)
    }))

    # TSS
    sensitivity <- sapply(THRESH_SEQ,
                          function(t) mean(jack_preds >= t, na.rm = TRUE))
    specificity <- sapply(THRESH_SEQ,
                          function(t) mean(abs_preds < t))
    tss_vals    <- sensitivity + specificity - 1
    best_thresh <- THRESH_SEQ[which.max(tss_vals)]
    tss         <- max(tss_vals)

    message("  AUC (jackknife): ", round(auc, 3))
    message("  TSS (jackknife): ", round(tss, 3),
            " at threshold: ", round(best_thresh, 3))

    # MCC threshold — using jackknife preds vs background preds
    mcc_result <- compute_mcc_threshold(
      jack_preds[!is.na(jack_preds)], bg_preds
    )
    mcc_thresh <- mcc_result$threshold
    mcc_val    <- mcc_result$mcc
    message("  MCC threshold: ", round(mcc_thresh, 3),
            " | MCC: ", round(mcc_val, 3))
    mcc_thresh  <- mcc_result$threshold
    mcc_val     <- mcc_result$mcc
    message("  MCC threshold: ", round(mcc_thresh, 3),
            " | MCC: ", round(mcc_val, 3))


  } else {

    # ---- 20% hold-out evaluation ----
    pres_rows  <- which(sp_data$pres_abs == 1)
    abs_rows   <- which(sp_data$pres_abs == 0)

    hold_pres  <- sample(pres_rows,
                         size    = floor(length(pres_rows) * HOLDOUT_PROP),
                         replace = FALSE)
    train_pres <- setdiff(pres_rows, hold_pres)
    train_idx  <- c(train_pres, abs_rows)

    train_data <- sp_data[train_idx, ]
    test_data  <- sp_data[hold_pres, ]

    n_pres_train <- sum(train_data$pres_abs == 1)
    n_abs_train  <- sum(train_data$pres_abs == 0)

    mod_eval <- tryCatch(
      ranger(
        formula      = formula_rf,
        data         = train_data %>% select(pres_abs, all_of(predictor_cols)),
        num.trees    = N_TREES,
        probability  = TRUE,
        num.threads  = N_CORES,
        case.weights = ifelse(train_data$pres_abs == 1,
                              1,
                              n_pres_train / n_abs_train),
        seed         = 42
      ),
      error = function(e) { message("  Eval model failed: ", e$message); NULL }
    )

    if (is.null(mod_eval)) next

    test_preds <- predict(mod_eval,
                          data = test_data %>%
                            select(all_of(predictor_cols)))$predictions[, "1"]

    abs_preds  <- predict(mod_eval,
                          data = sp_data[abs_rows, ] %>%
                            select(all_of(predictor_cols)))$predictions[, "1"]

    # AUC
    auc <- mean(sapply(test_preds, function(p) mean(p > abs_preds)))

    # TSS
    sensitivity <- sapply(THRESH_SEQ, function(t) mean(test_preds >= t))
    specificity <- sapply(THRESH_SEQ, function(t) mean(abs_preds < t))
    tss_vals    <- sensitivity + specificity - 1
    best_thresh <- THRESH_SEQ[which.max(tss_vals)]
    tss         <- max(tss_vals)

    message("  AUC (20% hold-out): ", round(auc, 3))
    message("  TSS (20% hold-out): ", round(tss, 3),
            " at threshold: ", round(best_thresh, 3))

    # MCC threshold
    mcc_result <- compute_mcc_threshold(test_preds, bg_preds)
    mcc_thresh <- mcc_result$threshold
    mcc_val    <- mcc_result$mcc
    message("  MCC threshold: ", round(mcc_thresh, 3),
            " | MCC: ", round(mcc_val, 3))



    # Refit on all data for final predictions + variable importance
    mod_full <- tryCatch(
      ranger(
        formula      = formula_rf,
        data         = sp_data %>% select(pres_abs, all_of(predictor_cols)),
        num.trees    = N_TREES,
        probability  = TRUE,
        importance   = "permutation",
        num.threads  = N_CORES,
        case.weights = ifelse(sp_data$pres_abs == 1,
                              1,
                              n_pres / n_abs),
        seed         = 42
      ),
      error = function(e) { message("  Full model failed: ", e$message); NULL }
    )

    if (is.null(mod_full)) next
  }

  # ---- Save model ----
  saveRDS(mod_full, paste0("sdm/rf_models/rf_", sp, ".rds"))
  message("  Model saved")

  # ---- Variable importance ----
  imp <- data.frame(
    species   = sp,
    predictor = names(mod_full$variable.importance),
    importance = as.numeric(mod_full$variable.importance)
  ) %>% arrange(desc(importance))

  importance_list[[sp]] <- imp

  message("  Top 5 predictors:")
  print(head(imp %>% select(predictor, importance), 5))

  # ---- Predict across full basin ----
  basin_preds <- predict(mod_full,
                         data = basin_pred_data %>%
                           select(all_of(predictor_cols)))$predictions[, "1"]

  pred_df <- data.frame(
    subc_id     = basin_pred_data$subc_id,
    probability = round(as.numeric(basin_preds), 4),
    species     = sp
  )

  fwrite(pred_df, paste0("sdm/predictions/pred_rf_", sp, ".csv"))
  message("  Predictions saved: ", nrow(pred_df), " basin subcatchments")
  message("  Probability range: ", round(min(pred_df$probability), 3),
          " — ", round(max(pred_df$probability), 3))

  # ---- Store evaluation ----
  evaluation_list[[sp]] <- data.frame(
    species          = sp,
    n_presences      = n_pres,
    eval_strategy    = ifelse(use_jackknife, "jackknife", "20pct_holdout"),
    AUC              = round(auc, 3),
    TSS              = round(tss, 3),
    best_threshold_tss = round(best_thresh, 3),  # rename from best_threshold
    MCC              = round(mcc_val, 3),
    best_threshold_mcc = round(mcc_thresh, 3)
  )
}

# ============================================================
# STEP 3: Save evaluation + variable importance
# ============================================================

message("\n=== Step 3: Saving outputs ===")

eval_df <- rbindlist(evaluation_list)
print(eval_df)
fwrite(eval_df, "sdm/rf_models/rf_evaluation.csv")
message("  Saved: sdm/rf_models/rf_evaluation.csv")

imp_df <- rbindlist(importance_list)
fwrite(imp_df, "sdm/rf_models/rf_variable_importance.csv")
message("  Saved: sdm/rf_models/rf_variable_importance.csv")

# ============================================================
# STEP 4: Join subbasin predictions to network gpkg
# ============================================================

message("\n=== Step 4: Joining predictions to subbasin network ===")

network <- st_read("spatial/subbasin/stream_network_pruned.gpkg")

pred_files <- list.files("sdm/predictions",
                         pattern = "pred_rf_.*\\.csv$",
                         full.names = TRUE)

for (f in pred_files) {
  pred <- fread(f)
  sp   <- unique(pred$species)
  col  <- paste0("prob_", sp)

  pred_clean <- pred %>%
    filter(subc_id %in% subbasin_subc_ids) %>%
    distinct(subc_id, .keep_all = TRUE) %>%
    select(subc_id, probability) %>%
    rename(!!col := probability)

  network <- network %>%
    left_join(pred_clean, by = "subc_id")

  message("  Joined: ", col)
}

st_write(network,
         "spatial/subbasin/stream_network_predictions_rf.gpkg",
         delete_dsn = TRUE)
message("  Saved: spatial/subbasin/stream_network_predictions_rf.gpkg")

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("RANDOM FOREST MODELLING COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("\nOutputs:")
message("  sdm/rf_models/rf_{species}.rds")
message("  sdm/rf_models/rf_evaluation.csv")
message("  sdm/rf_models/rf_variable_importance.csv")
message("  sdm/predictions/pred_rf_{species}.csv  (full basin)")
message("  spatial/subbasin/stream_network_predictions_rf.gpkg")
message("\nNext: 07_ensemble.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 07_ensemble.R
#
# Combine SSN, MaxEnt and Random Forest predictions into an ensemble
# species distribution model for freshwater fish in the Sarantaporos subbasin.
#
# Ensemble method: mean probability averaging across contributing models
#   - Alburnoides_prespensis: SSN + MaxEnt + RF (3 models)
#   - Barbus_prespensis:      SSN + MaxEnt + RF (3 models)
#   - All other species:      MaxEnt + RF (2 models)
#
# SSN excluded for most species due to perfect separation with small samples.
# Continuous probability averaging preserves gradient information for
# connectivity analyses downstream.
#
# Input:
#   - sdm/predictions/pred_{species}.csv           (SSN, full basin)
#   - sdm/predictions/pred_maxent_{species}.csv    (MaxEnt, full basin)
#   - sdm/predictions/pred_rf_{species}.csv        (RF, full basin)
#   - spatial/subbasin/subbasin_subc_ids_pruned.csv
#   - spatial/subbasin/stream_network_pruned.gpkg
#   - points_original/fish/species_list_sarantaporos.txt
#
# Output:
#   - sdm/ensemble/ensemble_{species}.csv          (full basin)
#   - sdm/ensemble/ensemble_summary.csv
#   - spatial/subbasin/stream_network_ensemble.gpkg
#
# LOCATION: workflows/07_sdm/07_ensemble.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(data.table)
library(dplyr)
library(sf)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

dir.create("sdm/ensemble", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Species where SSN is included in ensemble
# (only species without perfect separation)
SSN_SPECIES <- c("Alburnoides_prespensis", "Barbus_prespensis")

# Target species
target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>% unique()

# ============================================================
# STEP 1: Load subbasin IDs
# ============================================================

message("\n=== Step 1: Loading subbasin IDs ===")

subbasin_subc_ids <- fread("spatial/subbasin/subbasin_subc_ids_pruned.csv") %>%
  pull(subc_id)

message("  Subbasin subcatchments: ", length(subbasin_subc_ids))

# ============================================================
# STEP 2: Build ensemble per species
# ============================================================

message("\n=== Step 2: Building ensemble predictions ===")

ensemble_summary <- list()

for (sp in target_species) {

  message("\n", paste(rep("=", 60), collapse = ""))
  message("  SPECIES: ", sp)
  message(paste(rep("=", 60), collapse = ""))

  # ---- Load model predictions ----
  models_used <- character(0)
  pred_list   <- list()

  # MaxEnt
  maxent_file <- paste0("sdm/predictions/pred_maxent_", sp, ".csv")
  if (file.exists(maxent_file)) {
    pred_maxent <- fread(maxent_file) %>%
      select(subc_id, probability) %>%
      rename(prob_maxent = probability)
    pred_list[["maxent"]] <- pred_maxent
    models_used <- c(models_used, "maxent")
    message("  MaxEnt loaded: ", nrow(pred_maxent), " subcatchments")
  } else {
    message("  MaxEnt file not found — skipping")
  }

  # Random Forest
  rf_file <- paste0("sdm/predictions/pred_rf_", sp, ".csv")
  if (file.exists(rf_file)) {
    pred_rf <- fread(rf_file) %>%
      select(subc_id, probability) %>%
      rename(prob_rf = probability)
    pred_list[["rf"]] <- pred_rf
    models_used <- c(models_used, "rf")
    message("  RF loaded: ", nrow(pred_rf), " subcatchments")
  } else {
    message("  RF file not found — skipping")
  }

  # SSN — only for reliable species
  if (sp %in% SSN_SPECIES) {
    ssn_file <- paste0("sdm/predictions/pred_", sp, ".csv")
    if (file.exists(ssn_file)) {
      pred_ssn <- fread(ssn_file) %>%
        select(subc_id, probability) %>%
        rename(prob_ssn = probability)
      pred_list[["ssn"]] <- pred_ssn
      models_used <- c(models_used, "ssn")
      message("  SSN loaded: ", nrow(pred_ssn), " subcatchments")
    } else {
      message("  SSN file not found — skipping")
    }
  }

  if (length(models_used) == 0) {
    message("  No model predictions found — skipping")
    next
  }

  message("  Models in ensemble: ", paste(models_used, collapse = " + "))

  # ---- Join all model predictions by subc_id ----
  ensemble_df <- pred_list[[1]]

  for (i in seq_along(pred_list)[-1]) {
    ensemble_df <- ensemble_df %>%
      full_join(pred_list[[i]], by = "subc_id")
  }

  # ---- Compute ensemble mean probability ----
  prob_cols <- paste0("prob_", models_used)

  ensemble_df <- ensemble_df %>%
    mutate(
      ensemble_mean = rowMeans(across(all_of(prob_cols)), na.rm = TRUE),
      n_models      = rowSums(!is.na(across(all_of(prob_cols)))),
      species       = sp,
      models_used   = paste(models_used, collapse = "+")
    )

  # ---- Check coverage ----
  n_subbasin_covered <- sum(subbasin_subc_ids %in% ensemble_df$subc_id)
  message("  Subbasin coverage: ", n_subbasin_covered, " / ",
          length(subbasin_subc_ids))

  # ---- Summary stats ----
  subbasin_ens <- ensemble_df %>%
    filter(subc_id %in% subbasin_subc_ids) %>%
    pull(ensemble_mean)

  message("  Ensemble probability range (subbasin): ",
          round(min(subbasin_ens, na.rm=TRUE), 3),
          " — ", round(max(subbasin_ens, na.rm=TRUE), 3))
  message("  Ensemble median (subbasin): ",
          round(median(subbasin_ens, na.rm=TRUE), 3))

  # ---- Save full basin ensemble ----
  fwrite(ensemble_df,
         paste0("sdm/ensemble/ensemble_", sp, ".csv"))
  message("  Saved: sdm/ensemble/ensemble_", sp, ".csv")

  ensemble_summary[[sp]] <- data.frame(
    species         = sp,
    models_used     = paste(models_used, collapse = "+"),
    n_models        = length(models_used),
    n_subcs_basin   = nrow(ensemble_df),
    n_subcs_subbasin = n_subbasin_covered,
    ens_min         = round(min(subbasin_ens, na.rm=TRUE), 3),
    ens_median      = round(median(subbasin_ens, na.rm=TRUE), 3),
    ens_max         = round(max(subbasin_ens, na.rm=TRUE), 3)
  )
}

# ============================================================
# STEP 3: Save ensemble summary
# ============================================================

message("\n=== Step 3: Saving ensemble summary ===")

summary_df <- rbindlist(ensemble_summary)
print(summary_df)
fwrite(summary_df, "sdm/ensemble/ensemble_summary.csv")
message("  Saved: sdm/ensemble/ensemble_summary.csv")

# ============================================================
# STEP 4: Join ensemble predictions to subbasin network gpkg
# ============================================================

message("\n=== Step 4: Joining ensemble to subbasin network ===")

network <- st_read("spatial/subbasin/stream_network_pruned.gpkg")

ensemble_files <- list.files("sdm/ensemble",
                             pattern = "ensemble_.*\\.csv$",
                             full.names = TRUE) %>%
  .[!grepl("summary", .)] %>%
  .[!grepl("thresholds", .)]

for (f in ensemble_files) {

  ens <- fread(f)
  sp  <- unique(ens$species)
  col <- paste0("ens_", sp)

  ens_clean <- ens %>%
    filter(subc_id %in% subbasin_subc_ids) %>%
    distinct(subc_id, .keep_all = TRUE) %>%
    select(subc_id, ensemble_mean) %>%
    rename(!!col := ensemble_mean)

  network <- network %>%
    left_join(ens_clean, by = "subc_id")

  message("  Joined: ", col)
}

# Also join individual model predictions for comparison in QGIS
for (sp in target_species) {

  # MaxEnt
  f_me <- paste0("sdm/predictions/pred_maxent_", sp, ".csv")
  if (file.exists(f_me)) {
    pred <- fread(f_me) %>%
      filter(subc_id %in% subbasin_subc_ids) %>%
      distinct(subc_id, .keep_all = TRUE) %>%
      select(subc_id, probability) %>%
      rename(!!paste0("me_", sp) := probability)
    network <- network %>% left_join(pred, by = "subc_id")
  }

  # RF
  f_rf <- paste0("sdm/predictions/pred_rf_", sp, ".csv")
  if (file.exists(f_rf)) {
    pred <- fread(f_rf) %>%
      filter(subc_id %in% subbasin_subc_ids) %>%
      distinct(subc_id, .keep_all = TRUE) %>%
      select(subc_id, probability) %>%
      rename(!!paste0("rf_", sp) := probability)
    network <- network %>% left_join(pred, by = "subc_id")
  }

  # SSN (only for reliable species)
  if (sp %in% SSN_SPECIES) {
    f_ssn <- paste0("sdm/predictions/pred_", sp, ".csv")
    if (file.exists(f_ssn)) {
      pred <- fread(f_ssn) %>%
        filter(subc_id %in% subbasin_subc_ids) %>%
        distinct(subc_id, .keep_all = TRUE) %>%
        select(subc_id, probability) %>%
        rename(!!paste0("ssn_", sp) := probability)
      network <- network %>% left_join(pred, by = "subc_id")
    }
  }
}

st_write(network,
         "spatial/subbasin/stream_network_ensemble.gpkg",
         delete_dsn = TRUE)
message("  Saved: spatial/subbasin/stream_network_ensemble.gpkg")


## Ensemble threshold
ssn_eval <- fread("sdm/ssn_models/model_summary.csv") %>%
  filter(species %in% SSN_SPECIES) %>%
  select(species,
         thresh_ssn_tss = best_threshold_tss,
         thresh_ssn_mcc = best_threshold_mcc)

maxent_eval <- fread("sdm/maxent_models/maxent_evaluation.csv") %>%
  select(species,
         thresh_maxent_tss = best_threshold_tss,
         thresh_maxent_mcc = best_threshold_mcc)

rf_eval <- fread("sdm/rf_models/rf_evaluation.csv") %>%
  select(species,
         thresh_rf_tss = best_threshold_tss,
         thresh_rf_mcc = best_threshold_mcc)

# For SSN species: mean of 3 models; for others: mean of 2
ensemble_thresholds <- maxent_eval %>%
  left_join(rf_eval, by = "species") %>%
  left_join(ssn_eval, by = "species") %>%
  mutate(
    threshold_tss = case_when(
      species %in% SSN_SPECIES ~
        round((thresh_maxent_tss + thresh_rf_tss + thresh_ssn_tss) / 3, 3),
      TRUE ~
        round((thresh_maxent_tss + thresh_rf_tss) / 2, 3)
    ),
    threshold_mcc = case_when(
      species %in% SSN_SPECIES ~
        round((thresh_maxent_mcc + thresh_rf_mcc + thresh_ssn_mcc) / 3, 3),
      TRUE ~
        round((thresh_maxent_mcc + thresh_rf_mcc) / 2, 3)
    )
  )

fwrite(ensemble_thresholds, "sdm/ensemble/ensemble_thresholds.csv")


# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("ENSEMBLE COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("\nEnsemble composition:")
message("  Alburnoides_prespensis: SSN + MaxEnt + RF")
message("  Barbus_prespensis:      SSN + MaxEnt + RF")
message("  All other species:      MaxEnt + RF")
message("\nOutputs:")
message("  sdm/ensemble/ensemble_{species}.csv  (full basin)")
message("  sdm/ensemble/ensemble_summary.csv")
message("  spatial/subbasin/stream_network_ensemble.gpkg")
message("    — columns: ens_{species}, me_{species}, rf_{species},")
message("      ssn_{species} (Alburnoides + Barbus only)")
message("\nNext: check fragmentation in QGIS,")
message("  then proceed to connectivity integration")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 08_habitat_classification.R
#
# Classify continuous ensemble SDM predictions into suitable/unsuitable
# habitat for freshwater fish species in the Sarantaporos + Voidomatis
# subbasin.
#
# Approach:
#   TSS threshold applied directly to ENSEMBLE predictions (primary analysis).
#   MCC threshold applied as sensitivity analysis (Hellegers et al. 2025).
#   No IDW applied at this stage — if a specific species requires
#   spatial constraining after visual inspection, IDW can be applied
#   selectively in a separate step.
#
# Workflow per species (run twice: once for TSS, once for MCC):
#   1. Apply threshold to ensemble predictions
#   2. Fill short unsuitable gaps using fill_habitat_gaps() with sigma_mob
#   3. Remove isolated single suitable reaches (no suitable neighbours)
#   4. Output binary (0/1) and semi-binary (0 or ensemble prob) versions
#
# Threshold:
#   Two methods following Hellegers et al. (2025):
#   - TSS: species-specific mean of MaxEnt + RF + SSN thresholds
#          (SSN included for Alburnoides and Barbus only, where models
#           converged without separation; MaxEnt + RF only for all other species)
#   - MCC: same approach
#   Note: SSN thresholds are in-sample (fitted values), while MaxEnt and RF
#         thresholds are out-of-sample (held-out test data)
#
# Output gpkgs:
#   - stream_network_habitat_tss.gpkg: primary analysis (TSS threshold)
#   - stream_network_habitat_mcc.gpkg: sensitivity analysis (MCC threshold)
#   Each has 4 columns per species: bin_, semi_, gap_, isol_
#
# Gap filling:
#   Uses fill_habitat_gaps() with sigma_mob as gap threshold.
#   Optional MAX_GAP_M caps the maximum gap filled regardless of sigma_mob.
#
# Isolation filter:
#   Single suitable reaches with no suitable immediate neighbours removed.
#
# Input:
#   - spatial/subbasin/stream_network_ensemble.gpkg
#   - sdm/maxent_models/maxent_evaluation.csv
#   - sdm/rf_models/rf_evaluation.csv
#   - sdm/ssn_models/model_summary.csv  (SSN thresholds for Alburnoides + Barbus)
#   - traits/fish_dis_class.txt
#   - points_original/fish/species_list_sarantaporos.txt
#   - spatial/basin/stream_network_pruned.gpkg  (for reach length)
#
# Output:
#   - sdm/habitat/habitat_{species}.csv
#     columns: subc_id, ens_prob,
#              binary_tss, semibinary_tss, gap_filled_tss, isolated_removed_tss,
#              binary_mcc, semibinary_mcc, gap_filled_mcc, isolated_removed_mcc
#   - sdm/habitat/habitat_summary.csv
#   - spatial/subbasin/stream_network_habitat_tss.gpkg  (primary)
#   - spatial/subbasin/stream_network_habitat_mcc.gpkg  (sensitivity)
#
# LOCATION: workflows/07_sdm/08_habitat_classification.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(data.table)
library(dplyr)
library(sf)
library(igraph)
library(hydrographr)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# Source fill_habitat_gaps function
# TODO: move to hydrographr package
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/fill_habitat_gaps.R")

dir.create("sdm/habitat", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Maximum gap length to fill regardless of sigma_mob
# NULL = use only sigma_mob per species
MAX_GAP_M <- NULL

target_species <- fread("points_original/fish/species_list_sarantaporos.txt") %>%
  pull(species) %>% unique()

# ============================================================
# STEP 1: Load data
# ============================================================

message("\n=== Step 1: Loading data ===")

# Ensemble network
network_sf <- st_read("spatial/subbasin/stream_network_ensemble.gpkg")
network_dt <- network_sf %>% st_drop_geometry()

message("  Network reaches: ", nrow(network_dt))

ens_cols <- names(network_dt)[grepl("^ens_", names(network_dt))]
message("  Ensemble columns: ", paste(ens_cols, collapse = ", "))

# Build graph for gap filling and isolation filter
# Rebuild with correct subc_id → target topology
ensemble_dt <- read_geopackage(
  "spatial/subbasin/stream_network_ensemble.gpkg",
  import_as = "data.table"
)

edges_df <- ensemble_dt %>%
  select(from = subc_id, to = target) %>%
  filter(!is.na(to)) %>%
  mutate(from = as.character(from),
         to   = as.character(to))

network_g <- graph_from_data_frame(edges_df, directed = TRUE)

message("  Graph: ", vcount(network_g), " nodes, ",
        ecount(network_g), " edges")

# Add reach length as edge attribute
############ TODO: REPLACE WHEN get_stream_segments IS FIXED
basin_network_dt <- read_geopackage(
  "spatial/basin/stream_network_pruned.gpkg",
  import_as = "data.table"
) %>% select(subc_id, length)

E(network_g)$weight <- basin_network_dt$length[
  match(edges_df$from, as.character(basin_network_dt$subc_id))
]
############

# Named vector of reach lengths for fill_habitat_gaps()
edge_length <- setNames(E(network_g)$weight, edges_df$from)
cat("Edge length range (m):", range(edge_length, na.rm = TRUE), "\n")
cat("NAs in edge length:", sum(is.na(edge_length)), "\n")

# TSS + MCC thresholds — mean of MaxEnt + RF per species
# SSN excluded (RMSPE not comparable to AUC/TSS/MCC)
maxent_eval <- fread("sdm/maxent_models/maxent_evaluation.csv") %>%
  select(species,
         thresh_maxent_tss = best_threshold_tss,
         thresh_maxent_mcc = best_threshold_mcc)

rf_eval <- fread("sdm/rf_models/rf_evaluation.csv") %>%
  select(species,
         thresh_rf_tss = best_threshold_tss,
         thresh_rf_mcc = best_threshold_mcc)

ssn_eval <- fread("sdm/ssn_models/model_summary.csv") %>%
  filter(species %in% c("Alburnoides_prespensis", "Barbus_prespensis")) %>%
  select(species,
         thresh_ssn_tss = best_threshold_tss,
         thresh_ssn_mcc = best_threshold_mcc)

tss_thresholds <- maxent_eval %>%
  left_join(rf_eval, by = "species") %>%
  left_join(ssn_eval, by = "species") %>%
  mutate(
    threshold_tss = case_when(
      !is.na(thresh_ssn_tss) ~
        round((thresh_maxent_tss + thresh_rf_tss + thresh_ssn_tss) / 3, 3),
      TRUE ~
        round((thresh_maxent_tss + thresh_rf_tss) / 2, 3)
    ),
    threshold_mcc = case_when(
      !is.na(thresh_ssn_mcc) ~
        round((thresh_maxent_mcc + thresh_rf_mcc + thresh_ssn_mcc) / 3, 3),
      TRUE ~
        round((thresh_maxent_mcc + thresh_rf_mcc) / 2, 3)
    )
  )
# tss_thresholds <- maxent_eval %>%
#   left_join(rf_eval, by = "species") %>%
#   mutate(
#     threshold_tss = round((thresh_maxent_tss + thresh_rf_tss) / 2, 3),
#     threshold_mcc = round((thresh_maxent_mcc + thresh_rf_mcc) / 2, 3)
#   )

cat("\nSpecies-specific thresholds:\n")
print(tss_thresholds %>%
        select(species, thresh_maxent_tss, thresh_rf_tss, threshold_tss,
               thresh_maxent_mcc, thresh_rf_mcc, threshold_mcc))

# Dispersal distances (sigma_mob) per species
fish_dis_class <- fread("traits/fish_dis_class.txt", sep = "\t") %>%
  rename(species = species_name) %>%
  filter(species %in% target_species) %>%
  select(species, sigma_mob = distance)

cat("\nSigma_mob per species (m):\n")
print(fish_dis_class)

# ============================================================
# STEP 2: Classify habitat per species
# ============================================================

message("\n=== Step 2: Classifying habitat per species ===")

habitat_summary <- list()

for (sp in target_species) {

  ens_col <- paste0("ens_", sp)

  if (!ens_col %in% names(network_dt)) {
    message("\nSkipping ", sp, " — ensemble column not found")
    next
  }

  message("\n", paste(rep("=", 50), collapse = ""))
  message("  SPECIES: ", sp)
  message(paste(rep("=", 50), collapse = ""))

  # Get species-specific thresholds
  sp_thresh <- tss_thresholds %>% filter(species == sp)

  if (nrow(sp_thresh) == 0 || is.na(sp_thresh$threshold_tss)) {
    message("  WARNING: No thresholds found — using 0.5 for both")
    sp_threshold_tss <- 0.5
    sp_threshold_mcc <- 0.5
  } else {
    sp_threshold_tss <- sp_thresh$threshold_tss
    sp_threshold_mcc <- sp_thresh$threshold_mcc
  }

  message("  Threshold TSS: ", sp_threshold_tss)
  message("  Threshold MCC: ", sp_threshold_mcc)

  # Get sigma_mob
  sigma_mob <- fish_dis_class %>%
    filter(species == sp) %>%
    pull(sigma_mob)

  if (length(sigma_mob) == 0) {
    message("  WARNING: No sigma_mob — using 5000m default")
    sigma_mob <- 5000
  }
  message("  sigma_mob: ", round(sigma_mob), "m")

  ens_probs <- network_dt[[ens_col]]

  # ---- Inner function: run classification for one threshold ----
  run_classification <- function(threshold, label) {

    suitable_before <- as.character(
      network_dt$subc_id[ens_probs >= threshold]
    )
    message("  [", label, "] Suitable above threshold: ",
            length(suitable_before),
            " (", round(100 * length(suitable_before) / nrow(network_dt), 1),
            "%)")

    # Gap filling
    suitable_after <- fill_habitat_gaps(
      g            = network_g,
      suitable_ids = suitable_before,
      edge_length  = edge_length,
      sigma_mob    = sigma_mob,
      max_gap_m    = MAX_GAP_M
    )
    gap_filled_ids <- setdiff(as.character(suitable_after),
                              as.character(suitable_before))
    message("  [", label, "] After gap filling: ", length(suitable_after),
            " (", length(gap_filled_ids), " filled)")

    # Remove isolated single suitable reaches
    isolated <- c()
    for (node in suitable_after) {
      up_n  <- neighbors(network_g, node, mode = "in")$name
      dn_n  <- neighbors(network_g, node, mode = "out")$name
      all_n <- c(up_n, dn_n)
      if (!any(all_n %in% suitable_after)) {
        isolated <- c(isolated, node)
      }
    }
    suitable_final <- setdiff(suitable_after, isolated)
    message("  [", label, "] Isolated removed: ", length(isolated),
            " | Final: ", length(suitable_final))

    total_length_km <- sum(edge_length[suitable_final], na.rm = TRUE) / 1000
    message("  [", label, "] Final suitable length: ",
            round(total_length_km, 1), " km")

    list(
      suitable_final = suitable_final,
      gap_filled_ids = gap_filled_ids,
      isolated       = isolated,
      n_raw          = length(suitable_before),
      n_gap_filled   = length(gap_filled_ids),
      n_isolated     = length(isolated),
      n_final        = length(suitable_final),
      length_km      = round(total_length_km, 1)
    )
  }

  message("  --- TSS ---")
  res_tss <- run_classification(sp_threshold_tss, "TSS")

  message("  --- MCC ---")
  res_mcc <- run_classification(sp_threshold_mcc, "MCC")

  # ---- Build classification columns ----
  sp_habitat <- network_dt %>%
    select(subc_id, ens_prob = all_of(ens_col)) %>%
    mutate(
      # TSS threshold
      binary_tss           = as.integer(
        as.character(subc_id) %in% res_tss$suitable_final),
      semibinary_tss        = ifelse(binary_tss == 1, ens_prob, 0),
      gap_filled_tss        = as.integer(
        as.character(subc_id) %in% res_tss$gap_filled_ids),
      isolated_removed_tss  = as.integer(
        as.character(subc_id) %in% res_tss$isolated),

      # MCC threshold
      binary_mcc           = as.integer(
        as.character(subc_id) %in% res_mcc$suitable_final),
      semibinary_mcc        = ifelse(binary_mcc == 1, ens_prob, 0),
      gap_filled_mcc        = as.integer(
        as.character(subc_id) %in% res_mcc$gap_filled_ids),
      isolated_removed_mcc  = as.integer(
        as.character(subc_id) %in% res_mcc$isolated),

      species = sp
    )

  fwrite(sp_habitat, paste0("sdm/habitat/habitat_", sp, ".csv"))
  message("  Saved: sdm/habitat/habitat_", sp, ".csv")

  habitat_summary[[sp]] <- data.frame(
    species          = sp,
    threshold_tss    = sp_threshold_tss,
    threshold_mcc    = sp_threshold_mcc,
    sigma_mob_m      = round(sigma_mob),
    n_suitable_tss   = res_tss$n_final,
    n_gap_tss        = res_tss$n_gap_filled,
    n_isol_tss       = res_tss$n_isolated,
    length_km_tss    = res_tss$length_km,
    n_suitable_mcc   = res_mcc$n_final,
    n_gap_mcc        = res_mcc$n_gap_filled,
    n_isol_mcc       = res_mcc$n_isolated,
    length_km_mcc    = res_mcc$length_km
  )
}

# ============================================================
# STEP 3: Save summary
# ============================================================

message("\n=== Step 3: Saving summary ===")

summary_df <- rbindlist(habitat_summary)
print(summary_df)
fwrite(summary_df, "sdm/habitat/habitat_summary.csv")
message("  Saved: sdm/habitat/habitat_summary.csv")

# ============================================================
# STEP 4: Join to network gpkgs — one per threshold method
# ============================================================

message("\n=== Step 4: Joining to network gpkgs ===")

network_tss <- network_sf
network_mcc <- network_sf

for (sp in target_species) {

  hab_file <- paste0("sdm/habitat/habitat_", sp, ".csv")
  if (!file.exists(hab_file)) next

  hab <- fread(hab_file)

  # TSS columns
  hab_tss <- hab %>%
    select(
      subc_id,
      !!paste0("bin_",  sp) := binary_tss,
      !!paste0("semi_", sp) := semibinary_tss,
      !!paste0("gap_",  sp) := gap_filled_tss,
      !!paste0("isol_", sp) := isolated_removed_tss
    )
  network_tss <- network_tss %>% left_join(hab_tss, by = "subc_id")

  # MCC columns
  hab_mcc <- hab %>%
    select(
      subc_id,
      !!paste0("bin_",  sp) := binary_mcc,
      !!paste0("semi_", sp) := semibinary_mcc,
      !!paste0("gap_",  sp) := gap_filled_mcc,
      !!paste0("isol_", sp) := isolated_removed_mcc
    )
  network_mcc <- network_mcc %>% left_join(hab_mcc, by = "subc_id")

  message("  Joined: ", sp)
}

# Save TSS gpkg — primary analysis
st_write(network_tss,
         "spatial/subbasin/stream_network_habitat_tss.gpkg",
         delete_dsn = TRUE)
message("  Saved: spatial/subbasin/stream_network_habitat_tss.gpkg (primary)")

# Save MCC gpkg — sensitivity analysis
st_write(network_mcc,
         "spatial/subbasin/stream_network_habitat_mcc.gpkg",
         delete_dsn = TRUE)
message("  Saved: spatial/subbasin/stream_network_habitat_mcc.gpkg (sensitivity)")

message("  Columns per species in each gpkg: bin_, semi_, gap_, isol_")

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("HABITAT CLASSIFICATION COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("\nTwo threshold methods (Hellegers et al. 2025):")
message("  TSS — primary analysis: stream_network_habitat_tss.gpkg")
message("  MCC — sensitivity analysis: stream_network_habitat_mcc.gpkg")
message("\nGap filling: sigma_mob per species",
        ifelse(!is.null(MAX_GAP_M),
               paste0(" (capped at ", MAX_GAP_M, "m)"),
               " (no cap)"))
message("Isolation filter: single reaches with no suitable neighbours removed")
message("Note: IDW not applied — apply selectively per species if needed")
message("\nOutputs:")
message("  sdm/habitat/habitat_{species}.csv")
message("  sdm/habitat/habitat_summary.csv")
message("  spatial/subbasin/stream_network_habitat_tss.gpkg  (primary)")
message("  spatial/subbasin/stream_network_habitat_mcc.gpkg  (sensitivity)")
message("\nNext: 10_patch_metrics.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 07c_ensemble_prediction_map.R
# Workflow paper (Paper 1) — Module 7 figure
#
# Produces two figures:
#   Figure main  — 7-panel grid, one per species, reaches coloured by
#                  ensemble_mean probability (continuous, viridis)
#                  Grey reaches = below TSS threshold (unsuitable)
#   Figure supp  — same but binary (suitable/unsuitable) using TSS threshold
#
# READS:
#   sdm/ensemble/ensemble_<species>.csv   (subc_id, ensemble_mean, ...)
#   sdm/ensemble/ensemble_thresholds.csv  (species, threshold_tss)
#   spatial/subbasin/stream_network_pruned.gpkg
#
# WRITES:
#   figures/sdm/fig_ensemble_continuous.png   (main, 173 mm wide)
#   figures/sdm/fig_ensemble_binary.png       (supplement)
#
# Species (7): Alburnoides_prespensis, Barbus_prespensis,
#              Chondrostoma_ohridanum, Anguilla_anguilla,
#              Salmo_farioides, Squalius_platyceps, Oxynoemacheilus_pindus
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(sf)
library(dplyr)
library(data.table)
library(ggplot2)
library(patchwork)
library(scales)      # label_number for legend

source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("figures/sdm", recursive = TRUE, showWarnings = FALSE)

# ── parameters ───────────────────────────────────────────────────────────────
TARGET_SPECIES <- c(
  "Alburnoides_prespensis",
  "Barbus_prespensis",
  "Chondrostoma_ohridanum",
  "Anguilla_anguilla",
  "Salmo_farioides",
  "Squalius_platyceps",
  "Oxynoemacheilus_pindus"
)

# clean labels for panel titles (italics via expression() in ggplot)
SPECIES_LABELS <- c(
  "Alburnoides_prespensis"   = "A. prespensis",
  "Barbus_prespensis"        = "B. prespensis",
  "Chondrostoma_ohridanum"   = "C. ohridanum",
  "Anguilla_anguilla"        = "A. anguilla",
  "Salmo_farioides"          = "S. farioides",
  "Squalius_platyceps"       = "S. platyceps",
  "Oxynoemacheilus_pindus"   = "O. pindus"
)

FIG_W_MM <- 173
FIG_DPI  <- 300

COL_UNSUITABLE <- "grey88"
COL_NETWORK_BG <- "grey75"   # reaches not in prediction table (outside basin)

# ── load network geometry ────────────────────────────────────────────────────
message("Loading stream network...")
network_sf <- st_read("spatial/subbasin_sarantaporos/stream_network_pruned.gpkg",
                      quiet = TRUE) %>%
  st_transform(4326)
message("  ", nrow(network_sf), " reaches loaded.")

# ── load TSS thresholds ──────────────────────────────────────────────────────
thresholds <- fread("sdm/ensemble/ensemble_thresholds.csv")
# expects columns: species, threshold_tss

# ── shared map theme ─────────────────────────────────────────────────────────
theme_map_sdm <- function(base_size = 8) {
  theme_void(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      plot.title       = element_text(size = 7, face = "italic", hjust = 0.5,
                                      margin = margin(b = 2)),
      plot.margin      = margin(3, 3, 3, 3),
      legend.position  = "none"   # shared legend added via patchwork
    )
}

# ── build one panel per species ──────────────────────────────────────────────
make_panel_continuous <- function(sp) {
  f <- file.path("sdm/ensemble", paste0("ensemble_", sp, ".csv"))
  if (!file.exists(f)) {
    message("  WARNING: not found — ", f)
    return(NULL)
  }

  dt  <- fread(f)
  thr <- thresholds[species == sp, threshold_tss]
  if (length(thr) == 0 || is.na(thr)) thr <- 0

  # join predictions to network
  net <- network_sf %>%
    left_join(dt %>% select(subc_id, ensemble_mean), by = "subc_id") %>%
    mutate(
      suitable = !is.na(ensemble_mean) & ensemble_mean >= thr,
      prob     = if_else(suitable, ensemble_mean, NA_real_)
    )

  ggplot() +
    # background — all reaches in light grey
    geom_sf(data = net,
            colour = COL_UNSUITABLE, linewidth = 0.3) +
    # suitable reaches coloured by ensemble probability
    geom_sf(data = net %>% filter(suitable),
            aes(colour = prob), linewidth = 0.65) +
    scale_colour_viridis_c(
      name   = "Ensemble\nprobability",
      option = "viridis",
      limits = c(0, 1),
      breaks = c(0, 0.5, 1),
      labels = c("0", "0.5", "1"),
      na.value = COL_UNSUITABLE
    ) +
    labs(title = SPECIES_LABELS[sp]) +
    theme_map_sdm()
}

make_panel_binary <- function(sp) {
  f <- file.path("sdm/ensemble", paste0("ensemble_", sp, ".csv"))
  if (!file.exists(f)) return(NULL)

  dt  <- fread(f)
  thr <- thresholds[species == sp, threshold_tss]
  if (length(thr) == 0 || is.na(thr)) thr <- 0

  net <- network_sf %>%
    left_join(dt %>% select(subc_id, ensemble_mean), by = "subc_id") %>%
    mutate(suitable = !is.na(ensemble_mean) & ensemble_mean >= thr)

  n_suitable <- sum(net$suitable, na.rm = TRUE)
  pct <- round(100 * n_suitable / nrow(net), 1)

  ggplot() +
    geom_sf(data = net %>% filter(!suitable),
            colour = COL_UNSUITABLE, linewidth = 0.3) +
    geom_sf(data = net %>% filter(suitable),
            colour = "#1a7a3c", linewidth = 0.65) +
    labs(title = paste0(SPECIES_LABELS[sp], "  (", pct, "% suitable)")) +
    theme_map_sdm()
}

# ── build panel lists ────────────────────────────────────────────────────────
message("Building panels...")

panels_cont <- lapply(TARGET_SPECIES, make_panel_continuous)
names(panels_cont) <- TARGET_SPECIES
panels_cont <- Filter(Negate(is.null), panels_cont)

panels_bin  <- lapply(TARGET_SPECIES, make_panel_binary)
names(panels_bin) <- TARGET_SPECIES
panels_bin  <- Filter(Negate(is.null), panels_bin)

# ── shared colour legend ─────────────────────────────────────────────────────
# extract legend from one panel by temporarily turning it on
legend_panel <- panels_cont[[1]] +
  theme(
    legend.position  = "bottom",
    legend.title     = element_text(size = 6, face = "bold"),
    legend.text      = element_text(size = 6),
    legend.key.width = unit(12, "mm"),
    legend.key.height = unit(2.5, "mm")
  ) +
  guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))

get_legend <- function(p) {
  # extract just the legend grob from a ggplot
  gt   <- ggplot_gtable(ggplot_build(p))
  leg  <- which(sapply(gt$grobs, function(g) g$name) == "guide-box")
  if (length(leg) == 0) return(NULL)
  gt$grobs[[leg]]
}

leg_grob <- get_legend(legend_panel)

# ── assemble figure — continuous ─────────────────────────────────────────────
message("Assembling continuous figure...")

p_cont <- wrap_plots(panels_cont, ncol = 4) +
  plot_annotation(
    title    = "Ensemble SDM predictions — Sarantaporos sub-basin",
    subtitle = "Reaches coloured by ensemble probability (grey = below TSS threshold)",
    theme    = theme(
      plot.title    = element_text(size = 9,  face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 7.5, colour = "grey40", hjust = 0.5)
    )
  )

# add shared legend below
if (!is.null(leg_grob)) {
  p_cont_final <- p_cont /
    wrap_elements(leg_grob) +
    plot_layout(heights = c(20, 1))
} else {
  p_cont_final <- p_cont
}

ggsave("figures/sdm/fig_ensemble_continuous.png",
       p_cont_final,
       width  = FIG_W_MM,
       height = FIG_W_MM * 0.65,   # roughly 2 rows of 4 panels
       units  = "mm",
       dpi    = FIG_DPI,
       bg     = "white")
message("Saved: figures/sdm/fig_ensemble_continuous.png")

# ── assemble figure — binary ─────────────────────────────────────────────────
message("Assembling binary figure...")

p_bin <- wrap_plots(panels_bin, ncol = 4) +
  plot_annotation(
    title    = "Ensemble SDM predictions — suitable habitat (TSS threshold)",
    subtitle = "Green = suitable reaches · Grey = unsuitable",
    theme    = theme(
      plot.title    = element_text(size = 9,  face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 7.5, colour = "grey40", hjust = 0.5)
    )
  )

ggsave("figures/sdm/fig_ensemble_binary.png",
       p_bin,
       width  = FIG_W_MM,
       height = FIG_W_MM * 0.65,
       units  = "mm",
       dpi    = FIG_DPI,
       bg     = "white")
message("Saved: figures/sdm/fig_ensemble_binary.png")

message("\n=== Done ===")
message("figures/sdm/fig_ensemble_continuous.png")
message("figures/sdm/fig_ensemble_binary.png")

# ============================================================
# BONUS FIGURE — Barbus prespensis continuous probability (single panel)
# ============================================================

message("\nBuilding Barbus prespensis standalone figure...")

sp_barbus <- "Barbus_prespensis"
dt_barbus <- fread(file.path("sdm/ensemble",
                             paste0("ensemble_", sp_barbus, ".csv")))
thr_barbus <- thresholds[species == sp_barbus, threshold_tss]
if (length(thr_barbus) == 0 || is.na(thr_barbus)) thr_barbus <- 0

net_barbus <- network_sf %>%
  left_join(dt_barbus %>% select(subc_id, ensemble_mean), by = "subc_id") %>%
  mutate(
    suitable = !is.na(ensemble_mean) & ensemble_mean >= thr_barbus,
    prob     = if_else(suitable, ensemble_mean, NA_real_)
  )

n_suit <- sum(net_barbus$suitable, na.rm = TRUE)
pct_suit <- round(100 * n_suit / nrow(net_barbus), 1)

p_barbus <- ggplot() +
  # unsuitable reaches as grey background
  geom_sf(data = net_barbus %>% filter(!suitable),
          colour = COL_UNSUITABLE, linewidth = 0.35) +
  # suitable reaches coloured by continuous probability
  geom_sf(data = net_barbus %>% filter(suitable),
          aes(colour = prob), linewidth = 0.9) +
  scale_colour_viridis_c(
    name   = "Ensemble probability",
    option = "viridis",
    limits = c(thr_barbus, 1),
    breaks = scales::breaks_pretty(n = 4),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(
    title    = expression(italic("Barbus prespensis") ~ "— suitable habitat"),
    subtitle = paste0("Sarantaporos sub-basin · ", n_suit, " reaches suitable (",
                      pct_suit, "%) · grey = below TSS threshold")
  ) +
  theme_void(base_size = 9) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    plot.title       = element_text(size = 9, hjust = 0.5,
                                    margin = margin(b = 2)),
    plot.subtitle    = element_text(size = 7, colour = "grey40", hjust = 0.5,
                                    margin = margin(b = 4)),
    plot.margin      = margin(5, 5, 5, 5),
    legend.position  = "bottom",
    legend.title     = element_text(size = 7, face = "bold"),
    legend.text      = element_text(size = 6.5),
    legend.key.width  = unit(18, "mm"),
    legend.key.height = unit(2.5, "mm")
  ) +
  guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5))

ggsave("figures/sdm/fig_barbus_prespensis_habitat.png",
       p_barbus,
       width  = FIG_W_MM,
       height = FIG_W_MM * 0.85,
       units  = "mm",
       dpi    = FIG_DPI,
       bg     = "white")
message("Saved: figures/sdm/fig_barbus_prespensis_habitat.png")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_habitat_fragmentation_metrics.R   (Module 8 -- Habitat Fragmentation)
#
# Compute habitat patch metrics and dam-related fragmentation for freshwater
# fish species in the Sarantaporos subbasin.
#
# SDM predictions were made on Sarantaporos + Voidomatis (subbasin);
# here we trim to Sarantaporos only for connectivity analysis by filtering
# network reaches to those in subbasin_sarantaporos/subbasin_subc_ids_pruned.csv
# (produced by 01b_extract_sarantaporos_subbasin.R).
#
# Metrics organised in three groups:
#
#   GROUP A — Habitat structure (no dams)
#     A1. Total suitable habitat length per species (km)
#     A2. Number of habitat patches per species
#     A3. Inter-patch distances (along network, outer tip to outer tip)
#         + Inter-point distances (between occurrence points)
#     A4. Longest along-network path:
#           (a) between occurrence points
#           (b) between the two most distant suitable reaches
#
#   GROUP B — Dam geometry
#     Dam buffers computed once on the full Sarantaporos network.
#     B1. Along-network distance of each patch from its nearest dam
#         (current and future scenarios)
#     B2. Overlap of patches with dam impact buffers
#           upstream:   50 m  (inundation / backwater)
#           downstream: 2000 m (dewatered reach, run-of-river)
#
#   GROUP C — Fragmentation after dams
#     Uses pre-built scenario graphs (river_graph_current.RDS /
#     river_graph_future.RDS from 04_network_analyses/01_generate_network_graph.R).
#     Edges flagged as barriers (E()$barrier == TRUE, i.e. a dam on the
#     reach) are deleted for binary structural fragmentation.
#     Suitable subgraph induced on cut graph = habitat split into fragments.
#     C1. Number of fragments per species per scenario (current / future)
#     C2. Total suitable habitat (km) per fragment
#     C3. Number of occurrence points per fragment
#     C4. Species ranking by fragment increase (delta_fragments)
#
# NOTE on passability:
#   Group C fragment counts are STRUCTURAL and uniform across species:
#   every dam is treated as a cut, regardless of the species' ability to
#   pass it. The species-specific passability (0.8 / 0.5 / 0; see below)
#   enters only the population-level connectivity (PCI) step in Module 10.
#   For Salmo farioides and Anguilla anguilla (passability 0.8) the
#   structural fragment counts therefore represent a worst case relative
#   to their actual movement ability, but they still lose habitat in the
#   dewatered reach downstream of run-of-river plants.
#
# Species passability for the PCI step (exported to species_passability.csv):
#   Anguilla anguilla, Salmo farioides               -> 0.8
#   Chondrostoma ohridanum, Alburnoides prespensis,
#     Barbus prespensis                               -> 0.5
#   Oxynoemacheilus pindus, Squalius platyceps        -> 0.0
#
# Prerequisite scripts:
#   01b_extract_sarantaporos_subbasin.R               (Sarantaporos subc_ids)
#   04_network_analyses/01_generate_network_graph.R   (scenario graphs)
#   06_sdm/08_habitat_classification.R                (bin_ columns in habitat gpkg)
#
# Inputs:
#   spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv
#   spatial/subbasin_sarantaporos/stream_network_pruned.gpkg  (reach lengths)
#   spatial/subbasin/stream_network_habitat_tss.gpkg          (SDM predictions)
#   spatial/stream_networks/river_graph_current.RDS
#   spatial/stream_networks/river_graph_future.RDS
#   points_snapped/dams/dams_snapped_points.csv
#   points_snapped/basin/fish_sdm_basin.csv
#
# Outputs (all in sdm/patch_metrics/): see Step 7
#
# Figures: see 02_habitat_fragmentation_figures.R
#
# LOCATION: workflows/08_habitat_fragmentation/01_habitat_fragmentation_metrics.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(igraph)
library(sf)
library(data.table)
library(dplyr)

select <- dplyr::select

source("~/Documents/Postdoc/code/workflow_paper/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

TARGET_SPECIES <- c(
  "Alburnoides_prespensis",
  "Anguilla_anguilla",
  "Barbus_prespensis",
  "Chondrostoma_ohridanum",
  "Oxynoemacheilus_pindus",
  "Salmo_farioides",
  "Squalius_platyceps"
)

# Species passability used in the PCI step — stored here for reference/export.
# Expert-based values rescaled per supervisor: species that can largely pass
# small hydropower structures take 0.8 (not 1.0, to reflect uncertainty).
SPECIES_PASSABILITY <- c(
  Anguilla_anguilla      = 0.8,
  Salmo_farioides        = 0.8,
  Chondrostoma_ohridanum = 0.5,
  Alburnoides_prespensis = 0.5,
  Barbus_prespensis      = 0.5,
  Oxynoemacheilus_pindus = 0.0,
  Squalius_platyceps     = 0.0
)

# Dam buffer radii (metres) — see MS methods
DAM_BUFFER_UP_M   <-   50   # upstream:   inundation / backwater
DAM_BUFFER_DOWN_M <- 2000   # downstream: dewatered reach (run-of-river)

# Minimum patch size — guard against single-reach artefacts
MIN_PATCH_REACHES <- 2L

# Threshold method for SDM habitat classification
THRESHOLD_METHOD <- "tss"

# ============================================================
# SETUP
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("HABITAT FRAGMENTATION METRICS — SARANTAPOROS")
message(paste(rep("=", 80), collapse = ""))

dir.create("sdm/patch_metrics", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# STEP 1: Build Sarantaporos network graph with reach lengths
# ============================================================

message("\n=== Step 1: Building Sarantaporos network graph ===")

outlet_df <- data.frame(
  site_id   = "sarantaporos_outlet",
  longitude = 20.5897,
  latitude  = 40.070519
)

message("  Getting outlet subc_id via api_get_ids() ...")
outlet_ids <- api_get_ids(
  points          = outlet_df,
  mode            = "local",
  colname_lon     = "longitude",
  colname_lat     = "latitude",
  colname_site_id = "site_id"
)

outlet_subc_id <- outlet_ids$subc_id[1]
message("  Outlet subc_id: ", outlet_subc_id)

message("  Loading full Vjosa pruned network ...")
vjosa_g <- read_geopackage(
  gpkg      = "spatial/basin/stream_network_pruned.gpkg",
  import_as = "graph"
)
message("  Vjosa graph: ", vcount(vjosa_g), " nodes | ",
        ecount(vjosa_g), " edges")

message("  Extracting Sarantaporos subgraph via get_catchment_graph() ...")
network_g <- get_catchment_graph(
  g        = vjosa_g,
  subc_id  = outlet_subc_id,
  mode     = "in",
  as_graph = TRUE,
  n_cores  = 1
)
message("  Sarantaporos graph: ", vcount(network_g), " nodes | ",
        ecount(network_g), " edges")

sarantaporos_ids_pruned <- fread(
  "spatial/subbasin_sarantaporos/subbasin_subc_ids_pruned.csv"
)$subc_id

network_g <- induced_subgraph(
  network_g,
  vids = V(network_g)[name %in% as.character(sarantaporos_ids_pruned)]
)
message("  After filtering to Sarantaporos: ", vcount(network_g), " nodes")

edge_df <- igraph::as_data_frame(network_g, what = "edges") %>%
  as.data.table()

network_dt <- edge_df %>%
  rename(subc_id        = from,
         target         = to,
         reach_length_m = length) %>%
  mutate(subc_id = as.integer(subc_id),
         target  = as.integer(target))

sarantaporos_ids <- network_dt$subc_id
message("  Sarantaporos reaches: ", length(sarantaporos_ids))

# Each edge represents one stream segment; use its length directly as weight.
E(network_g)$weight <- E(network_g)$length

# Load SDM habitat predictions and join bin_ columns to network_dt.
hab_gpkg <- paste0("spatial/subbasin/stream_network_habitat_",
                   THRESHOLD_METHOD, ".gpkg")
message("  Loading SDM predictions from: ", hab_gpkg)

hab_dt <- st_read(hab_gpkg, quiet = TRUE) %>%
  st_drop_geometry() %>%
  as.data.table() %>%
  filter(subc_id %in% sarantaporos_ids)

message("  SDM reaches (Sarantaporos): ", nrow(hab_dt))

bin_cols   <- names(hab_dt)[grepl("^(bin|semi|gap|isol)_", names(hab_dt))]
network_dt <- network_dt %>%
  left_join(
    hab_dt %>% select(subc_id, all_of(bin_cols)),
    by = "subc_id"
  )

network_sf <- st_read(hab_gpkg, quiet = TRUE) %>%
  filter(subc_id %in% sarantaporos_ids)

message("  Graph: ", vcount(network_g), " nodes | ",
        ecount(network_g), " edges")

# ============================================================
# STEP 2: Load scenario graphs (for fragment counting, Group C)
# ============================================================

message("\n=== Step 2: Loading scenario graphs ===")

# Scenario graphs flag dam edges via E()$barrier (TRUE where a dam sits on
# the reach), built by 04_network_analyses/01_generate_network_graph.R.
# A dam attributes to a single downstream edge, so one dam = one cut.
# We delete barrier edges for binary structural fragmentation.
river_graph_current <- readRDS("spatial/stream_networks/river_graph_current.RDS")
river_graph_future  <- readRDS("spatial/stream_networks/river_graph_future.RDS")

message("  Current: ", vcount(river_graph_current), " nodes | ",
        ecount(river_graph_current), " edges")
message("  Future:  ", vcount(river_graph_future),  " nodes | ",
        ecount(river_graph_future),  " edges")

if (!"barrier" %in% edge_attr_names(river_graph_current))
  stop("E()$barrier not found in river_graph_current — rerun ",
       "04_network_analyses/01_generate_network_graph.R")

message("  Barrier edges: current = ",
        sum(E(river_graph_current)$barrier, na.rm = TRUE),
        " | future = ",
        sum(E(river_graph_future)$barrier, na.rm = TRUE))

# ============================================================
# STEP 3: Load dam data and occurrence points
# ============================================================

message("\n=== Step 3: Loading dam data and occurrence points ===")

dams <- fread("points_snapped/dams/dams_snapped_points.csv") %>%
  filter(subc_id %in% network_dt$subc_id)

dams_current <- dams %>% filter(status == "existing")
dams_future  <- dams %>% filter(status %in% c("existing", "planned"))

message("  Dams current: ", nrow(dams_current),
        " | future: ", nrow(dams_future))

fish_pts_sub <- fread("points_snapped/basin/fish_sdm_basin.csv") %>%
  filter(subc_id %in% network_dt$subc_id)

message("  Occurrence records in Sarantaporos: ", nrow(fish_pts_sub))

# Export passability reference for the PCI step
data.table(
  species     = names(SPECIES_PASSABILITY),
  passability = SPECIES_PASSABILITY
) %>%
  fwrite("sdm/patch_metrics/species_passability.csv")
message("  Saved: sdm/patch_metrics/species_passability.csv")

# ============================================================
# STEP 4: Helper — fragment counting from scenario graph
# ============================================================

# Deletes barrier edges (a dam on the reach) from the scenario graph, then
# induces the suitable subgraph (reaches where bin_species == 1) on the cut
# graph. Weakly connected components of the resulting undirected subgraph are
# the habitat fragments after dam placement. Weak components ignore flow
# direction — correct because fish move both up- and downstream within a
# fragment.

get_fragments <- function(scenario_graph, suitable_ids_chr) {

  dam_edge_idx <- which(E(scenario_graph)$barrier)
  message("    Barrier edges deleted: ", length(dam_edge_idx))

  g_cut <- delete_edges(scenario_graph, dam_edge_idx)

  suitable_in_g <- suitable_ids_chr[
    suitable_ids_chr %in% as.character(V(g_cut)$name)
  ]

  if (length(suitable_in_g) < 1) return(NULL)

  g_cut %>%
    induced_subgraph(vids = V(.)[name %in% suitable_in_g]) %>%
    as_undirected(mode = "collapse", edge.attr.comb = "mean") %>%
    components(mode = "weak")
}

# ============================================================
# STEP 5: GROUP B — Dam buffer computation (once, all future dams)
# ============================================================

message("\n=== Step 5: Group B — Dam impact buffers ===")

network_sf_buf <- network_sf %>%
  left_join(
    network_dt %>% select(subc_id, length = reach_length_m),
    by = "subc_id"
  )

message("  Computing buffers for ", nrow(dams_future),
        " dams (future scenario — worst case) ...")
message("  Upstream: ", DAM_BUFFER_UP_M,
        " m | Downstream: ", DAM_BUFFER_DOWN_M, " m")

buffer_dt <- tryCatch({
  get_buffer_along_the_network(
    lines_sf    = network_sf_buf,
    target_ids  = unique(as.character(dams_future$subc_id)),
    up_radius   = DAM_BUFFER_UP_M,
    down_radius = DAM_BUFFER_DOWN_M
  ) %>%
    as.data.table()
}, error = function(e) {
  message("  ERROR in get_buffer_along_the_network: ", e$message)
  NULL
})

BUF_SUBC_COL <- "subc_id"

if (!is.null(buffer_dt)) {
  message("  Buffer reaches: ", nrow(buffer_dt))
  fwrite(buffer_dt, "sdm/patch_metrics/dam_buffer_reaches.csv")
  message("  Saved: sdm/patch_metrics/dam_buffer_reaches.csv")
}

# ============================================================
# STEP 6: Species loop — Groups A, B (per species), C
# ============================================================

message("\n=== Step 6: Species loop ===")

all_patch_summary   <- list()
longest_path_rows   <- list()
dam_proximity_rows  <- list()
buffer_overlap_rows <- list()
fragment_rows       <- list()

for (sp in TARGET_SPECIES) {

  message("\n", paste(rep("-", 60), collapse = ""))
  message("  SPECIES: ", sp)

  bin_col <- paste0("bin_", sp)
  if (!bin_col %in% names(network_dt)) {
    message("  Column '", bin_col, "' not found — skipping"); next
  }

  suitable_ids <- network_dt %>%
    filter(.data[[bin_col]] == 1L) %>%
    pull(subc_id) %>%
    as.character()

  n_suitable <- length(suitable_ids)
  message("  Suitable reaches: ", n_suitable)
  if (n_suitable == 0) { message("  No suitable reaches — skipping"); next }

  # ----------------------------------------------------------
  # A1-A2: Patch identification (no dams, network_g topology)
  # ----------------------------------------------------------
  suitable_subg_undir <- network_g %>%
    induced_subgraph(vids = V(.)[name %in% suitable_ids]) %>%
    as_undirected(mode = "collapse", edge.attr.comb = "mean")

  comps <- components(suitable_subg_undir, mode = "weak")

  patch_membership <- data.table(
    species  = sp,
    subc_id  = as.integer(V(suitable_subg_undir)$name),
    patch_id = comps$membership
  ) %>%
    left_join(
      network_dt %>% select(subc_id, reach_length_m),
      by = "subc_id"
    )

  patch_summary <- patch_membership %>%
    group_by(species, patch_id) %>%
    summarise(
      n_reaches = n(),
      length_km = round(sum(reach_length_m, na.rm = TRUE) / 1000, 3),
      .groups   = "drop"
    ) %>%
    filter(n_reaches >= MIN_PATCH_REACHES) %>%
    as.data.table()

  valid_patches <- patch_summary$patch_id
  n_patches     <- length(valid_patches)
  total_len_km  <- round(sum(patch_summary$length_km), 3)

  message("  Patches (>= ", MIN_PATCH_REACHES, " reaches): ", n_patches,
          " | Total: ", total_len_km, " km")

  patch_membership <- patch_membership %>%
    filter(patch_id %in% valid_patches) %>%
    as.data.table()

  fwrite(patch_membership,
         paste0("sdm/patch_metrics/patch_membership_", sp, ".csv"))

  patch_nodes     <- patch_membership %>%
    select(patch_id, subc_id) %>%
    mutate(subc_id = as.character(subc_id)) %>%
    as.data.table()
  all_patch_nodes <- unique(patch_nodes$subc_id)

  # ----------------------------------------------------------
  # A3: Inter-patch distances
  # ----------------------------------------------------------
  dist_patch_long <- NULL
  all_pairs_dist  <- NULL

  if (n_patches >= 2 || (n_patches == 1 && length(all_patch_nodes) >= 2)) {

    message("  Computing inter-patch distances ...")

    all_pairs_dist <- igraph::distances(
      network_g,
      v    = V(network_g)[name %in% all_patch_nodes],
      to   = V(network_g)[name %in% all_patch_nodes],
      mode = "all"
    )

    if (n_patches >= 2) {

      dist_patch_long <- combn(valid_patches, 2, simplify = FALSE) %>%
        lapply(function(pair) {
          nodes_a <- patch_nodes %>% filter(patch_id == pair[1]) %>% pull(subc_id)
          nodes_b <- patch_nodes %>% filter(patch_id == pair[2]) %>% pull(subc_id)
          sub_mat <- all_pairs_dist[rownames(all_pairs_dist) %in% nodes_a,
                                    colnames(all_pairs_dist) %in% nodes_b,
                                    drop = FALSE]
          min_d   <- min(sub_mat[is.finite(sub_mat)], na.rm = TRUE)
          data.table(
            species     = sp,
            patch_a     = pair[1],
            patch_b     = pair[2],
            dist_km     = round(if (is.finite(min_d)) min_d / 1000 else NA_real_, 4),
            length_km_a = patch_summary[patch_id == pair[1], length_km],
            length_km_b = patch_summary[patch_id == pair[2], length_km]
          )
        }) %>%
        rbindlist()

      fwrite(dist_patch_long,
             paste0("sdm/patch_metrics/dist_patch_", sp, ".csv"))
      message("  Inter-patch pairs: ", nrow(dist_patch_long),
              " | Range: [",
              round(min(dist_patch_long$dist_km, na.rm = TRUE), 2), ", ",
              round(max(dist_patch_long$dist_km, na.rm = TRUE), 2), "] km")
    }
  }

  # ----------------------------------------------------------
  # A3: Inter-point distances
  # ----------------------------------------------------------
  dist_point_long <- NULL

  sp_pt_ids <- fish_pts_sub %>%
    filter(species == sp) %>%
    pull(subc_id) %>%
    unique() %>%
    as.character() %>%
    .[. %in% as.character(V(network_g)$name)]

  n_miss_pts <- fish_pts_sub %>%
    filter(species == sp) %>%
    pull(subc_id) %>%
    unique() %>%
    as.character() %>%
    setdiff(as.character(V(network_g)$name)) %>%
    length()
  if (n_miss_pts > 0)
    message("  WARNING: ", n_miss_pts,
            " occurrence subc_id(s) not in graph — excluded")

  if (length(sp_pt_ids) >= 2) {

    message("  Computing inter-point distances (",
            length(sp_pt_ids), " points) ...")

    dist_pt_mat <- igraph::distances(
      network_g,
      v    = V(network_g)[name %in% sp_pt_ids],
      to   = V(network_g)[name %in% sp_pt_ids],
      mode = "all"
    )

    dist_point_long <- combn(sp_pt_ids, 2, simplify = FALSE) %>%
      lapply(function(pair) {
        d <- dist_pt_mat[pair[1], pair[2]]
        data.table(
          species   = sp,
          subc_id_a = as.integer(pair[1]),
          subc_id_b = as.integer(pair[2]),
          dist_km   = round(if (is.finite(d)) d / 1000 else NA_real_, 4)
        )
      }) %>%
      rbindlist()

    fwrite(dist_point_long,
           paste0("sdm/patch_metrics/dist_point_", sp, ".csv"))
    message("  Inter-point pairs: ", nrow(dist_point_long),
            " | Range: [",
            round(min(dist_point_long$dist_km, na.rm = TRUE), 2), ", ",
            round(max(dist_point_long$dist_km, na.rm = TRUE), 2), "] km")
  }

  # ----------------------------------------------------------
  # A4: Longest path
  # ----------------------------------------------------------
  longest_point_km   <- NA_real_
  longest_habitat_km <- NA_real_

  if (!is.null(dist_point_long) && nrow(dist_point_long) > 0)
    longest_point_km <- max(dist_point_long$dist_km, na.rm = TRUE)

  if (!is.null(all_pairs_dist))
    longest_habitat_km <- round(
      max(all_pairs_dist[is.finite(all_pairs_dist)], na.rm = TRUE) / 1000, 3
    )

  longest_path_rows[[sp]] <- data.table(
    species            = sp,
    longest_point_km   = round(longest_point_km,   3),
    longest_habitat_km = round(longest_habitat_km, 3)
  )
  message("  Longest path — points: ", longest_point_km,
          " km | habitat: ", longest_habitat_km, " km")

  # ----------------------------------------------------------
  # B1: Along-network distance of each patch from nearest dam
  # ----------------------------------------------------------
  for (scenario_name in c("current", "future")) {

    dam_ids_chr <- if (scenario_name == "current")
      as.character(dams_current$subc_id)
    else
      as.character(dams_future$subc_id)

    if (length(dam_ids_chr) == 0 || length(all_patch_nodes) == 0) next

    dam_ids_in_g <- dam_ids_chr[
      dam_ids_chr %in% as.character(V(network_g)$name)
    ]
    if (length(dam_ids_in_g) == 0) next

    dist_to_dams <- igraph::distances(
      network_g,
      v    = V(network_g)[name %in% all_patch_nodes],
      to   = V(network_g)[name %in% dam_ids_in_g],
      mode = "all"
    )

    for (pid in valid_patches) {
      nodes_p <- patch_nodes %>% filter(patch_id == pid) %>% pull(subc_id)
      sub_d   <- dist_to_dams[rownames(dist_to_dams) %in% nodes_p, , drop = FALSE]
      min_d   <- min(sub_d[is.finite(sub_d)], na.rm = TRUE)
      dam_proximity_rows[[paste(sp, scenario_name, pid)]] <- data.table(
        species         = sp,
        scenario        = scenario_name,
        patch_id        = pid,
        patch_length_km = patch_summary[patch_id == pid, length_km],
        min_dist_dam_km = round(if (is.finite(min_d)) min_d / 1000 else NA_real_, 4)
      )
    }
  }

  # ----------------------------------------------------------
  # B2: Overlap of each patch with dam impact buffers
  # ----------------------------------------------------------
  if (!is.null(buffer_dt)) {

    up_ids   <- buffer_dt %>%
      filter(direction == "upstream") %>%
      pull(!!BUF_SUBC_COL) %>%
      as.character()
    dn_ids   <- buffer_dt %>%
      filter(direction == "downstream") %>%
      pull(!!BUF_SUBC_COL) %>%
      as.character()

    for (pid in valid_patches) {

      patch_ids_chr <- patch_membership %>%
        filter(patch_id == pid) %>%
        pull(subc_id) %>%
        as.character()

      overlap_ids    <- intersect(patch_ids_chr, union(up_ids, dn_ids))
      n_overlap      <- length(overlap_ids)

      overlap_len_km <- if (n_overlap > 0)
        network_dt %>%
        filter(subc_id %in% as.integer(overlap_ids)) %>%
        summarise(km = round(sum(reach_length_m, na.rm = TRUE) / 1000, 3)) %>%
        pull(km)
      else 0

      patch_len <- patch_summary[patch_id == pid, length_km]

      buffer_overlap_rows[[paste(sp, pid)]] <- data.table(
        species           = sp,
        patch_id          = pid,
        patch_length_km   = patch_len,
        n_reaches_overlap = n_overlap,
        overlap_length_km = overlap_len_km,
        pct_overlap       = round(100 * overlap_len_km / patch_len, 1)
      )
    }
  }

  # ----------------------------------------------------------
  # C1-C3: Fragmentation after dams using scenario graphs
  # ----------------------------------------------------------
  # Structural fragmentation is uniform across species: every dam is a cut,
  # regardless of the species' passability. The species-specific passability
  # (0.8 / 0.5 / 0) enters only the PCI step (Module 10), not here.

  for (scenario_name in c("current", "future")) {

    g_scen <- if (scenario_name == "current") river_graph_current
    else                             river_graph_future

    message("  [", scenario_name, "] Computing fragments ...")
    frags <- get_fragments(g_scen, suitable_ids)

    if (is.null(frags)) {
      message("  [", scenario_name,
              "] No suitable reaches in scenario graph — skipping")
      next
    }

    # Rebuild cut graph to retrieve node names for the reach length join
    g_cut         <- delete_edges(g_scen, which(E(g_scen)$barrier))
    suitable_in_g <- suitable_ids[suitable_ids %in% as.character(V(g_cut)$name)]
    cut_subg      <- induced_subgraph(g_cut,
                                      vids = V(g_cut)[name %in% suitable_in_g])

    frag_dt <- data.table(
      species     = sp,
      subc_id     = as.integer(V(cut_subg)$name),
      fragment_id = frags$membership
    ) %>%
      left_join(
        network_dt %>% select(subc_id, reach_length_m),
        by = "subc_id"
      )

    # Per-reach membership (subc_id -> fragment_id) for the figures script,
    # which colours reaches on the fragment maps.
    fwrite(frag_dt,
           paste0("sdm/patch_metrics/fragment_membership_",
                  scenario_name, "_", sp, ".csv"))

    frag_summary_sp <- frag_dt %>%
      group_by(fragment_id) %>%
      summarise(
        n_reaches = n(),
        length_km = round(sum(reach_length_m, na.rm = TRUE) / 1000, 3),
        .groups   = "drop"
      ) %>%
      as.data.table()

    sp_pts_chr <- fish_pts_sub %>%
      filter(species == sp) %>%
      pull(subc_id) %>%
      as.character()

    frag_summary_sp <- frag_summary_sp %>%
      left_join(
        frag_dt %>%
          mutate(has_occurrence = subc_id %in% as.integer(sp_pts_chr)) %>%
          group_by(fragment_id) %>%
          summarise(n_occurrences = sum(has_occurrence), .groups = "drop"),
        by = "fragment_id"
      ) %>%
      mutate(
        species          = sp,
        scenario         = scenario_name
      ) %>%
      as.data.table()

    fwrite(frag_summary_sp,
           paste0("sdm/patch_metrics/fragments_",
                  scenario_name, "_", sp, ".csv"))

    fragment_rows[[paste(sp, scenario_name)]] <- data.table(
      species          = sp,
      scenario         = scenario_name,
      n_fragments      = frags$no,
      total_length_km  = round(sum(frag_summary_sp$length_km), 3),
      n_occ_in_network = sum(sp_pts_chr %in% as.character(frag_dt$subc_id))
    )

    message("  [", scenario_name, "] Fragments: ", frags$no,
            " | Suitable: ",
            round(sum(frag_summary_sp$length_km), 3), " km")
  }

  # ----------------------------------------------------------
  # Species summary row
  # ----------------------------------------------------------
  all_patch_summary[[sp]] <- data.table(
    species                = sp,
    n_suitable_reaches     = n_suitable,
    total_length_km        = total_len_km,
    n_patches              = n_patches,
    median_patch_length_km = round(median(patch_summary$length_km), 3),
    max_patch_length_km    = round(max(patch_summary$length_km),    3),
    n_occ_points           = nrow(fish_pts_sub %>% filter(species == sp)),
    longest_point_km       = round(longest_point_km,   3),
    longest_habitat_km     = round(longest_habitat_km, 3),
    min_interpatch_dist_km = if (!is.null(dist_patch_long))
      round(min(dist_patch_long$dist_km, na.rm = TRUE), 3) else NA_real_,
    max_interpatch_dist_km = if (!is.null(dist_patch_long))
      round(max(dist_patch_long$dist_km, na.rm = TRUE), 3) else NA_real_,
    min_interpoint_dist_km = if (!is.null(dist_point_long))
      round(min(dist_point_long$dist_km, na.rm = TRUE), 3) else NA_real_,
    max_interpoint_dist_km = if (!is.null(dist_point_long))
      round(max(dist_point_long$dist_km, na.rm = TRUE), 3) else NA_real_
  )
}

# ============================================================
# STEP 7: Save combined outputs
# ============================================================

message("\n=== Step 7: Saving combined outputs ===")

rbindlist(all_patch_summary, fill = TRUE) %>%
  { print(.); . } %>%
  fwrite("sdm/patch_metrics/patch_summary_all.csv")
message("  Saved: sdm/patch_metrics/patch_summary_all.csv")

rbindlist(longest_path_rows, fill = TRUE) %>%
  fwrite("sdm/patch_metrics/longest_path_all.csv")
message("  Saved: sdm/patch_metrics/longest_path_all.csv")

if (length(dam_proximity_rows) > 0) {
  rbindlist(dam_proximity_rows, fill = TRUE) %>%
    fwrite("sdm/patch_metrics/dam_patch_proximity.csv")
  message("  Saved: sdm/patch_metrics/dam_patch_proximity.csv")
}

if (length(buffer_overlap_rows) > 0) {
  rbindlist(buffer_overlap_rows, fill = TRUE) %>%
    fwrite("sdm/patch_metrics/dam_buffer_overlap.csv")
  message("  Saved: sdm/patch_metrics/dam_buffer_overlap.csv")
}

frag_summary_all <- rbindlist(fragment_rows, fill = TRUE)
fwrite(frag_summary_all, "sdm/patch_metrics/fragment_summary_all.csv")
message("  Saved: sdm/patch_metrics/fragment_summary_all.csv")

# ============================================================
# STEP 8: C4 — Species impact ranking
# ============================================================

message("\n=== Step 8: Species impact ranking ===")

ranking <- frag_summary_all %>%
  filter(scenario == "current") %>%
  select(species, n_frag_current = n_fragments,
         length_current = total_length_km) %>%
  left_join(
    frag_summary_all %>%
      filter(scenario == "future") %>%
      select(species, n_frag_future = n_fragments,
             length_future = total_length_km),
    by = "species"
  ) %>%
  mutate(delta_fragments = n_frag_future - n_frag_current) %>%
  arrange(desc(delta_fragments)) %>%
  mutate(impact_rank = row_number()) %>%
  as.data.table()

print(ranking)
fwrite(ranking, "sdm/patch_metrics/species_impact_ranking.csv")
message("  Saved: sdm/patch_metrics/species_impact_ranking.csv")

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("HABITAT FRAGMENTATION METRICS COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("\nExtent:    Sarantaporos subbasin (trimmed from SDM predictions)")
message("Threshold: ", toupper(THRESHOLD_METHOD))
message("Scenario graphs: river_graph_current.RDS / river_graph_future.RDS")
message("\nNOTE — structural fragmentation (Group C) is uniform across species:")
message("  every dam is a cut. Species passability (0.8/0.5/0) enters only PCI.")
message("\nNext: 02_habitat_fragmentation_figures.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_habitat_fragmentation_figures.R
#
# Produce all figures for the habitat fragmentation analysis.
# Reads CSV outputs from 07a_habitat_fragmentation_metrics.R and the
# Sarantaporos stream network for spatial context.
#
# Figures:
#
#   Histograms (figures/patch_metrics/):
#     hist_dist_point_{species}.png   — inter-point distances, all species
#     hist_dist_patch_{species}.png   — inter-patch distances, all species
#     hist_dist_barbus_salmo_main.png — combined panel, main text figure
#
#   Maps (figures/maps/):
#     Map 1 — Habitat patches (no dams)
#       map1_patches_{species}_main.png          — Barbus + Salmo
#       map1_patches_all_species_supplementary.png
#
#     Map 2 — Dam buffer overlap
#       map2_buffer_{species}_main.png            — Barbus + Salmo
#       map2_buffer_all_species_supplementary.png
#
#     Map 3 — Habitat fragments, current vs future scenario
#       map3_fragments_{species}_main.png         — Barbus + Salmo, two-panel
#       map3_fragments_all_species_supplementary.png
#
# Inputs (from 01_habitat_fragmentation_metrics.R):
#   sdm/patch_metrics/patch_membership_{species}.csv
#   sdm/patch_metrics/dist_point_{species}.csv
#   sdm/patch_metrics/dist_patch_{species}.csv
#   sdm/patch_metrics/dam_buffer_reaches.csv
#   sdm/patch_metrics/fragments_{scenario}_{species}.csv
#   sdm/patch_metrics/species_impact_ranking.csv
#
# Network for maps:
#   spatial/subbasin_sarantaporos/stream_network_pruned.gpkg
#
# Dam points:
#   points_snapped/dams/dams_snapped_points.csv
#
# LOCATION: workflows/08_habitat_fragmentation/02_habitat_fragmentation_figures.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(sf)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

select <- dplyr::select

source("~/Documents/Postdoc/code/workflow_paper/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

TARGET_SPECIES <- c(
  "Alburnoides_prespensis",
  "Anguilla_anguilla",
  "Barbus_prespensis",
  "Chondrostoma_ohridanum",
  "Oxynoemacheilus_pindus",
  "Salmo_farioides",
  "Squalius_platyceps"
)

# Species shown in main text (others go to supplementary)
MAIN_TEXT_SPECIES <- c("Barbus_prespensis", "Salmo_farioides")

# Map colour constants
COL_UNSUITABLE <- "grey85"   # non-suitable reaches
COL_DAM_EXIST  <- "black"
COL_DAM_PLAN   <- "#E41A1C"

# ============================================================
# SETUP
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("HABITAT FRAGMENTATION FIGURES")
message(paste(rep("=", 80), collapse = ""))

dir.create("figures/patch_metrics", recursive = TRUE, showWarnings = FALSE)
dir.create("figures/maps",          recursive = TRUE, showWarnings = FALSE)

# ============================================================
# STEP 1: Load spatial data for maps
# ============================================================

message("\n=== Step 1: Loading spatial data ===")

# Sarantaporos pruned network — background for all maps
network_sf <- st_read(
  "spatial/subbasin_sarantaporos/stream_network_pruned.gpkg",
  quiet = TRUE
)
message("  Network reaches: ", nrow(network_sf))

# Dam points
dams <- fread("points_snapped/dams/dams_snapped_points.csv") %>%
  filter(subc_id %in% network_sf$subc_id)

dams_current_sf <- dams %>%
  filter(status == "existing") %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

dams_future_sf  <- dams %>%
  filter(status %in% c("existing", "planned")) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

dams_planned_sf <- dams %>%
  filter(status == "planned") %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

message("  Dams current: ", nrow(dams_current_sf),
        " | future: ", nrow(dams_future_sf))

# Fish occurrence points — shown on all fragment maps
fish_pts_sf <- fread("points_snapped/basin/fish_sdm_basin.csv") %>%
  filter(subc_id %in% network_sf$subc_id,
         !is.na(longitude_snapped), !is.na(latitude_snapped)) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

message("  Fish occurrence points in Sarantaporos: ", nrow(fish_pts_sf))

# Buffer reaches (for Map 2)
buffer_dt <- tryCatch(
  fread("sdm/patch_metrics/dam_buffer_reaches.csv"),
  error = function(e) { message("  dam_buffer_reaches.csv not found — Map 2 will be skipped"); NULL }
)
BUF_SUBC_COL <- "subc_id"

# ============================================================
# STEP 2: Shared map theme
# ============================================================

theme_map <- function(base_size = 10) {
  theme_void(base_size = base_size) +
    theme(
      plot.title      = element_text(face = "italic", size = base_size,
                                     hjust = 0.5, margin = margin(b = 4)),
      plot.subtitle   = element_text(size = base_size - 1, hjust = 0.5,
                                     colour = "grey40", margin = margin(b = 4)),
      legend.position = "bottom",
      legend.title    = element_text(size = base_size - 1),
      legend.text     = element_text(size = base_size - 2),
      legend.key.size = unit(0.4, "cm"),
      plot.margin     = margin(4, 4, 4, 4)
    )
}

# ============================================================
# STEP 3: Histograms — all species (supplementary)
# ============================================================

message("\n=== Step 3: Histograms — all species ===")

# Using png() directly to avoid ragg graphics API version mismatch
plot_dist_histograms <- function(sp,
                                 out_dir   = "figures/patch_metrics",
                                 width_in  = 7,
                                 height_in = 4) {
  sp_label <- gsub("_", " ", sp)
  f_point  <- paste0("sdm/patch_metrics/dist_point_", sp, ".csv")
  f_patch  <- paste0("sdm/patch_metrics/dist_patch_", sp, ".csv")

  if (file.exists(f_point)) {
    dp  <- fread(f_point) %>% filter(!is.na(dist_km))
    p   <- ggplot(dp, aes(x = dist_km)) +
      geom_histogram(bins = 20, fill = "#2C7BB6",
                     colour = "white", linewidth = 0.3) +
      labs(title    = paste0(sp_label, " — point-based distances"),
           subtitle = paste0("n = ", nrow(dp), " pairwise distances"),
           x = "Along-network distance (km)", y = "Count") +
      theme_bw(base_size = 11) +
      theme(plot.title = element_text(face = "italic"))
    out <- file.path(out_dir, paste0("hist_dist_point_", sp, ".png"))
    png(out, width = width_in, height = height_in, units = "in", res = 200)
    print(p); dev.off()
    message("  Saved: ", out)
  }

  if (file.exists(f_patch)) {
    dp2 <- fread(f_patch) %>% filter(!is.na(dist_km))
    p2  <- ggplot(dp2, aes(x = dist_km)) +
      geom_histogram(bins = 20, fill = "#D7191C",
                     colour = "white", linewidth = 0.3) +
      labs(title    = paste0(sp_label, " — patch-based distances"),
           subtitle = paste0("n = ", nrow(dp2), " patch pairs"),
           x = "Along-network distance between patches (km)",
           y = "Count") +
      theme_bw(base_size = 11) +
      theme(plot.title = element_text(face = "italic"))
    out2 <- file.path(out_dir, paste0("hist_dist_patch_", sp, ".png"))
    png(out2, width = width_in, height = height_in, units = "in", res = 200)
    print(p2); dev.off()
    message("  Saved: ", out2)
  }
}

for (sp in TARGET_SPECIES) plot_dist_histograms(sp)

# ============================================================
# STEP 4: Main text histogram — Barbus + Salmo, both approaches
# ============================================================

message("\n=== Step 4: Main text histogram ===")

combined_rows <- lapply(MAIN_TEXT_SPECIES, function(sp) {
  sp_label <- gsub("_", " ", sp)
  rows     <- list()
  f_point  <- paste0("sdm/patch_metrics/dist_point_", sp, ".csv")
  f_patch  <- paste0("sdm/patch_metrics/dist_patch_", sp, ".csv")
  if (file.exists(f_point))
    rows[["point"]] <- fread(f_point) %>%
    filter(!is.na(dist_km)) %>%
    mutate(species = sp_label, approach = "Point-based") %>%
    select(species, approach, dist_km)
  if (file.exists(f_patch))
    rows[["patch"]] <- fread(f_patch) %>%
    filter(!is.na(dist_km)) %>%
    mutate(species = sp_label, approach = "Patch-based") %>%
    select(species, approach, dist_km)
  rbindlist(rows, fill = TRUE)
}) %>%
  rbindlist(fill = TRUE)

if (nrow(combined_rows) > 0) {

  combined_rows <- combined_rows %>%
    mutate(approach = factor(approach,
                             levels = c("Point-based", "Patch-based")))

  p_main <- ggplot(combined_rows, aes(x = dist_km, fill = approach)) +
    geom_histogram(bins = 20, colour = "white", linewidth = 0.3,
                   position = "identity", alpha = 0.85) +
    scale_fill_manual(values = c("Point-based" = "#2C7BB6",
                                 "Patch-based"  = "#D7191C")) +
    facet_grid(approach ~ species, scales = "free_y") +
    labs(
      x        = "Along-network distance (km)",
      y        = "Count",
      title    = "Inter-reach distances: point-based vs patch-based",
      subtitle = paste0(
        "Patch-based distances span the full contiguous suitable habitat;\n",
        "point-based distances reflect only sampled occurrence locations."
      )
    ) +
    theme_bw(base_size = 11) +
    theme(
      strip.text.x    = element_text(face = "italic"),
      legend.position = "none",
      plot.subtitle   = element_text(size = 9, colour = "grey40")
    )

  out_main <- "figures/patch_metrics/hist_dist_barbus_salmo_main.png"
  png(out_main, width = 8, height = 5, units = "in", res = 200)
  print(p_main); dev.off()
  message("  Saved: ", out_main)
}

# ============================================================
# STEP 5: Map 1 — Habitat patches (no dams)
# ============================================================

message("\n=== Step 5: Map 1 — Habitat patches ===")

# Each patch gets a distinct colour from RColorBrewer Set3.
# All reaches shown in grey as background — shows full network extent.
# Existing dams = filled circles; planned dams = triangles.

make_patch_map <- function(sp) {
  sp_label <- gsub("_", " ", sp)
  pm_file  <- paste0("sdm/patch_metrics/patch_membership_", sp, ".csv")
  if (!file.exists(pm_file)) return(NULL)

  pm    <- fread(pm_file)
  n_p   <- length(unique(pm$patch_id))
  # Set3 has max 12 colours — recycle if more patches
  pal   <- rep(brewer.pal(min(max(3, n_p), 12), "Set3"), length.out = n_p)
  names(pal) <- as.character(sort(unique(pm$patch_id)))

  net_map <- network_sf %>%
    left_join(pm %>% select(subc_id, patch_id), by = "subc_id") %>%
    mutate(patch_id = as.factor(patch_id))

  ggplot() +
    geom_sf(data = network_sf,
            colour = COL_UNSUITABLE, linewidth = 0.3, show.legend = FALSE) +
    geom_sf(data = net_map %>% filter(!is.na(patch_id)),
            aes(colour = patch_id), linewidth = 0.7) +
    geom_sf(data = dams_current_sf, shape = 21,
            fill = COL_DAM_EXIST, colour = "white", size = 2, stroke = 0.4) +
    geom_sf(data = dams_planned_sf, shape = 24,
            fill = COL_DAM_PLAN,  colour = "white", size = 2, stroke = 0.4) +
    scale_colour_manual(
      values = pal, name = "Patch",
      guide  = guide_legend(nrow = 2, override.aes = list(linewidth = 2))
    ) +
    labs(title    = bquote(italic(.(sp_label))),
         subtitle = paste0(n_p, " habitat patch",
                           ifelse(n_p != 1, "es", ""))) +
    theme_map()
}

for (sp in MAIN_TEXT_SPECIES) {
  p <- make_patch_map(sp); if (is.null(p)) next
  out <- paste0("figures/maps/map1_patches_", sp, "_main.png")
  png(out, width = 6, height = 5, units = "in", res = 300)
  print(p); dev.off()
  message("  Saved: ", out)
}

patch_plots_all <- Filter(
  Negate(is.null),
  lapply(setNames(TARGET_SPECIES, TARGET_SPECIES), make_patch_map)
)
if (length(patch_plots_all) > 0) {
  n_col  <- 3
  p_supp <- wrap_plots(patch_plots_all, ncol = n_col) +
    plot_annotation(
      title   = "Habitat patches per species",
      caption = paste0("Grey: unsuitable. Coloured: patches. ",
                       "\u25cf Existing  \u25b2 Planned dams."),
      theme   = theme(
        plot.title   = element_text(face = "bold", size = 11, hjust = 0.5),
        plot.caption = element_text(size = 8, hjust = 0)
      )
    )
  out_supp <- "figures/maps/map1_patches_all_species_supplementary.png"
  png(out_supp,
      width  = n_col * 4,
      height = ceiling(length(patch_plots_all) / n_col) * 4,
      units  = "in", res = 300)
  print(p_supp); dev.off()
  message("  Saved: ", out_supp)
}

# ============================================================
# STEP 6: Map 2 — Fragments + dam buffers (combined, two-panel)
# ============================================================

message("\n=== Step 6: Map 2 — Fragment + buffer maps ===")

# Combined map: fragments and dam buffer zones shown together.
# Visual hierarchy (each layer drawn on top of the previous):
#   1. Full network — plain grey (unsuitable, outside buffer)
#   2. Buffer reaches — grey with black outline (dam-affected zone)
#   3. Fragment reaches — fragment colour with black outline (suitable habitat)
#   4. Dam points
#
# The black outline is the unifying device: it flags both buffer reaches
# and fragment reaches as "notable", while colour distinguishes fragments
# from buffer-only reaches. Plain grey reaches are neither suitable nor
# within a buffer zone.
#
# Outline effect: draw each notable layer twice —
#   first pass: slightly thicker, in black (the outline)
#   second pass: slightly thinner, in the fill colour (the line itself)

# Pre-compute buffer reach ids once (same for all species)
if (!is.null(buffer_dt)) {
  buffer_ids_chr <- buffer_dt %>%
    pull(!!BUF_SUBC_COL) %>%
    as.character() %>%
    unique()
  network_sf_buffer <- network_sf %>%
    filter(as.character(subc_id) %in% buffer_ids_chr)
} else {
  buffer_ids_chr    <- character(0)
  network_sf_buffer <- network_sf[0, ]
  message("  dam_buffer_reaches.csv not found — buffer layer omitted from maps")
}

make_fragment_panel <- function(sp, scenario_name, dams_sf_panel, title_label) {

  memb_file <- paste0("sdm/patch_metrics/fragment_membership_",
                      scenario_name, "_", sp, ".csv")
  if (!file.exists(memb_file)) return(NULL)

  frag_memb <- fread(memb_file)
  n_frags   <- max(frag_memb$fragment_id, na.rm = TRUE)

  # Set2 palette — suppressWarnings because Set2 max is 8 colours;
  # rep() handles recycling for species with more fragments
  pal <- rep(
    suppressWarnings(brewer.pal(min(max(3, n_frags), 12), "Set2")),
    length.out = n_frags
  )
  names(pal) <- as.character(seq_len(n_frags))

  # Buffer zones only shown in future scenario — the buffer was computed
  # from future dams (existing + planned). Showing it in the current
  # scenario panel would be misleading since planned dams don't exist yet.
  active_buffer_ids <- if (scenario_name == "future") buffer_ids_chr else character(0)
  active_buffer_sf  <- if (scenario_name == "future") network_sf_buffer else network_sf[0, ]

  # Fragment reaches joined to network geometry
  net_frags <- network_sf %>%
    left_join(
      frag_memb %>%
        select(subc_id, fragment_id) %>%
        mutate(fragment_id = as.factor(fragment_id)),
      by = "subc_id"
    )

  frag_ids_chr <- as.character(frag_memb$subc_id)

  # Buffer reaches that are NOT fragment reaches — grey with black outline
  network_sf_buffer_only <- active_buffer_sf %>%
    filter(!as.character(subc_id) %in% frag_ids_chr)

  # Fragment reaches inside the buffer zone — get the outline
  net_frags_in_buffer <- net_frags %>%
    filter(!is.na(fragment_id),
           as.character(subc_id) %in% active_buffer_ids)

  # Fragment reaches outside the buffer zone — no outline
  net_frags_out_buffer <- net_frags %>%
    filter(!is.na(fragment_id),
           !as.character(subc_id) %in% active_buffer_ids)

  ggplot() +

    # Layer 1: full network — grey background
    geom_sf(data = network_sf,
            colour = COL_UNSUITABLE, linewidth = 0.3, show.legend = FALSE) +

    # Layer 2a: buffer-only reaches — black outline
    geom_sf(data = network_sf_buffer_only,
            colour = "black", linewidth = 1.3, show.legend = FALSE) +
    # Layer 2b: buffer-only reaches — grey fill on top of outline
    geom_sf(data = network_sf_buffer_only,
            colour = "grey60", linewidth = 0.65, show.legend = FALSE) +

    # Layer 3a: fragment reaches outside buffer — fragment colour, no outline
    geom_sf(data = net_frags_out_buffer,
            aes(colour = fragment_id), linewidth = 0.8) +

    # Layer 3b: fragment reaches inside buffer — black outline first,
    # then fragment colour on top (outline flags dam impact on suitable habitat)
    geom_sf(data = net_frags_in_buffer,
            colour = "black", linewidth = 1.3, show.legend = FALSE) +
    geom_sf(data = net_frags_in_buffer,
            aes(colour = fragment_id), linewidth = 0.65) +

    # Layer 4: fish occurrence points — black dots
    geom_sf(data = fish_pts_sf %>% filter(species == sp),
            colour = "black", size = 1.2, shape = 19, alpha = 0.8) +

    # Layer 5: dam points — red filled triangles
    geom_sf(data = dams_sf_panel, shape = 24,
            fill = "#E41A1C", colour = "white", size = 2.5, stroke = 0.5) +

    scale_colour_manual(
      values = pal, name = "Fragment",
      guide  = guide_legend(nrow = 2, override.aes = list(linewidth = 2))
    ) +
    labs(subtitle = paste0(title_label, " — ", n_frags, " fragment",
                           ifelse(n_frags != 1, "s", ""))) +
    theme_map()
}

make_fragment_map <- function(sp) {
  sp_label  <- gsub("_", " ", sp)
  p_current <- make_fragment_panel(sp, "current", dams_current_sf, "Current")
  p_future  <- make_fragment_panel(sp, "future",  dams_future_sf,  "Future")
  if (is.null(p_current) || is.null(p_future)) return(NULL)
  (p_current | p_future) +
    plot_annotation(
      title   = bquote(italic(.(sp_label))),
      caption = paste0(
        "Coloured lines: suitable habitat fragments (isolated by dams). ",
        "Grey with outline: dam buffer zone (not suitable).\n",
        "Plain grey: unsuitable, outside buffer. ",
        "\u25b2 Dams  \u25cf Fish occurrences."
      ),
      theme = theme(
        plot.title   = element_text(face = "italic", size = 11, hjust = 0.5),
        plot.caption = element_text(size = 8, hjust = 0)
      )
    )
}

for (sp in MAIN_TEXT_SPECIES) {
  p <- make_fragment_map(sp); if (is.null(p)) next
  out <- paste0("figures/maps/map2_fragments_buffer_", sp, "_main.png")
  png(out, width = 10, height = 5, units = "in", res = 300)
  print(p); dev.off()
  message("  Saved: ", out)
}

# Supplementary: rows = species, columns = scenario (current | future)
frag_plots_supp <- list()
for (sp in TARGET_SPECIES) {
  p_cur <- make_fragment_panel(
    sp, "current", dams_current_sf,
    paste0(gsub("_", " ", sp), " — current")
  )
  p_fut <- make_fragment_panel(
    sp, "future",  dams_future_sf,
    paste0(gsub("_", " ", sp), " — future")
  )
  if (!is.null(p_cur)) frag_plots_supp[[paste0(sp, "_current")]] <- p_cur
  if (!is.null(p_fut)) frag_plots_supp[[paste0(sp, "_future")]]  <- p_fut
}

if (length(frag_plots_supp) > 0) {
  p_supp <- wrap_plots(frag_plots_supp, ncol = 2, byrow = TRUE) +
    plot_annotation(
      title   = "Habitat fragments + dam buffers per species — current vs future",
      caption = paste0(
        "Coloured: fragments. Grey with outline: dam buffer zone (future only). ",
        "Plain grey: unsuitable.\n\u25b2 Dams  \u25cf Fish occurrences."
      ),
      theme = theme(
        plot.title   = element_text(face = "bold", size = 11, hjust = 0.5),
        plot.caption = element_text(size = 8, hjust = 0)
      )
    )
  out_supp <- "figures/maps/map2_fragments_buffer_all_species_supplementary.png"
  png(out_supp,
      width  = 10,
      height = length(TARGET_SPECIES) * 4,
      units  = "in", res = 300)
  print(p_supp); dev.off()
  message("  Saved: ", out_supp)
}

# ============================================================
# STEP 7: Species impact ranking barplot
# ============================================================

message("\n=== Step 7: Species impact ranking barplot ===")

# Grouped barplot: n_fragments in current vs future scenario per species,
# ordered by delta_fragments (most impacted species first).
# Shows both the baseline fragmentation and the additional impact of
# planned dams side by side.

ranking <- fread("sdm/patch_metrics/species_impact_ranking.csv")

# Reshape to long format for grouped bars
ranking_long <- ranking %>%
  select(species, n_frag_current, n_frag_future, impact_rank) %>%
  tidyr::pivot_longer(
    cols      = c(n_frag_current, n_frag_future),
    names_to  = "scenario",
    values_to = "n_fragments"
  ) %>%
  mutate(
    scenario    = if_else(scenario == "n_frag_current", "Current", "Future"),
    scenario    = factor(scenario, levels = c("Current", "Future")),
    # Order species by impact rank (most impacted first)
    species_label = factor(
      gsub("_", " ", species),
      levels = gsub("_", " ", ranking$species[order(ranking$impact_rank)])
    )
  )

p_ranking <- ggplot(ranking_long,
                    aes(x = species_label, y = n_fragments, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  # Annotate delta_fragments above the future bar
  geom_text(
    data = ranking %>%
      mutate(species_label = factor(gsub("_", " ", species),
                                    levels = gsub("_", " ", ranking$species[order(ranking$impact_rank)]))),
    aes(x = species_label, y = n_frag_future + 0.8,
        label = paste0("+", delta_fragments)),
    inherit.aes = FALSE,
    size = 3, colour = "grey30"
  ) +
  scale_fill_manual(
    values = c("Current" = "#74add1", "Future" = "#d73027"),
    name   = "Scenario"
  ) +
  scale_x_discrete(labels = function(x) expression(italic(x))) +
  labs(
    x        = NULL,
    y        = "Number of habitat fragments",
    title    = "Habitat fragmentation per species — current vs future dam scenario",
    subtitle = "Values above bars show additional fragments created by planned dams"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x  = element_text(face = "italic", angle = 35, hjust = 1),
    legend.position = "top",
    plot.subtitle   = element_text(size = 9, colour = "grey40")
  )

out_ranking <- "figures/patch_metrics/barplot_species_impact_ranking.png"
png(out_ranking, width = 8, height = 5, units = "in", res = 200)
print(p_ranking); dev.off()
message("  Saved: ", out_ranking)

# ============================================================
# STEP 8: Patch size stacked barplot
# ============================================================

message("\n=== Step 8: Patch size stacked barplot ===")

# Stacked barplot: total suitable habitat length per species,
# each bar segment = one patch, ordered by patch size (largest at bottom).
# Shows both total habitat extent and how it is distributed across patches.

patch_summary_all <- fread("sdm/patch_metrics/patch_summary_all.csv")

# Load per-patch summaries for all species
patch_per_species <- lapply(TARGET_SPECIES, function(sp) {
  f <- paste0("sdm/patch_metrics/patch_membership_", sp, ".csv")
  if (!file.exists(f)) return(NULL)
  fread(f) %>%
    group_by(species, patch_id) %>%
    summarise(length_km = round(sum(reach_length_m, na.rm = TRUE) / 1000, 3),
              .groups = "drop") %>%
    as.data.table()
}) %>%
  rbindlist(fill = TRUE)

# Order species by total suitable length (longest first)
sp_order <- patch_summary_all %>%
  arrange(desc(total_length_km)) %>%
  pull(species) %>%
  gsub("_", " ", .)

patch_per_species <- patch_per_species %>%
  mutate(
    species_label = factor(gsub("_", " ", species), levels = sp_order),
    # Within each species, order patches largest to smallest
    patch_id      = as.character(patch_id)
  )

p_patches <- ggplot(patch_per_species,
                    aes(x = species_label, y = length_km,
                        fill = reorder(patch_id, -length_km))) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = rep(brewer.pal(12, "Set3"), length.out = nrow(patch_per_species)),
    guide  = "none"   # too many patches to show a useful legend
  ) +
  labs(
    x        = NULL,
    y        = "Suitable habitat length (km)",
    title    = "Suitable habitat per species — length and patch structure",
    subtitle = "Each bar segment = one habitat patch; ordered largest to smallest"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x  = element_text(face = "italic", angle = 35, hjust = 1),
    plot.subtitle = element_text(size = 9, colour = "grey40")
  )

out_patches <- "figures/patch_metrics/barplot_patch_sizes.png"
png(out_patches, width = 7, height = 5, units = "in", res = 200)
print(p_patches); dev.off()
message("  Saved: ", out_patches)

# ============================================================
# STEP 9: Dam proximity dotplot
# ============================================================

message("\n=== Step 9: Dam proximity dotplot ===")

# Dotplot: one dot per patch per species, x = minimum along-network
# distance to nearest dam, y = patch length (km).
# Point size = patch length; colour = scenario.
# Shows which patches are immediately threatened (dist ~ 0) vs remote.

dam_prox <- fread("sdm/patch_metrics/dam_patch_proximity.csv") %>%
  mutate(
    species_label = factor(gsub("_", " ", species),
                           levels = gsub("_", " ", TARGET_SPECIES)),
    scenario      = factor(scenario, levels = c("current", "future"),
                           labels = c("Current", "Future"))
  )

p_proximity <- ggplot(dam_prox,
                      aes(x = min_dist_dam_km, y = species_label,
                          colour = scenario, size = patch_length_km)) +
  geom_point(alpha = 0.75, position = position_jitter(height = 0.15, seed = 42)) +
  scale_colour_manual(
    values = c("Current" = "#74add1", "Future" = "#d73027"),
    name   = "Scenario"
  ) +
  scale_size_continuous(
    name   = "Patch length (km)",
    range  = c(1.5, 7),
    breaks = c(5, 20, 50, 100)
  ) +
  # Flag patches that directly overlap a dam
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  labs(
    x        = "Minimum along-network distance to nearest dam (km)",
    y        = NULL,
    title    = "Distance of habitat patches from nearest dam",
    subtitle = "Point size = patch length (km). Dashed line = patch overlaps dam reach."
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.y  = element_text(face = "italic"),
    legend.position = "right",
    plot.subtitle   = element_text(size = 9, colour = "grey40")
  )

out_proximity <- "figures/patch_metrics/dotplot_dam_proximity.png"
png(out_proximity, width = 8, height = 5, units = "in", res = 200)
print(p_proximity); dev.off()
message("  Saved: ", out_proximity)

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 60), collapse = ""))
message("HABITAT FRAGMENTATION FIGURES COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("\nHistograms:")
message("  figures/patch_metrics/hist_dist_point_{species}.png")
message("  figures/patch_metrics/hist_dist_patch_{species}.png")
message("  figures/patch_metrics/hist_dist_barbus_salmo_main.png")
message("\nSummary charts:")
message("  figures/patch_metrics/barplot_species_impact_ranking.png")
message("  figures/patch_metrics/barplot_patch_sizes.png")
message("  figures/patch_metrics/dotplot_dam_proximity.png")
message("\nMaps:")
message("  figures/maps/map1_patches_{species}_main.png")
message("  figures/maps/map1_patches_all_species_supplementary.png")
message("  figures/maps/map2_fragments_buffer_{species}_main.png")
message("  figures/maps/map2_fragments_buffer_all_species_supplementary.png")
message("\nNext: 08_pci.R")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 01_spatial_prioritization.R   (Module 9 -- Spatial Conservation Prioritization)
#
# Spatial conservation prioritization of river reaches in the Sarantaporos
# sub-basin using prioritizr, under TWO barrier scenarios.
#
# Planning units = sub-catchments. Features = per-species thresholded
# ensemble SDM predictions. Cost = Human Footprint Index. A boundary
# penalty rewards spatially connected priority networks.
#
# BARRIER-AWARE CONNECTIVITY (reviewer suggestion):
#   The boundary (connectivity) file lists which planning units are
#   adjacent. A dam breaks the connection between the reaches it sits
#   between. We therefore build the boundary file from edges that are
#   NOT blocked by a dam, separately for each scenario:
#     current : drop the single existing dam's edge
#     future  : drop all dam edges (existing + planned)
#   Habitat and cost are identical between scenarios; only the
#   connectivity changes. The shift in priority areas between scenarios
#   therefore reflects how dams reshape the connected priority network.
#   (Note: a barrier is treated as fully blocking for all species --
#   prioritizr cannot sever connectivity per species.)
#
# Targets solved: 20%, 30%, 40%, 50% (summary table for all; the
# current-vs-future comparison map is drawn at the 30% target).
#
# SOLVER GAP: the optimality gap is the solver's guarantee that the returned
#   solution is within X% of the true minimum cost. A gap-sensitivity check
#   (see block below) showed solutions are near-identical from 10% to 0%
#   (Jaccard >= 0.97 vs proven-optimal), and gap = 0.001 reproduces the
#   proven-optimal solution exactly in ~15 s. Main results therefore use
#   gap = 0.001; the (exploratory) calibration loop uses a loose gap = 0.1
#   for speed, since it only needs the cost-vs-compactness trend.
#
# Input:
#   - spatial/subbasin_sarantaporos/stream_network_pruned.gpkg
#   - spatial/basin/stream_network.gpkg            (reach lengths)
#   - spatial/hfp_zonal_stats.csv
#   - sdm/ensemble/ensemble_{species}.csv
#   - sdm/ensemble/ensemble_thresholds.csv
#   - sdm/habitat/habitat_{species}.csv            (binary/gap/semibinary TSS)
#   - points_snapped/dams/dams_snapped_points.csv
#   - spatial/stream_networks/river_graph_current.RDS
#   - spatial/stream_networks/river_graph_future.RDS
#
# Output:
#   - prioritization/pu_dat.csv, puvspr_dat.csv
#   - prioritization/solutions/solution_{scenario}_{target_pct}.csv
#   - prioritization/summary_table.csv          (all scenarios x targets)
#   - prioritization/comparison_30pct.csv        (current vs future, 30%)
#   - prioritization/cost_scenario_AB_30pct.csv  (HFI vs HFI+MW)
#   - prioritization/sensitivity_k1_k3.csv
#   - prioritization/boundary_penalty_calibration.csv
#   - prioritization/maps/priority_comparison_30pct.png
#   - prioritization/maps/summary_selected_reaches.png
#
# LOCATION: workflows/09_spatial_prioritization/01_spatial_prioritization.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(prioritizr)
# CBC solver required
library(sf)
library(data.table)
library(dplyr)
library(tidyr)
library(igraph)
library(ggplot2)
library(patchwork)
library(hydrographr)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

TARGETS           <- c(0.2, 0.3, 0.4, 0.5)
COMPARISON_TARGET <- 0.3            # target used for the current-vs-future map
BOUNDARY_PENALTY  <- 0.03           # calibrated via cost-compactness trade-off (knee ~0.01-0.03; see calibration block)
SOLVER_GAP        <- 0.001          # tightened from 0.1: gap-sensitivity showed 0.001 reproduces the proven-optimal solution (Jaccard 1.0) in ~15s
CALIB_GAP         <- 0.1            # loose gap for the exploratory calibration loop (trend only)
N_THREADS         <- 4
CONNECTIVITY_K    <- 3              # multi-hop connectivity neighbourhood

TARGET_SPECIES <- c(
  "Alburnoides_prespensis", "Anguilla_anguilla", "Barbus_prespensis",
  "Chondrostoma_ohridanum", "Oxynoemacheilus_pindus", "Salmo_farioides",
  "Squalius_platyceps"
)

# ============================================================
# SETUP
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("SPATIAL CONSERVATION PRIORITIZATION (current vs future barriers)")
message(paste(rep("=", 80), collapse = ""))

dir.create("prioritization/solutions", recursive = TRUE, showWarnings = FALSE)
dir.create("prioritization/maps",      recursive = TRUE, showWarnings = FALSE)

# ============================================================
# STEP 1: Load inputs
# ============================================================

message("\n=== Step 1: Loading inputs ===")

stream_lines <- read_sf(
  "spatial/subbasin_sarantaporos/stream_network_pruned.gpkg",
  layer = "stream_network_pruned"
)
message("  Stream reaches (planning units): ", nrow(stream_lines))

stream_network_full <- read_sf("spatial/basin/stream_network.gpkg") %>%
  st_drop_geometry() %>%
  select(subc_id, length)

stream_lines <- left_join(stream_lines, stream_network_full)

hfi <- fread("spatial/hfp_zonal_stats.csv")
message("  HFI rows: ", nrow(hfi))

message("  Loading SDM predictions for ", length(TARGET_SPECIES), " species...")
if (exists("preds")) rm(preds)

sdm_list <- lapply(TARGET_SPECIES, function(sp) {
  f <- file.path("sdm/ensemble", paste0("ensemble_", sp, ".csv"))
  if (!file.exists(f)) { message("    WARNING: file not found -- ", f); return(NULL) }
  dt <- fread(f)
  if (!"ensemble_mean" %in% names(dt)) {
    message("    WARNING: 'ensemble_mean' column missing in ", f); return(NULL)
  }
  dt
})
names(sdm_list) <- TARGET_SPECIES

# Apply TSS thresholds (semi-binary: below threshold -> 0)
thresholds <- fread("sdm/ensemble/ensemble_thresholds.csv")
sdm_list <- lapply(TARGET_SPECIES, function(sp) {
  dt <- sdm_list[[sp]]; if (is.null(dt)) return(NULL)
  thr <- thresholds[species == sp, threshold_tss]
  if (length(thr) == 0 || is.na(thr)) return(dt)
  dt[ensemble_mean < thr, ensemble_mean := 0]
  dt
})
names(sdm_list) <- TARGET_SPECIES

# Both scenario graphs -- used to build barrier-aware connectivity
river_graph_current <- readRDS("spatial/stream_networks/river_graph_current.RDS")
river_graph_future  <- readRDS("spatial/stream_networks/river_graph_future.RDS")
message("  Graphs loaded (current + future)")

# ============================================================
# STEP 2: Planning unit table (pu_dat)
# ============================================================

message("\n=== Step 2: Building planning unit table ===")

pu_dat <- stream_lines %>%
  st_drop_geometry() %>%
  select(subc_id) %>%
  rename(id = subc_id) %>%
  left_join(stream_network_full %>% rename(id = subc_id), by = "id") %>%
  mutate(length_km = length / 1000) %>%
  left_join(
    hfi %>% mutate(id = as.integer(subc_id)) %>% select(id, hfp_wgs_mean),
    by = "id"
  ) %>%
  rename(hfi_mean = hfp_wgs_mean)

n_na_hfi <- sum(is.na(pu_dat$hfi_mean))
if (n_na_hfi > 0) stop("HFI join failed: ", n_na_hfi, " NAs in hfi_mean")

hfi_range <- max(pu_dat$hfi_mean) - min(pu_dat$hfi_mean)
pu_dat <- pu_dat %>%
  mutate(cost = (hfi_mean - min(hfi_mean)) / hfi_range,
         cost = ifelse(cost == 0, 1e-6, cost))

message("  Planning units: ", nrow(pu_dat),
        " | total length: ", round(sum(pu_dat$length_km, na.rm = TRUE), 1), " km")

# (single fwrite after Step 2b, once MW columns are added)

# ============================================================
# STEP 2b: Barrier opportunity-cost layer (planned dams, MW)
# ============================================================
# Opportunity cost = energy forgone by NOT building a PLANNED dam.
# Existing dams carry no opportunity cost (already built) but DO still
# sever connectivity in the boundary file -- different layer, different
# logic. Per-reach cost = scaled summed planned-dam MW; dam-free reaches
# contribute 0. Reaches with >1 planned dam get summed MW (avoiding the
# barrier means forgoing all dams on it). Min-max scaled to match HFI.

message("\n=== Step 2b: Building MW opportunity-cost layer (planned only) ===")

dam_mw <- fread("points_snapped/dams/dams_snapped_points.csv")

# PLANNED dams only -- existing dam carries no opportunity cost
n_all <- nrow(dam_mw)
dam_mw <- dam_mw[status == "planned"]
message("  Dams in basin: ", n_all,
        " | planned (kept for cost): ", nrow(dam_mw))

# sum MW per reach (co-located planned dams -> summed)
reach_mw   <- dam_mw[, .(mw_sum = sum(power_mw, na.rm = TRUE)), by = subc_id]
shared_ids <- dam_mw[, .N, by = subc_id][N > 1, subc_id]
message("  Reaches with a planned dam: ", nrow(reach_mw),
        " | reaches with >1 (MW summed): ", length(shared_ids))

# join onto planning units; dam-free reaches -> 0
pu_dat <- pu_dat %>%
  left_join(reach_mw %>% rename(id = subc_id), by = "id") %>%
  mutate(mw_sum = ifelse(is.na(mw_sum), 0, mw_sum))

# scale MW 0-1 (min is 0, so mw / max)
mw_max  <- max(pu_dat$mw_sum)
pu_dat  <- pu_dat %>% mutate(mw_cost = mw_sum / mw_max)

# two cost columns: HFI-only (Scenario A) and HFI+MW (Scenario B)
pu_dat <- pu_dat %>%
  mutate(cost_hfi      = cost,                              # Scenario A
         cost_hfi_mw   = cost + mw_cost,                    # Scenario B
         cost_hfi      = ifelse(cost_hfi    == 0, 1e-6, cost_hfi),
         cost_hfi_mw   = ifelse(cost_hfi_mw == 0, 1e-6, cost_hfi_mw))

message("  Reaches w/ MW cost: ", sum(pu_dat$mw_cost > 0),
        " | max planned MW (", round(mw_max, 2), ") -> mw_cost = 1")

fwrite(pu_dat, "prioritization/pu_dat.csv")

# ============================================================
# STEP 3: Species-planning-unit table (puvspr_dat)
#         Gap-filled reaches (suitable in classification but below the
#         TSS threshold, so semibinary = 0) are assigned the species'
#         TSS threshold value. This preserves the habitat continuity
#         established in Module 8 (08_habitat_classification.R) inside
#         the prioritization, instead of dropping those reaches to 0.
# ============================================================

message("\n=== Step 3: Building species-planning-unit table ===")

spec_dat <- data.frame(id = seq_along(TARGET_SPECIES), name = TARGET_SPECIES)

puvspr_dat <- lapply(seq_along(TARGET_SPECIES), function(i) {
  sp <- TARGET_SPECIES[i]

  hab_f <- file.path("sdm/habitat", paste0("habitat_", sp, ".csv"))
  if (!file.exists(hab_f)) {
    message("    WARNING: habitat file not found -- ", hab_f); return(NULL)
  }
  hab <- fread(hab_f)

  thr <- thresholds[species == sp, threshold_tss]
  if (length(thr) == 0 || is.na(thr)) thr <- 0

  # suitability = semibinary value, but gap-filled reaches (suitable,
  # below threshold) get the threshold value instead of 0.
  hab[, amount := semibinary_tss]
  hab[binary_tss == 1L & gap_filled_tss == 1L & semibinary_tss == 0,
      amount := thr]

  out <- hab[amount > 0, .(pu = subc_id, amount)]
  out[, species := i]
  n_gap <- hab[binary_tss == 1L & gap_filled_tss == 1L & semibinary_tss == 0, .N]
  message("    ", sp, ": ", nrow(out), " suitable reaches (",
          n_gap, " gap-filled set to threshold ", thr, ")")
  out
}) %>% rbindlist(use.names = TRUE, fill = TRUE)

puvspr_dat <- puvspr_dat[pu %in% pu_dat$id]
message("  Species-PU rows: ", nrow(puvspr_dat))

fwrite(puvspr_dat, "prioritization/puvspr_dat.csv")

# ============================================================
# STEP 4: Barrier-aware boundary files (one per scenario)
#         Multi-hop connectivity with distance decay.
# ============================================================

message("\n=== Step 4: Building barrier-aware boundary files ===")

subc_ids_basin <- as.character(pu_dat$id)

# Maximum connectivity neighbourhood, in reaches (hops). k = 1 reproduces
# the immediate-neighbour boundary file; k > 1 lets prioritizr register
# that two non-adjacent reaches are (or are not) separated by a barrier,
# for species whose populations span several consecutive reaches.
# Boundary weight decays as 1 / hop_distance: adjacent pairs contribute 1,
# 2-hop pairs 0.5, 3-hop pairs 0.33. This keeps the dominant compactness
# signal on close neighbours while letting distant pairs register weakly.
# NOTE: the 1/hop decay is a deliberate simplification and could be
# refined (e.g. by reach length or a dispersal-kernel weighting).

# Build a symmetric, multi-hop boundary table from a scenario graph.
# Barrier edges are deleted FIRST, so any path computed afterwards is
# barrier-free by construction: a dam between two reaches removes every
# path between them, and they never appear as a connected pair at any
# hop distance. Reachability is treated as UNDIRECTED (fish move both
# ways; dams block both ways), matching the weak-component fragmentation.
build_boundary <- function(g, label, k = CONNECTIVITY_K) {

  # 1. delete barrier edges on the ORIGINAL (directed) graph, where the
  #    barrier attribute is known to be intact, THEN make it undirected.
  is_barrier <- igraph::E(g)$barrier == TRUE
  g_open <- igraph::delete_edges(g, igraph::E(g)[is_barrier])
  n_barrier_edges <- sum(is_barrier)

  g_open <- igraph::as_undirected(g_open, mode = "collapse")

  # keep only nodes inside the study sub-basin
  g_open <- igraph::induced_subgraph(
    g_open,
    igraph::V(g_open)[names(igraph::V(g_open)) %in% subc_ids_basin]
  )

  # 2. all-pairs hop distances on the barrier-free graph
  hop <- igraph::distances(g_open)

  # 3. keep pairs within k hops (upper triangle), decay weight = 1 / hops
  hop[!is.finite(hop)] <- Inf
  hop[lower.tri(hop, diag = TRUE)] <- Inf

  idx <- which(hop >= 1 & hop <= k, arr.ind = TRUE)
  node_ids <- as.integer(rownames(hop))

  keep <- data.frame(
    id1      = node_ids[idx[, "row"]],
    id2      = node_ids[idx[, "col"]],
    boundary = 1 / hop[idx]
  )

  # 4. symmetrise
  bmat <- bind_rows(keep, keep %>% rename(id1 = id2, id2 = id1)) %>%
    distinct(id1, id2, .keep_all = TRUE)

  n_adj   <- sum(keep$boundary == 1)
  n_multi <- sum(keep$boundary <  1)
  message("  [", label, "] k = ", k,
          " | barrier edges removed: ", n_barrier_edges,
          " | adjacent pairs: ", n_adj,
          " | multi-hop pairs: ", n_multi)
  bmat
}

bmat_current <- build_boundary(river_graph_current, "current")
bmat_future  <- build_boundary(river_graph_future,  "future")

# ============================================================
# STEP 5: Define solver, then run all solves
# ============================================================

message("\n=== Step 5: Solving ===")

if (!requireNamespace("rcbc", quietly = TRUE))
  stop("rcbc not found. install.packages('rcbc', repos = 'https://cran.r-universe.dev')")

# ---- solve_one(): parameterised by boundary matrix, target, cost column ----
# Uses the tightened SOLVER_GAP (main results). Defined BEFORE any call below.
solve_one <- function(bmat, target, cost_col = "cost_hfi") {
  problem(pu_dat, spec_dat, cost_column = cost_col, rij = puvspr_dat) %>%
    add_min_set_objective() %>%
    add_relative_targets(target) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(penalty = BOUNDARY_PENALTY, data = bmat) %>%
    add_cbc_solver(gap = SOLVER_GAP, threads = N_THREADS, verbose = FALSE) %>%
    solve()
}

# ============================================================
# BOUNDARY PENALTY CALIBRATION (cost vs compactness trade-off)
#   The boundary penalty is a compactness trade-off term with no intrinsic
#   ecological value; it must be calibrated per study area (it scales with
#   network size, reach count, and the cost range). We solve across a
#   penalty grid and record the trade-off between solution cost and total
#   boundary length (perimeter). The "knee" of the cost-vs-boundary curve
#   is the principled choice: maximal compactness gain per unit extra cost.
#   Uses a loose gap (CALIB_GAP) for speed -- only the trend is needed.
#   Done on the CURRENT scenario at the comparison target.
# ============================================================

message("\n=== Boundary penalty calibration (cost vs compactness) ===")

PENALTY_GRID <- c(0, 0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1)

# total boundary (perimeter) of a selection, using the same bmat weights.
# For each selected unit, sum the boundary weights to NON-selected units
# (exposed edges) -> this is the perimeter the penalty acts on.
boundary_length <- function(selected_ids, bmat) {
  sel <- as.integer(selected_ids)
  # edges where exactly one endpoint is selected = exposed perimeter
  exposed <- bmat[(bmat$id1 %in% sel) & !(bmat$id2 %in% sel), ]
  sum(exposed$boundary, na.rm = TRUE)
}

calib <- lapply(PENALTY_GRID, function(pen) {

  p <- problem(pu_dat, spec_dat, cost_column = "cost_hfi", rij = puvspr_dat) %>%
    add_min_set_objective() %>%
    add_relative_targets(COMPARISON_TARGET) %>%
    add_binary_decisions() %>%
    add_cbc_solver(gap = CALIB_GAP, threads = N_THREADS, verbose = FALSE)
  if (pen > 0)
    p <- p %>% add_boundary_penalties(penalty = pen, data = bmat_current)

  s   <- solve(p)
  sel <- s$id[s$solution_1 == 1]

  data.frame(
    penalty      = pen,
    n_selected   = length(sel),
    selected_km  = round(sum(pu_dat$length_km[pu_dat$id %in% sel], na.rm = TRUE), 1),
    total_cost   = round(sum(s$cost[s$solution_1 == 1], na.rm = TRUE), 3),
    boundary_len = round(boundary_length(sel, bmat_current), 2)
  )
}) %>% rbindlist()

# normalise both axes to [0,1] so the knee is comparable
calib[, cost_norm := (total_cost   - min(total_cost))   / (max(total_cost)   - min(total_cost))]
calib[, bound_norm := (boundary_len - min(boundary_len)) / (max(boundary_len) - min(boundary_len))]

message("\n  Calibration (current scenario, ", COMPARISON_TARGET * 100, "% target):")
print(calib)
fwrite(calib, "prioritization/boundary_penalty_calibration.csv")

# ---- trade-off curve: cost vs boundary, knee is the elbow ----
p_tradeoff <- ggplot(calib, aes(x = boundary_len, y = total_cost)) +
  geom_path(colour = "grey60") +
  geom_point(size = 3, colour = "#2c7bb6") +
  geom_text(aes(label = penalty), vjust = -0.8, size = 3) +
  labs(x = "Total boundary length (perimeter; lower = more compact)",
       y = "Solution cost (HFI)",
       title = "Boundary-penalty calibration: cost vs compactness trade-off",
       subtitle = paste0("Current scenario, ", COMPARISON_TARGET * 100,
                         "% target. Labels = penalty. The elbow is the",
                         " principled penalty: max compactness gain per unit cost.")) +
  theme_bw(base_size = 11) +
  theme(plot.subtitle = element_text(size = 8.5, colour = "grey40"))

png("prioritization/maps/boundary_penalty_calibration.png",
    width = 7, height = 5.5, units = "in", res = 200)
print(p_tradeoff); dev.off()
message("  Saved: prioritization/maps/boundary_penalty_calibration.png")

# ---- automatic knee detection (max distance from the line joining endpoints) ----
ord <- calib[order(penalty)]
x <- ord$bound_norm; y <- ord$cost_norm
x1 <- x[1]; y1 <- y[1]; x2 <- x[length(x)]; y2 <- y[length(y)]
denom <- sqrt((y2 - y1)^2 + (x2 - x1)^2)
ord[, knee_dist := abs((y2 - y1) * x - (x2 - x1) * y + x2 * y1 - y2 * x1) / denom]
knee_penalty <- ord[which.max(knee_dist), penalty]
message("\n  Suggested knee penalty: ", knee_penalty,
        "  (point of maximum curvature in the cost-vs-compactness trade-off)")
message("  -> BOUNDARY_PENALTY is set to ", BOUNDARY_PENALTY,
        "; report the curve and this choice as a calibration step in Methods.")

# ------------------------------------------------------------
# 5a. SENSITIVITY: k = 1 (immediate neighbours) vs k = 3 (multi-hop)
#     Current scenario, 30% target. (Uses main SOLVER_GAP.)
# ------------------------------------------------------------
message("\n  --- Sensitivity check: k = 1 vs k = 3 (current, 30%) ---")

bmat_k1 <- build_boundary(river_graph_current, "current_k1", k = 1)
bmat_k3 <- build_boundary(river_graph_current, "current_k3", k = 3)

sol_k1 <- solve_one(bmat_k1, 0.3)
sol_k3 <- solve_one(bmat_k3, 0.3)

sel_k1 <- sol_k1$id[sol_k1$solution_1 == 1]
sel_k3 <- sol_k3$id[sol_k3$solution_1 == 1]

km_k1 <- sum(pu_dat$length_km[pu_dat$id %in% sel_k1], na.rm = TRUE)
km_k3 <- sum(pu_dat$length_km[pu_dat$id %in% sel_k3], na.rm = TRUE)

sensitivity <- data.frame(
  k            = c(1, 3),
  n_selected   = c(length(sel_k1), length(sel_k3)),
  selected_km  = round(c(km_k1, km_k3), 1),
  cost         = round(c(sum(sol_k1$cost[sol_k1$solution_1 == 1]),
                         sum(sol_k3$cost[sol_k3$solution_1 == 1])), 3)
)

shared    <- length(intersect(sel_k1, sel_k3))
only_k1   <- length(setdiff(sel_k1, sel_k3))
only_k3   <- length(setdiff(sel_k3, sel_k1))
jaccard   <- shared / length(union(sel_k1, sel_k3))

message("  Solution comparison:")
print(sensitivity)
message("  Shared reaches: ", shared,
        " | k=1 only: ", only_k1,
        " | k=3 only: ", only_k3)
message("  Jaccard similarity: ", round(jaccard, 3))
message("  (Jaccard near 1 = multi-hop barely changes the solution;",
        " lower = k matters)")

fwrite(sensitivity, "prioritization/sensitivity_k1_k3.csv")

# ------------------------------------------------------------
# 5b. Cost scenarios A (HFI) vs B (HFI + MW), future barriers, 30%
# ------------------------------------------------------------
message("\n  --- Cost scenarios A (HFI) vs B (HFI+MW), future barriers, ",
        COMPARISON_TARGET * 100, "% ---")

sol_A <- solve_one(bmat_future, COMPARISON_TARGET, cost_col = "cost_hfi")
sol_B <- solve_one(bmat_future, COMPARISON_TARGET, cost_col = "cost_hfi_mw")

sel_A <- sol_A$id[sol_A$solution_1 == 1]
sel_B <- sol_B$id[sol_B$solution_1 == 1]

cost_cmp <- data.frame(
  scenario      = c("A_HFI", "B_HFI_MW"),
  n_selected    = c(length(sel_A), length(sel_B)),
  selected_km   = round(c(sum(pu_dat$length_km[pu_dat$id %in% sel_A], na.rm = TRUE),
                          sum(pu_dat$length_km[pu_dat$id %in% sel_B], na.rm = TRUE)), 1),
  # how many selected reaches carry a planned dam (the conflict count)
  n_dam_reaches = c(sum(pu_dat$mw_cost[pu_dat$id %in% sel_A] > 0),
                    sum(pu_dat$mw_cost[pu_dat$id %in% sel_B] > 0))
)
print(cost_cmp)

shared_AB  <- length(intersect(sel_A, sel_B))
jaccard_AB <- shared_AB / length(union(sel_A, sel_B))
message("  Shared: ", shared_AB,
        " | A only: ", length(setdiff(sel_A, sel_B)),
        " | B only: ", length(setdiff(sel_B, sel_A)),
        " | Jaccard: ", round(jaccard_AB, 3))
message("  (n_dam_reaches: selected reaches with a planned dam = conflict reaches;",
        " B should pull these DOWN by penalising high-MW reaches)")

fwrite(cost_cmp, "prioritization/cost_scenario_AB_30pct.csv")

# ------------------------------------------------------------
# 5c. Main loop: both scenarios x all targets
# ------------------------------------------------------------
message("\n  --- Main solve loop (2 scenarios x ", length(TARGETS), " targets) ---")

scenarios     <- list(current = bmat_current, future = bmat_future)
all_solutions <- list()   # keyed "scenario_targetpct"
summary_rows  <- list()

for (scen in names(scenarios)) {
  for (target in TARGETS) {

    key <- paste0(scen, "_", target * 100, "pct")
    message("\n  [", scen, "] target ", target * 100, "%")

    s <- tryCatch(solve_one(scenarios[[scen]], target),
                  error = function(e) { message("  ERROR: ", conditionMessage(e)); NULL })
    if (is.null(s)) next

    # s is pu_dat + solution_1, so it already carries length_km and cost --
    # do NOT re-join length_km (that creates length_km.x/.y and breaks sums).
    n_selected  <- sum(s$solution_1 == 1)
    selected_km <- sum(s$length_km[s$solution_1 == 1], na.rm = TRUE)
    total_km    <- sum(pu_dat$length_km, na.rm = TRUE)
    total_cost  <- sum(s$cost[s$solution_1 == 1], na.rm = TRUE)

    message("    selected: ", n_selected, " reaches (",
            round(selected_km, 1), " km, ",
            round(100 * selected_km / total_km, 1), "%)")

    all_solutions[[key]] <- s
    summary_rows[[key]]  <- data.frame(
      scenario       = scen,
      target         = target,
      n_selected     = n_selected,
      pct_network    = round(100 * n_selected / nrow(pu_dat), 1),
      selected_km    = round(selected_km, 1),
      pct_length     = round(100 * selected_km / total_km, 1),
      total_hfi_cost = round(total_cost, 2)
    )

    fwrite(s, paste0("prioritization/solutions/solution_", key, ".csv"))
  }
}

# ============================================================
# STEP 6: Summary table (all scenarios x targets)
# ============================================================

message("\n=== Step 6: Summary table ===")

summary_df <- rbindlist(summary_rows)
fwrite(summary_df, "prioritization/summary_table.csv")
print(summary_df)

# ============================================================
# STEP 7: Current vs future comparison at the chosen target
# ============================================================

message("\n=== Step 7: Current vs future comparison (",
        COMPARISON_TARGET * 100, "%) ===")

key_cur <- paste0("current_", COMPARISON_TARGET * 100, "pct")
key_fut <- paste0("future_",  COMPARISON_TARGET * 100, "pct")

sol_cur <- all_solutions[[key_cur]] %>% select(id, sol_current = solution_1)
sol_fut <- all_solutions[[key_fut]] %>% select(id, sol_future  = solution_1)

comparison <- pu_dat %>%
  select(id, length_km) %>%
  left_join(sol_cur, by = "id") %>%
  left_join(sol_fut, by = "id") %>%
  mutate(
    status = case_when(
      sol_current == 1 & sol_future == 1 ~ "Both",
      sol_current == 1 & sol_future == 0 ~ "Current only",
      sol_current == 0 & sol_future == 1 ~ "Future only",
      TRUE                               ~ "Neither"
    )
  )

fwrite(comparison, "prioritization/comparison_30pct.csv")

# how much the priority network shifts when dams are added
shift_tbl <- comparison %>%
  group_by(status) %>%
  summarise(n_reaches = n(),
            length_km = round(sum(length_km, na.rm = TRUE), 1), .groups = "drop")
message("  Priority-area shift (current -> future):")
print(shift_tbl)

# ---- Comparison map ----
comp_sf <- stream_lines %>%
  left_join(comparison %>% rename(subc_id = id), by = "subc_id") %>%
  mutate(status = factor(status,
                         levels = c("Both", "Current only", "Future only", "Neither"))) %>%
  st_transform(4326)

status_cols <- c("Both" = "#1a7a3c", "Current only" = "#d7191c",
                 "Future only" = "#2c7bb6", "Neither" = "grey85")

p_comp <- ggplot(comp_sf) +
  geom_sf(aes(colour = status, linewidth = status == "Neither")) +
  scale_colour_manual(values = status_cols, name = "Priority status") +
  scale_linewidth_manual(values = c(`TRUE` = 0.3, `FALSE` = 0.9), guide = "none") +
  labs(title = paste0("Priority reaches: current vs future barrier scenario (",
                      COMPARISON_TARGET * 100, "% target)"),
       subtitle = "Red = priority only without planned dams; blue = priority only once dams sever connectivity") +
  theme_void(base_size = 11) +
  theme(plot.subtitle = element_text(size = 9, colour = "grey40"))

png("prioritization/maps/priority_comparison_30pct.png",
    width = 9, height = 7, units = "in", res = 200)
print(p_comp); dev.off()
message("  Saved: prioritization/maps/priority_comparison_30pct.png")

# ============================================================
# STEP 8: Summary plot -- selected length by target and scenario
# ============================================================

message("\n=== Step 8: Summary plot ===")

p_summary <- ggplot(summary_df,
                    aes(x = factor(target * 100), y = selected_km,
                        fill = scenario)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.9) +
  scale_fill_manual(values = c("current" = "#74add1", "future" = "#d73027"),
                    name = "Scenario") +
  labs(x = "Conservation target (%)",
       y = "Selected network length (km)",
       title = "Prioritization solutions -- current vs future barriers",
       subtitle = paste0("Minimum-set, cost = HFI, boundary penalty = ",
                         BOUNDARY_PENALTY)) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top",
        plot.subtitle = element_text(size = 9, colour = "grey40"))

png("prioritization/maps/summary_selected_reaches.png",
    width = 7, height = 5, units = "in", res = 200)
print(p_summary); dev.off()
message("  Saved: prioritization/maps/summary_selected_reaches.png")

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("SPATIAL PRIORITIZATION COMPLETE")
message(paste(rep("=", 80), collapse = ""))
message("Planning units: ", nrow(pu_dat),
        " | targets: ", paste0(TARGETS * 100, "%", collapse = ", "))
message("Scenarios: current (existing dam) vs future (existing + planned)")
message("Comparison map drawn at ", COMPARISON_TARGET * 100, "% target")
message("Solver gap (main): ", SOLVER_GAP, " | calibration gap: ", CALIB_GAP)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_planned_dam_ranking.R   (Module 11 -- Planned-dam ranking)
#
# Ranks planned dams by the connectivity damage each causes, expressed
# per MW of forgone energy. Damage = summed SDM habitat suitability
# (across the target species) of the reach-set that each dam isolates
# from the otherwise-connected current network. No re-solving; this is a
# graph-component analysis layered on the Module 9 prioritization inputs.
#
# A dam sits on one network edge. Removing that edge splits the network
# into the piece still connected to the outlet and a smaller piece
# stranded above the dam (the "severed set"). The severed set's summed
# suitability is the dam's damage. Dams isolating much good habitat for
# little energy rank worst.
#
# Each dam is scored independently against the CURRENT network (existing
# dam present, planned dams absent), so the score is a marginal measure:
# "what this dam alone would isolate". Cumulative multi-dam effects are a
# separate question (see Discussion).
#
# Input:
#   - prioritization/puvspr_dat.csv          (species x PU suitability)
#   - prioritization/pu_dat.csv              (planning units, length_km)
#   - spatial/stream_networks/river_graph_current.RDS
#   - points_snapped/dams/dams_snapped_points.csv
#   - points_cleaned/dams/dams_sarantaporos_clean.gpkg
#
# Output:
#   - prioritization/planned_dam_ranking.csv
#
# LOCATION: workflows/09_spatial_prioritization/02_planned_dam_ranking.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(sf)
library(data.table)
library(dplyr)
library(igraph)
library(hydrographr)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

message("\n", paste(rep("=", 80), collapse = ""))
message("PLANNED-DAM RANKING (connectivity damage per MW)")
message(paste(rep("=", 80), collapse = ""))

# ============================================================
# STEP 1: Read inputs (this script is self-contained)
# ============================================================

message("\n=== Step 1: Reading inputs ===")

puvspr_dat <- fread("prioritization/puvspr_dat.csv")
pu_dat     <- fread("prioritization/pu_dat.csv")
subc_ids_basin <- as.character(pu_dat$id)
message("  Planning units: ", nrow(pu_dat),
        " | species-PU rows: ", nrow(puvspr_dat))

river_graph_current <- readRDS("spatial/stream_networks/river_graph_current.RDS")
message("  Current network graph loaded")

# ============================================================
# STEP 2: Per-reach habitat value (summed SDM suitability)
# ============================================================

message("\n=== Step 2: Per-reach habitat value ===")

reach_value <- puvspr_dat[, .(suitability = sum(amount, na.rm = TRUE)), by = pu]
setnames(reach_value, "pu", "subc_id")
message("  Reaches with non-zero suitability: ", nrow(reach_value))

# ============================================================
# STEP 3: Planned dams — reach + summed MW (co-located summed)
# ============================================================

message("\n=== Step 3: Planned-dam reaches and MW ===")

dams <- fread("points_snapped/dams/dams_snapped_points.csv")
# power   <-   read_geopackage("points_cleaned/dams/dams_sarantaporos_clean.gpkg",
#                              import_as = "data.table")
#
# planned <- snapped[status == "planned", .(site_id, subc_id)][
#   power[, .(site_id, power_mw)], on = "site_id", nomatch = NULL]
planned <- dams %>% filter(status == "planned")

dam_reach <- planned[, .(power_mw = sum(power_mw, na.rm = TRUE),
                         n_dams   = .N,
                         site_ids = paste(site_id, collapse = "; ")),
                     by = subc_id]
message("  Planned-dam reaches to rank: ", nrow(dam_reach),
        " (from ", nrow(planned), " dams)")

# ============================================================
# STEP 4: Current network — directed (to find downstream edge)
#         and undirected (to measure the stranded component)
# ============================================================

message("\n=== Step 4: Building current network ===")

# directed graph (upstream -> downstream): used ONLY to find which edge of
# a dam reach points downstream, i.e. the edge a dam actually breaks.
g_dir <- induced_subgraph(
  river_graph_current,
  V(river_graph_current)[names(V(river_graph_current)) %in% subc_ids_basin])
message("  Nodes: ", vcount(g_dir), " | edges: ", ecount(g_dir))

# severed set for a dam reach: cut its single DOWNSTREAM edge, then take the
# component (treated undirected) that contains the dam reach. That component
# is the dam reach plus everything upstream of it — what the dam isolates
# from the outlet. Direction is used only to pick the edge; reachability
# within the stranded piece is undirected (weak components), matching the
# fragmentation logic.
severed_reaches <- function(dam_subc) {
  v <- as.character(dam_subc)
  if (!v %in% names(V(g_dir))) return(character(0))

  # outgoing edge(s) = downstream. In a tree each reach has exactly one.
  out_e <- incident(g_dir, v, mode = "out")
  if (length(out_e) == 0) {
    # no downstream edge: dam reach is the outlet itself — nothing upstream
    # is separable from the outlet by this cut. Treat severed set as empty
    # of *additional* reaches beyond the dam reach.
    return(v)
  }

  g_cut <- delete_edges(g_dir, out_e)
  comp  <- components(g_cut, mode = "weak")   # undirected reachability
  dam_comp <- comp$membership[v]
  names(comp$membership)[comp$membership == dam_comp]
}



# Downstream dewatering zone for a run-of-river dam: walk downstream from
# the dam reach, accumulating reach length, and keep reaches until the
# cumulative downstream distance reaches DEWATER_M (2 km, the bypassed/
# dewatered channel). Returns the dam reach plus the downstream reaches
# within that distance. Uses the directed graph (downstream = "out").
DEWATER_M <- 2000

dewatered_reaches <- function(dam_subc) {
  v <- as.character(dam_subc)
  if (!v %in% names(V(g_dir))) return(character(0))

  reach_len <- setNames(pu_dat$length_km * 1000, as.character(pu_dat$id))

  visited <- character(0)
  cum_m   <- 0
  current <- v
  repeat {
    out_e <- incident(g_dir, current, mode = "out")
    if (length(out_e) == 0) break
    nxt <- ends(g_dir, out_e)[1, 2]          # single downstream neighbour
    len <- reach_len[as.character(nxt)]
    if (is.na(len)) break
    visited <- c(visited, nxt)
    cum_m   <- cum_m + len
    if (cum_m >= DEWATER_M) break
    current <- nxt
  }
  visited
}

# ============================================================
# STEP 5: Score and rank
#   Damage footprint = upstream-isolated reaches (connectivity loss)
#   PLUS downstream dewatered reaches (RoR habitat degradation).
#   Reported as two columns; ranked on their sum per MW.
# ============================================================

message("\n=== Step 5: Scoring planned dams ===")

rank_rows <- lapply(seq_len(nrow(dam_reach)), function(i) {
  ds <- dam_reach$subc_id[i]

  sev_int  <- as.integer(severed_reaches(ds))    # upstream isolated
  dew_int  <- as.integer(dewatered_reaches(ds))  # downstream dewatered
  # a reach cannot be both; downstream set excludes the upstream component
  dew_int  <- setdiff(dew_int, sev_int)

  dmg_up  <- reach_value[subc_id %in% sev_int, sum(suitability, na.rm = TRUE)]
  dmg_dn  <- reach_value[subc_id %in% dew_int, sum(suitability, na.rm = TRUE)]
  dmg_tot <- dmg_up + dmg_dn

  km_up <- sum(pu_dat$length_km[pu_dat$id %in% sev_int], na.rm = TRUE)
  km_dn <- sum(pu_dat$length_km[pu_dat$id %in% dew_int], na.rm = TRUE)

  data.frame(
    subc_id            = ds,
    n_dams             = dam_reach$n_dams[i],
    power_mw           = dam_reach$power_mw[i],
    n_isolated         = length(sev_int),
    n_dewatered        = length(dew_int),
    km_isolated        = round(km_up, 2),
    km_dewatered       = round(km_dn, 2),
    damage_isolated    = round(dmg_up, 3),
    damage_dewatered   = round(dmg_dn, 3),
    damage_total       = round(dmg_tot, 3),
    damage_per_mw      = round(dmg_tot / dam_reach$power_mw[i], 3)
  )
})

dam_rank <- rbindlist(rank_rows)[order(-damage_per_mw)]
dam_rank[, rank := seq_len(.N)]
setcolorder(dam_rank, c("rank", "subc_id", "n_dams", "power_mw",
                        "n_isolated", "n_dewatered",
                        "km_isolated", "km_dewatered",
                        "damage_isolated", "damage_dewatered",
                        "damage_total", "damage_per_mw"))

message("\n--- Planned dams ranked by total damage (isolated + dewatered) per MW ---")
print(dam_rank)

fwrite(dam_rank, "prioritization/planned_dam_ranking.csv")
message("\nSaved: prioritization/planned_dam_ranking.csv")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03_figures_prioritization.R   (Module 9 -- Planned-dam ranking figures)
#
# Three figures from the planned-dam ranking and the prioritization
# solutions:
#   FIG 1. Ranked bar chart of planned dams by damage-per-MW, with the
#          isolated/dewatered split shown within each bar. The outlet
#          dam fragments essentially the whole sub-basin and is orders
#          of magnitude above the rest, so it is set aside and annotated
#          off-scale; the remaining dams are shown on a linear axis where
#          differences among the actionable dams are readable.
#   FIG 2. Map of planned dams on the Sarantaporos network, sized and
#          coloured by total damage (isolated + dewatered habitat).
#   FIG 3. Current vs future priority reaches at the 30% target, drawn
#          as four-category status (Both / Current only / Future only /
#          Neither) on the stream network.
#
# Self-contained: reads everything from disk.
#
# Input:
#   - prioritization/planned_dam_ranking.csv          (from 02_)
#   - prioritization/comparison_30pct.csv             (from 01_, Step 7)
#   - spatial/subbasin_sarantaporos/stream_network_pruned.gpkg
#   - points_snapped/dams/dams_snapped_points.csv
#
# Output:
#   - prioritization/maps/fig1_dam_ranking_bars.png
#   - prioritization/maps/fig2_dam_damage_map.png
#   - prioritization/maps/fig3_priority_comparison_30pct.png
#
# LOCATION: workflows/09_spatial_prioritization/03_figures_prioritization.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(sf)
library(data.table)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(patchwork)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

dir.create("prioritization/maps", recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Damage-per-MW above this is treated as "off-scale" and annotated
# rather than plotted (the outlet dam that fragments the whole basin).
OFFSCALE_CUTOFF <- 1000

col_isolated  <- "#d73027"   # upstream connectivity loss
col_dewatered <- "#4575b4"   # downstream dewatering (run-of-river)

status_cols <- c("Both"         = "#1a7a3c",
                 "Current only" = "#d7191c",
                 "Future only"  = "#2c7bb6",
                 "Neither"      = "grey85")

# Place-name lookup (Greek -> Latin). Edit values as preferred.
thesh_lookup <- tribble(
  ~thesh,                            ~thesh_lat,
  "Π.ΣΑΡΑΝΤΑΠΟΡΟΣ",                  "Sarantaporos",
  "ΕΠΤΑΧΩΡΙ",                        "Eptachori",
  "ΠΛΑΓΙΑ",                          "Plagia",
  "ΚΕΦΑΛΟΒΡΥΣΟ",                     "Kefalovryso",
  "ΒΟΥΡΜΠΙΑΝΗ - Ρ. ΒΟΥΡΜΠΙΑΝΙΤΙΚΟ",  "Vourbianitiko",
  "ΡΕΜΑ ΜΕΣΟΠΟΤΑΜΟΣ",               "Mesopotamos",
  "Ρ.Χελιμόδι",                      "Chelimodi",
  "ΡΕΜΑ ΜΑΝΟΥΡΑΣ",                   "Manouras",
  "ΡΕΜΑ ΠΗΓΩΝ ΣΑΡΑΝΤΑΠΟΡΟΥ",        "Piges Sarantaporou",
  "ΑΜΑΡΑΝΤΟΣ",                       "Amarantos",
  "ΡΑΧΟΒΙΤΣΑ",                       "Rachovitsa",
  "ΡΕΜΑ ΕΛΛΗΝΙΚΟ",                   "Elliniko",
  "ΛΑΪΣΤΑ",                          "Laista"
)

# ============================================================
# STEP 1: Load inputs
# ============================================================

message("\n=== Step 1: Loading inputs ===")

dam_rank <- fread("prioritization/planned_dam_ranking.csv")
message("  Dam ranking rows: ", nrow(dam_rank))

stream_lines <- read_sf(
  "spatial/subbasin_sarantaporos/stream_network_pruned.gpkg",
  layer = "stream_network_pruned"
) %>% st_transform(4326)

dams_snapped <- fread("points_snapped/dams/dams_snapped_points.csv")

# one place-name + one snapped coordinate per ranked reach (co-located
# dams share the reach; the ranking already summed their MW and damage).
dam_meta <- dams_snapped %>%
  group_by(subc_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(subc_id, thesh, longitude_snapped, latitude_snapped) %>%
  left_join(thesh_lookup, by = "thesh") %>%
  mutate(thesh_lat = coalesce(thesh_lat, as.character(subc_id)))

dam_rank <- dam_rank %>%
  left_join(dam_meta, by = "subc_id")

n_no_xy <- sum(is.na(dam_rank$longitude_snapped))
if (n_no_xy > 0)
  message("  WARNING: ", n_no_xy, " ranked dams have no snapped coordinates")

# ============================================================
# STEP 2: FIG 1 -- ranked bar chart (damage-per-MW)
# ============================================================

message("\n=== Step 2: Fig 1 -- ranked bar chart ===")

offscale <- dam_rank %>% filter(damage_per_mw > OFFSCALE_CUTOFF)
onscale  <- dam_rank %>% filter(damage_per_mw <= OFFSCALE_CUTOFF)

message("  Off-scale dams (annotated): ", nrow(offscale),
        " | on-scale dams (plotted): ", nrow(onscale))

# long format so each bar is split into isolated vs dewatered damage.
# Components scaled to per-MW so the stacked bar height equals damage_per_mw.
bar_long <- onscale %>%
  mutate(
    per_mw_isolated  = damage_isolated  / power_mw,
    per_mw_dewatered = damage_dewatered / power_mw,
    dam_label        = paste0(thesh_lat, " (", subc_id, ")",
                              ifelse(n_dams > 1,
                                     paste0(" [", n_dams, " dams]"), ""))
  ) %>%
  arrange(damage_per_mw) %>%                       # ascending -> worst on top
  mutate(dam_label = factor(dam_label, levels = unique(dam_label))) %>%
  select(dam_label, power_mw,
         Isolated = per_mw_isolated, Dewatered = per_mw_dewatered) %>%
  pivot_longer(c(Isolated, Dewatered),
               names_to = "component", values_to = "per_mw") %>%
  mutate(component = factor(component, levels = c("Dewatered", "Isolated")))

offscale_note <- if (nrow(offscale) > 0) {
  paste0("Off-scale: ", offscale$thesh_lat[1], " (reach ", offscale$subc_id[1],
         ", damage/MW = ", round(offscale$damage_per_mw[1]),
         "), near the outlet, fragments the whole sub-basin")
} else NULL

p1 <- ggplot(bar_long,
             aes(x = dam_label, y = per_mw, fill = component)) +
  geom_col(width = 0.7, alpha = 0.9) +
  coord_flip() +
  scale_fill_manual(values = c("Isolated"  = col_isolated,
                               "Dewatered" = col_dewatered),
                    name = "Damage component",
                    breaks = c("Isolated", "Dewatered")) +
  labs(
    x = "Planned dam (location, reach)",
    y = "Connectivity + dewatering damage per MW",
    title = "Planned dams ranked by habitat damage per MW",
    subtitle = paste0("Damage = summed SDM suitability of isolated (upstream) ",
                      "and dewatered (downstream 2 km) reaches",
                      if (!is.null(offscale_note)) paste0("\n", offscale_note) else "")
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top",
        plot.subtitle = element_text(size = 8, colour = "grey40"),
        axis.text.y   = element_text(size = 7))

png("prioritization/maps/fig1_dam_ranking_bars.png",
    width = 8, height = 8, units = "in", res = 200)
print(p1); dev.off()
message("  Saved: prioritization/maps/fig1_dam_ranking_bars.png")

# ============================================================
# STEP 3: FIG 2 -- dam damage map
# ============================================================

message("\n=== Step 3: Fig 2 -- dam damage map ===")

dam_sf <- dam_rank %>%
  filter(!is.na(longitude_snapped)) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

# Split off the off-scale (outlet) dam so it doesn't compress the colour
# and size scales for the other dams.
dam_sf_on  <- dam_sf %>% filter(damage_per_mw <= OFFSCALE_CUTOFF)
dam_sf_off <- dam_sf %>% filter(damage_per_mw >  OFFSCALE_CUTOFF)

message("  On-scale dams: ", nrow(dam_sf_on),
        " | off-scale (outlet, drawn separately): ", nrow(dam_sf_off))

p2 <- ggplot() +
  geom_sf(data = stream_lines, colour = "grey80", linewidth = 0.3) +
  geom_sf(data = dam_sf_on,
          aes(size = damage_total, colour = damage_total),
          alpha = 0.85) +
  scale_colour_viridis_c(option = "inferno", direction = -1,
                         name = "Total damage\n(SDM suitability)") +
  scale_size_continuous(range = c(1.5, 8),
                        name = "Total damage\n(SDM suitability)") +
  guides(colour = guide_legend(), size = guide_legend())     # merge legends

if (nrow(dam_sf_off) > 0) {
  p2 <- p2 +
    geom_sf(data = dam_sf_off, shape = 21, fill = NA,
            colour = "black", stroke = 1.1, size = 9) +
    geom_sf_text(data = dam_sf_off,
                 aes(label = paste0(thesh_lat, "\n(outlet, damage ",
                                    round(damage_total), ")")),
                 size = 2.8, colour = "black",
                 nudge_y = 0.012, lineheight = 0.9)
}

p2 <- p2 +
  labs(title = "Planned-dam habitat damage across the Sarantaporos network",
       subtitle = paste0("Point size and colour = summed isolated + dewatered ",
                         "SDM suitability (outlet dam shown off-scale)")) +
  theme_void(base_size = 11) +
  theme(plot.subtitle = element_text(size = 9, colour = "grey40"),
        legend.position = "right")

png("prioritization/maps/fig2_dam_damage_map.png",
    width = 9, height = 7, units = "in", res = 200)
print(p2); dev.off()
message("  Saved: prioritization/maps/fig2_dam_damage_map.png")

# ============================================================
# STEP 4: FIG 3 -- current vs future priority comparison (30%)
# ============================================================

message("\n=== Step 4: Fig 3 -- priority comparison (30%) ===")

comparison <- fread("prioritization/comparison_30pct.csv")

comp_sf <- stream_lines %>%
  left_join(comparison %>% rename(subc_id = id), by = "subc_id") %>%
  mutate(status = factor(status,
                         levels = c("Both", "Current only",
                                    "Future only", "Neither")))

p3 <- ggplot(comp_sf) +
  geom_sf(aes(colour = status, linewidth = status == "Neither")) +
  scale_colour_manual(values = status_cols, name = "Priority status") +
  scale_linewidth_manual(values = c(`TRUE` = 0.3, `FALSE` = 0.9),
                         guide = "none") +
  labs(
    title = "Priority reaches: current vs future barrier scenario (30% target)",
    subtitle = paste0("Red = priority only without planned dams (lost once dams sever ",
                      "connectivity); blue = priority only under future dams")
  ) +
  theme_void(base_size = 11) +
  theme(plot.subtitle = element_text(size = 8, colour = "grey40"),
        legend.position = "right")

png("prioritization/maps/fig3_priority_comparison_30pct.png",
    width = 9, height = 7, units = "in", res = 200)
print(p3); dev.off()
message("  Saved: prioritization/maps/fig3_priority_comparison_30pct.png")

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("DAM RANKING FIGURES COMPLETE")
message(paste(rep("=", 80), collapse = ""))
message("  Fig 1: prioritization/maps/fig1_dam_ranking_bars.png")
message("  Fig 2: prioritization/maps/fig2_dam_damage_map.png")
message("  Fig 3: prioritization/maps/fig3_priority_comparison_30pct.png")
library(hydrographr)

# ============================================================================
# DOWNLOAD HUMAN FOOT PRINT
# ============================================================================
# Purpose: Download Human Foot Print to use as penalty layer in the prioritization exercise
# Input: GLobal layer at 100 m resolution
# Output: Cropped raster layer for the study area
# ============================================================================
# Date: 2026-04-29
# ============================================================================

# ============================================================================
# SETUP
# ============================================================================

#source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
#BASE_DIR
#setwd(BASE_DIR)
# setwd("/mnt/shared/workflow_paper/data/spatial/")

# ============================================================================
# DOWNLOAD HUMAN FOOT PRINT FILE
# ============================================================================

# download.file("https://data.source.coop/vizzuality/hfp-100/hfp_2021_100m_v1-2_cog.tif", destfile = "spatial/hfp_2021_100m_v1-2_cog.tif")

# ============================================================================
# CROP FILE TO STUDY AREA REGION
# ============================================================================

crop_to_extent(raster_layer="spatial/hfp_2021_100m_v1-2_cog.tif",
                     vector_layer="spatial/subbasin_sarantaporos/subbasin_polygon.gpkg",
                     out_dir="spatial/",
                     file_name="hfp_crop.tif",
                     read = FALSE,
                     quiet = TRUE)

# ============================================================================
# SPECIFY PROJECTION SYSTEM
# ============================================================================

hfp <- terra::rast("spatial/hfp_crop.tif")
hfp_wgs <- terra::project(hfp, "EPSG:4326")
terra::writeRaster(hfp_wgs, "spatial/hfp_wgs.tif", overwrite = TRUE)

# ============================================================================
# CALCULATE ZONAL STATISTICS
# ============================================================================

# First we need to prepare the raster file of sub-catchments for the study area

# The study area overlaps with two tiles
tile_id <- c("h20v04")

# Download the .tif tiles for the sub-catchment variable
download_tiles(variable = "sub_catchment", tile_id = tile_id, file_format = "tif",
              download_dir = "spatial/")

# Crop orginal raster files to the extent of the study area
tfiles  <- list.files("spatial/r.watershed/sub_catchment_tiles20d",
                        pattern = ".tif$",
                        full.names = TRUE)

for(rast in tfiles) {
      crop_to_extent(
        raster_layer = tfiles,
        vector_layer = "spatial/subbasin_sarantaporos/subbasin_polygon.gpkg",
        out_dir = "spatial/",
        file_name = "subcatchment_sarantaporos.tif",
        read = FALSE,
        quiet = TRUE)
  }

# # Merge the two cropped files
# merge_tiles(tile_dir=".",
#             tile_names = list.files(".", full.names = FALSE,
#                                     pattern = "sub_catchment_.*_crop\\.tif"),
#             out_dir=".",
#             file_name = "subcatchment_vjosa.tif",
#             read = FALSE)

# Calculate the zonal statistics
stats_table_zon <- extract_zonal_stat(
                    data_dir = paste0(getwd(), "/spatial/"),
                    subc_layer = "spatial/subcatchment_sarantaporos.tif",
                    subc_id = "all",
                    var_layer = "hfp_wgs.tif",
                    out_dir = "spatial/",
                    file_name = "hfp_zonal_stats.csv",
                    n_cores = 1)


# ============================================================
# SENSITIVITY: boundary penalty sweep
#   How much does the current->future priority shift depend on the
#   boundary (connectivity) penalty? At penalty = 0 connectivity is
#   ignored (dams cannot matter); as penalty rises, the barrier-aware
#   connectivity should reshape priorities more. We solve both scenarios
#   at the comparison target (30%) across a range of penalties and report
#   the shift (reaches/km that change priority status).
# ============================================================

message("\n=== Sensitivity: boundary penalty sweep (", COMPARISON_TARGET * 100, "%) ===")

PENALTY_GRID <- c(0, 0.001, 0.01, 0.03, 0.1, 0.3, 1)

penalty_sweep <- lapply(PENALTY_GRID, function(pen) {

  # solve current & future at this penalty (cost = HFI only)
  solve_pen <- function(bmat) {
    p <- problem(pu_dat, spec_dat, cost_column = "cost_hfi", rij = puvspr_dat) %>%
      add_min_set_objective() %>%
      add_relative_targets(COMPARISON_TARGET) %>%
      add_binary_decisions() %>%
      add_cbc_solver(gap = SOLVER_GAP, threads = N_THREADS, verbose = FALSE)
    # penalty = 0 -> no boundary term at all (avoids degenerate 0-weight call)
    if (pen > 0)
      p <- p %>% add_boundary_penalties(penalty = pen, data = bmat)
    solve(p)
  }

  s_cur <- solve_pen(bmat_current)
  s_fut <- solve_pen(bmat_future)

  sel_cur <- s_cur$id[s_cur$solution_1 == 1]
  sel_fut <- s_fut$id[s_fut$solution_1 == 1]

  cur_only <- setdiff(sel_cur, sel_fut)
  fut_only <- setdiff(sel_fut, sel_cur)
  shared   <- intersect(sel_cur, sel_fut)

  km <- function(ids) sum(pu_dat$length_km[pu_dat$id %in% ids], na.rm = TRUE)

  data.frame(
    penalty        = pen,
    n_current      = length(sel_cur),
    n_future       = length(sel_fut),
    km_current     = round(km(sel_cur), 1),
    km_future      = round(km(sel_fut), 1),
    n_shared       = length(shared),
    n_current_only = length(cur_only),
    n_future_only  = length(fut_only),
    km_shift       = round(km(cur_only) + km(fut_only), 1),   # total km that changes status
    jaccard        = round(length(shared) / length(union(sel_cur, sel_fut)), 3)
  )
}) %>% rbindlist()

message("\n  Boundary-penalty sweep (current vs future, ",
        COMPARISON_TARGET * 100, "% target):")
print(penalty_sweep)
message("\n  Interpretation: km_shift = km changing priority status between",
        " scenarios; jaccard near 1 = scenarios agree (dams barely move",
        " priorities); lower jaccard / higher km_shift = penalty makes the",
        " barrier-aware connectivity bite.")

fwrite(penalty_sweep, "prioritization/boundary_penalty_sweep.csv")
message("  Saved: prioritization/boundary_penalty_sweep.csv")

# quick plot: shift vs penalty
p_pen <- ggplot(penalty_sweep, aes(x = factor(penalty))) +
  geom_col(aes(y = km_shift), fill = "#d73027", alpha = 0.85, width = 0.6) +
  geom_text(aes(y = km_shift, label = km_shift), vjust = -0.4, size = 3) +
  labs(x = "Boundary penalty",
       y = "Priority shift current -> future (km)",
       title = "Sensitivity of priority shift to the boundary penalty",
       subtitle = paste0("Minimum-set, cost = HFI, ", COMPARISON_TARGET * 100,
                         "% target; km changing priority status between scenarios")) +
  theme_bw(base_size = 11) +
  theme(plot.subtitle = element_text(size = 9, colour = "grey40"))

png("prioritization/maps/boundary_penalty_sweep.png",
    width = 7, height = 5, units = "in", res = 200)
print(p_pen); dev.off()
message("  Saved: prioritization/maps/boundary_penalty_sweep.png")



# ============================================================
# SENSITIVITY: optimality gap
#   The gap is the solver's guarantee that the returned solution is within
#   X% of the true minimum cost (gap = (upper-lower)/lower). A looser gap
#   is faster but less precise. We check that the solution is stable across
#   gaps (current scenario, comparison target) and record solve time.
# ============================================================

message("\n=== Sensitivity: optimality gap ===")

GAP_GRID <- c(0.1, 0.05, 0.01, 0.001, 0)   # 0 = prove optimality

gap_sens <- lapply(GAP_GRID, function(g) {
  t0 <- Sys.time()
  s <- problem(pu_dat, spec_dat, cost_column = "cost_hfi", rij = puvspr_dat) %>%
    add_min_set_objective() %>%
    add_relative_targets(COMPARISON_TARGET) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(penalty = BOUNDARY_PENALTY, data = bmat_current) %>%
    add_cbc_solver(gap = g, threads = N_THREADS, verbose = FALSE) %>%
    solve()
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  sel  <- s$id[s$solution_1 == 1]
  data.frame(
    gap         = g,
    n_selected  = length(sel),
    selected_km = round(sum(pu_dat$length_km[pu_dat$id %in% sel], na.rm = TRUE), 1),
    total_cost  = round(sum(s$cost[s$solution_1 == 1], na.rm = TRUE), 4),
    solve_secs  = round(secs, 1),
    sel_ids     = I(list(sel))   # keep for Jaccard vs the tightest
  )
}) %>% rbindlist()

# Jaccard of each gap's solution vs the gap=0 (proven-optimal) solution
opt_sel <- gap_sens$sel_ids[[which(gap_sens$gap == 0)]]
gap_sens[, jaccard_vs_opt := sapply(sel_ids, function(s)
  length(intersect(s, opt_sel)) / length(union(s, opt_sel)))]

print(gap_sens[, .(gap, n_selected, selected_km, total_cost, solve_secs, jaccard_vs_opt)])
fwrite(gap_sens[, .(gap, n_selected, selected_km, total_cost, solve_secs, jaccard_vs_opt)],
       "prioritization/sensitivity_gap.csv")



# install.packages("fwtraits")
library(fwtraits)
library(tidyverse)
library(data.table)

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- BASE_DIR
setwd(BASE_DIR)

# ============================================================
# STEP 1: Authenticate with freshwaterecology.info
# ============================================================

# First time: register at https://www.freshwaterecology.info/
# and request an API key from the database managers.
# Store your API key in .Renviron:
#   fw_setapikey()
#   Add line: API_KEY=your_api_key_here
#   Then restart R

fw_be4ustart()
fw_setapikey()  # will prompt for API key or read from .Renviron

# ============================================================
# STEP 2: Check what parameters are available for fish
# ============================================================

# Get the database guide — lists all organism groups and parameters
db_guide <- fw_dbguide()

# Filter to fish parameters only
fish_params <- db_guide %>%
  filter(grepl("fi", organismgroup, ignore.case = TRUE))

cat("Available fish parameters:", nrow(fish_params), "\n")
print(fish_params %>% select(parameters_cleaned, DataType) %>% distinct())

# ============================================================
# STEP 3: Prepare species list
# ============================================================

occurrences <- fread("points_snapped/fish/fish_all_species_snapped.csv")

# fwtraits expects "Genus species" format (with space)
species_list <- data.frame(
  scientificName = gsub("_", " ", unique(occurrences$species))
)

cat("Species to query:", nrow(species_list), "\n")

# ============================================================
# STEP 4: Fetch traits — key ecological parameters
# ============================================================

# Parameters relevant for connectivity/fragmentation analysis:
# - rheophily habitat: flow preference (rheophilic vs limnophilic)
# - migration: migratory behaviour
# - spawning habitat: reproductive strategy
# - feeding diet adult: trophic guild
# - temperature tolerance: thermal sensitivity
# - body size: max body length

# Fetch traits
fish_traits_fwe <- fw_fetchdata(
  data = species_list,
  ecoparams = c(
    "rheophily habitat",
    "migration",
    "spawning habitat",
    "feeding diet adult",
    "temperature tolerance",
    "body size"
  ),
  taxonomic_column = "scientificName",
  organismgroup = "fi"
)

cat("\nTraits retrieved for", length(unique(fish_traits_fwe$taxasearched$clean)), "species\n")

# ============================================================
# STEP 5: Check coverage — which species have data?
# ============================================================

# How many species have each trait?
trait_coverage <- fish_traits_fwe$ecodata %>%
  group_by(Parameter) %>%
  summarize(
    n_species_with_data = sum(!is.na(value)),
    n_species_total = n(),
    pct_coverage = round(100 * sum(!is.na(value)) / n(), 1),
    .groups = "drop"
  )

cat("\nTrait coverage:\n")
print(trait_coverage)

# Which species have NO data at all?
species_with_data <- fish_traits_fwe %>%
  filter(!is.na(value)) %>%
  pull(scientificName) %>%
  unique()

species_missing <- setdiff(species_list$scientificName, species_with_data)
cat("\nSpecies with no data in freshwaterecology.info:", length(species_missing), "\n")
if (length(species_missing) > 0) print(species_missing)

# ============================================================
# STEP 6: Also try fetching ALL available fish parameters
# ============================================================

# Get all parameter names for fish
all_fish_params <- fish_params$parameter_cleaned

# Fetch everything (may take a while)
fish_traits_all <- fw_fetchdata(
  data = species_list,
  ecoparams = all_fish_params,
  taxonomic_column = "scientificName",
  organismgroup = "fi"
)

# Summarize what we got
all_coverage <- fish_traits_all %>%
  filter(!is.na(value)) %>%
  group_by(parameter) %>%
  summarize(n_species = n_distinct(scientificName), .groups = "drop") %>%
  arrange(desc(n_species))

cat("\nAll parameters with data (sorted by coverage):\n")
print(all_coverage, n = 30)

# ============================================================
# STEP 7: Save
# ============================================================

fwrite(fish_traits_fwe, "traits/fwtraits_fish_greece.csv")
fwrite(fish_traits_all, "traits/fwtraits_fish_greece_all.csv")
fwrite(as.data.frame(trait_coverage), "traits/fwtraits_coverage_summary.csv")

cat("\nSaved:\n")
cat("  traits/fwtraits_fish_greece.csv (selected parameters)\n")
cat("  traits/fwtraits_fish_greece_all.csv (all parameters)\n")
cat("  traits/fwtraits_coverage_summary.csv\n")





migration <- fw_fetchdata(data = 'Abramis brama',
                          organismgroup = 'fi',
                          ecoparams = 'migration',
                          cachefolder = 'cache',
                          warn = TRUE,
                          inform = TRUE,
                          details = TRUE)#the species spelling is checked
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_dispersal_classification.R
# Estimate species dispersal distances using fishmove,
# bin into quantile classes, assign dispersal probability (PD)
#
# Strategy:
#   1. Run fishmove(L, AR, SO, T) for 11 species with known aspect ratio
#   2. Fit model: log(sigma_mob) ~ log(TL) + sqrt(SO) to predict dispersal
#      for species without AR data (R² = 0.985)
#   3. Classify all species into 7 dispersal classes (PD = 0.3 to 0.9)
#
# References:
#   Radinger & Wolter (2014) Patterns and predictors of fish dispersal in rivers
#   Baldan et al. (2022) riverconn: an R package to assess river connectivity
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(readxl)
library(data.table)
# pak:pak("cran/fishmove")
library(fishmove)

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select
rename <- dplyr::rename
group_by <- dplyr::group_by

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================
T_REF          <- 3650   # 10-year dispersal window (population-level; Mekong study)
SO_DEFAULT     <- 5      # default stream order for species without occurrence data
N_DISP_CLASSES <- 7      # number of dispersal classes
PD_MIN         <- 0.3    # minimum dispersal probability
PD_MAX         <- 0.9    # maximum dispersal probability


# ============================================================
# STEP 1: Read trait data
# ============================================================

fish_traits <- read_xlsx(
  "points_original/fish/Fish distributional & traits data (1).xlsx",
  sheet = "Traits"
) %>%
  dplyr::rename(species = Species) %>%
  dplyr::select(species, max_TL, Caudal_fin, Migration) %>%
  dplyr::mutate(
    Caudal_fin_label = case_when(
      Caudal_fin == 1 ~ "Rounded",
      Caudal_fin == 2 ~ "Truncated",
      Caudal_fin == 3 ~ "Emarginate",
      Caudal_fin == 4 ~ "Forked",
      Caudal_fin == 5 ~ "Heterocercal",
      Caudal_fin == 6 ~ "Pointed",
      TRUE ~ "Unknown"
    ),
    Migration_label = case_when(
      Migration == 0 ~ "Non-migratory",
      Migration == 1 ~ "Potamodromous",
      Migration == 2 ~ "Long_distance",
      TRUE ~ "Unknown"
    )
  )

message("Trait data: ", nrow(fish_traits), " species")


# ============================================================
# STEP 2: Get species-specific stream order from occurrences
# ============================================================

occurrences <- fread("points_snapped/fish/fish_all_species_snapped.csv")

species_stream_order <- occurrences %>%
  group_by(species) %>%
  dplyr::summarise(
    max_SO = max(strahler, na.rm = TRUE),
    n_obs  = n(),
    .groups = "drop"
  )

fish_traits <- fish_traits %>%
  left_join(species_stream_order, by = "species")

# Fill missing stream order with default
missing_SO <- fish_traits %>% filter(is.na(max_SO))
if (nrow(missing_SO) > 0) {
  message("WARNING: ", nrow(missing_SO), " species have no stream order data. Using default SO=", SO_DEFAULT)
  fish_traits <- fish_traits %>%
    mutate(max_SO = ifelse(is.na(max_SO), SO_DEFAULT, max_SO))
}


# ============================================================
# STEP 3: Read aspect ratio data and average duplicates
# ============================================================

ar_data <- read.csv("traits/AspectRatioData.csv") %>%
  group_by(species) %>%
  summarize(AR = mean(AR, na.rm = TRUE), .groups = "drop") %>%
  mutate(species = gsub(" ", "_", species))

# Handle name mismatch
ar_data <- ar_data %>%
  mutate(species = ifelse(species == "Gasterosteus_aculeatus",
                          "Gasterosteus_gymnurus", species))

message("AR data for ", nrow(ar_data), " species")


# ============================================================
# STEP 4: Run fishmove for calibration species (with AR)
# ============================================================

calibration <- fish_traits %>%
  inner_join(ar_data, by = "species")

message("Calibration species (with TL + AR): ", nrow(calibration))

fm_results <- data.frame()

for (i in 1:nrow(calibration)) {
  sp    <- calibration$species[i]
  tl_mm <- calibration$max_TL[i] * 10  # fishmove expects mm
  ar    <- calibration$AR[i]
  so    <- calibration$max_SO[i]

  cat("  fishmove:", sp, "(L=", tl_mm, "mm, AR=", round(ar, 2), ", SO=", so, ")\n")

  tryCatch({
    fm <- fishmove(L = tl_mm, AR = ar, SO = so, T = T_REF,
                   rep = 200, seed = 42)

    fm_results <- rbind(fm_results, data.frame(
      species      = sp,
      sigma_mob_m  = fm$pred.fishmove["fit", "sigma_mob", , , , ]
    ))
  }, error = function(e) cat("    ERROR:", conditionMessage(e), "\n"))
}


# ============================================================
# STEP 5: Fit predictive model for species without AR
# Model: log(sigma_mob) ~ log(TL) + sqrt(SO)
# This mirrors fishmove's internal regression structure minus AR
# ============================================================

cal_data <- fm_results %>%
  left_join(fish_traits, by = "species") %>%
  mutate(log_sigma_mob = log(sigma_mob_m),
         log_TL  = log(max_TL),
         sqrt_SO = sqrt(max_SO))

best_model <- lm(log_sigma_mob ~ log_TL + sqrt_SO, data = cal_data)

cat("\n--- Predictive model summary ---\n")
print(summary(best_model))


# ============================================================
# STEP 6: Predict dispersal for ALL species
# ============================================================

predict_data <- fish_traits %>%
  mutate(log_TL  = log(max_TL),
         sqrt_SO = sqrt(max_SO))

predictions <- predict(best_model, newdata = predict_data,
                       interval = "prediction", level = 0.95)

fish_traits <- fish_traits %>%
  mutate(
    sigma_mob_predicted = exp(predictions[, "fit"]),
    sigma_mob_lwr       = exp(predictions[, "lwr"]),
    sigma_mob_upr       = exp(predictions[, "upr"]),
    # Use fishmove value for calibration species, prediction for the rest
    sigma_mob_final = ifelse(
      species %in% fm_results$species,
      fm_results$sigma_mob_m[match(species, fm_results$species)],
      sigma_mob_predicted
    ),
    source = ifelse(species %in% fm_results$species,
                    "AR available", "AR unavailable")
  )


# ============================================================
# STEP 7: Bin into dispersal classes (Mekong method)
# 7 quantile-based classes mapped to PD = 0.3 to 0.9
# ============================================================

quantile_breaks <- quantile(fish_traits$sigma_mob_final,
                            probs = seq(0, 1, length.out = N_DISP_CLASSES + 1),
                            na.rm = TRUE)

pd_values <- seq(PD_MIN, PD_MAX, length.out = N_DISP_CLASSES)

fish_traits <- fish_traits %>%
  mutate(
    dispersal_class = cut(sigma_mob_final,
                          breaks = quantile_breaks,
                          labels = FALSE,
                          include.lowest = TRUE),
    dispersal_prob = pd_values[dispersal_class]
  )


# ============================================================
# STEP 8: Save output
# ============================================================

fish_dis_class <- fish_traits %>%
  dplyr::select(
    species,
    distance = sigma_mob_final,
    dispersal_prob,
    dispersal_class,
    source,
    max_TL,
    max_SO,
    Migration_label,
    Caudal_fin_label
  ) %>%
  arrange(species)

fwrite(fish_dis_class, "traits/fish_dis_class.txt", sep = "\t")
message("\nfish_dis_class.txt saved")


# ============================================================
# STEP 9: Diagnostic plots
# ============================================================

# Plot 1: Calibration -- fishmove (with AR) vs predicted (without AR)
cal_check <- cal_data %>%
  mutate(predicted = exp(predict(best_model)))

p1 <- ggplot(cal_check, aes(x = sigma_mob_m, y = predicted)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_text(aes(label = species), hjust = -0.1, vjust = -0.5, size = 2.5) +
  scale_x_log10() + scale_y_log10() +
  labs(x = "sigma_mob (m) — with AR",
       y = "sigma_mob (m) — without AR",
       title = "Calibration: with vs without Aspect Ratio") +
  theme_bw()

# Plot 2: All species dispersal by migration type and data source
p2 <- ggplot(fish_dis_class, aes(x = max_TL, y = distance / 1000,
                                 color = Migration_label, shape = source)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_y_log10() + scale_x_log10() +
  labs(x = "Max total length (cm)",
       y = "Dispersal distance (km, log scale)",
       color = "Migration",
       title = "Dispersal estimates for all Greek freshwater fish") +
  theme_bw()

cowplot::plot_grid(p1, p2, ncol = 1, align = "v") %>%
  ggsave("traits/dispersal_diagnostics.png", ., width = 10, height = 12)

cat("\nPlots saved: traits/dispersal_diagnostics.png\n")


# ============================================================
# STEP 10: Summary
# ============================================================

cat("\n========== SUMMARY ==========\n")
cat("Calibration species (AR available):", sum(fish_dis_class$source == "AR available"), "\n")
cat("Predicted species (AR unavailable):", sum(fish_dis_class$source == "AR unavailable"), "\n\n")

cat("Dispersal distance (km) by migration type:\n")
fish_dis_class %>%
  group_by(Migration_label) %>%
  summarize(
    n = n(),
    median_km = round(median(distance / 1000), 1),
    min_km = round(min(distance / 1000), 1),
    max_km = round(max(distance / 1000), 1),
    .groups = "drop"
  ) %>% print()

cat("\nDispersal probability distribution:\n")
print(table(fish_dis_class$dispersal_prob))
library(imager)
library(magick)

# Load image
img <- load.image("traits/barbus_prespensis01-500.jpg")

# Display and click points
plot(img)
# Click to mark:
# 1-2: Total length line (for scale)
# 3-4: Fin height (tip to tip)
# Then trace fin outline for area

# Manual measurement script
measure_CAR <- function(img_path, total_length_cm = 30) {

  # Load and display image LARGE
  img <- image_read(img_path)

  # Get image info
  info <- image_info(img)
  cat("Image dimensions:", info$width, "x", info$height, "\n\n")

  # Open a large plotting window
  dev.new(width = 14, height = 10)  # Large window

  # Display image
  plot(img)

  # STEP 1: Set scale
  cat("STEP 1: Click TWO points along the 30cm body length\n")
  cat("(Click point 1, then point 2)\n")
  scale_points <- locator(2, type = "p", pch = 20, col = "red", cex = 2)

  # Draw scale line
  lines(scale_points$x, scale_points$y, col = "red", lwd = 3)

  scale_pixels <- sqrt((scale_points$x[2] - scale_points$x[1])^2 +
                         (scale_points$y[2] - scale_points$y[1])^2)
  pixels_per_cm <- scale_pixels / total_length_cm

  cat("Scale set:", round(pixels_per_cm, 2), "pixels per cm\n\n")

  # STEP 2: Measure fin height
  cat("STEP 2: Click fin height (top lobe tip, then bottom lobe tip)\n")
  fin_points <- locator(2, type = "p", pch = 20, col = "blue", cex = 2)

  # Draw height line
  lines(fin_points$x, fin_points$y, col = "blue", lwd = 3)

  fin_height_pixels <- sqrt((fin_points$x[2] - fin_points$x[1])^2 +
                              (fin_points$y[2] - fin_points$y[1])^2)
  fin_height_cm <- fin_height_pixels / pixels_per_cm

  cat("Fin height:", round(fin_height_cm, 2), "cm\n\n")

  # STEP 3: Trace fin outline - FIXED VERSION
  cat("STEP 3: Trace fin outline\n")
  cat("Click points around the fin edge\n")
  cat("Press ESC when done (not right-click)\n\n")

  # Collect points until user presses ESC
  area_points <- locator(n = 1000, type = "o", col = "green", lwd = 2)

  # Close polygon
  if (length(area_points$x) > 2) {
    lines(c(area_points$x[length(area_points$x)], area_points$x[1]),
          c(area_points$y[length(area_points$y)], area_points$y[1]),
          col = "green", lwd = 2)
  }

  # Calculate polygon area (shoelace formula)
  n <- length(area_points$x)
  if (n < 3) {
    stop("Need at least 3 points to calculate area")
  }

  # Add first point to end to close polygon
  x <- c(area_points$x, area_points$x[1])
  y <- c(area_points$y, area_points$y[1])

  area_pixels <- 0.5 * abs(sum(x[1:n] * y[2:(n+1)] - x[2:(n+1)] * y[1:n]))
  area_cm2 <- area_pixels / (pixels_per_cm^2)

  cat("Fin area:", round(area_cm2, 2), "cm²\n")
  cat("Number of outline points:", n, "\n\n")

  # Calculate CAR
  CAR <- (fin_height_cm^2) / area_cm2

  # Summary
  cat("=" , rep("=", 50), "\n", sep = "")
  cat("RESULTS:\n")
  cat("  Fin height (h):", round(fin_height_cm, 2), "cm\n")
  cat("  Fin area (A):  ", round(area_cm2, 2), "cm²\n")
  cat("  CAR = h²/A:    ", round(CAR, 2), "\n")
  cat("=" , rep("=", 50), "\n", sep = "")

  # Sanity check
  if (CAR < 1.5) {
    warning("CAR is very low (<1.5) - check if fin outline is correct")
  } else if (CAR > 5.0) {
    warning("CAR is very high (>5.0) - check if fin outline is correct")
  } else {
    cat("\n✓ CAR value looks reasonable for a forked fin (2.5-4.0)\n")
  }

  return(list(
    height_cm = fin_height_cm,
    area_cm2 = area_cm2,
    CAR = CAR,
    n_points = n
  ))
}

# Use it
results <- measure_CAR("traits/barbus_prespensis01-500.jpg", total_length_cm = 30)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03_pci_calculation.R
# Calculate PCI for current vs future dam scenarios + Fragmentation Index
# ADAPTED FOR MULTIPLE DISCONNECTED BASINS
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(riverconn)
library(data.table)
library(dplyr)
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/pci_sparse.R")


# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select
rename <- dplyr::rename
group_by <- dplyr::group_by

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================
MIN_SUBCATCHMENTS <- 2   # species must occupy >= this many subcatchments

# ============================================================
# READ INPUTS
# ============================================================

river_graph_current <- readRDS("spatial/stream_networks/river_graph_current.RDS")
river_graph_future  <- readRDS("spatial/stream_networks/river_graph_future.RDS")

# Convert length_reach from meters to tens of km
# (matching Mekong study convention; ensures param^distance
# produces meaningful values with dispersal_prob range 0.3-0.9)
V(river_graph_current)$length_reach <- V(river_graph_current)$length_reach / 10000
V(river_graph_future)$length_reach  <- V(river_graph_future)$length_reach / 10000

occurrences <- fread("points_snapped/fish/fish_all_species_snapped.csv")

fish_dis_class <- fread("traits/fish_dis_class.txt") %>%
  filter(!is.na(dispersal_prob))

# ============================================================
# EXTRACT BASIN_ID FROM GRAPH (same topology for both scenarios)
# ============================================================
node_basins <- data.frame(
  name = V(river_graph_current)$name,
  basin_id = V(river_graph_current)$basin_id
) %>%
  filter(!is.na(basin_id))

message("Network has ", length(unique(node_basins$basin_id)), " basins")

# ============================================================
# PREPARE SPECIES LIST
# ============================================================
fish_distribution <- occurrences %>%
  select(species, subc_id) %>%
  distinct() %>%
  group_by(species) %>%
  filter(n() >= MIN_SUBCATCHMENTS) %>%
  ungroup()

fish_species <- fish_distribution %>%
  filter(species %in% fish_dis_class$species) %>%
  pull(species) %>%
  unique()

message(length(fish_species), " species will be processed")

# ============================================================
# HELPER: Attach species presence weights to a graph
# ============================================================
attach_presence <- function(g, presence_df) {
  graph_from_data_frame(
    as_data_frame(g, "edges"),
    v = as_data_frame(g, "vertices") %>%
      select(-weight) %>%
      left_join(presence_df, by = "name") %>%
      mutate(weight = ifelse(is.na(weight), 0, weight))
  )
}

# ============================================================
# PROCESS ALL SPECIES
# ============================================================

catchment_pci_list    <- list()
start_time <- Sys.time()

for (name_loop in fish_species) {

  # Species presence
  subcatchments_presence <- fish_distribution %>%
    filter(species == name_loop) %>%
    mutate(name = as.character(subc_id)) %>%
    select(name) %>%
    distinct() %>%
    mutate(weight = 1)

  # Which basins?
  species_basins <- node_basins %>%
    filter(name %in% subcatchments_presence$name) %>%
    pull(basin_id) %>%
    unique()

  if (length(species_basins) == 0) next

  # Dispersal parameter
  dispersal_exp <- fish_dis_class %>%
    filter(species == name_loop) %>%
    pull(dispersal_prob)

  # Loop over basins
  for (basin_loop in species_basins) {

    # Extract basin nodes
    basin_nodes <- node_basins %>%
      filter(basin_id == basin_loop) %>%
      pull(name)

    # Species presence in this basin
    basin_presence <- subcatchments_presence %>%
      filter(name %in% basin_nodes)

    if (nrow(basin_presence) < MIN_SUBCATCHMENTS) next

    # Extract basin subgraphs for both scenarios
    basin_graph_current <- induced_subgraph(river_graph_current, basin_nodes)
    basin_graph_future  <- induced_subgraph(river_graph_future, basin_nodes)

    # Attach presence to both
    graph_current <- attach_presence(basin_graph_current, basin_presence)
    graph_future  <- attach_presence(basin_graph_future, basin_presence)

    # Calculate PCI — CURRENT scenario
    pci_current <- pci_sparse(
      graph_current,
      weight     = "weight",
      field_B    = "length_reach",
      param      = dispersal_exp,
      index_type = "full"
    ) %>%
      mutate(species  = name_loop,
             basin_id = basin_loop,
             scenario = "current")

    # Calculate PCI — FUTURE scenario
    pci_future <- pci_sparse(
      graph_future,
      weight     = "weight",
      field_B    = "length_reach",
      param      = dispersal_exp,
      index_type = "full"
    ) %>%
      mutate(species  = name_loop,
             basin_id = basin_loop,
             scenario = "future")

    # Fragmentation Index: % connectivity loss from planned dams
    fi_val <- (pci_current$index - pci_future$index) / pci_current$index * 100

    result_key <- paste0(name_loop, "_basin", basin_loop)

    catchment_pci_list[[result_key]] <- bind_rows(pci_current, pci_future) %>%
      mutate(FI = ifelse(scenario == "future", fi_val, NA))


    # Cleanup
    rm(basin_graph_current, basin_graph_future, graph_current, graph_future,
       pci_current, pci_future, basin_presence)

  } # End basin loop

  # Progress
  sp_idx <- which(fish_species == name_loop)
  if (sp_idx %% 10 == 0) {
    message("Processed ", sp_idx, " / ", length(fish_species), " species")
    gc(verbose = FALSE)
  }

} # End species loop

end_time <- Sys.time()
elapsed_time <- end_time - start_time
message("Processing complete!")
message("Time elapsed: ", round(elapsed_time, 2), " ", units(elapsed_time))

# ============================================================
# SAVE RESULTS
# ============================================================

# one PCI value per species per basin.
# It's a single number summarizing overall connectivity across
# the entire basin for that species. This is what we use for
# the Fragmentation Index (FI) — comparing current vs future
# at the whole-basin level.
catchment_pci_full    <- do.call(rbind, catchment_pci_list)

dir.create("connectivity/pci", recursive = TRUE, showWarnings = FALSE)
saveRDS(catchment_pci_full,    "connectivity/pci/catchment_pci_full.RDS")
# catchment_pci_full <- readRDS("connectivity/pci/catchment_pci_full.RDS")
# ============================================================
# SUMMARY TABLE
# ============================================================

fi_summary <- catchment_pci_full %>%
  pivot_wider(
    id_cols = c(species, basin_id),
    names_from = scenario,
    values_from = c(index, FI)
  ) %>%
  rename(
    PCI_current = index_current,
    PCI_future  = index_future,
    FI = FI_future
  ) %>%
  select(species, basin_id, PCI_current, PCI_future, FI) %>%
  arrange(desc(FI))

write.table(fi_summary, "connectivity/pci/fi_summary.txt",
            row.names = FALSE, quote = FALSE, sep = "\t")

message("\nDone! ", nrow(fi_summary), " species x basin combinations")

# ============================================================
# QUICK DIAGNOSTICS
# ============================================================

cat("\n========== RESULTS SUMMARY ==========\n")

cat("\nCatchment PCI by scenario:\n")
catchment_pci_full %>%
  group_by(scenario) %>%
  summarize(
    n = n(),
    min_pci = round(min(index, na.rm = TRUE), 3),
    median_pci = round(median(index, na.rm = TRUE), 3),
    max_pci = round(max(index, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>% print()

cat("\nFragmentation Index (connectivity loss from planned dams):\n")
fi_summary %>%
  summarize(
    n_total      = n(),
    n_FI_zero    = sum(FI == 0, na.rm = TRUE),
    n_FI_above0  = sum(FI > 0, na.rm = TRUE),
    n_FI_above10 = sum(FI > 10, na.rm = TRUE),
    n_FI_above25 = sum(FI > 25, na.rm = TRUE),
    median_FI    = round(median(FI, na.rm = TRUE), 1),
    max_FI       = round(max(FI, na.rm = TRUE), 1)
  ) %>% print()

cat("\nTop 10 most impacted species x basin:\n")
fi_summary %>% head(10) %>% print()

# ============================================================
# CHECK VJOSA
# ============================================================
cat("\n========== VJOSA BASIN (1292502) ==========\n")
fi_summary %>% filter(basin_id == 1292502) %>% print()


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 04_pci_visualizations.R
# Visualize PCI results: current vs future dam scenarios
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(data.table)
library(ggrepel)
library(leaflet)
library(sf)
library(htmlwidgets)
library(rnaturalearth)


# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")

BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# READ INPUTS
# ============================================================

fi_summary <- read.table("connectivity/pci/fi_summary_subhabitats.txt", header = TRUE)

basin_lookup <- fread("spatial/basin_name_lookup.csv") %>%
  mutate(basin_id = as.integer(basin_id))

fi_summary <- fi_summary %>%
  left_join(basin_lookup, by = "basin_id")

fish_dis_class <- fread("traits/fish_dis_class.txt")

# Join traits for coloring/faceting
fi_plot <- fi_summary %>%
  left_join(fish_dis_class %>%
              rename(species = species_name) %>%
              select(species, Migration_label, Caudal_fin_label,
                     dispersal_prob, max_TL),
            by = "species")


basin_polygons <- read_sf("spatial/stream_networks/basin_polygons.gpkg")

# ============================================================
# SCATTERPLOT: PCI current vs PCI future
# ============================================================

# Label only the most impacted points (top FI)
fi_plot <- fi_plot %>%
  mutate(label = ifelse(FI > 25,
                        paste0(gsub("_", " ", species), "\n(FI=", round(FI, 0), "%)"),
                        NA))

p1 <- ggplot(fi_plot, aes(x = PCI_current, y = PCI_future)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "grey50", linewidth = 0.5) +
  geom_point(aes(color = Migration_label), size = 2, alpha = 0.6) +
  geom_text_repel(aes(label = label), size = 2.5, max.overlaps = 15,
                  segment.color = "grey60", segment.size = 0.3,
                  na.rm = TRUE) +
  scale_color_manual(
    values = c("Non-migratory" = "#2196F3",
               "Potamodromous" = "#FF9800",
               "Long_distance" = "#F44336")
  ) +
  labs(x = "PCI (current dams)",
       y = "PCI (current + planned dams)",
       color = "Migration type") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_bw() +
  theme(legend.position = "right")

p1
ggsave("connectivity/pci/pci_current_vs_future.png",
       p1, width = 10, height = 8, dpi = 300)

cat("Plot saved: connectivity/pci/pci_current_vs_future.png\n")




# ============================================================
# TOP IMPACTED SPECIES × BASIN COMBINATIONS
# ============================================================

# Add readable basin names if available, otherwise use basin_id
# You can replace this with actual river names if you have a lookup table
fi_plot_top <- fi_summary %>%
  filter(FI > 0) %>%
  left_join(fish_dis_class %>%
              select(species, Migration_label),
            by = "species") %>%
  mutate(
    species_label = gsub("_", " ", species),
    # Combine species + basin for unique labels
    combo_label =paste0(species_label, " — ", basin_name)
  ) %>%
  arrange(desc(FI)) %>%
  head(25)


# Reorder factor by FI for plotting
fi_plot_top <- fi_plot_top %>%
  mutate(combo_label = fct_reorder(combo_label, FI))

p_top <- ggplot(fi_plot_top, aes(x = FI, y = combo_label, fill = Migration_label)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(
    values = c("Non-migratory" = "#2196F3",
               "Potamodromous" = "#FF9800",
               "Long_distance" = "#F44336")
  ) +
  labs(x = "Fragmentation Index (%)",
       y = NULL,
       fill = "Migration type",
       title = "Top 25 most impacted species × basin combinations",
       subtitle = "% connectivity loss from planned dams") +
  theme_bw() +
  theme(axis.text.y = element_text(face = "italic", size = 9))

p_top

ggsave("connectivity/pci/top_impacted_species.png",
       p_top, width = 10, height = 8, dpi = 300)

cat("Plot saved: connectivity/pci/top_impacted_species.png\n")


# ============================================================
# LEAFLET MAP: Mean FI by basin
# ============================================================

# Aggregate FI by basin
fi_by_basin <- fi_summary %>%
  group_by(basin_name, basin_id) %>%
  summarize(
    mean_FI = mean(FI, na.rm = TRUE),
    n_species = n(),
    n_impacted = sum(FI > 0, na.rm = TRUE),
    .groups = "drop"
  )

# Join with spatial data and transform to WGS84
basins_plot <- basin_polygons %>%
  left_join(fi_by_basin, by = "basin_id") %>%
  st_transform(4326)

# Color palette
pal_fill <- colorNumeric("YlOrRd", domain = basins_plot$mean_FI, na.color = "grey90")

# Build map
m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%

  addPolygons(
    data = basins_plot,
    fillColor = ~pal_fill(mean_FI),
    fillOpacity = 0.7,
    color = "grey40",
    weight = 0.5,
    popup = ~paste0(
      "<b>Basin: </b>", basin_name, "<br>",
      "<b>Mean FI: </b>", round(mean_FI, 1), "%<br>",
      "<b>Species assessed: </b>", n_species, "<br>",
      "<b>Species impacted: </b>", n_impacted
    )
  ) %>%

  addLegend(
    position = "bottomright",
    pal = pal_fill,
    values = basins_plot$mean_FI,
    title = "Mean FI (%)",
    na.label = "No data"
  ) %>%

  addLayersControl(
    baseGroups = c("Light", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  )

m
saveWidget(m, "connectivity/pci/fi_map_by_basin.html", selfcontained = TRUE)
cat("Map saved: connectivity/pci/fi_map_by_basin.html\n")



#################
# same map but with ggplot
# ============================================================

# STATIC MAP: Mean FI by basin (ggplot)

# ============================================================



# Aggregate FI by basin

fi_by_basin <- fi_summary %>%
  group_by(basin_name, basin_id) %>%
  summarize(
    mean_FI = mean(FI, na.rm = TRUE),
    n_species = n(),
    n_impacted = sum(FI > 0, na.rm = TRUE),
    .groups = "drop"

  )

# Join with spatial data

basins_plot <- basin_polygons %>%
  left_join(fi_by_basin, by = "basin_id") %>%
  st_transform(4326)


# Get country boundaries
greece <- ne_countries(scale = 50, country = "Greece", returnclass = "sf")
neighbors <- ne_countries(scale = 50,
                          country = c("Turkey", "Bulgaria", "North Macedonia",
                                      "Albania", "Italy"),
                          returnclass = "sf")

# Get bounding box of your basins for map limits
bbox <- st_bbox(basins_plot)

p_map <- ggplot() +
  geom_sf(data = neighbors, fill = "grey95", color = "grey70", linewidth = 0.2) +
  geom_sf(data = greece, fill = "grey85", color = "grey50", linewidth = 0.3) +
  geom_sf(data = basins_plot, aes(fill = mean_FI), color = "grey40", linewidth = 0.2) +
  scale_fill_gradient(low = "#FFF3E0", high = "#BF360C", name = "Mean FI (%)",
                      na.value = "grey90") +
  coord_sf(xlim = c(bbox["xmin"] - 0.5, bbox["xmax"] + 0.5),
           ylim = c(bbox["ymin"] - 0.5, bbox["ymax"] + 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 8))
p_map

ggsave("connectivity/pci/fi_map_by_basin_static.png",
       p_map, width = 10, height = 8, dpi = 300)

#######################



# ============================================================
# BAR CHART: Species affected vs unaffected per basin
# ============================================================
# Aggregate by basin NAME (merge multiple basin_ids)
fi_by_basin <- fi_summary %>%
  group_by(basin_name) %>%
  summarize(
    n_impacted = sum(FI > 0, na.rm = TRUE),
    n_unaffected = sum(FI == 0, na.rm = TRUE),
    n_total = n(),
    pct_impacted = round(100 * sum(FI > 0, na.rm = TRUE) / n(), 0),
    .groups = "drop"
  ) %>%
  filter(n_impacted > 0)

fi_by_basin_long <- fi_by_basin %>%
  pivot_longer(cols = c(n_impacted, n_unaffected),
               names_to = "status",
               values_to = "n_species") %>%
  mutate(
    status = ifelse(status == "n_impacted", "Affected", "Unaffected"),
    basin_name = factor(basin_name,
                        levels = fi_by_basin %>% arrange(n_impacted) %>% pull(basin_name))
  )

p_affected <- ggplot(fi_by_basin_long, aes(x = basin_name, y = n_species, fill = status)) +
  geom_col(alpha = 0.8) +
  geom_text(data = fi_by_basin %>%
              mutate(basin_name = factor(basin_name,
                                         levels = fi_by_basin %>% arrange(n_impacted) %>% pull(basin_name))),
            aes(x = basin_name, y = n_total, fill = NULL,
                label = paste0(pct_impacted, "%")),
            hjust = -0.2, size = 3) +
  scale_fill_manual(values = c("Affected" = "#E07A5A", "Unaffected" = "#E8E0D5"),
                    name = NULL) +
  coord_flip() +
  labs(x = NULL,
       y = "Number of species",
       title = "Species affected by planned dams per river basin") +
  theme_bw() +
  theme(legend.position = "top")

p_affected

ggsave("connectivity/pci/species_affected_per_basin.png",
       p_affected, width = 8, height = 6, dpi = 300)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03_pci_calculation.R
# Calculate PCI for current vs future dam scenarios + Fragmentation Index
# ADAPTED FOR SPECIES HABITAT SUBGRAPHS (occurrence-based, not basin-based)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(riverconn)
library(data.table)
library(dplyr)
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/pci_sparse.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/get_subgraph_between_points.R")

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select <- dplyr::select
rename <- dplyr::rename
group_by <- dplyr::group_by

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================
MIN_SUBCATCHMENTS <- 2   # species must occupy >= this many subcatchments
UPSTREAM_BUFFER   <- 3   # reaches to extend upstream from occurrence points

# ============================================================
# READ INPUTS
# ============================================================

river_graph_current <- readRDS("spatial/stream_networks/river_graph_current.RDS")
river_graph_future  <- readRDS("spatial/stream_networks/river_graph_future.RDS")

# Convert length_reach from meters to tens of km
V(river_graph_current)$length_reach <- V(river_graph_current)$length_reach / 10000
V(river_graph_future)$length_reach  <- V(river_graph_future)$length_reach  / 10000

occurrences    <- fread("points_snapped/fish/fish_all_species_snapped.csv")
fish_dis_class <- fread("traits/fish_dis_class.txt") %>%
  filter(!is.na(dispersal_prob))

# ============================================================
# EXTRACT BASIN_ID FROM GRAPH
# (kept for output labelling — still useful to know which basin
#  a species' subgraph falls in)
# ============================================================
node_basins <- data.frame(
  name     = V(river_graph_current)$name,
  basin_id = V(river_graph_current)$basin_id
) %>%
  filter(!is.na(basin_id))

message("Network has ", length(unique(node_basins$basin_id)), " basins")

# ============================================================
# PREPARE SPECIES LIST
# ============================================================
fish_distribution <- occurrences %>%
  select(species, subc_id) %>%
  distinct() %>%
  group_by(species) %>%
  filter(n() >= MIN_SUBCATCHMENTS) %>%
  ungroup()

fish_species <- fish_distribution %>%
  filter(species %in% fish_dis_class$species) %>%
  pull(species) %>%
  unique()

message(length(fish_species), " species will be processed")

# ============================================================
# HELPER: Attach species presence weights to a graph
# ============================================================
attach_presence <- function(g, presence_df) {
  graph_from_data_frame(
    as_data_frame(g, "edges"),
    vertices = as_data_frame(g, "vertices") %>%
      select(-weight) %>%
      left_join(presence_df, by = "name") %>%
      mutate(weight = ifelse(is.na(weight), 0, weight))
  )
}

# ============================================================
# PRE-COMPUTE SPECIES HABITAT SUBGRAPH NODE SETS
# (topology from river_graph_current; same nodes used for future)
# ============================================================
message("Pre-computing species habitat subgraphs...")

species_habitat_nodes <- lapply(fish_species, function(sp) {
  sp_reach_ids <- fish_distribution %>%
    filter(species == sp) %>%
    pull(subc_id)

  sg <- get_subgraph_between_points(
    graph             = river_graph_current,
    species_reach_ids = sp_reach_ids,
    upstream_buffer   = UPSTREAM_BUFFER
  )

  if (is.null(sg)) return(NULL)
  igraph::V(sg)$name
})
names(species_habitat_nodes) <- fish_species

n_null <- sum(sapply(species_habitat_nodes, is.null))
message("  Subgraphs built: ", length(fish_species) - n_null,
        " | Failed (NULL): ", n_null)

# ============================================================
# PROCESS ALL SPECIES
# ============================================================

catchment_pci_list <- list()
start_time <- Sys.time()

for (name_loop in fish_species) {

  habitat_nodes <- species_habitat_nodes[[name_loop]]
  if (is.null(habitat_nodes)) next

  # Species presence (node weights)
  subcatchments_presence <- fish_distribution %>%
    filter(species == name_loop) %>%
    mutate(name = as.character(subc_id)) %>%
    select(name) %>%
    distinct() %>%
    mutate(weight = 1)

  # Label output with basin_id(s) — informational only
  # A species may span multiple basins; collapse to string
  species_basins <- node_basins %>%
    filter(name %in% subcatchments_presence$name) %>%
    pull(basin_id) %>%
    unique()

  if (length(species_basins) == 0) next

  # Extract habitat subgraphs for both scenarios from same node set
  habitat_graph_current <- induced_subgraph(
    river_graph_current,
    V(river_graph_current)[V(river_graph_current)$name %in% habitat_nodes]
  )
  habitat_graph_future <- induced_subgraph(
    river_graph_future,
    V(river_graph_future)[V(river_graph_future)$name %in% habitat_nodes]
  )

  # Attach presence weights
  graph_current <- attach_presence(habitat_graph_current, subcatchments_presence)
  graph_future  <- attach_presence(habitat_graph_future,  subcatchments_presence)

  # Calculate PCI — CURRENT scenario
  pci_current <- pci_sparse(
    graph_current,
    weight     = "weight",
    field_B    = "length_reach",
    param      = fish_dis_class %>% filter(species_name == name_loop) %>% pull(dispersal_prob),
    index_type = "full"
  ) %>%
    mutate(species  = name_loop,
           basin_id = paste(species_basins, collapse = ";"),
           scenario = "current")

  # Calculate PCI — FUTURE scenario
  pci_future <- pci_sparse(
    graph_future,
    weight     = "weight",
    field_B    = "length_reach",
    param      = fish_dis_class %>% filter(species_name == name_loop) %>% pull(dispersal_prob),
    index_type = "full"
  ) %>%
    mutate(species  = name_loop,
           basin_id = paste(species_basins, collapse = ";"),
           scenario = "future")

  # Fragmentation Index
  fi_val <- (pci_current$index - pci_future$index) / pci_current$index * 100

  catchment_pci_list[[name_loop]] <- bind_rows(pci_current, pci_future) %>%
    mutate(FI = ifelse(scenario == "future", fi_val, NA))

  # Cleanup
  rm(habitat_graph_current, habitat_graph_future, graph_current, graph_future,
     pci_current, pci_future)

  # Progress
  sp_idx <- which(fish_species == name_loop)
  if (sp_idx %% 10 == 0) {
    message("Processed ", sp_idx, " / ", length(fish_species), " species")
    gc(verbose = FALSE)
  }

} # End species loop

end_time <- Sys.time()
elapsed_time <- end_time - start_time
message("Processing complete!")
message("Time elapsed: ", round(elapsed_time, 2), " ", units(elapsed_time))

# ============================================================
# SAVE RESULTS
# ============================================================
catchment_pci_full <- do.call(rbind, catchment_pci_list)

dir.create("connectivity/pci", recursive = TRUE, showWarnings = FALSE)
saveRDS(catchment_pci_full, "connectivity/pci/catchment_pci_full_subhabitats.RDS")

# ============================================================
# SUMMARY TABLE
# ============================================================
fi_summary <- catchment_pci_full %>%
  pivot_wider(
    id_cols     = c(species, basin_id),
    names_from  = scenario,
    values_from = c(index, FI)
  ) %>%
  rename(
    PCI_current = index_current,
    PCI_future  = index_future,
    FI          = FI_future
  ) %>%
  select(species, basin_id, PCI_current, PCI_future, FI) %>%
  arrange(desc(FI))

write.table(fi_summary, "connectivity/pci/fi_summary_subhabitats.txt",
            row.names = FALSE, quote = FALSE, sep = "\t")

message("\nDone! ", nrow(fi_summary), " species processed")

# ============================================================
# QUICK DIAGNOSTICS
# ============================================================
cat("\n========== RESULTS SUMMARY ==========\n")

cat("\nPCI by scenario:\n")
catchment_pci_full %>%
  group_by(scenario) %>%
  summarize(
    n          = n(),
    min_pci    = round(min(index, na.rm = TRUE), 3),
    median_pci = round(median(index, na.rm = TRUE), 3),
    max_pci    = round(max(index, na.rm = TRUE), 3),
    .groups    = "drop"
  ) %>% print()

cat("\nFragmentation Index (connectivity loss from planned dams):\n")
fi_summary %>%
  summarize(
    n_total      = n(),
    n_FI_zero    = sum(FI == 0,  na.rm = TRUE),
    n_FI_above0  = sum(FI > 0,   na.rm = TRUE),
    n_FI_above10 = sum(FI > 10,  na.rm = TRUE),
    n_FI_above25 = sum(FI > 25,  na.rm = TRUE),
    median_FI    = round(median(FI, na.rm = TRUE), 1),
    max_FI       = round(max(FI,    na.rm = TRUE), 1)
  ) %>% print()

cat("\nTop 10 most impacted species:\n")
fi_summary %>% head(10) %>% print()

# ============================================================
# CHECK VJOSA
# ============================================================
cat("\n========== VJOSA BASIN (1292502) ==========\n")
fi_summary %>% filter(grepl("1292502", basin_id)) %>% print()
# ============================================================================
# CREATE VISUALIZATION MAPS
# ============================================================================
# Purpose: Generate interactive maps of snapped points and networks
# Input: Results from previous steps
# Output: Interactive HTML maps
# ============================================================================

library(leaflet)
library(sf)
library(data.table)
library(htmlwidgets)

# Load data
all_snapped <- fread("../results/all_snapped_with_basins.csv")
all_streams <- st_read("../results/all_stream_networks.gpkg")

# Convert to sf
points_sf <- st_as_sf(
  all_snapped,
  coords = c("longitude_snapped", "latitude_snapped"),
  crs = 4326
)

# ============================================================================
# MAP 1: All Snapped Points by Source
# ============================================================================

message("Creating map 1: Snapped points...")

map1 <- leaflet(points_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    color = ~ifelse(source == "HCMR", "red", "blue"),
    radius = 3,
    fillOpacity = 0.7,
    popup = ~paste0(
      "Source: ", source, "<br>",
      "Site: ", site_id, "<br>",
      "Basin: ", basin_id, "<br>",
      "Snap distance: ", round(distance_metres, 1), "m"
    )
  ) %>%
  addLegend(
    colors = c("red", "blue"),
    labels = c("HCMR", "GBIF"),
    title = "Data Source"
  )

saveWidget(map1, "../results/maps/map_all_points.html", selfcontained = TRUE)

# ============================================================================
# MAP 2: Stream Networks with Points
# ============================================================================

message("Creating map 2: Streams and points...")

map2 <- leaflet() %>%
  addTiles() %>%
  addPolylines(data = all_streams, color = "blue", weight = 1, opacity = 0.6) %>%
  addCircleMarkers(
    data = points_sf, 
    color = ~ifelse(source == "HCMR", "red", "orange"),
    radius = 4,
    fillOpacity = 0.8
  )

saveWidget(map2, "../results/maps/map_streams_and_points.html", selfcontained = TRUE)

message("\n=== Maps created in results/maps/ ===")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02 -- fragmentation_ALL_species.R
# Calculate network fragmentation metrics for ALL species in ALL basins
# WITH SCENARIO ANALYSIS (existing vs existing+planned dams)
# Parallelized for efficiency
#
# SCENARIOS:
#   1. Current state (existing dams only)
#   2. Future scenario (existing + planned dams)
#
# INPUT:
#   - river_graph.RDS
#   - dams_snapped_points.csv
#   - fish_all_species_snapped.csv
#
# OUTPUT:
#   - fragmentation_all_scenarios.csv (all combinations, both scenarios)
#   - fragmentation_by_species.csv (species summaries)
#   - fragmentation_by_basin.csv (basin summaries)
#   - scenario_impact_summary.csv (additional impact of planned dams)
#
# LOCATION: workflows/04_connectivity/02b_fragmentation_all_species.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(sf)
library(data.table)
library(parallel)
library(foreach)
library(doParallel)

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)

# Create output directories
dir.create("connectivity", showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

MIN_OCCURRENCES <- 2   # Minimum species occurrences in a basin to analyze
N_CORES <- detectCores() - 1  # Parallel processing cores

message("=== NETWORK FRAGMENTATION ANALYSIS - ALL SPECIES & BASINS WITH SCENARIOS ===")
message("Date: ", Sys.Date())
message("Cores: ", N_CORES)

# ============================================================
# STEP 1: Load river network
# ============================================================

message("\n[1/7] Loading river network graph...")
river_graph <- readRDS("spatial/stream_networks/river_graph.RDS")

message("  Network loaded: ",
        vcount(river_graph), " nodes, ",
        ecount(river_graph), " edges")

# Extract node data
node_data <- data.frame(
  name = V(river_graph)$name,
  basin_id = V(river_graph)$basin_id,
  length_reach = V(river_graph)$length_reach
) %>%
  filter(!is.na(basin_id))

# ============================================================
# STEP 2: Load fish occurrences and dams
# ============================================================

message("\n[2/7] Loading fish and dam data...")

fish_all <- fread("points_snapped/fish/fish_all_species_snapped.csv")
dams_all <- fread("points_snapped/dams/dams_snapped_points.csv")

# Filter dams by status
dams_existing <- dams_all %>%
  filter(status == "existing")

dams_planned <- dams_all %>%
  filter(status == "planned") %>%
  filter(phase != "R")  # Exclude rejected dams

dams_all_future <- dams_all %>%
  filter(phase != "R") %>%
  filter(status %in% c("existing", "planned"))

# Add basin_id to dams
dams_existing <- dams_existing %>%
  mutate(subc_id = as.character(subc_id)) %>%
  left_join(
    node_data %>% select(name, basin_id),
    by = c("subc_id" = "name")
  ) %>%
  filter(!is.na(basin_id))

dams_planned <- dams_planned %>%
  mutate(subc_id = as.character(subc_id)) %>%
  left_join(
    node_data %>% select(name, basin_id),
    by = c("subc_id" = "name")
  ) %>%
  filter(!is.na(basin_id))

dams_all_future <- dams_all_future %>%
  mutate(subc_id = as.character(subc_id)) %>%
  left_join(
    node_data %>% select(name, basin_id),
    by = c("subc_id" = "name")
  ) %>%
  filter(!is.na(basin_id))

message("  Fish occurrences: ", nrow(fish_all))
message("  Species: ", length(unique(fish_all$species)))
message("  Existing dams: ", nrow(dams_existing))
message("  Planned dams: ", nrow(dams_planned))
message("  Total future dams: ", nrow(dams_all_future))

# ============================================================
# STEP 3: Identify species × basin combinations to analyze
# ============================================================

message("\n[3/7] Identifying species × basin combinations...")

# Count occurrences per species per basin
species_basin_counts <- fish_all %>%
  mutate(subc_id = as.character(subc_id)) %>%
  left_join(
    node_data %>% select(name, basin_id),
    by = c("subc_id" = "name")
  ) %>%
  filter(!is.na(basin_id), basin_id != "") %>%
  group_by(species, basin_id) %>%
  summarise(
    n_occurrences = n(),
    .groups = "drop"
  ) %>%
  filter(n_occurrences >= MIN_OCCURRENCES)

# Keep only combinations where basin has dams (existing OR planned)
basins_with_dams <- unique(c(
  dams_existing$basin_id,
  dams_planned$basin_id
)) %>% as.character()

species_basin_counts <- species_basin_counts %>%
  filter(basin_id %in% basins_with_dams)

message("  Basins with dams: ", length(basins_with_dams))
message("  Total combinations to analyze: ", nrow(species_basin_counts))
message("  Species: ", length(unique(species_basin_counts$species)))
message("  Basins: ", length(unique(species_basin_counts$basin_id)))

# ============================================================
# STEP 4: Define analysis function for one species × basin × scenario
# ============================================================

analyze_species_basin_scenario <- function(species_name, basin_id_val,
                                           river_graph, node_data,
                                           fish_all, dams_existing, dams_all_future) {

  tryCatch({

    # Extract basin subgraph
    basin_nodes <- node_data %>%
      filter(basin_id == basin_id_val) %>%
      pull(name)

    if (length(basin_nodes) == 0) {
      return(NULL)
    }

    basin_graph <- induced_subgraph(river_graph, basin_nodes)

    # Get species occurrences
    species_occurrences <- fish_all %>%
      filter(species == species_name,
             as.character(subc_id) %in% basin_nodes) %>%
      distinct(subc_id, .keep_all = TRUE)

    n_occurrences <- nrow(species_occurrences)

    if (n_occurrences < 2) {
      return(NULL)
    }

    species_nodes <- as.character(species_occurrences$subc_id)
    species_node_indices <- which(V(basin_graph)$name %in% species_nodes)

    # Get dams in this basin
    dams_basin_existing <- dams_existing %>%
      filter(as.character(basin_id) == basin_id_val)

    dams_basin_future <- dams_all_future %>%
      filter(as.character(basin_id) == basin_id_val)

    # Calculate original (unfragmented) distance matrix
    distance_matrix_original <- distances(
      basin_graph,
      v = species_node_indices,
      to = species_node_indices,
      weights = E(basin_graph)$length_reach,
      mode = "all"
    )

    # ------------------------------------------------------------
    # SCENARIO 1: Existing dams
    # ------------------------------------------------------------

    if (nrow(dams_basin_existing) > 0) {
      # Fragment by existing dams
      dam_nodes_existing <- unique(as.character(dams_basin_existing$subc_id))
      dam_nodes_in_graph_existing <- dam_nodes_existing[dam_nodes_existing %in% V(basin_graph)$name]

      edge_df <- as_data_frame(basin_graph, "edges")
      dam_edge_ids_existing <- which(edge_df$from %in% dam_nodes_in_graph_existing |
                                       edge_df$to %in% dam_nodes_in_graph_existing)

      if (length(dam_edge_ids_existing) > 0) {
        basin_fragmented_existing <- delete_edges(basin_graph, dam_edge_ids_existing)
      } else {
        basin_fragmented_existing <- basin_graph
      }
    } else {
      basin_fragmented_existing <- basin_graph
      dam_edge_ids_existing <- integer(0)
    }

    # Calculate distances in existing scenario
    distance_matrix_existing <- distances(
      basin_fragmented_existing,
      v = species_node_indices,
      to = species_node_indices,
      weights = E(basin_fragmented_existing)$length_reach,
      mode = "all"
    )

    # Analyze pairs - existing scenario
    results_existing <- analyze_pairs(
      distance_matrix_original,
      distance_matrix_existing,
      species_nodes
    )

    # ------------------------------------------------------------
    # SCENARIO 2: Future (existing + planned dams)
    # ------------------------------------------------------------

    if (nrow(dams_basin_future) > 0) {
      # Fragment by all future dams
      dam_nodes_future <- unique(as.character(dams_basin_future$subc_id))
      dam_nodes_in_graph_future <- dam_nodes_future[dam_nodes_future %in% V(basin_graph)$name]

      edge_df <- as_data_frame(basin_graph, "edges")
      dam_edge_ids_future <- which(edge_df$from %in% dam_nodes_in_graph_future |
                                     edge_df$to %in% dam_nodes_in_graph_future)

      if (length(dam_edge_ids_future) > 0) {
        basin_fragmented_future <- delete_edges(basin_graph, dam_edge_ids_future)
      } else {
        basin_fragmented_future <- basin_graph
      }
    } else {
      basin_fragmented_future <- basin_graph
      dam_edge_ids_future <- integer(0)
    }

    # Calculate distances in future scenario
    distance_matrix_future <- distances(
      basin_fragmented_future,
      v = species_node_indices,
      to = species_node_indices,
      weights = E(basin_fragmented_future)$length_reach,
      mode = "all"
    )

    # Analyze pairs - future scenario
    results_future <- analyze_pairs(
      distance_matrix_original,
      distance_matrix_future,
      species_nodes
    )

    # ------------------------------------------------------------
    # Combine results
    # ------------------------------------------------------------

    data.frame(
      species = species_name,
      basin_id = basin_id_val,
      n_occurrences = n_occurrences,

      # Existing scenario
      n_dams_existing = nrow(dams_basin_existing),
      n_dam_edges_removed_existing = length(dam_edge_ids_existing),
      total_pairs = results_existing$n_total,
      disconnected_pairs_existing = results_existing$n_disconnected,
      percent_lost_existing = round(100 * results_existing$n_disconnected / results_existing$n_total, 2),

      # Future scenario
      n_dams_future = nrow(dams_basin_future),
      n_dam_edges_removed_future = length(dam_edge_ids_future),
      disconnected_pairs_future = results_future$n_disconnected,
      percent_lost_future = round(100 * results_future$n_disconnected / results_future$n_total, 2),

      # Additional impact of planned dams
      n_dams_planned = nrow(dams_basin_future) - nrow(dams_basin_existing),
      additional_pairs_disconnected = results_future$n_disconnected - results_existing$n_disconnected,
      additional_percent_lost = round(
        (100 * results_future$n_disconnected / results_future$n_total) -
          (100 * results_existing$n_disconnected / results_existing$n_total), 2
      ),

      # Distance statistics (existing scenario)
      mean_dist_connected_existing = results_existing$mean_dist_connected,
      mean_dist_disconnected_existing = results_existing$mean_dist_disconnected,
      t_test_p_existing = results_existing$t_test_p,

      # Distance statistics (future scenario)
      mean_dist_connected_future = results_future$mean_dist_connected,
      mean_dist_disconnected_future = results_future$mean_dist_disconnected,
      t_test_p_future = results_future$t_test_p,

      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    # Return error info
    data.frame(
      species = species_name,
      basin_id = basin_id_val,
      error = TRUE,
      error_message = as.character(e),
      stringsAsFactors = FALSE
    )
  })
}

# Helper function to analyze pairs
analyze_pairs <- function(dist_original, dist_fragmented, species_nodes) {

  n_total <- 0
  n_disconnected <- 0

  dist_connected <- c()
  dist_disconnected_original <- c()

  for (i in 1:(length(species_nodes) - 1)) {
    for (j = (i + 1):length(species_nodes)) {

      n_total <- n_total + 1

      dist_orig <- dist_original[i, j]
      dist_frag <- dist_fragmented[i, j]

      if (is.finite(dist_orig) && !is.finite(dist_frag)) {
        # Newly disconnected
        n_disconnected <- n_disconnected + 1
        dist_disconnected_original <- c(dist_disconnected_original, dist_orig)
      } else if (is.finite(dist_frag)) {
        # Still connected
        dist_connected <- c(dist_connected, dist_frag)
      }
    }
  }

  # Statistical test - ONLY if BOTH groups have data
  t_test_p <- NA
  if (length(dist_connected) > 1 && length(dist_disconnected_original) > 1) {
    t_result <- tryCatch(
      t.test(dist_connected, dist_disconnected_original),
      error = function(e) NULL
    )
    if (!is.null(t_result)) {
      t_test_p <- round(t_result$p.value, 4)
    }
  }

  list(
    n_total = n_total,
    n_disconnected = n_disconnected,
    n_connected = n_total - n_disconnected,  # ADD THIS
    mean_dist_connected = if (length(dist_connected) > 0) round(mean(dist_connected), 1) else NA,
    mean_dist_disconnected = if (length(dist_disconnected_original) > 0) {
      round(mean(dist_disconnected_original), 1)
    } else {
      NA
    },
    t_test_p = t_test_p,
    test_feasible = length(dist_connected) > 1 && length(dist_disconnected_original) > 1  # ADD THIS
  )
}

# ============================================================
# STEP 5: Run parallel analysis
# ============================================================

message("\n[4/7] Running parallel scenario analysis...")
message("  Processing ", nrow(species_basin_counts), " combinations...")

# Set up parallel cluster
cl <- makeCluster(N_CORES, type = "FORK")
registerDoParallel(cl)

start_time <- Sys.time()

# Run analysis in parallel
results <- foreach(
  i = 1:nrow(species_basin_counts),
  .packages = c("igraph", "dplyr"),
  .combine = rbind,
  .errorhandling = "pass"
) %dopar% {

  if (i %% 50 == 0) {
    cat(sprintf("Processed %d / %d combinations\n", i, nrow(species_basin_counts)))
  }

  row <- species_basin_counts[i, ]

  analyze_species_basin_scenario(
    species_name = row$species,
    basin_id_val = row$basin_id,
    river_graph = river_graph,
    node_data = node_data,
    fish_all = fish_all,
    dams_existing = dams_existing,
    dams_all_future = dams_all_future
  )
}

stopCluster(cl)

end_time <- Sys.time()
elapsed <- end_time - start_time

message("  Analysis complete!")
message("  Time elapsed: ", round(elapsed, 2), " ", units(elapsed))

# ============================================================
# STEP 6: Process and save results
# ============================================================

message("\n[5/7] Processing results...")

# Filter out errors
if ("error" %in% colnames(results)) {
  errors <- results %>% filter(error == TRUE)
  if (nrow(errors) > 0) {
    message("  WARNING: ", nrow(errors), " combinations failed")
    fwrite(errors, "connectivity/fragmentation_errors.csv")
  }
  results <- results %>% filter(is.na(error) | error == FALSE)
}

results <- results %>% select(-matches("^error"))

message("  Successfully analyzed: ", nrow(results), " combinations")

# Save full results
fwrite(results, "connectivity/fragmentation_all_scenarios.csv")
message("  Saved: connectivity/fragmentation_all_scenarios.csv")

# ============================================================
# STEP 7: Create summaries
# ============================================================

message("\n[6/7] Creating summary statistics...")

# Overall summary
overall_summary <- results %>%
  summarise(
    n_combinations = n(),
    n_species = n_distinct(species),
    n_basins = n_distinct(basin_id),

    # Existing scenario
    mean_loss_existing = mean(percent_lost_existing, na.rm = TRUE),
    median_loss_existing = median(percent_lost_existing, na.rm = TRUE),

    # Future scenario
    mean_loss_future = mean(percent_lost_future, na.rm = TRUE),
    median_loss_future = median(percent_lost_future, na.rm = TRUE),

    # Impact of planned dams
    mean_additional_loss = mean(additional_percent_lost, na.rm = TRUE),
    n_with_planned_impact = sum(additional_pairs_disconnected > 0, na.rm = TRUE),

    total_dams_existing = sum(n_dams_existing, na.rm = TRUE),
    total_dams_planned = sum(n_dams_planned, na.rm = TRUE)
  )

print(overall_summary)

fwrite(overall_summary, "connectivity/scenario_impact_summary.csv")

# Species summary
species_summary <- results %>%
  group_by(species) %>%
  summarise(
    n_basins = n(),
    mean_loss_existing = mean(percent_lost_existing, na.rm = TRUE),
    mean_loss_future = mean(percent_lost_future, na.rm = TRUE),
    mean_additional_loss = mean(additional_percent_lost, na.rm = TRUE),
    max_loss_existing = max(percent_lost_existing, na.rm = TRUE),
    max_loss_future = max(percent_lost_future, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_loss_future))

fwrite(species_summary, "connectivity/fragmentation_by_species.csv")

# Basin summary
basin_summary <- results %>%
  group_by(basin_id) %>%
  summarise(
    n_species = n(),
    mean_loss_existing = mean(percent_lost_existing, na.rm = TRUE),
    mean_loss_future = mean(percent_lost_future, na.rm = TRUE),
    mean_additional_loss = mean(additional_percent_lost, na.rm = TRUE),
    total_dams_existing = first(n_dams_existing),
    total_dams_planned = first(n_dams_planned),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_loss_future))

fwrite(basin_summary, "connectivity/fragmentation_by_basin.csv")

# ============================================================
# STEP 8: Create visualizations
# ============================================================

message("\n[7/7] Creating visualizations...")

png("connectivity/scenario_comparison_greece.png",
    width = 14, height = 6, units = "in", res = 300)

par(mfrow = c(1, 3), mar = c(5, 5, 4, 2))

# Panel A: Distribution comparison
hist(results$percent_lost_existing,
     breaks = 30, col = rgb(0.99, 0.71, 0.38, 0.5),
     border = "white",
     main = "Connectivity Loss Distribution",
     xlab = "Connectivity loss (%)",
     ylab = "Number of combinations",
     xlim = c(0, 100),
     cex.lab = 1.3, cex.main = 1.5)

hist(results$percent_lost_future,
     breaks = 30, col = rgb(0.99, 0.55, 0.38, 0.5),
     border = "white", add = TRUE)

legend("topright",
       c("Existing dams", "Existing + Planned"),
       fill = c(rgb(0.99, 0.71, 0.38, 0.5), rgb(0.99, 0.55, 0.38, 0.5)),
       border = "white", bty = "n")

# Panel B: Additional impact
barplot(c(
  overall_summary$mean_loss_existing,
  overall_summary$mean_loss_future
),
names.arg = c("Current", "Future"),
col = c("#FDB462", "#FC8D62"),
border = "white",
main = "Mean Connectivity Loss",
ylab = "Mean loss (%)",
ylim = c(0, max(c(overall_summary$mean_loss_existing, overall_summary$mean_loss_future)) * 1.2),
cex.lab = 1.3, cex.main = 1.5)

text(0.7, overall_summary$mean_loss_existing + 2,
     paste0(round(overall_summary$mean_loss_existing, 1), "%"),
     cex = 1.2, font = 2)

text(1.9, overall_summary$mean_loss_future + 2,
     paste0(round(overall_summary$mean_loss_future, 1), "%"),
     cex = 1.2, font = 2)

# Panel C: Most affected basins
top_basins <- basin_summary %>%
  head(15) %>%
  arrange(mean_loss_future)

barplot(top_basins$mean_loss_future,
        names.arg = top_basins$basin_id,
        horiz = TRUE,
        col = "#FC8D62",
        border = "white",
        main = "Top 15 Most Fragmented Basins",
        xlab = "Mean connectivity loss (%)",
        cex.lab = 1.3, cex.main = 1.5,
        cex.names = 0.8,
        las = 1)

dev.off()

message("  Saved: connectivity/scenario_comparison_greece.png")

# ============================================================
# FINAL SUMMARY
# ============================================================

message("\n=== SCENARIO ANALYSIS COMPLETE ===")
message("Output files:")
message("  • connectivity/fragmentation_all_scenarios.csv")
message("  • connectivity/fragmentation_by_species.csv")
message("  • connectivity/fragmentation_by_basin.csv")
message("  • connectivity/scenario_impact_summary.csv")
message("  • connectivity/scenario_comparison_greece.png")
message("\nKey findings:")
message("  • ", nrow(results), " species × basin combinations analyzed")
message("  • ", overall_summary$n_species, " species across ", overall_summary$n_basins, " basins")
message("  • Current (existing dams): ", round(overall_summary$mean_loss_existing, 1), "% mean loss")
message("  • Future (+ planned): ", round(overall_summary$mean_loss_future, 1), "% mean loss")
message("  • Additional impact: +", round(overall_summary$mean_additional_loss, 1), "% from planned dams")
message("  • ", overall_summary$n_with_planned_impact, " combinations affected by planned dams")
message("\nAnalysis completed: ", Sys.time())
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02 -- fragmentation_scenarios.R
# Calculate network fragmentation metrics for 2 dam scenarios
# Uses igraph (consistent with PCI workflow)
#
# SCENARIOS:
#   1. Current state (existing dams only)
#   2. Future scenario (existing + planned dams)
#
# INPUT:
#   - river_graph.RDS
#   - dams_snapped_points.csv
#   - fish_all_species_snapped.csv
#
# OUTPUT:
#   - Scenario comparison figures (2-panel plots)
#   - Summary statistics for both scenarios
#
# LOCATION: workflows/04_connectivity/02_fragmentation_scenarios.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(sf)
library(data.table)
library(leaflet)
library(htmlwidgets)
library(ggplot2)
library(ggspatial)

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)

# Create output directory
dir.create("connectivity", showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

TARGET_BASIN_ID <- 1292502
EXAMPLE_SPECIES <- "Barbus_prespensis"

message("=== NETWORK FRAGMENTATION SCENARIOS ANALYSIS ===")
message("Basin: ", TARGET_BASIN_ID)
message("Species: ", EXAMPLE_SPECIES)
message("Date: ", Sys.Date())

# ============================================================
# STEP 1: Load river network
# ============================================================

message("\n[1/9] Loading river network graph...")
river_graph <- readRDS("spatial/stream_networks/river_graph.RDS")

message("  Network loaded: ",
        vcount(river_graph), " nodes, ",
        ecount(river_graph), " edges")

# ============================================================
# STEP 2: Extract target basin
# ============================================================

message("\n[2/9] Extracting target basin...")

# Extract basin info from nodes
node_data <- data.frame(
  name = V(river_graph)$name,
  basin_id = V(river_graph)$basin_id,
  length_reach = V(river_graph)$length_reach
) %>%
  filter(!is.na(basin_id))

# Get nodes in target basin
basin_nodes <- node_data %>%
  filter(basin_id == TARGET_BASIN_ID) %>%
  pull(name)

# Extract subgraph
basin_graph <- induced_subgraph(river_graph, basin_nodes)

message("  Basin extracted:")
message("    Reaches: ", vcount(basin_graph))

# ============================================================
# STEP 3: Load fish occurrences
# ============================================================

message("\n[3/9] Loading fish occurrence data...")

fish_all <- fread("points_snapped/fish/fish_all_species_snapped.csv")

# Filter to target basin
fish_basin <- fish_all %>%
  filter(as.character(subc_id) %in% basin_nodes)

# Extract example species
species_occurrences <- fish_basin %>%
  filter(species == EXAMPLE_SPECIES |
           species == gsub("_", " ", EXAMPLE_SPECIES)) %>%
  distinct(subc_id, .keep_all = TRUE)

n_occurrences <- nrow(species_occurrences)
message("  Species occurrences: ", n_occurrences)

# Get species node indices
species_nodes <- as.character(species_occurrences$subc_id)
species_nodes <- species_nodes[species_nodes %in% V(basin_graph)$name]
species_node_indices <- which(V(basin_graph)$name %in% species_nodes)

max_connections <- (length(species_nodes) * (length(species_nodes) - 1)) / 2
message("  Maximum possible connections: ", max_connections)

# ============================================================
# STEP 4: Load and categorize dams
# ============================================================

message("\n[4/9] Loading dam data...")

dams_all <- fread("points_snapped/dams/dams_snapped_points.csv")

# Filter to target basin
dams_basin <- dams_all %>%
  filter(as.character(subc_id) %in% basin_nodes)

message("  Total dams in basin: ", nrow(dams_basin))

# Separate by status
dams_existing <- dams_basin %>%
  filter(status == "existing")

dams_planned <- dams_basin %>%
  filter(status == "planned") %>%
  filter(phase != "R")

dams_all_future <- dams_basin %>%
  filter(phase != "R") %>%
  filter(status %in% c("existing", "planned"))

message("  Existing dams (current): ", nrow(dams_existing))
message("  Planned dams: ", nrow(dams_planned))
message("  Total future dams: ", nrow(dams_all_future))

# ============================================================
# STEP 5: Define fragmentation function
# ============================================================

calculate_fragmentation <- function(basin_graph, species_node_indices,
                                    dam_subcatchments = NULL,
                                    scenario_name = "Baseline") {

  # Fragment network if dams provided
  if (!is.null(dam_subcatchments) && length(dam_subcatchments) > 0) {

    dam_nodes_in_graph <- dam_subcatchments[dam_subcatchments %in% V(basin_graph)$name]

    if (length(dam_nodes_in_graph) > 0) {
      edge_df <- as_data_frame(basin_graph, "edges")
      dam_edge_ids <- which(edge_df$from %in% dam_nodes_in_graph |
                              edge_df$to %in% dam_nodes_in_graph)

      if (length(dam_edge_ids) > 0) {
        fragmented_graph <- delete_edges(basin_graph, dam_edge_ids)
        n_edges_removed <- length(dam_edge_ids)
      } else {
        fragmented_graph <- basin_graph
        n_edges_removed <- 0
      }
    } else {
      fragmented_graph <- basin_graph
      n_edges_removed <- 0
    }
  } else {
    fragmented_graph <- basin_graph
    n_edges_removed <- 0
  }

  # Calculate distances
  distance_matrix <- distances(
    fragmented_graph,
    v = species_node_indices,
    to = species_node_indices,
    weights = E(fragmented_graph)$length_reach,
    mode = "all"
  )

  # Analyze pairwise connections
  n_total <- 0
  n_connected <- 0
  n_disconnected <- 0

  dist_connected_vals <- c()
  dist_disconnected_original <- c()

  # Also need original (unfragmented) distances for comparison
  distance_matrix_original <- distances(
    basin_graph,
    v = species_node_indices,
    to = species_node_indices,
    weights = E(basin_graph)$length_reach,
    mode = "all"
  )

  for (i in 1:(nrow(distance_matrix) - 1)) {
    for (j in (i + 1):ncol(distance_matrix)) {

      n_total <- n_total + 1
      dist_current <- distance_matrix[i, j]
      dist_original <- distance_matrix_original[i, j]

      if (is.finite(dist_current)) {
        n_connected <- n_connected + 1
        dist_connected_vals <- c(dist_connected_vals, dist_current)
      } else {
        n_disconnected <- n_disconnected + 1
        dist_disconnected_original <- c(dist_disconnected_original, dist_original)
      }
    }
  }

  # Component analysis
  comp <- components(fragmented_graph)

  # Statistical test
  t_test_p <- NA
  if (length(dist_connected_vals) > 1 && length(dist_disconnected_original) > 1) {
    t_result <- tryCatch(
      t.test(dist_connected_vals, dist_disconnected_original),
      error = function(e) NULL
    )
    if (!is.null(t_result)) {
      t_test_p <- t_result$p.value
    }
  }

  # Return results
  list(
    scenario = scenario_name,
    n_total = n_total,
    n_connected = n_connected,
    n_disconnected = n_disconnected,
    percent_lost = round(100 * n_disconnected / n_total, 2),
    dist_connected = dist_connected_vals,
    dist_disconnected_original = dist_disconnected_original,
    mean_dist_connected = ifelse(length(dist_connected_vals) > 0,
                                 mean(dist_connected_vals), NA),
    mean_dist_disconnected = ifelse(length(dist_disconnected_original) > 0,
                                    mean(dist_disconnected_original), NA),
    t_test_p = t_test_p,
    n_components = comp$no,
    n_edges_removed = n_edges_removed,
    graph = fragmented_graph
  )
}

# ============================================================
# STEP 6: Run both scenarios
# ============================================================

message("\n[5/9] Calculating fragmentation for both scenarios...")

# Scenario 1: Current state (existing dams)
message("  Scenario 1: Current state (existing dams)")
scenario_current <- calculate_fragmentation(
  basin_graph,
  species_node_indices,
  dam_subcatchments = unique(as.character(dams_existing$subc_id)),
  scenario_name = "Current"
)

# Scenario 2: Future state (existing + planned dams)
message("  Scenario 2: Future state (existing + planned dams)")
scenario_future <- calculate_fragmentation(
  basin_graph,
  species_node_indices,
  dam_subcatchments = unique(as.character(dams_all_future$subc_id)),
  scenario_name = "Future"
)

# ============================================================
# STEP 7: Create summary comparison
# ============================================================

message("\n[6/9] Creating scenario comparison summary...")

summary_df <- data.frame(
  Scenario = c("Current (existing dams)", "Future (existing + planned)"),
  Total_pairs = c(scenario_current$n_total, scenario_future$n_total),
  Connected_pairs = c(scenario_current$n_connected, scenario_future$n_connected),
  Disconnected_pairs = c(scenario_current$n_disconnected, scenario_future$n_disconnected),
  Percent_lost = c(scenario_current$percent_lost, scenario_future$percent_lost),
  N_components = c(scenario_current$n_components, scenario_future$n_components),
  N_dams = c(nrow(dams_existing), nrow(dams_all_future)),
  Mean_dist_connected = c(scenario_current$mean_dist_connected,
                          scenario_future$mean_dist_connected),
  Mean_dist_disconnected = c(scenario_current$mean_dist_disconnected,
                             scenario_future$mean_dist_disconnected)
)

print(summary_df)

write.csv(summary_df,
          "connectivity/scenario_comparison_summary.csv",
          row.names = FALSE)

# Calculate additional impact of planned dams
additional_disconnected <- scenario_future$n_disconnected - scenario_current$n_disconnected
additional_percent <- scenario_future$percent_lost - scenario_current$percent_lost

message("\nAdditional impact of planned dams:")
message("  Additional pairs disconnected: ", additional_disconnected)
message("  Additional connectivity loss: ", round(additional_percent, 1), " percentage points")

# ============================================================
# FINAL SUMMARY
# ============================================================

message("\n=== SCENARIO ANALYSIS COMPLETE ===")
message("Basin: ", TARGET_BASIN_ID)
message("Species: ", EXAMPLE_SPECIES)
message("\nScenario Comparison:")
message("  CURRENT STATE (existing dams):")
message("     - Dams: ", nrow(dams_existing))
message("     - Disconnected pairs: ", scenario_current$n_disconnected, " / ", scenario_current$n_total)
message("     - Connectivity loss: ", scenario_current$percent_lost, "%")
message("     - Network components: ", scenario_current$n_components)
message("")
message("  FUTURE STATE (existing + planned dams):")
message("     - Dams: ", nrow(dams_all_future), " (+", nrow(dams_planned), " planned)")
message("     - Disconnected pairs: ", scenario_future$n_disconnected, " / ", scenario_future$n_total)
message("     - Connectivity loss: ", scenario_future$percent_lost, "%")
message("     - Network components: ", scenario_future$n_components)
message("")
message("  ADDITIONAL IMPACT OF PLANNED DAMS:")
message("     - Additional pairs disconnected: +", additional_disconnected)
message("     - Additional connectivity loss: +", round(additional_percent, 1), " percentage points")
message("     - Additional components created: +",
        scenario_future$n_components - scenario_current$n_components)
message("\nConservation message:")
message("  Preventing construction of ", nrow(dams_planned), " planned dams would avoid")
message("  disconnecting ", additional_disconnected, " additional population pairs.")
message("\nOutput Files:")
message("  • connectivity/scenario_comparison_summary.csv")
message("  • connectivity/scenario_comparison_bars.png")
message("  • connectivity/Figure_Scenarios_MEE.png")
message("  • connectivity/scenario_comparison_map.html")
message("  • connectivity/Figure_Scenarios_caption.txt")
message("\nAnalysis complete: ", Sys.time())
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_generate_network_graph.R
# Build igraph river network from H90M subcatchments + barriers
# TWO SCENARIOS: current (existing dam only) vs future (existing + planned)
#
# NOTE ON PASSABILITY:
#   This script is species-agnostic. It carries the number of dams per
#   reach (n_shp) on nodes and edges, but does NOT assign a passability
#   value. Structural fragmentation (Module 5) treats any edge with
#   n_shp > 0 as a blocking barrier. Species-specific passability
#   (0.8 / 0.5 / 0) is applied later, at PCI computation time
#   (Module 10 / pci script), as pass = species_passability ^ n_shp.
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(sf)
library(igraph)
library(dplyr)

# ============================================================
# FIX: Prevent MASS::select from masking dplyr::select
# ============================================================
select   <- dplyr::select
rename   <- dplyr::rename
group_by <- dplyr::group_by

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

# ============================================================
# INPUT FILES
# ============================================================
subcatchments <- st_read("spatial/subbasin_sarantaporos/stream_network_pruned.gpkg")

# temporary fix until length is returned by api_getupstreamstreamsegments
basin_stream_length <- read_geopackage("spatial/basin/stream_network_pruned.gpkg",
                                       import_as = "data.table") %>%
  select(subc_id, length)

subcatchments <- subcatchments %>% left_join(basin_stream_length)

# New snapped dam inventory (status column: "existing" / "planned")
barriers <- read.csv("points_snapped/dams/dams_snapped_points.csv") %>%
  filter(subc_id %in% subcatchments$subc_id)   # keep dams on the Sarantaporos network

message("Barriers on network: ", nrow(barriers),
        "  (existing: ", sum(barriers$status == "existing"),
        ", planned: ",   sum(barriers$status == "planned"), ")")

# ============================================================
# BARRIER SCENARIOS  (driven by the 'status' column)
# ============================================================

# Current: only the existing operational dam
barrier_counts_current <- barriers %>%
  filter(status == "existing") %>%
  group_by(subc_id) %>%
  summarise(n_shp = n(), .groups = "drop")

# Future: existing + all planned (rejected/EXCLUDE already removed upstream)
barrier_counts_future <- barriers %>%
  group_by(subc_id) %>%
  summarise(n_shp = n(), .groups = "drop")

message("Current scenario: ", sum(barrier_counts_current$n_shp), " dams in ",
        nrow(barrier_counts_current), " subcatchments")
message("Future scenario:  ", sum(barrier_counts_future$n_shp), " dams in ",
        nrow(barrier_counts_future), " subcatchments")

# ============================================================
# FUNCTION: Build river graph from subcatchments + barrier counts
#   Carries n_shp (dam count) on nodes and edges. No passability.
# ============================================================
build_river_graph <- function(subcatchments_raw, barrier_counts) {

  # Step 1: Join barrier counts with subcatchments
  sc <- subcatchments_raw %>%
    left_join(barrier_counts, by = "subc_id") %>%
    mutate(n_shp = ifelse(is.na(n_shp), 0, n_shp))

  # Step 2: Deduplicate
  sc <- sc %>%
    distinct(subc_id, .keep_all = TRUE)

  # Step 3: Build edges
  edges_df <- sc %>%
    st_drop_geometry() %>%
    select(subc_id, target) %>%
    rename(from = subc_id, to = target) %>%
    mutate(from = as.character(from),
           to   = as.character(to))

  # Step 4: Build vertices (carry reach length, dam count, Strahler order)
  vertices_df <- sc %>%
    st_drop_geometry() %>%
    select(subc_id, length, strahler, n_shp) %>%
    rename(name = subc_id, length_reach = length) %>%
    mutate(name         = as.character(name),
           length_reach = ifelse(is.na(length_reach), 0, length_reach),
           n_shp        = ifelse(is.na(n_shp), 0, n_shp))

  # Step 5: Create igraph (first pass) and attach vertex attributes
  rg <- igraph::graph_from_data_frame(edges_df)

  rg_v_df <- igraph::as_data_frame(rg, "vertices") %>%
    left_join(vertices_df, by = "name") %>%
    mutate(
      length_reach = ifelse(name == "0" | is.na(length_reach), 1, length_reach),
      n_shp        = ifelse(name == "0" | is.na(n_shp),        0, n_shp)
    )

  rg_tmp <- igraph::graph_from_data_frame(edges_df, v = rg_v_df)

  # Step 6: Transfer dam count from nodes -> edges
  #   A dam sits ON a reach (node), but a barrier severs ONE connection.
  #   The network is a directed tree rooted at the outlet, so each node has
  #   exactly one downstream edge (the edge where from == node). We attribute
  #   the dam to that single downstream edge only, so that one dam cuts the
  #   network once and produces two fragments (rather than isolating the
  #   dammed reach by cutting on both sides).
  graph_v_df <- igraph::as_data_frame(rg_tmp, "vertices")

  graph_e_df <- igraph::as_data_frame(rg_tmp, "edges") %>%
    left_join(graph_v_df %>% select(name, n_shp) %>% rename(from = name),
              by = "from") %>%
    rename(n_shp_edge = n_shp) %>%
    mutate(n_shp_edge = ifelse(is.na(n_shp_edge), 0, n_shp_edge)) %>%
    select(from, to, n_shp_edge)

  # Step 7: Final graph
  rg_final <- igraph::graph_from_data_frame(d = graph_e_df, vertices = graph_v_df)

  V(rg_final)$weight   <- 1
  E(rg_final)$n_shp    <- ifelse(is.na(E(rg_final)$n_shp_edge), 0,
                                 E(rg_final)$n_shp_edge)
  # convenience flag for structural fragmentation (Module 5)
  E(rg_final)$barrier  <- E(rg_final)$n_shp > 0

  return(rg_final)
}

# ============================================================
# BUILD BOTH GRAPHS
# ============================================================

message("\nBuilding current scenario graph...")
river_graph_current <- build_river_graph(subcatchments, barrier_counts_current)
message("Current: ", vcount(river_graph_current), " nodes, ",
        ecount(river_graph_current), " edges")

message("\nBuilding future scenario graph...")
river_graph_future <- build_river_graph(subcatchments, barrier_counts_future)
message("Future:  ", vcount(river_graph_future), " nodes, ",
        ecount(river_graph_future), " edges")

# ============================================================
# SAVE
# ============================================================
saveRDS(river_graph_current, "spatial/stream_networks/river_graph_current.RDS")
saveRDS(river_graph_future,  "spatial/stream_networks/river_graph_future.RDS")

message("\nBoth graphs saved!")

# ============================================================
# DIAGNOSTICS
# ============================================================
message("\n--- Current scenario ---")
message("Barriered edges: ", sum(E(river_graph_current)$barrier),
        " out of ", ecount(river_graph_current))

message("\n--- Future scenario ---")
message("Barriered edges: ", sum(E(river_graph_future)$barrier),
        " out of ", ecount(river_graph_future))

# How many additional edges are barriered in the future?
n_affected <- sum(E(river_graph_future)$barrier) -
  sum(E(river_graph_current)$barrier)
message("\nAdditional barriered edges in future scenario: ", n_affected)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03 -- dam_impact_zones.R
# Calculate affected habitat within impact zones around dams
# impact zone = upstream/downstream buffer zones from each dam
#
# CONCEPT:
#   - Each dam affects habitat within a certain radius (up/downstream)
#   - impact zone = river segments within these radii
#   - Quantifies total habitat length affected by dams
#
# INPUT:
#   - river_graph.RDS (from script 04_spatial_network/02_generate_network_graph.R)
#   - partial_stream_network.gpkg (spatial network)
#   - dams_snapped_points.csv
#   - fish_all_species_snapped.csv
#
# OUTPUT:
#   - impact_zone_by_dam.gpkg (spatial impact zones)
#   - impact_zone_summary.csv (affected lengths by dam/scenario)
#   - impact_zone_map.html (interactive map)
#
# LOCATION: workflows/04_connectivity/03_dam_impact_zones.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(igraph)
library(sf)
library(data.table)
library(lwgeom)
library(leaflet)
library(htmlwidgets)

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)

# Create output directory
dir.create("connectivity", showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

TARGET_BASIN_ID <- 1292502  # Same basin as fragmentation analysis
EXAMPLE_SPECIES <- "Barbus_prespensis"

# impact zone radii (meters)
UPSTREAM_RADIUS <- 500    # Distance upstream affected by dam
DOWNSTREAM_RADIUS <- 2000  # Distance downstream affected by dam

message("=== DAM IMPACT ZONE ANALYSIS ===")
message("Basin: ", TARGET_BASIN_ID)
message("Species: ", EXAMPLE_SPECIES)
message("Upstream radius: ", UPSTREAM_RADIUS, " m")
message("Downstream radius: ", DOWNSTREAM_RADIUS, " m")
message("Date: ", Sys.Date())

# ============================================================
# STEP 1: Load river network (spatial version for impact zones)
# ============================================================

message("\n[1/7] Loading spatial river network...")

stream_network <- st_read("spatial/stream_networks/partial_stream_network.gpkg",
                          quiet = TRUE)

# Filter to target basin
# First need to get basin_id from graph or node data
river_graph <- readRDS("spatial/stream_networks/river_graph.RDS")

node_data <- data.frame(
  name = V(river_graph)$name,
  basin_id = V(river_graph)$basin_id
) %>%
  filter(!is.na(basin_id))

basin_nodes <- node_data %>%
  filter(basin_id == TARGET_BASIN_ID) %>%
  pull(name)

# Filter spatial network to basin
basin_streams <- stream_network %>%
  filter(subc_id %in% basin_nodes)

message("  Basin streams: ", nrow(basin_streams))
message("  Total length: ", round(sum(basin_streams$length) / 1000, 1), " km")

# ============================================================
# STEP 2: Load dams and categorize
# ============================================================

message("\n[2/7] Loading dam data...")

dams_all <- fread("points_snapped/dams/dams_snapped_points.csv")

# Filter to target basin
dams_basin <- dams_all %>%
  filter(as.character(subc_id) %in% basin_nodes)

# Separate by status
dams_existing <- dams_basin %>%
  filter(status == "existing")

dams_planned <- dams_basin %>%
  filter(status == "planned") %>%
  filter(phase != "R")

message("  Existing dams: ", nrow(dams_existing))
message("  Planned dams: ", nrow(dams_planned))

# ============================================================
# STEP 3: Load species occurrences
# ============================================================

message("\n[3/7] Loading species occurrence data...")

fish_all <- fread("points_snapped/fish/fish_all_species_snapped.csv")

# Filter to target basin and species
species_occurrences <- fish_all %>%
  filter(as.character(subc_id) %in% basin_nodes) %>%
  filter(species == EXAMPLE_SPECIES |
           species == gsub("_", " ", EXAMPLE_SPECIES))

message("  Species occurrences: ", nrow(species_occurrences))

# ============================================================
# STEP 4: Calculate impact zones for existing dams
# ============================================================

message("\n[4/7] Calculating impact zones for existing dams...")

# Convert existing dams to sf points
dams_existing_sf <- dams_existing %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"),
           crs = 4326)

# Calculate impact zone using hydrographr function
message("  Running get_buffer_along_the_network()...")

impact_zone_existing <- get_buffer_along_the_network(
  lines_sf = basin_streams,
  start_points = dams_existing_sf,
  up_radius = UPSTREAM_RADIUS,
  down_radius = DOWNSTREAM_RADIUS
)

message("  Impact zone segments: ", nrow(impact_zone_existing))

# Add dam metadata
impact_zone_existing <- impact_zone_existing %>%
  mutate(dam_status = "existing")

# ============================================================
# STEP 5: Calculate impact zones for planned dams
# ============================================================

message("\n[5/7] Calculating impact zones for planned dams...")

if (nrow(dams_planned) > 0) {

  # Convert planned dams to sf points
  dams_planned_sf <- dams_planned %>%
    st_as_sf(coords = c("longitude_snapped", "latitude_snapped"),
             crs = 4326)

  impact_zone_planned <- get_buffer_along_the_network(
    lines_sf = basin_streams,
    start_points = dams_planned_sf,
    up_radius = UPSTREAM_RADIUS,
    down_radius = DOWNSTREAM_RADIUS
  )

  message("  Impact zone segments: ", nrow(impact_zone_planned))

  # Add dam metadata
  impact_zone_planned <- impact_zone_planned %>%
    mutate(dam_status = "planned")

  # Combine both scenarios
  impact_zone_all <- bind_rows(
    impact_zone_existing,
    impact_zone_planned
  )

} else {
  message("  No planned dams - skipping")
  impact_zone_all <- impact_zone_existing
}

# Save impact zones
st_write(impact_zone_all,
         "connectivity/impact_zone_by_dam.gpkg",
         delete_dsn = TRUE)

message("  Saved: connectivity/impact_zone_by_dam.gpkg")

# ============================================================
# STEP 6: Calculate affected habitat statistics
# ============================================================

message("\n[6/7] Calculating affected habitat statistics...")

# Total length of basin
total_basin_length <- sum(basin_streams$length, na.rm = TRUE)

# Affected length by existing dams
affected_length_existing <- impact_zone_existing %>%
  st_drop_geometry() %>%
  summarise(total_length = sum(length, na.rm = TRUE)) %>%
  pull(total_length)

# Affected length by all dams (existing + planned)
affected_length_all <- impact_zone_all %>%
  st_drop_geometry() %>%
  summarise(total_length = sum(length, na.rm = TRUE)) %>%
  pull(total_length)

# Calculate proportions
prop_existing <- affected_length_existing / total_basin_length
prop_all <- affected_length_all / total_basin_length

# Create summary
impact_zone_summary <- data.frame(
  basin_id = TARGET_BASIN_ID,
  species = EXAMPLE_SPECIES,
  upstream_radius_m = UPSTREAM_RADIUS,
  downstream_radius_m = DOWNSTREAM_RADIUS,
  total_basin_length_m = total_basin_length,
  n_dams_existing = nrow(dams_existing),
  n_dams_planned = nrow(dams_planned),
  affected_length_existing_m = affected_length_existing,
  affected_length_all_m = affected_length_all,
  proportion_affected_existing = round(prop_existing, 4),
  proportion_affected_all = round(prop_all, 4),
  additional_affected_m = affected_length_all - affected_length_existing,
  additional_proportion = round(prop_all - prop_existing, 4)
)

print(impact_zone_summary)

write.csv(impact_zone_summary,
          "connectivity/impact_zone_summary.csv",
          row.names = FALSE)

message("\nSummary Statistics:")
message("  Total basin length: ", round(total_basin_length / 1000, 1), " km")
message("  Affected by existing dams: ", round(affected_length_existing / 1000, 1),
        " km (", round(100 * prop_existing, 1), "%)")
message("  Affected by all dams: ", round(affected_length_all / 1000, 1),
        " km (", round(100 * prop_all, 1), "%)")
message("  Additional impact of planned dams: ",
        round((affected_length_all - affected_length_existing) / 1000, 1), " km")


# ============================================================
# FINAL SUMMARY
# ============================================================

message("\n=== IMPACT ZONE ANALYSIS COMPLETE ===")
message("Basin: ", TARGET_BASIN_ID)
message("Species: ", EXAMPLE_SPECIES)
message("\nImpact Zone Parameters:")
message("  Upstream radius: ", UPSTREAM_RADIUS, " m")
message("  Downstream radius: ", DOWNSTREAM_RADIUS, " m")
message("\nHabitat Impact:")
message("  Total basin length: ", round(total_basin_length / 1000, 1), " km")
message("  Affected by existing dams: ", round(affected_length_existing / 1000, 1),
        " km (", round(100 * prop_existing, 1), "%)")
message("  Additional impact if planned dams built: ",
        round((affected_length_all - affected_length_existing) / 1000, 1), " km")
message("  Total affected (existing + planned): ", round(affected_length_all / 1000, 1),
        " km (", round(100 * prop_all, 1), "%)")
message("\nOutput Files:")
message("  • connectivity/impact_zone_by_dam.gpkg")
message("  • connectivity/impact_zone_summary.csv")
message("  • connectivity/impact_zone_map.html")
message("\nAnalysis complete: ", Sys.time())
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 04_clean_dam_data.R
#
# Process barrier data from HCMR,
#  keep only true dams, classify into existing vs. planned,
# filter to the target basin, and prepare for snapping.
#
# Workflow:
#   1. Load and clean dam inventory (keep type == "DAM" only)
#   2. Classify existing vs. planned
#   3. Filter to target basin via api_get_ids()
#   4. Visualise
#   5. Data quality checks
#   6. Duplicate analysis
#
# Input:
#   - points_original/dams/dams_sarantaporos_table.csv   (semicolon-delimited;
#       columns: site_id;Name;Name_GR;type;longitude;latitude
#       type takes values FACTORY, EXCLUDE, DAM — only DAM is retained)
#   - config/study_area_params.csv              (BASIN_ID from 01_clean_hcmr_fish.R)
#
# Output:
#   - points_cleaned/dams/dams_all_clean.csv    (basin-filtered DAMs, existing+planned)
#   - points_cleaned/dams/dams_duplicates.csv
#   - points_cleaned/maps/dams_all_sources_overview.html
#   - points_cleaned/maps/dams_duplicates_map.html
#
# LOCATION: workflows/01_data_preparation/04_clean_dam_data.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(sf)
library(leaflet)
library(htmlwidgets)

select <- dplyr::select

source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

dir.create("points_original/dams", recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/dams",  recursive = TRUE, showWarnings = FALSE)
dir.create("points_cleaned/maps",  recursive = TRUE, showWarnings = FALSE)

# ============================================================
# PARAMETERS
# ============================================================

# Path to the curated dam inventory (semicolon-delimited)
DAM_FILE <- "points_original/dams/dams_sarantaporos_table.csv"

# site_id of the single existing operational dam.
# All other DAMs are treated as planned.
EXISTING_DAM_ID <- "37793"

# ============================================================
# STEP 1: Load and clean dam inventory
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("DAM DATA CLEANING")
message(paste(rep("=", 80), collapse = ""))

message("\n=== Step 1: Loading dam inventory ===")

if (!file.exists(DAM_FILE)) stop("Dam inventory not found: ", DAM_FILE)

dams_raw <- fread(DAM_FILE, sep = ";")

message("  Rows loaded: ", nrow(dams_raw))
message("  type values: ",
        paste(names(table(dams_raw$type)), table(dams_raw$type),
              sep = "=", collapse = ", "))

# Keep only true dams; drop FACTORY and EXCLUDE
dams_all <- dams_raw %>%
  mutate(site_id = as.character(site_id)) %>%
  filter(type == "DAM") %>%
  select(site_id, Name, longitude, latitude) %>%
  mutate(
    longitude = as.numeric(longitude),
    latitude  = as.numeric(latitude)
  )

message("  DAM records retained: ", nrow(dams_all))

# ============================================================
# STEP 2: Classify existing vs. planned
# ============================================================

message("\n=== Step 2: Classifying existing vs. planned ===")

dams_all <- dams_all %>%
  mutate(status = if_else(site_id == EXISTING_DAM_ID, "existing", "planned"))

if (!any(dams_all$status == "existing")) {
  warning("Existing dam id ", EXISTING_DAM_ID,
          " not found among DAM records — check EXISTING_DAM_ID.")
}

message("  Existing dams: ", sum(dams_all$status == "existing"))
message("  Planned dams:  ", sum(dams_all$status == "planned"))

# ============================================================
# STEP 3: Filter to target basin
# ============================================================

message("\n=== Step 3: Filtering to target basin ===")

# Load basin ID derived in 01_clean_hcmr_fish.R
study_params <- fread("config/study_area_params.csv")
BASIN_ID <- study_params[param == "BASIN_ID", as.integer(value)]
message("  Target basin ID: ", BASIN_ID)

dams_unique_locs <- dams_all %>%
  distinct(site_id, longitude, latitude) %>%
  filter(!is.na(longitude), !is.na(latitude))

message("  Unique dam locations to query: ", nrow(dams_unique_locs))

basin_ids_dams <- api_get_ids(
  points          = dams_unique_locs,
  colname_lon     = "longitude",
  colname_lat     = "latitude",
  colname_site_id = "site_id",
  mode            = "local"
)

dams_all <- dams_all %>%
  left_join(basin_ids_dams) %>%
  filter(basin_id == BASIN_ID) %>%
  select(-basin_id, -subc_id, -reg_id)

message("  Dams in target basin: ", nrow(dams_all))
message("    existing: ", sum(dams_all$status == "existing"))
message("    planned:  ", sum(dams_all$status == "planned"))

fwrite(dams_all, "points_cleaned/dams/dams_all_clean.csv")
message("  Saved: points_cleaned/dams/dams_all_clean.csv")

# ============================================================
# STEP 4: Visualise
# ============================================================

message("\n=== Step 4: Creating visualisation ===")

status_colors <- colorFactor(
  palette = c("existing" = "darkblue", "planned" = "orange"),
  domain  = c("existing", "planned")
)

dams_map <- leaflet(dams_all) %>%
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addCircleMarkers(
    lng         = ~longitude,
    lat         = ~latitude,
    color       = ~status_colors(status),
    fillColor   = ~status_colors(status),
    radius      = 4,
    fillOpacity = 0.7,
    stroke      = TRUE,
    weight      = 1,
    popup       = ~paste0(
      "<b>Dam ID:</b> ", site_id, "<br>",
      "<b>Name:</b> ",   Name, "<br>",
      "<b>Status:</b> ", status
    )
  ) %>%
  addLayersControl(
    baseGroups = c("CartoDB", "Satellite"),
    options    = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(
    position = "bottomright",
    pal      = status_colors,
    values   = ~status,
    title    = "Dam status",
    opacity  = 0.7
  )

saveWidget(dams_map,
           "points_cleaned/maps/dams_all_sources_overview.html",
           selfcontained = TRUE)
save_to_nimbus(dams_map, "points_cleaned/maps/dams_all_sources_overview.html")
message("  Saved: points_cleaned/maps/dams_all_sources_overview.html")

# ============================================================
# STEP 5: Data quality checks
# ============================================================

message("\n=== Step 5: Data quality checks ===")

missing_coords <- dams_all %>% filter(is.na(longitude) | is.na(latitude))
if (nrow(missing_coords) > 0) {
  message("  WARNING: ", nrow(missing_coords), " dams with missing coordinates")
} else {
  message("  No missing coordinates")
}

out_of_bounds <- dams_all %>%
  filter(longitude < 19 | longitude > 28 | latitude < 34 | latitude > 42)
if (nrow(out_of_bounds) > 0) {
  message("  WARNING: ", nrow(out_of_bounds),
          " dams outside Greece bounds — check manually")
} else {
  message("  All coordinates within Greece bounds")
}

# ============================================================
# STEP 6: Duplicate analysis
# ============================================================

message("\n=== Step 6: Duplicate analysis ===")

duplicates_detailed <- dams_all %>%
  group_by(longitude, latitude) %>%
  mutate(n_at_location = n(), is_duplicate = n() > 1) %>%
  ungroup() %>%
  filter(is_duplicate) %>%
  arrange(longitude, latitude)

if (nrow(duplicates_detailed) > 0) {

  message("  Records at duplicate locations: ", nrow(duplicates_detailed))
  message("  Unique duplicate locations: ",
          n_distinct(paste(duplicates_detailed$longitude,
                           duplicates_detailed$latitude)))

  fwrite(duplicates_detailed, "points_cleaned/dams/dams_duplicates.csv")
  message("  Saved: points_cleaned/dams/dams_duplicates.csv")

  duplicates_map <- leaflet(duplicates_detailed) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(
      lng         = ~longitude,
      lat         = ~latitude,
      color       = "red",
      fillColor   = "red",
      radius      = 6,
      fillOpacity = 0.8,
      stroke      = TRUE,
      weight      = 2,
      popup       = ~paste0(
        "<b style='color:red;'>DUPLICATE LOCATION</b><br>",
        "<b>Records here:</b> ", n_at_location, "<br>",
        "<b>Dam ID:</b> ", site_id, "<br>",
        "<b>Status:</b> ", status
      )
    )

  saveWidget(duplicates_map,
             "points_cleaned/maps/dams_duplicates_map.html",
             selfcontained = TRUE)
  save_to_nimbus(duplicates_map, "points_cleaned/maps/dams_duplicates_map.html")
  message("  Saved: points_cleaned/maps/dams_duplicates_map.html")

} else {
  message("  No duplicate locations found")
}

# ============================================================
# SUMMARY
# ============================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("DAM DATA CLEANING COMPLETE")
message(paste(rep("=", 80), collapse = ""))
message("\nTarget basin ID: ", BASIN_ID)
message("Dams in basin: ", nrow(dams_all),
        "  (existing: ", sum(dams_all$status == "existing"),
        ", planned: ",   sum(dams_all$status == "planned"), ")")
message("\nNext: snapping script")
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 03 -- combined_visualizations.R
# Create publication-ready figures combining fragmentation and impact zone
# Reads analysis outputs from scripts 01 and 02
#
# READS:
#   - connectivity/scenario_comparison_summary.csv (from script 01)
#   - connectivity/impact_zone_summary.csv (from script 02)
#   - connectivity/impact_zone_by_dam.gpkg (from script 02)
#   - spatial/stream_networks/partial_stream_network.gpkg
#   - points_snapped/dams/dams_snapped_points.csv
#   - points_snapped/fish/fish_all_species_snapped.csv
#   - spatial/stream_networks/river_graph.RDS
#
# CREATES:
#   - connectivity/Figure_Combined_MEE.png (4-panel combined figure)
#   - connectivity/Figure_Combined_MEE.tiff (for publication)
#   - connectivity/combined_impact_map.html (interactive map)
#   - connectivity/combined_summary_table.csv (integrated metrics)
#   - connectivity/Figure_Combined_caption.txt (figure caption)
#   - connectivity/scenario_comparison_bars.png (from script 01)
#   - connectivity/Figure_Scenarios_MEE.png (from script 01)
#   - connectivity/scenario_comparison_map.html (from script 01)
#
# LOCATION: workflows/04_connectivity/04_combined_visualizations.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(tidyverse)
library(sf)
library(data.table)
library(leaflet)
library(htmlwidgets)
library(igraph)

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)

# ============================================================
# PARAMETERS
# ============================================================

TARGET_BASIN_ID <- 1292502
EXAMPLE_SPECIES <- "Barbus_prespensis"

message("=== COMBINED IMPACT VISUALIZATIONS ===")
message("Basin: ", TARGET_BASIN_ID)
message("Species: ", EXAMPLE_SPECIES)
message("Date: ", Sys.Date())

# ============================================================
# STEP 1: Load analysis results
# ============================================================

message("\n[1/6] Loading analysis results...")

# Fragmentation results (from script 01)
fragmentation_summary <- read.csv("connectivity/scenario_comparison_summary.csv")

# Impact zone results (from script 02)
impact_zone_summary <- read.csv("connectivity/impact_zone_summary.csv")

# Impact zone spatial data (from script 02)
impact_zone_all <- st_read("connectivity/impact_zone_by_dam.gpkg", quiet = TRUE)

message("  Fragmentation data loaded")
message("  Impact zone data loaded")

# ============================================================
# STEP 2: Load spatial data for maps
# ============================================================

message("\n[2/6] Loading spatial data...")

# River network
river_graph <- readRDS("spatial/stream_networks/river_graph.RDS")

node_data <- data.frame(
  name = V(river_graph)$name,
  basin_id = V(river_graph)$basin_id
) %>%
  filter(!is.na(basin_id))

basin_nodes <- node_data %>%
  filter(basin_id == TARGET_BASIN_ID) %>%
  pull(name)

stream_spatial <- st_read("spatial/stream_networks/partial_stream_network.gpkg", quiet = TRUE)

basin_streams <- stream_spatial %>%
  filter(subc_id %in% basin_nodes) %>%
  st_transform(4326)

# Dams
dams_all <- fread("points_snapped/dams/dams_snapped_points.csv")

dams_basin <- dams_all %>%
  filter(as.character(subc_id) %in% basin_nodes)

dams_existing <- dams_basin %>%
  filter(status == "existing")

dams_planned <- dams_basin %>%
  filter(status == "planned") %>%
  filter(phase != "R")

dams_existing_sf <- dams_existing %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

dams_planned_sf <- dams_planned %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

# Species
fish_all <- fread("points_snapped/fish/fish_all_species_snapped.csv")

species_occurrences <- fish_all %>%
  filter(as.character(subc_id) %in% basin_nodes) %>%
  filter(species == EXAMPLE_SPECIES | species == gsub("_", " ", EXAMPLE_SPECIES))

species_sf <- species_occurrences %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

# Impact zones
impact_zone_existing <- impact_zone_all %>%
  filter(dam_status == "existing")

impact_zone_planned <- impact_zone_all %>%
  filter(dam_status == "planned")

# Dam-affected streams
dam_existing_streams <- basin_streams %>%
  filter(subc_id %in% unique(as.character(dams_existing$subc_id)))

dam_planned_streams <- basin_streams %>%
  filter(subc_id %in% unique(as.character(dams_planned$subc_id)))

message("  Spatial data prepared")

# ============================================================
# STEP 3: Create combined summary table
# ============================================================

message("\n[3/6] Creating combined summary table...")

combined_summary <- data.frame(
  Basin_ID = TARGET_BASIN_ID,
  Species = EXAMPLE_SPECIES,

  # Existing Dams Scenario
  Existing_Dams_N = fragmentation_summary$N_dams[1],
  Existing_Habitat_Affected_km = round(impact_zone_summary$affected_length_existing_m / 1000, 2),
  Existing_Habitat_Affected_Pct = round(100 * impact_zone_summary$proportion_affected_existing, 1),
  Existing_Pairs_Disconnected = fragmentation_summary$Disconnected_pairs[1],
  Existing_Connectivity_Loss_Pct = fragmentation_summary$Percent_lost[1],
  Existing_Network_Components = fragmentation_summary$N_components[1],

  # Future Scenario (Existing + Planned)
  Future_Dams_N = fragmentation_summary$N_dams[2],
  Future_Habitat_Affected_km = round(impact_zone_summary$affected_length_all_m / 1000, 2),
  Future_Habitat_Affected_Pct = round(100 * impact_zone_summary$proportion_affected_all, 1),
  Future_Pairs_Disconnected = fragmentation_summary$Disconnected_pairs[2],
  Future_Connectivity_Loss_Pct = fragmentation_summary$Percent_lost[2],
  Future_Network_Components = fragmentation_summary$N_components[2],

  # Additional Impact of Planned Dams
  Additional_Habitat_Affected_km = round(impact_zone_summary$additional_affected_m / 1000, 2),
  Additional_Habitat_Affected_Pct = round(100 * impact_zone_summary$additional_proportion, 1),
  Additional_Pairs_Disconnected = fragmentation_summary$Disconnected_pairs[2] -
    fragmentation_summary$Disconnected_pairs[1],
  Additional_Connectivity_Loss_Pct = round(fragmentation_summary$Percent_lost[2] -
                                             fragmentation_summary$Percent_lost[1], 1)
)

# Transpose for readability
combined_summary_t <- t(combined_summary)
colnames(combined_summary_t) <- "Value"

write.csv(combined_summary_t,
          "connectivity/combined_summary_table.csv",
          row.names = TRUE)

print(combined_summary_t)

message("  Saved: connectivity/combined_summary_table.csv")

# ============================================================
# STEP 4: Create combined MEE figure (4 panels)
# ============================================================

message("\n[4/6] Creating combined MEE figure...")

png("connectivity/Figure_Combined_MEE.png",
    width = 173, height = 140, units = "mm", res = 300)

layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))

# Extract values for plotting
habitat_affected <- c(
  100 * impact_zone_summary$proportion_affected_existing,
  100 * impact_zone_summary$proportion_affected_all
)

conn_loss <- c(
  fragmentation_summary$Percent_lost[1],
  fragmentation_summary$Percent_lost[2]
)

pairs_lost <- c(
  fragmentation_summary$Disconnected_pairs[1],
  fragmentation_summary$Disconnected_pairs[2]
)

additional_metrics <- c(
  fragmentation_summary$Percent_lost[2] - fragmentation_summary$Percent_lost[1],
  100 * impact_zone_summary$additional_proportion
)

# ------------------------------------------------------------
# Panel A: Connectivity Loss
# ------------------------------------------------------------

par(mar = c(5, 4.5, 3, 0.5))

barplot(conn_loss,
        names.arg = c("Current", "Future"),
        col = c("#7fbf7b", "#FC8D62"),
        ylab = "Connectivity loss (%)",
        main = "(a) Population Connectivity",
        cex.lab = 1.2,
        cex.main = 1.4,
        cex.names = 1.1,
        cex.axis = 1.0,
        ylim = c(0, max(conn_loss) * 1.3),
        border = "white",
        las = 1)

text(c(0.7, 1.9),
     conn_loss + max(conn_loss) * 0.05,
     paste0(round(conn_loss, 1), "%"),
     cex = 1.2,
     font = 2)

mtext(paste(nrow(dams_existing), "dams          ",
            nrow(dams_existing) + nrow(dams_planned), "dams"),
      side = 1, line = 3.5, cex = 0.85, font = 3)

# ------------------------------------------------------------
# Panel B: Habitat Affected
# ------------------------------------------------------------

par(mar = c(5, 4.5, 3, 0.5))

barplot(habitat_affected,
        names.arg = c("Current", "Future"),
        col = c("#7fbf7b", "#FC8D62"),
        ylab = "Habitat affected (%)",
        main = "(b) Habitat Impact",
        cex.lab = 1.2,
        cex.main = 1.4,
        cex.names = 1.1,
        cex.axis = 1.0,
        ylim = c(0, max(habitat_affected) * 1.3),
        border = "white",
        las = 1)

text(c(0.7, 1.9),
     habitat_affected + max(habitat_affected) * 0.05,
     paste0(round(habitat_affected, 1), "%"),
     cex = 1.2,
     font = 2)

mtext(paste(round(impact_zone_summary$affected_length_existing_m/1000, 1), "km          ",
            round(impact_zone_summary$affected_length_all_m/1000, 1), "km"),
      side = 1, line = 3.5, cex = 0.85, font = 3)

# ------------------------------------------------------------
# Panel C: Disconnected Pairs
# ------------------------------------------------------------

par(mar = c(5, 4.5, 3, 0.5))

barplot(pairs_lost,
        names.arg = c("Current", "Future"),
        col = c("#7fbf7b", "#FC8D62"),
        ylab = "Disconnected pairs",
        main = "(c) Population Isolation",
        cex.lab = 1.2,
        cex.main = 1.4,
        cex.names = 1.1,
        cex.axis = 1.0,
        ylim = c(0, max(pairs_lost) * 1.3),
        border = "white",
        las = 1)

text(c(0.7, 1.9),
     pairs_lost + max(pairs_lost) * 0.05,
     pairs_lost,
     cex = 1.2,
     font = 2)

# ------------------------------------------------------------
# Panel D: Additional Impact Comparison
# ------------------------------------------------------------

par(mar = c(5, 4.5, 3, 0.5))

barplot(additional_metrics,
        names.arg = c("Connectivity\nloss", "Habitat\naffected"),
        col = "#FC8D62",
        ylab = "Additional impact (% points)",
        main = "(d) Planned Dams Impact",
        cex.lab = 1.2,
        cex.main = 1.4,
        cex.names = 1.0,
        cex.axis = 1.0,
        ylim = c(0, max(additional_metrics) * 1.3),
        border = "white",
        las = 1)

text(c(0.7, 1.9),
     additional_metrics + max(additional_metrics) * 0.05,
     paste0("+", round(additional_metrics, 1), "%"),
     cex = 1.2,
     font = 2,
     col = "darkred")

dev.off()

message("  Saved: connectivity/Figure_Combined_MEE.png")

# Create TIFF version
tiff("connectivity/Figure_Combined_MEE.tiff",
     width = 173, height = 140, units = "mm", res = 300,
     compression = "lzw")

# Repeat same plotting code
layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))

# Panel A
par(mar = c(5, 4.5, 3, 0.5))
barplot(conn_loss, names.arg = c("Current", "Future"),
        col = c("#7fbf7b", "#FC8D62"), ylab = "Connectivity loss (%)",
        main = "(a) Population Connectivity", cex.lab = 1.2, cex.main = 1.4,
        cex.names = 1.1, cex.axis = 1.0, ylim = c(0, max(conn_loss) * 1.3),
        border = "white", las = 1)
text(c(0.7, 1.9), conn_loss + max(conn_loss) * 0.05,
     paste0(round(conn_loss, 1), "%"), cex = 1.2, font = 2)
mtext(paste(nrow(dams_existing), "dams          ",
            nrow(dams_existing) + nrow(dams_planned), "dams"),
      side = 1, line = 3.5, cex = 0.85, font = 3)

# Panel B
par(mar = c(5, 4.5, 3, 0.5))
barplot(habitat_affected, names.arg = c("Current", "Future"),
        col = c("#7fbf7b", "#FC8D62"), ylab = "Habitat affected (%)",
        main = "(b) Habitat Impact", cex.lab = 1.2, cex.main = 1.4,
        cex.names = 1.1, cex.axis = 1.0, ylim = c(0, max(habitat_affected) * 1.3),
        border = "white", las = 1)
text(c(0.7, 1.9), habitat_affected + max(habitat_affected) * 0.05,
     paste0(round(habitat_affected, 1), "%"), cex = 1.2, font = 2)
mtext(paste(round(impact_zone_summary$affected_length_existing_m/1000, 1), "km          ",
            round(impact_zone_summary$affected_length_all_m/1000, 1), "km"),
      side = 1, line = 3.5, cex = 0.85, font = 3)

# Panel C
par(mar = c(5, 4.5, 3, 0.5))
barplot(pairs_lost, names.arg = c("Current", "Future"),
        col = c("#7fbf7b", "#FC8D62"), ylab = "Disconnected pairs",
        main = "(c) Population Isolation", cex.lab = 1.2, cex.main = 1.4,
        cex.names = 1.1, cex.axis = 1.0, ylim = c(0, max(pairs_lost) * 1.3),
        border = "white", las = 1)
text(c(0.7, 1.9), pairs_lost + max(pairs_lost) * 0.05,
     pairs_lost, cex = 1.2, font = 2)

# Panel D
par(mar = c(5, 4.5, 3, 0.5))
barplot(additional_metrics, names.arg = c("Connectivity\nloss", "Habitat\naffected"),
        col = "#FC8D62", ylab = "Additional impact (% points)",
        main = "(d) Planned Dams Impact", cex.lab = 1.2, cex.main = 1.4,
        cex.names = 1.0, cex.axis = 1.0, ylim = c(0, max(additional_metrics) * 1.3),
        border = "white", las = 1)
text(c(0.7, 1.9), additional_metrics + max(additional_metrics) * 0.05,
     paste0("+", round(additional_metrics, 1), "%"),
     cex = 1.2, font = 2, col = "darkred")

dev.off()

message("  Saved: connectivity/Figure_Combined_MEE.tiff")

# ============================================================
# Create combined figure caption
# ============================================================

caption_combined <- paste0(
  "Integrated dam impact analysis on ", EXAMPLE_SPECIES, " in basin ", TARGET_BASIN_ID,
  " combining population connectivity and habitat degradation metrics under current and future dam scenarios. ",

  "(a) Population connectivity loss: Dams disconnect ", fragmentation_summary$Percent_lost[1], "% ",
  "of population pairs under current conditions (", nrow(dams_existing), " existing dams), ",
  "increasing to ", fragmentation_summary$Percent_lost[2], "% if all ", nrow(dams_planned),
  " planned dams are constructed (", nrow(dams_existing) + nrow(dams_planned), " total dams). ",

  "(b) Habitat impact: Dam footprints (", service_area_summary$upstream_radius_m, " m upstream, ",
  service_area_summary$downstream_radius_m, " m downstream) affect ",
  round(100 * service_area_summary$proportion_affected_existing, 1), "% of total habitat ",
  "(", round(service_area_summary$affected_length_existing_m / 1000, 1), " km) under current conditions, ",
  "increasing to ", round(100 * service_area_summary$proportion_affected_all, 1), "% ",
  "(", round(service_area_summary$affected_length_all_m / 1000, 1), " km) with planned dam construction. ",

  "(c) Population isolation: Number of disconnected population pairs increases from ",
  pairs_lost[1], " (current) to ", pairs_lost[2], " (future), representing ",
  pairs_lost[2] - pairs_lost[1], " additional isolated populations. ",

  "(d) Cumulative impact of planned dams: Construction of ", nrow(dams_planned), " planned dams would cause ",
  "an additional ", round(additional_metrics[1], 1), " percentage point loss in connectivity and ",
  round(additional_metrics[2], 1), " percentage point increase in habitat degradation. ",

  "The combined analysis reveals compound effects: planned dams would simultaneously increase ",
  "both direct habitat loss (physical footprint) and functional habitat loss (population isolation). ",
  "Dam footprints represent immediate physical impacts within service areas, while connectivity loss ",
  "represents network-scale functional fragmentation that can extend far beyond dam locations. ",
  "This dual impact threatens metapopulation viability through both reduced habitat quantity and ",
  "compromised population connectivity essential for genetic exchange and recolonization."
)

writeLines(caption_combined, "connectivity/Figure_Combined_caption.txt")

message("  Saved: connectivity/Figure_Combined_caption.txt")





# ============================================================
# STEP 5: Create standalone fragmentation figures (from script 01)
# ============================================================

message("\n[5/6] Creating standalone fragmentation figures...")

# Figure: Side-by-side comparison bars
png("connectivity/scenario_comparison_bars.png",
    width = 14, height = 6, units = "in", res = 300)

par(mfrow = c(1, 3), mar = c(6, 5, 4, 2))

# Panel A: Connectivity loss
barplot(conn_loss,
        names.arg = c("Current\n(existing)", "Future\n(+ planned)"),
        col = c("#7fbf7b", "#FC8D62"),
        ylab = "Connectivity loss (%)",
        main = "(a) Connectivity Loss",
        cex.lab = 1.4, cex.main = 1.6, cex.names = 1.3, cex.axis = 1.2,
        ylim = c(0, max(conn_loss) * 1.25), border = "white")
text(c(0.7, 1.9), conn_loss + max(conn_loss) * 0.05,
     paste0(conn_loss, "%"), cex = 1.4, font = 2)

# Panel B: Network components
barplot(fragmentation_summary$N_components,
        names.arg = c("Current\n(existing)", "Future\n(+ planned)"),
        col = c("#7fbf7b", "#FC8D62"),
        ylab = "Network components",
        main = "(b) Network Fragmentation",
        cex.lab = 1.4, cex.main = 1.6, cex.names = 1.3, cex.axis = 1.2,
        ylim = c(0, max(fragmentation_summary$N_components) * 1.25), border = "white")
text(c(0.7, 1.9), fragmentation_summary$N_components + max(fragmentation_summary$N_components) * 0.05,
     fragmentation_summary$N_components, cex = 1.4, font = 2)

# Panel C: Disconnected pairs
barplot(pairs_lost,
        names.arg = c("Current\n(existing)", "Future\n(+ planned)"),
        col = c("#7fbf7b", "#FC8D62"),
        ylab = "Disconnected pairs",
        main = "(c) Lost Connections",
        cex.lab = 1.4, cex.main = 1.6, cex.names = 1.3, cex.axis = 1.2,
        ylim = c(0, max(pairs_lost) * 1.25), border = "white")
text(c(0.7, 1.9), pairs_lost + max(pairs_lost) * 0.05,
     pairs_lost, cex = 1.4, font = 2)

dev.off()

message("  Saved: connectivity/scenario_comparison_bars.png")

# ============================================================
# STEP 6: Create combined interactive map
# ============================================================

message("\n[6/6] Creating combined interactive map...")
# Separate impact zones
impact_zone_existing_only <- impact_zone_all %>%
  filter(dam_status == "existing")

impact_zone_planned_only <- impact_zone_all %>%
  filter(dam_status == "planned")

# Create map
service_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  # Original network
  addPolylines(
    data = basin_streams,
    color = "#1f78b4",
    weight = 2,
    opacity = 0.6,
    group = "River Network",
    popup = ~paste0("Subcatchment: ", subc_id, "<br>",
                    "Length: ", round(length, 1), " m")
  ) %>%
  # Impact zone - existing dams
  addPolylines(
    data = impact_zone_existing_only,
    color = "#d7191c",      # strong red = existing impact
    weight = 4,
    opacity = 0.8,
    dashArray = "5,5",
    group = "Impact Zone (Existing)",
    popup = ~paste0("Affected by existing dam<br>",
                    "Length: ", round(length, 1), " m")
  ) %>%
  # Impact zone - planned dams
  addPolylines(
    data = impact_zone_planned_only,
    color = "#fdae61",      # amber = future/planned impact
    weight = 4,
    opacity = 0.8,
    dashArray = "10,5",
    group = "Impact Zone (Planned)",
    popup = ~paste0("Affected by planned dam<br>",
                    "Length: ", round(length, 1), " m")
  ) %>%
  # Existing dams
  addCircleMarkers(
    data = dams_existing_sf,
    radius = 6,
    color = "#252525",      # near-black border
    fillColor = "#636363",  # mid-grey fill = existing structure
    fillOpacity = 0.9,
    stroke = TRUE,
    weight = 2,
    group = "Existing Dams",
    popup = ~paste0("Dam: ", ifelse(is.na(id1), "Unnamed", id1), "<br>",
                    "Status: Existing")
  ) %>%
  # Planned dams
  addCircleMarkers(
    data = if(nrow(dams_planned) > 0) dams_planned_sf else NULL,
    radius = 6,
    color = "#252525",      # same border as existing
    fillColor = "#bdbdbd",  # light grey fill = planned (lighter = not yet built)
    fillOpacity = 0.9,
    stroke = TRUE,
    weight = 3,
    group = "Planned Dams",
    popup = ~paste0("Dam: ", ifelse(is.na(id1), "Unnamed", id1), "<br>",
                    "Status: Planned")
  ) %>%
  # Species occurrences
  addCircleMarkers(
    data = species_sf,
    radius = 5,
    color = "#1b7837",      # dark green border
    fillColor = "#7fbf7b",  # mid green fill = biological
    fillOpacity = 0.8,
    stroke = TRUE,
    weight = 2,
    group = EXAMPLE_SPECIES,
    popup = ~paste0("Species: ", species, "<br>",
                    "Site: ", site_id)
  ) %>%
  # Layer controls
  addLayersControl(
    baseGroups = c("Light", "Satellite"),
    overlayGroups = c(
      "River Network",
      "Impact Zone (Existing)",
      "Impact Zone (Planned)",
      "Existing Dams",
      "Planned Dams",
      EXAMPLE_SPECIES
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Legend
  addLegend(
    position = "bottomright",
    colors = c("#1f78b4", "#d7191c", "#fdae61", "#636363", "#bdbdbd", "#7fbf7b"),
    labels = c(
      "River Network",
      paste0("Impact Zone (Existing): ", round(affected_length_existing / 1000, 1), " km"),
      paste0("Additional (Planned): ",
             round((affected_length_all - affected_length_existing) / 1000, 1), " km"),
      "Existing Dams",
      "Planned Dams",
      EXAMPLE_SPECIES
    ),
    opacity = 0.8,
    title = "Legend"
  ) %>%
  addScaleBar(position = "bottomleft") %>%
# Title
  addControl(
    html = paste0(
      "<div style='background: white; padding: 10px; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);'>",
      "<h3 style='margin: 0; color: #333;'>Dam Impact Zones - Basin ", TARGET_BASIN_ID, "</h3>",
      "<p style='margin: 5px 0 0 0; color: #666;'>",
      "<strong>Species:</strong> ", EXAMPLE_SPECIES, "<br>",
      "<strong>Impact zone:</strong> ", UPSTREAM_RADIUS, " m up, ", DOWNSTREAM_RADIUS, " m down<br>",
      "<strong>Existing dams:</strong> ", nrow(dams_existing),
      " (", round(100 * prop_existing, 1), "% habitat affected)<br>",
      "<strong>+ Planned:</strong> ", nrow(dams_existing) + nrow(dams_planned),  # FIXED
      " total (", round(100 * prop_all, 1), "% habitat affected)",
      "</p></div>"
    ),
    position = "topright"
  )

service_map

# Save map
# save_to_nimbus(service_map,
#                "connectivity/impact_zone_map.html")
saveWidget(service_map,
               "connectivity/impact_zone_map.html")

message("  Saved: connectivity/impact_zone_map.html")


# ============================================================
# FINAL SUMMARY
# ============================================================

message("\n=== VISUALIZATION CREATION COMPLETE ===")
message("Basin: ", TARGET_BASIN_ID)
message("Species: ", EXAMPLE_SPECIES)
message("\nFigures Created:")
message("  • connectivity/Figure_Combined_MEE.png (4-panel combined)")
message("  • connectivity/Figure_Combined_MEE.tiff (for publication)")
message("  • connectivity/scenario_comparison_bars.png (3-panel fragmentation)")
message("  • connectivity/combined_impact_map.html (interactive map)")
message("\nData Products:")
message("  • connectivity/combined_summary_table.csv")
message("  • connectivity/Figure_Combined_caption.txt")
message("\nKey Findings:")
message("  Current Impact:")
message("    - Connectivity lost: ", fragmentation_summary$Percent_lost[1], "%")
message("    - Habitat affected: ", round(100 * impact_zone_summary$proportion_affected_existing, 1), "%")
message("  Future Impact:")
message("    - Connectivity lost: ", fragmentation_summary$Percent_lost[2], "%")
message("    - Habitat affected: ", round(100 * impact_zone_summary$proportion_affected_all, 1), "%")
message("  Additional Impact of Planned Dams:")
message("    - +", round(additional_metrics[1], 1), "% connectivity loss")
message("    - +", round(additional_metrics[2], 1), "% habitat affected")
message("\nAnalysis complete: ", Sys.time())
library(sf)
library(leaflet)
library(RColorBrewer)
library(htmlwidgets)

# Set working directory
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# Check working directory
BASE_DIR
setwd(BASE_DIR)


basin_file <- "/home/grigoropoulou/Documents/Postdoc/projects/hydrographr_use_case_greece/r.watershed/basin_tiles20d/basin_greece.gpkg"
basins <- read_geopackage(basin_file, import_as = "sf",subc_id = basin_summary$basin_id,
                          name = "ID")


# Get basin centroids from stream network
basin_centroids <- basins %>%
  group_by(ID) %>%
  summarize(geometry = st_union(geom)) %>%
  st_centroid() %>%
  st_transform(4326)

# Join with results
basin_map_data <- basin_summary %>%
  left_join(
    basin_centroids %>% st_drop_geometry() %>%
      mutate(lon = st_coordinates(basin_centroids)[,1],
             lat = st_coordinates(basin_centroids)[,2]),
    by = c("basin_id" = "ID")
  )





# CRITICAL FIX: Single color palette using the MAXIMUM value across BOTH scenarios
max_loss <- max(c(basin_summary$mean_loss_existing,
                  basin_summary$mean_loss_future), na.rm = TRUE)
# Create THREE layers:
# 1. Existing (baseline)
# 2. Future (total impact)
# 3. Change (additional impact from planned)

# Color palette for change (diverging)
pal_change <- colorNumeric(
  palette = "RdYlGn",
  domain = c(0, max(basin_summary$mean_additional_loss, na.rm = TRUE)),
  reverse = TRUE  # Red = high additional loss
)

# Color palette for absolute loss (consistent)
pal_abs <- colorNumeric(
  palette = "YlOrRd",
  domain = c(0, max_loss)
)

greece_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%

  # Layer 1: Existing scenario (yellow-red)
  addCircleMarkers(
    data = basin_map_data,
    lng = ~lon, lat = ~lat,
    radius = ~sqrt(n_species) * 3,
    color = "black",
    fillColor = ~pal_abs(mean_loss_existing),
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 1,
    group = "Current State",
    popup = ~paste0(
      "<b>Basin:</b> ", basin_id, "<br>",
      "<b>Species:</b> ", n_species, "<br>",
      "<b>Existing dams:</b> ", total_dams_existing, "<br>",
      "<b>Current loss:</b> ", round(mean_loss_existing, 1), "%"
    )
  ) %>%

  # Layer 2: Future scenario (same scale)
  addCircleMarkers(
    data = basin_map_data,
    lng = ~lon, lat = ~lat,
    radius = ~sqrt(n_species) * 3,
    color = "black",
    fillColor = ~pal_abs(mean_loss_future),
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 1,
    group = "Future State",
    popup = ~paste0(
      "<b>Basin:</b> ", basin_id, "<br>",
      "<b>Species:</b> ", n_species, "<br>",
      "<b>Total dams:</b> ", total_dams_existing + total_dams_planned,
      " (+", total_dams_planned, " planned)<br>",
      "<b>Future loss:</b> ", round(mean_loss_future, 1), "%"
    )
  ) %>%

  # Layer 3: Additional impact (different palette!)
  addCircleMarkers(
    data = basin_map_data,
    lng = ~lon, lat = ~lat,
    radius = ~sqrt(n_species) * 3,
    color = "black",
    fillColor = ~pal_change(mean_additional_loss),
    fillOpacity = 0.8,
    stroke = TRUE,
    weight = 1,
    group = "Additional Impact (Planned Dams)",
    popup = ~paste0(
      "<b>Basin:</b> ", basin_id, "<br>",
      "<b>Species:</b> ", n_species, "<br>",
      "<b>Planned dams:</b> ", total_dams_planned, "<br>",
      "<b>Additional loss:</b> <span style='color:red'>+",
      round(mean_additional_loss, 1), "%</span><br>",
      "<b>From:</b> ", round(mean_loss_existing, 1),
      "% → ", round(mean_loss_future, 1), "%"
    )
  ) %>%

  # Legend for Current/Future
  addLegend(
    "bottomright",
    pal = pal_abs,
    values = c(0, max_loss),
    title = "Connectivity loss (%)<br>(Current & Future)",
    opacity = 0.7,
    group = c("Current State", "Future State")
  ) %>%

  # Legend for Additional Impact
  addLegend(
    "bottomleft",
    pal = pal_change,
    values = c(0, max(basin_summary$mean_additional_loss, na.rm = TRUE)),
    title = "Additional impact<br>from planned dams (%)",
    opacity = 0.7,
    group = "Additional Impact (Planned Dams)"
  ) %>%

  # Layer controls
  addLayersControl(
    baseGroups = c("Current State", "Future State", "Additional Impact (Planned Dams)"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%

  # Info box
  addControl(
    html = paste0(
      "<div style='background: white; padding: 10px; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);'>",
      "<h4 style='margin: 0 0 10px 0;'>Dam Impact Scenarios - Greece</h4>",
      "<p style='margin: 0; font-size: 12px;'>",
      "<b>Current:</b> ", sum(basin_summary$total_dams_existing), " existing dams<br>",
      "<b>Future:</b> +", sum(basin_summary$total_dams_planned), " planned dams<br>",
      "<b>Circle size</b> = number of species<br>",
      "<b>Circle color</b> = connectivity loss",
      "</p></div>"
    ),
    position = "topright"
  )

saveWidget(greece_map, "connectivity/connectivity_loss_greece.html")
save_to_nimbus(greece_map, "connectivity/greece_scenario_map.html")
# =============================================================================
# Reconstruct true absences and check species overlap across basins
# =============================================================================

library(dplyr)
library(tidyr)

# --- Load files ---
occurr      <- read.csv("points_snapped/fish/occurr_for_sdm.csv")
sites       <- read.csv("points_snapped/all_snapped_with_basins.csv") %>%
  filter(source %in% c("HCMR", "GBIF")) %>%
  filter(subc_id %in% sites$subc_id) %>%
  select(subc_id, basin_id) %>% distinct()

# --- Join basin_id to occurrences via subc_id ---
occurr <- occurr %>%
  left_join(sites, by = "subc_id")

# Sanity check: any occurrences without a basin_id after join?
cat("Occurrences missing basin_id:", sum(is.na(occurr$basin_id)), "\n")

# =============================================================================
# Reconstruct true absences for HCMR sites
# Logic: for every HCMR site, any species NOT recorded there is truly absent
# This does NOT apply to GBIF records
# =============================================================================

# All HCMR sites with their basin
hcmr_sites <- occurr %>%
  filter(source == "HCMR") %>%
  select(site_id, subc_id, basin_id) %>%
  distinct()

# All species recorded at least once in HCMR data
hcmr_species <- occurr %>%
  filter(source == "HCMR") %>%
  distinct(species)

# Full crossed table: every HCMR site x every species
hcmr_full <- hcmr_sites %>%
  cross_join(hcmr_species) %>%
  # join back presences
  left_join(
    occurr %>% filter(source == "HCMR") %>% select(site_id, species) %>% mutate(presence = 1),
    by = c("site_id", "species")
  ) %>%
  mutate(presence = replace_na(presence, 0))

# Quick check
cat("Total HCMR site x species combinations:", nrow(hcmr_full), "\n")
cat("  Presences:", sum(hcmr_full$presence == 1), "\n")
cat("  True absences:", sum(hcmr_full$presence == 0), "\n")

# =============================================================================
# Species overlap check
# =============================================================================

sarantaporos_basin_id <- 1292502

# Species present in Sarantaporos (HCMR presences only)
species_sarant <- hcmr_full %>%
  filter(basin_id == sarantaporos_basin_id, presence == 1) %>%
  distinct(species) %>%
  pull(species)

# Species present in other Greek basins (HCMR presences only)
species_greece_other <- hcmr_full %>%
  filter(basin_id != sarantaporos_basin_id, presence == 1) %>%
  distinct(species) %>%
  pull(species)

# Species in GBIF
species_gbif <- occurr %>%
  filter(source == "GBIF") %>%
  distinct(species) %>%
  pull(species)

# --- Summary counts ---
cat("\n--- Species overlap summary ---\n")
cat("Species with HCMR presences in Sarantaporos:      ", length(species_sarant), "\n")
cat("Species with HCMR presences elsewhere in Greece:  ", length(species_greece_other), "\n")
cat("Species in GBIF:                                   ", length(species_gbif), "\n")
cat("Overlap Sarantaporos + Greece-wide HCMR:           ",
    length(intersect(species_sarant, species_greece_other)), "\n")
cat("Only in Sarantaporos (no HCMR elsewhere):          ",
    length(setdiff(species_sarant, species_greece_other)), "\n")

# --- Per-species breakdown table ---
all_species <- union(species_sarant, species_greece_other)

species_summary <- tibble(species = all_species) %>%
  mutate(
    in_sarantaporos    = species %in% species_sarant,
    in_greece_hcmr     = species %in% species_greece_other,
    in_gbif            = species %in% species_gbif,
    # true absences available in Sarantaporos for this species?
    true_abs_sarant    = species %in% (hcmr_full %>%
                                         filter(basin_id == sarantaporos_basin_id, presence == 0) %>%
                                         pull(species)),
    data_situation = case_when(
      in_sarantaporos & in_greece_hcmr  ~ "HCMR presences in both — true absences usable range-wide",
      in_sarantaporos & !in_greece_hcmr & in_gbif  ~ "Sarantaporos HCMR + GBIF only — true absences range-restricted",
      in_sarantaporos & !in_greece_hcmr & !in_gbif ~ "Sarantaporos HCMR only — consider basin-level model only",
      !in_sarantaporos & in_greece_hcmr ~ "Greece HCMR only — no Sarantaporos true absences",
      TRUE ~ "check"
    )
  )

print(species_summary, n = Inf)

# Count per situation — this is your decision table
species_summary %>% count(data_situation) %>% arrange(desc(n))
# ============================================================================
# CREATE PREDICTION TABLE FOR SPECIES DISTRIBUTION MODELING
# ============================================================================
# Purpose: Create prediction table from downloaded Environment90m data
# Input: Downloaded environmental tables from script 01
# Output: pred_tab.csv - ready for SDM modeling
# ============================================================================
# Date: 2026-02-05
# ============================================================================

library(hydrographr)
library(data.table)
library(dplyr)

# ============================================================================
# SETUP
# ============================================================================

# Set working directory
wdir <- "C:/Users/labbadi/OneDrive/Artigo_Peixes" # Created working directory
setwd(wdir)

# ============================================================================
# VERIFY INPUT FILES EXIST
# ============================================================================

message("\n=== Verifying Input Files ===")

# Check if subc_ids file exists
if (!file.exists("env90m/subc_ids.txt")) {
  stop("ERROR: env90m/subc_ids.txt not found!",
       "\n  Please run script 01_download_env90m_data.R first.")
}

# Check if downloaded data directories exist
required_dirs <- c(
  "env90m/chelsa_bioclim_v2_1",
  "env90m/esa_cci_landcover_v2_1_1",
  "env90m/hydrography90m_v1_0"
)

for (dir in required_dirs) {
  if (!dir.exists(dir)) {
    stop("ERROR: Required directory not found: ", dir,
         "\n  Please run script 01_download_env90m_data.R first.")
  }
}

message("✓ All required input files found")

# ============================================================================
# CONFIGURATION
# ============================================================================

message("\n=== Configuration ===")

# Define variables to include in prediction table
variables <- c(
  "bio01_1981-2010_observed","bio04_1981-2010_observed","bio05_1981-2010_observed","bio06_1981-2010_observed",
  "bio15_1981-2010_observed","bio17_1981-2010_observed","bio18_1981-2010_observed",
  
  # c10
  "c10_1992","c10_1993","c10_1994","c10_1995","c10_1996","c10_1997","c10_1998","c10_1999","c10_2000","c10_2001",
  "c10_2002","c10_2003","c10_2004","c10_2005","c10_2006","c10_2007","c10_2008","c10_2009","c10_2010","c10_2011",
  "c10_2012","c10_2013","c10_2014","c10_2015","c10_2016","c10_2017","c10_2018","c10_2019","c10_2020",
  
  # c20
  "c20_1992","c20_1993","c20_1994","c20_1995","c20_1996","c20_1997","c20_1998","c20_1999","c20_2000","c20_2001",
  "c20_2002","c20_2003","c20_2004","c20_2005","c20_2006","c20_2007","c20_2008","c20_2009","c20_2010","c20_2011",
  "c20_2012","c20_2013","c20_2014","c20_2015","c20_2016","c20_2017","c20_2018","c20_2019","c20_2020",
  
  # c30
  "c30_1992","c30_1993","c30_1994","c30_1995","c30_1996","c30_1997","c30_1998","c30_1999","c30_2000","c30_2001",
  "c30_2002","c30_2003","c30_2004","c30_2005","c30_2006","c30_2007","c30_2008","c30_2009","c30_2010","c30_2011",
  "c30_2012","c30_2013","c30_2014","c30_2015","c30_2016","c30_2017","c30_2018","c30_2019","c30_2020",
  
  # c40
  "c40_1992","c40_1993","c40_1994","c40_1995","c40_1996","c40_1997","c40_1998","c40_1999","c40_2000","c40_2001",
  "c40_2002","c40_2003","c40_2004","c40_2005","c40_2006","c40_2007","c40_2008","c40_2009","c40_2010","c40_2011",
  "c40_2012","c40_2013","c40_2014","c40_2015","c40_2016","c40_2017","c40_2018","c40_2019","c40_2020",
  
  # c50
  "c50_1992","c50_1993","c50_1994","c50_1995","c50_1996","c50_1997","c50_1998","c50_1999","c50_2000","c50_2001",
  "c50_2002","c50_2003","c50_2004","c50_2005","c50_2006","c50_2007","c50_2008","c50_2009","c50_2010","c50_2011",
  "c50_2012","c50_2013","c50_2014","c50_2015","c50_2016","c50_2017","c50_2018","c50_2019","c50_2020",
  
  # c60
  "c60_1992","c60_1993","c60_1994","c60_1995","c60_1996","c60_1997","c60_1998","c60_1999","c60_2000","c60_2001",
  "c60_2002","c60_2003","c60_2004","c60_2005","c60_2006","c60_2007","c60_2008","c60_2009","c60_2010","c60_2011",
  "c60_2012","c60_2013","c60_2014","c60_2015","c60_2016","c60_2017","c60_2018","c60_2019","c60_2020",
  
  # c120
  "c120_1992","c120_1993","c120_1994","c120_1995","c120_1996","c120_1997","c120_1998","c120_1999","c120_2000","c120_2001",
  "c120_2002","c120_2003","c120_2004","c120_2005","c120_2006","c120_2007","c120_2008","c120_2009","c120_2010","c120_2011",
  "c120_2012","c120_2013","c120_2014","c120_2015","c120_2016","c120_2017","c120_2018","c120_2019","c120_2020",
  
  # c130
  "c130_1992","c130_1993","c130_1994","c130_1995","c130_1996","c130_1997","c130_1998","c130_1999","c130_2000","c130_2001",
  "c130_2002","c130_2003","c130_2004","c130_2005","c130_2006","c130_2007","c130_2008","c130_2009","c130_2010","c130_2011",
  "c130_2012","c130_2013","c130_2014","c130_2015","c130_2016","c130_2017","c130_2018","c130_2019","c130_2020",
  
  # c150
  "c150_1992","c150_1993","c150_1994","c150_1995","c150_1996","c150_1997","c150_1998","c150_1999","c150_2000","c150_2001",
  "c150_2002","c150_2003","c150_2004","c150_2005","c150_2006","c150_2007","c150_2008","c150_2009","c150_2010","c150_2011",
  "c150_2012","c150_2013","c150_2014","c150_2015","c150_2016","c150_2017","c150_2018","c150_2019","c150_2020",
  
  # c160
  "c160_1992","c160_1993","c160_1994","c160_1995","c160_1996","c160_1997","c160_1998","c160_1999","c160_2000","c160_2001",
  "c160_2002","c160_2003","c160_2004","c160_2005","c160_2006","c160_2007","c160_2008","c160_2009","c160_2010","c160_2011",
  "c160_2012","c160_2013","c160_2014","c160_2015","c160_2016","c160_2017","c160_2018","c160_2019","c160_2020",
  
  # c180
  "c180_1992","c180_1993","c180_1994","c180_1995","c180_1996","c180_1997","c180_1998","c180_1999","c180_2000","c180_2001",
  "c180_2002","c180_2003","c180_2004","c180_2005","c180_2006","c180_2007","c180_2008","c180_2009","c180_2010","c180_2011",
  "c180_2012","c180_2013","c180_2014","c180_2015","c180_2016","c180_2017","c180_2018","c180_2019","c180_2020",
  
  # c190
  "c190_1992","c190_1993","c190_1994","c190_1995","c190_1996","c190_1997","c190_1998","c190_1999","c190_2000","c190_2001",
  "c190_2002","c190_2003","c190_2004","c190_2005","c190_2006","c190_2007","c190_2008","c190_2009","c190_2010","c190_2011",
  "c190_2012","c190_2013","c190_2014","c190_2015","c190_2016","c190_2017","c190_2018","c190_2019","c190_2020",
  
  # c200
  "c200_1992","c200_1993","c200_1994","c200_1995","c200_1996","c200_1997","c200_1998","c200_1999","c200_2000","c200_2001",
  "c200_2002","c200_2003","c200_2004","c200_2005","c200_2006","c200_2007","c200_2008","c200_2009","c200_2010","c200_2011",
  "c200_2012","c200_2013","c200_2014","c200_2015","c200_2016","c200_2017","c200_2018","c200_2019","c200_2020",
  
  # c210
  "c210_1992","c210_1993","c210_1994","c210_1995","c210_1996","c210_1997","c210_1998","c210_1999","c210_2000","c210_2001",
  "c210_2002","c210_2003","c210_2004","c210_2005","c210_2006","c210_2007","c210_2008","c210_2009","c210_2010","c210_2011",
  "c210_2012","c210_2013","c210_2014","c210_2015","c210_2016","c210_2017","c210_2018","c210_2019","c210_2020",
  
  "order_strahler","length","cum_length","gradient","elev_drop","accumulation",
  "channel_grad_dw_seg","channel_grad_up_seg","channel_elv_dw_seg","channel_elv_up_seg",
  "connections","stream_dist_dw_near","stream_dist_up_near","slope_grad_dw_cel")

# Define statistics to calculate
# Options: "mean"
statistics <- c("mean")

# Define tile IDs (same as in script 01)
tile_id <- c("h18v04", "h20v04")

# Detect available cores for parallel processing
n_cores <- parallel::detectCores() - 6

message(sprintf("Variables: %s", paste(variables, collapse = ", ")))
message(sprintf("Statistics: %s", paste(statistics, collapse = ", ")))
message(sprintf("Tiles: %s", paste(tile_id, collapse = ", ")))
message(sprintf("CPU cores to use: %d", n_cores))

# ============================================================================
# CREATE PREDICTION TABLE
# ============================================================================

message("\n=== Creating Prediction Table ===")
message("This may take several minutes depending on data size...")

# Define path
output_file <- "C:/Users/labbadi/OneDrive/Artigo_Peixes/env90m/pred_tab.csv"
input_file <- "C:/Users/labbadi/OneDrive/Artigo_Peixes/env90m"
subcatch_file <- "C:/Users/labbadi/OneDrive/Artigo_Peixes/env90m/subc_ids.txt"


# Run get_predict_table
pred_tab <- get_predict_table(
  variable = variables,
  statistics = statistics,
  tile_id = tile_id,
  input_var_path = input_file,
  subcatch_id = subcatch_file,
  out_file_path = output_file,
  read = TRUE,
  quiet = FALSE,
  n_cores = n_cores,
  overwrite = TRUE
)

message(sprintf("\n✓ Prediction table created: %d rows, %d columns",
                nrow(pred_tab),
                ncol(pred_tab)))


# ============================================================================
# HANDLE MISSING DATA (IF ANY)
# ============================================================================

message("\n=== Checking for Missing Data ===")

# Count missing values per column
missing_counts <- pred_tab[, lapply(.SD, function(x) sum(is.na(x)))]

# Check if there are any missing values
has_missing <- any(missing_counts > 0)

if (has_missing) {
  message("\n⚠ Missing values detected:")
  
  for (col in names(missing_counts)) {
    if (col != "subc_id") {
      n_missing <- missing_counts[[col]]
      if (n_missing > 0) {
        pct_missing <- 100 * n_missing / nrow(pred_tab)
        message(sprintf("  %s: %d (%.1f%%)", col, n_missing, pct_missing))
      }
    }
  }
  
  # Ask user what to do
  message("\nOptions for handling missing data:")
  message("  1. Keep all rows (NAs will need to be handled in modeling)")
  message("  2. Remove rows with any missing values")
  message("  3. Remove only specific problematic variables")
  
  # For automation, we'll save both versions
  message("\nSaving two versions:")
  
  # Version 1: All data (with NAs)
  fwrite(pred_tab, "env90m/pred_tab_full.csv")
  message("  ✓ Saved: env90m/pred_tab_full.csv (all rows, including NAs)")
  
  # Version 2: Complete cases only
  pred_tab_complete <- na.omit(pred_tab)
  fwrite(pred_tab_complete, "env90m/pred_tab_complete.csv")
  message(sprintf("  ✓ Saved: env90m/pred_tab_complete.csv (%d rows, NAs removed)",
                  nrow(pred_tab_complete)))
  
  message(sprintf("\nRemoved %d rows (%.1f%%) with missing values",
                  nrow(pred_tab) - nrow(pred_tab_complete),
                  100 * (nrow(pred_tab) - nrow(pred_tab_complete)) / nrow(pred_tab)))
  
  # Use complete version for subsequent analyses
  pred_tab <- pred_tab_complete
  
} else {
  message("✓ No missing values detected")
}

# ============================================================================
# DATA QUALITY CHECKS
# ============================================================================

message("\n=== Data Quality Summary ===")

# Dimensions
message(sprintf("\nDimensions: %d rows × %d columns", nrow(pred_tab), ncol(pred_tab)))

# Column names
message("\nColumn names:")
print(names(pred_tab))

# Basic statistics
message("\nBasic statistics:")
print(summary(pred_tab))

# Check for outliers
message("\nChecking for extreme values...")
for (col in names(pred_tab)) {
  if (col != "subc_id" && is.numeric(pred_tab[[col]])) {
    q <- quantile(pred_tab[[col]], probs = c(0.01, 0.99), na.rm = TRUE)
    n_outliers <- sum(pred_tab[[col]] < q[1] | pred_tab[[col]] > q[2], na.rm = TRUE)
    if (n_outliers > 0) {
      pct_outliers <- 100 * n_outliers / nrow(pred_tab)
      message(sprintf("  %s: %d values (%.1f%%) outside 1st-99th percentile",
                      col, n_outliers, pct_outliers))
    }
  }
}


# ============================================================================
# OPTIONAL: CLEAN UP DOWNLOADED TABLES
# ============================================================================

message("\n=== Disk Space Management ===")

# Calculate size of downloaded tables
downloaded_size <- sum(
  file.info(list.files("env90m", pattern = ".txt$",
                       recursive = TRUE, full.names = TRUE))$size,
  na.rm = TRUE
) / 1024 / 1024 / 1024  # GB

# Calculate size of prediction table
pred_tab_size <- file.info("env90m/pred_tab.csv")$size / 1024 / 1024  # MB

message(sprintf("Downloaded tables: %.2f GB", downloaded_size))
message(sprintf("Prediction table: %.2f MB", pred_tab_size))
message(sprintf("Space savings if deleted: %.2f GB", downloaded_size))

message("\nTo free up disk space, you can delete the downloaded tables:")
message("  env90m/chelsa_bioclim_v2_1/")
message("  env90m/esa_cci_landcover_v2_1_1/")
message("  env90m/hydrography90m_v1_0/")

message("\nUncomment the following lines to delete:")
message("# unlink('env90m/chelsa_bioclim_v2_1', recursive = TRUE)")
message("# unlink('env90m/esa_cci_landcover_v2_1_1', recursive = TRUE)")
message("# unlink('env90m/hydrography90m_v1_0', recursive = TRUE)")

# Uncomment to automatically delete
# unlink("env90m/chelsa_bioclim_v2_1", recursive = TRUE)
# unlink("env90m/esa_cci_landcover_v2_1_1", recursive = TRUE)
# unlink("env90m/hydrography90m_v1_0", recursive = TRUE)
# message("\n✓ Deleted downloaded tables")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

message("\n========================================")
message("=== PREDICTION TABLE CREATION COMPLETE ===")
message("========================================")

message("\nFiles created:")
message("  ✓ env90m/pred_tab.csv (main file)")
if (has_missing) {
  message("  ✓ env90m/pred_tab_full.csv (with NAs)")
  message("  ✓ env90m/pred_tab_complete.csv (NAs removed)")
}
message("  ✓ spatial/stream_networks/stream_network_with_env.csv")

message("\nPrediction table summary:")
message(sprintf("  Rows: %d", nrow(pred_tab)))
message(sprintf("  Columns: %d", ncol(pred_tab)))
message(sprintf("  Variables: %d", length(variables)))
message(sprintf("  File size: %.2f MB", pred_tab_size))

message("\nVariables included:")
for (var in variables) {
  var_cols <- grep(var, names(pred_tab), value = TRUE)
  message(sprintf("  %s: %s", var, paste(var_cols, collapse = ", ")))
}

message("\nNext steps:")
message("  1. Load pred_tab.csv in your SDM workflow")
message("  2. Join with species occurrence data")
message("  3. Run species distribution models")
message("  4. Predict habitat suitability")

message("\nExample usage:")
message("  pred_tab <- fread('env90m/pred_tab.csv')")
message("  # Use pred_tab for modeling...")

message("\n========================================\n")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# lookup_dam_power.R
#
# Look up power attributes (power_mw, max_power) for the Sarantaporos dams
# from the Greece-wide dam Excel.
#
# JOIN KEY: dams_sarantaporos_table.csv$Name_GR == Excel$aa  (the RAAY plant
# code). 'aa' is NOT unique across rows because each plant has multiple
# geometry parts, so we keep only part == "Y/L" (the plant location) before
# joining. The Excel has one sheet per licence type (Installation /
# Operational / Production / Evaluation); we read and stack ALL sheets.
#
# A coordinate match (from the clean WKT columns) is used only as a fallback
# for any Sarantaporos dam whose Name_GR does not match an 'aa'.
#
# Output: dams_sarantaporos_power.csv
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(data.table)
library(dplyr)
library(readxl)
library(stringr)
library(purrr)

source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)
# ------------------------------------------------------------
# PARAMETERS
# ------------------------------------------------------------

SARANTAPOROS_CSV <- "points_original/dams/dams_sarantaporos_table.csv"
GREECE_XLSX      <- "points_original/dams/All_hydro_Oct_2023.xlsx"
COL_POWER_MW     <- "power_mw"
COL_MAX_POWER    <- "max_power"
PART_KEEP        <- "Y/L"                              # plant-location rows
MATCH_DECIMALS   <- 4                                  # coord fallback (~10 m)

# ------------------------------------------------------------
# Helper: first lon/lat from a WKT POINT/MULTIPOINT string
# ------------------------------------------------------------
parse_lonlat <- function(wkt) {
  nums <- str_match(wkt, "(-?\\d+\\.\\d+)\\s+(-?\\d+\\.\\d+)")
  data.table(lon = as.numeric(nums[, 2]), lat = as.numeric(nums[, 3]))
}

# ------------------------------------------------------------
# STEP 1: Sarantaporos dams
# ------------------------------------------------------------
sar <- fread(SARANTAPOROS_CSV, sep = ";")
message("Sarantaporos rows: ", nrow(sar),
        "  | types: ", paste(sort(unique(sar$type)), collapse = ", "))

if (!"Name_GR" %in% names(sar))
  stop("Name_GR column not found in ", SARANTAPOROS_CSV)

sar_xy <- parse_lonlat(sar$geometry_wkt)
sar <- cbind(sar, sar_xy)
sar[, `:=`(lon_key = round(lon, MATCH_DECIMALS),
           lat_key = round(lat, MATCH_DECIMALS))]

# ------------------------------------------------------------
# STEP 2: Read ALL sheets of the Excel, stack, filter Y/L
# ------------------------------------------------------------
sheets <- excel_sheets(GREECE_XLSX)
message("\nExcel sheets (", length(sheets), "): ", paste(sheets, collapse = ", "))

gr <- map_dfr(sheets, function(sh) {
  d <- as.data.table(read_excel(GREECE_XLSX, sheet = sh))
  d[, sheet := sh]
  d
})
message("  Total rows across sheets: ", nrow(gr))
message("  Columns: ", paste(names(gr), collapse = ", "))

# filter to plant-location rows (Y/L) — resolves non-unique 'aa'
if ("part" %in% names(gr)) {
  n0 <- nrow(gr)
  gr <- gr[part == PART_KEEP]
  message("  Kept part == '", PART_KEEP, "': ", nrow(gr), " / ", n0, " rows")
} else {
  message("  WARNING: no 'part' column — cannot filter to Y/L; 'aa' may be non-unique")
}

# required columns present?
if (!"aa" %in% names(gr)) stop("'aa' column not found in Excel sheets")
power_cols <- intersect(c(COL_POWER_MW, COL_MAX_POWER), names(gr))
if (length(power_cols) == 0)
  stop("Power columns not found. Available: ", paste(names(gr), collapse = ", "))

# coordinates from the Excel geometry (for fallback + verification)
geom_col <- names(gr)[sapply(gr, function(x)
  is.character(x) && any(grepl("POINT", x, ignore.case = TRUE)))][1]
if (!is.na(geom_col)) {
  gr_xy <- parse_lonlat(gr[[geom_col]])
  gr <- cbind(gr, gr_xy)
  gr[, `:=`(lon_key = round(lon, MATCH_DECIMALS),
            lat_key = round(lat, MATCH_DECIMALS))]
}

# de-duplicate 'aa' if still repeated after Y/L filter (keep first; warn)
dup_aa <- gr[, .N, by = aa][N > 1]
if (nrow(dup_aa) > 0) {
  message("  NOTE: ", nrow(dup_aa), " 'aa' codes still appear >1x after Y/L filter; ",
          "keeping first occurrence of each.")
  gr <- gr[, .SD[1], by = aa]
}

id_cols  <- intersect(c("id1", "aa", "company", "sheet"), names(gr))
keep_cols <- unique(c(id_cols, power_cols,
                      intersect(c("lon", "lat", "lon_key", "lat_key"), names(gr))))
gr_small <- gr[, ..keep_cols]

# ------------------------------------------------------------
# STEP 3: Primary join — Name_GR == aa
# ------------------------------------------------------------
matched <- merge(sar, gr_small,
                 by.x = "Name_GR", by.y = "aa",
                 all.x = TRUE, suffixes = c("", "_gr"))

unmatched <- matched[is.na(get(power_cols[1]))]
message("\nID-matched (Name_GR=aa): ",
        nrow(matched) - nrow(unmatched), " / ", nrow(sar))

# ------------------------------------------------------------
# STEP 4: Coordinate fallback for unmatched rows
# ------------------------------------------------------------
if (nrow(unmatched) > 0 && !is.na(geom_col)) {
  message("  Trying coordinate fallback for ", nrow(unmatched), " unmatched dams...")
  fb <- merge(unmatched[, .(Name_GR, site_id, Name, type, lon_key, lat_key)],
              gr_small[, c("lon_key", "lat_key", power_cols), with = FALSE],
              by = c("lon_key", "lat_key"), all.x = TRUE)
  n_fb <- fb[!is.na(get(power_cols[1])), .N]
  message("  Coordinate fallback recovered: ", n_fb)
  if (n_fb > 0) {
    for (pc in power_cols) {
      idx <- match(matched$Name_GR, fb$Name_GR)
      need <- is.na(matched[[pc]]) & !is.na(idx)
      matched[[pc]][need] <- fb[[pc]][idx[need]]
    }
  }
}

still_missing <- matched[is.na(get(power_cols[1])), .(site_id, Name, Name_GR, type)]
if (nrow(still_missing) > 0) {
  message("\n  Still unmatched (no power found):")
  print(still_missing)
}

# ------------------------------------------------------------
# STEP 5: Report power spread for DAMs (cost-discrimination check)
# ------------------------------------------------------------
dams_only <- matched[type == "DAM"]
message("\n--- Power values for DAM-type barriers in Sarantaporos ---")
print(dams_only[, c("site_id", "Name_GR", power_cols), with = FALSE])

if (nrow(dams_only) > 0) {
  pv <- dams_only[[power_cols[2]]]; pv <- pv[!is.na(pv)]
  if (length(pv) > 0) {
    message("\n  ", power_cols[2], " across DAMs: range ",
            round(min(pv), 3), " - ", round(max(pv), 3),
            " | distinct values: ", length(unique(pv)))
    message("  (if ~1 distinct value, power does NOT discriminate dams -> ",
            "not useful as a cost layer)")
  }
}

fwrite(matched, "dams_sarantaporos_power.csv")
message("\nSaved: dams_sarantaporos_power.csv")

# ------------------------------------------------------------
# STEP 6: Write DAMs as GeoPackage (power columns only) for QGIS
# ------------------------------------------------------------
library(sf)

dams_gpkg <- dams_only[!is.na(lon_gr) & !is.na(lat_gr)]
keep_gpkg <- c("site_id", "Name", "Name_GR", "type", power_cols, "lon_gr", "lat_gr")
keep_gpkg <- intersect(keep_gpkg, names(dams_gpkg))
dams_gpkg <- dams_gpkg[, ..keep_gpkg]

dams_sf <- st_as_sf(dams_gpkg, coords = c("lon_gr", "lat_gr"), crs = 4326)

st_write(dams_sf, "points_cleaned/dams/dams_sarantaporos_power.gpkg", delete_dsn = TRUE, quiet = TRUE)
message("Saved: dams_sarantaporos_power.gpkg  (", nrow(dams_sf), " DAMs, columns: ",
        paste(setdiff(keep_gpkg, c("lon", "lat")), collapse = ", "), ")")


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 00_placeholder_maps.R
# Workflow paper (Paper 1) — introductory placeholder figures
#
# Produces four figures for the SVG workflow diagram / manuscript:
#   Figure A  — map of occurrences (OpenStreetMap basemap + species points, raw)
#   Figure B  — map of barriers (OpenStreetMap basemap + dam points, raw)
#   Figure C  — overview map: basin polygon + pruned network + snapped species & dams
#   Figure D  — snapping inset map: basin + network + zoomed inset showing
#               raw (open circle) vs snapped (filled) positions, with legend
#
# READS:
#   points_snapped/fish/fish_all_species_snapped.csv
#   points_snapped/dams/dams_snapped_points.csv
#   spatial/subbasin/subbasin_polygon.gpkg
#   spatial/subbasin/stream_network_pruned.gpkg
#
# WRITES:
#   figures/placeholder/figA_species_occurrences.png
#   figures/placeholder/figB_barriers.png
#   figures/placeholder/figC_overview_network.png
#   figures/placeholder/figD_snapping_inset.png
#
# All figures: 173 mm wide (MEE double-column), 300 dpi
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(patchwork)

source("~/Documents/PhD/scripts/hydrographr/workflows/helpers/save_to_nimbus.R")
source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
# BASE_DIR <- NIMBUS_DIR
setwd(BASE_DIR)

dir.create("figures/placeholder", recursive = TRUE, showWarnings = FALSE)

# ── MEE figure dimensions ────────────────────────────────────────────────────
FIG_W_MM  <- 173
FIG_H_MM  <- 130
FIG_DPI   <- 300

# ── Colour palette (colour-blind friendly) ───────────────────────────────────
COL_NETWORK    <- "#4393c3"   # steel blue  — pruned stream reaches
COL_SPECIES    <- "#d6604d"   # warm red    — fish occurrences (raw)
COL_SNAPPED    <- "#1a9641"   # green       — snapped positions
COL_DAM_EX     <- "#252525"   # near-black  — existing dams
COL_DAM_PL     <- "#969696"   # grey        — planned dams
COL_BASIN      <- "#f7f7f7"   # near-white  — basin fill
COL_BASIN_BD   <- "#636363"   # basin border
COL_INSET_BOX  <- "#e08214"   # orange      — inset rectangle

# ── shared map theme ─────────────────────────────────────────────────────────
theme_map <- function(base_size = 9) {
  theme_void(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      plot.margin      = margin(4, 4, 4, 4),
      legend.position  = "bottom",
      legend.title     = element_text(size = 7, face = "bold"),
      legend.text      = element_text(size = 6),
      legend.key.size  = unit(3, "mm"),
      plot.title       = element_text(size = 8, face = "bold", hjust = 0.5),
      plot.subtitle    = element_text(size = 7, colour = "grey40", hjust = 0.5)
    )
}

save_fig <- function(p, filename, w = FIG_W_MM, h = FIG_H_MM) {
  ggsave(filename, p, width = w, height = h, units = "mm", dpi = FIG_DPI,
         bg = "white")
  message("Saved: ", filename)
}

# ============================================================
# LOAD DATA
# ============================================================

message("Loading data...")

# Fish — raw original coordinates
fish_raw <- read.csv("points_snapped/fish/fish_all_species_snapped.csv") %>%
  filter(!is.na(longitude_original), !is.na(latitude_original)) %>%
  st_as_sf(coords = c("longitude_original", "latitude_original"), crs = 4326)

# Fish — snapped coordinates
fish_snapped <- read.csv("points_snapped/fish/fish_all_species_snapped.csv") %>%
  filter(!is.na(longitude_snapped), !is.na(latitude_snapped)) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

# Dams — raw original coordinates
dams_raw <- read.csv("points_snapped/dams/dams_snapped_points.csv") %>%
  filter(!is.na(longitude_original), !is.na(latitude_original)) %>%
  mutate(type = if_else(
    tolower(status) %in% c("operational licence", "production licence"),
    "Existing", "Planned"
  )) %>%
  st_as_sf(coords = c("longitude_original", "latitude_original"), crs = 4326)

# Dams — snapped coordinates
dams_snapped <- read.csv("points_snapped/dams/dams_snapped_points.csv") %>%
  filter(!is.na(longitude_snapped), !is.na(latitude_snapped)) %>%
  mutate(type = if_else(
    tolower(status) %in% c("operational licence", "production licence"),
    "Existing", "Planned"
  )) %>%
  st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

# Basin polygon
basin_sf <- st_read("spatial/subbasin/subbasin_polygon.gpkg", quiet = TRUE) %>%
  st_transform(4326)

# Pruned stream network (Strahler >= 4, used in C & D)
network_sf <- st_read("spatial/subbasin/stream_network_pruned.gpkg", quiet = TRUE) %>%
  st_transform(4326)

network_full_sf <- st_read("spatial/subbasin/stream_network.gpkg", quiet = TRUE) %>%
  st_transform(4326)

message("Data loaded.")

# ============================================================
# FIGURE A — Species occurrence points
# Background: full open stream network (no basin polygon)
# ============================================================

message("\n=== Figure A: Species occurrences ===")

p_A <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 10, alpha = 1) +
  geom_sf(data = fish_raw,
          shape = 21, size = 2.8,
          fill = COL_SPECIES, colour = "white", stroke = 0.4,
          alpha = 1) +
  annotation_scale(location = "bl", width_hint = 0.25,
                   text_cex = 0.6, line_width = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(6, "mm"), width = unit(5, "mm"),
                         style = north_arrow_minimal()) +
  labs(
    title    = "Fish occurrence records",
    subtitle = paste0(nrow(fish_raw), " records · GBIF + HCMR")
  ) +
  theme_map()

save_fig(p_A, "figures/placeholder/figA_species_occurrences.png")

# ============================================================
# FIGURE B — Barrier points
# Background: full open stream network (no basin polygon)
# ============================================================

message("\n=== Figure B: Barriers ===")

dam_cols   <- c("Existing" = COL_DAM_EX, "Planned" = COL_DAM_PL)
dam_shapes <- c("Existing" = 24, "Planned" = 25)

p_B <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 10, alpha = 1) +
  geom_sf(data = dams_raw,
          aes(colour = type, fill = type, shape = type),
          size = 2.8, stroke = 0.5) +
  scale_colour_manual(values = dam_cols, name = "Dam status") +
  scale_fill_manual(values   = dam_cols, name = "Dam status") +
  scale_shape_manual(values  = dam_shapes, name = "Dam status") +
  annotation_scale(location = "bl", width_hint = 0.25,
                   text_cex = 0.6, line_width = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(6, "mm"), width = unit(5, "mm"),
                         style = north_arrow_minimal()) +
  labs(
    title    = "Hydropower barriers",
    subtitle = paste0(nrow(dams_raw), " dams · RAE + AMBER")
  ) +
  theme_map()

save_fig(p_B, "figures/placeholder/figB_barriers.png")

# ============================================================
# FIGURE C — Overview: basin + pruned network + snapped species & dams
# ============================================================

message("\n=== Figure C: Overview network map ===")

p_C <- ggplot() +
  geom_sf(data = basin_sf,
          fill = COL_BASIN, colour = COL_BASIN_BD, linewidth = 0.5) +
  geom_sf(data = network_full_sf,
          colour = COL_NETWORK, linewidth = 0.35, alpha = 0.8) +
  geom_sf(data = fish_snapped,
          shape = 21, size = 1.6,
          fill = COL_SPECIES, colour = "white", stroke = 0.35,
          alpha = 0.85) +
  geom_sf(data = dams_snapped,
          aes(shape = type),
          colour = COL_DAM_EX, fill = COL_DAM_EX,
          size = 2.4, stroke = 0.5) +
  scale_shape_manual(values = c("Existing" = 24, "Planned" = 25),
                     name = "Dam status") +
  annotation_scale(location = "bl", width_hint = 0.25,
                   text_cex = 0.6, line_width = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(6, "mm"), width = unit(5, "mm"),
                         style = north_arrow_minimal()) +
  labs(
    title    = "Sarantaporos and Voidomatis sub-basin — full stream network",
    subtitle = "Strahler ≥ 2 network · snapped species (circles) & barriers (triangles)"
  ) +
  theme_map()

save_fig(p_C, "figures/placeholder/figC_overview_full_network.png")


message("\n=== Figure C: Overview network map ===")

p_C <- ggplot() +
  geom_sf(data = basin_sf,
          fill = COL_BASIN, colour = COL_BASIN_BD, linewidth = 0.5) +
  geom_sf(data = network_sf,
          colour = COL_NETWORK, linewidth = 0.35, alpha = 0.8) +
  geom_sf(data = fish_snapped,
          shape = 21, size = 1.6,
          fill = COL_SPECIES, colour = "white", stroke = 0.35,
          alpha = 0.85) +
  geom_sf(data = dams_snapped,
          aes(shape = type),
          colour = COL_DAM_EX, fill = COL_DAM_EX,
          size = 2.4, stroke = 0.5) +
  scale_shape_manual(values = c("Existing" = 24, "Planned" = 25),
                     name = "Dam status") +
  annotation_scale(location = "bl", width_hint = 0.25,
                   text_cex = 0.6, line_width = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(6, "mm"), width = unit(5, "mm"),
                         style = north_arrow_minimal()) +
  labs(
    title    = "Sarantaporos and Voidomatis sub-basin — pruned stream network",
    subtitle = "Strahler ≥ 4 network · snapped species (circles) & barriers (triangles)"
  ) +
  theme_map()

save_fig(p_C, "figures/placeholder/figC_overview_network.png")

# ============================================================
# FIGURE D — Snapping inset map
# Main panel: basin + pruned network + snapped points + orange inset box
# Inset: tight zoom, raw (open circle, red) vs snapped (filled, green) + legend
# ============================================================

message("\n=== Figure D: Snapping inset map ===")

# --- define inset bounding box ---
# Centred on the spatial mean of snapped fish; tighter than before
cx <- 20.9692849
cy <- 40.2074501
INSET_HALF_DEG <- 0.001  # ~100 m — tight enough to see individual reaches

inset_box <- st_bbox(c(
  xmin = cx - INSET_HALF_DEG, xmax = cx + INSET_HALF_DEG,
  ymin = cy - INSET_HALF_DEG, ymax = cy + INSET_HALF_DEG
), crs = 4326) %>% st_as_sfc()

# clip layers to inset extent
network_inset <- st_crop(network_sf,   inset_box)
fish_raw_in   <- st_crop(fish_raw,     inset_box)
fish_snap_in  <- st_crop(fish_snapped, inset_box)
dams_snap_in  <- st_crop(dams_snapped, inset_box)

# displacement lines: raw → snapped
# rows are aligned because both sf objects come from the same CSV (same row order)
make_snap_lines <- function(raw_sf, snapped_sf) {
  n <- min(nrow(raw_sf), nrow(snapped_sf))
  if (n == 0) return(NULL)
  coords_raw  <- st_coordinates(raw_sf[seq_len(n), ])
  coords_snap <- st_coordinates(snapped_sf[seq_len(n), ])
  lines <- lapply(seq_len(n), function(i) {
    st_linestring(rbind(coords_raw[i, ], coords_snap[i, ]))
  })
  st_sf(geometry = st_sfc(lines, crs = 4326))
}

snap_lines_in <- make_snap_lines(fish_raw_in, fish_snap_in)

# ---- main panel ----
p_D_main <- ggplot() +
  geom_sf(data = basin_sf,
          fill = COL_BASIN, colour = COL_BASIN_BD, linewidth = 0.5) +
  geom_sf(data = network_sf,
          colour = COL_NETWORK, linewidth = 0.35, alpha = 0.8) +
  # raw fish — open circles in red
  geom_sf(data = fish_raw,
          shape = 1, size = 1.6,
          colour = COL_SPECIES, stroke = 0.5) +
  # snapped fish — filled circles in green
  geom_sf(data = fish_snapped,
          shape = 21, size = 1.6,
          fill = COL_SNAPPED, colour = "white", stroke = 0.3) +
  # snapped dams — triangles
  geom_sf(data = dams_snapped,
          aes(shape = type),
          colour = COL_DAM_EX, fill = COL_DAM_EX,
          size = 2.2, stroke = 0.5) +
  geom_sf(data = inset_box,
          fill = NA, colour = COL_INSET_BOX, linewidth = 0.8) +
  scale_shape_manual(values = c("Existing" = 24, "Planned" = 25),
                     name = "Dam status") +
  annotation_scale(location = "bl", width_hint = 0.25,
                   text_cex = 0.6, line_width = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(6, "mm"), width = unit(5, "mm"),
                         style = north_arrow_minimal()) +
  labs(title    = "Network snapping",
       subtitle = "Orange box = inset detail") +
  theme_map()

# ---- inset panel ----
# Build a dummy data frame for the legend (raw vs snapped)
legend_df <- data.frame(
  label    = c("Original", "Snapped"),
  x        = c(0, 0), y = c(0, 0)   # dummy coords, not plotted
)

p_D_inset <- ggplot() +
  geom_sf(data = network_inset,
          colour = COL_NETWORK, linewidth = 0.7) +
  # displacement dashes: raw → snapped
  {if (!is.null(snap_lines_in) && nrow(snap_lines_in) > 0)
    geom_sf(data = snap_lines_in,
            colour = "#737373", linewidth = 0.45, linetype = "dashed")
  } +
  # raw positions — open circle in red (drawn first, underneath)
  geom_sf(data = fish_raw_in %>% mutate(position = "Original"),
          aes(colour = position, fill = position, shape = position),
          size = 2.8, stroke = 0.8) +
  # snapped positions — filled circle in green (drawn on top)
  geom_sf(data = fish_snap_in %>% mutate(position = "Snapped"),
          aes(colour = position, fill = position, shape = position),
          size = 2.8, stroke = 0.5) +
  # snapped dams
  {if (nrow(dams_snap_in) > 0)
    geom_sf(data = dams_snap_in,
            shape = 24, size = 3,
            colour = COL_DAM_EX, fill = COL_DAM_EX, stroke = 0.5)
  } +
  scale_colour_manual(
    name   = "Position",
    values = c("Original" = COL_SPECIES, "Snapped" = COL_SNAPPED)
  ) +
  scale_fill_manual(
    name   = "Position",
    values = c("Original" = NA,          "Snapped" = COL_SNAPPED)
  ) +
  scale_shape_manual(
    name   = "Position",
    values = c("Original" = 1,           "Snapped" = 21)   # 1=open circle, 21=filled
  ) +
  guides(
    colour = guide_legend(override.aes = list(
      shape  = c(1, 21),
      fill   = c(NA, COL_SNAPPED),
      colour = c(COL_SPECIES, COL_SNAPPED),
      size   = 2.5
    ))
  ) +
  theme_void(base_size = 7) +
  theme(
    panel.border     = element_rect(colour = COL_INSET_BOX, fill = NA,
                                    linewidth = 1.2),
    plot.background  = element_rect(fill = "white", colour = NA),
    plot.margin      = margin(2, 2, 2, 2),
    legend.position  = "bottom",
    legend.title     = element_text(size = 6, face = "bold"),
    legend.text      = element_text(size = 5.5),
    legend.key.size  = unit(2.5, "mm")
  )

# ---- combine: inset placed lower-right of main panel ----
p_D <- p_D_main +
  inset_element(p_D_inset,
                left   = 0.55, right  = 1.0,
                bottom = 0.0,  top    = 0.45,
                align_to = "plot")

save_fig(p_D, "figures/placeholder/figD_snapping_inset.png",
         w = FIG_W_MM, h = FIG_H_MM + 20)


# ============================================================
message("\n=== All placeholder figures done ===")
message("figures/placeholder/figA_species_occurrences.png")
message("figures/placeholder/figB_barriers.png")
message("figures/placeholder/figC_overview_network.png")
message("figures/placeholder/figD_snapping_inset.png")
library(sf)
library(sfnetworks)
library(igraph)
library(tidygraph)
library(reshape2)
library(dplyr)
library(leaflet)
library(htmlwidgets)

#devtools::install_github("glowabio/hydrographr", ref = "workflow")
library(hydrographr)


setwd("/mnt/shared/connectivity")
setwd("~/proyectos/workflow")


# read graph
#vjosa_streams = st_read("vjosa_partial.gpkg")
#streamSA = st_read("partial_segment_Greece_with_upbuffer_2.gpkg")
streamSA = st_read("partial_stream_network.gpkg")
streamSA

# the description says 30 geometries are empty (probably no edges? outlets?)
# Keep non-empty geometries
clean_sf <- streamSA[!st_is_empty(streamSA), ]

# convert to sfnetwork
net = as_sfnetwork(clean_sf, directed=FALSE)
net

# net shows 617 components: each component represents a Basin

##-----------------------------------------------------------------------------
#display

##-----------------------------------------------------------------------------

# Calculate total length for each connected component
component_lengths <- net %>%
  # 1. Start with nodes to identify components
  activate("nodes") %>%
  mutate(comp_id = group_components()) %>%
  
  # 2. Switch to edges to calculate spatial lengths
  activate("edges") %>%
  mutate(length = edge_length()) %>%
  
  # 3. Bring the component ID from nodes to edges
  # .N() access node data of the current edge's endpoints
  mutate(comp_id = .N()$comp_id[from]) %>%
  
  # 4. Summarize
  as_tibble() %>%
  group_by(comp_id) %>%
  summarise(total_length = sum(length))

component_lengths
summary(component_lengths$total_length)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 6.970e+01 9.730e+02 2.431e+03 1.503e+05 1.366e+04 1.571e+07



# extract only one of the components

# step1 : create an id for each component
nestor <- net %>%
  activate("nodes") %>%
  mutate(comp_id = group_components())

# step 2 extract the component related to basin id N. 1294609
# MESTA / NESTOS RIVER Sub-basin
# 2. Find which comp_id is associated with your target edge
# We use .N() to look up the 'comp_id' of the 'from' node of the target edge
target_id <- nestor %>%
  activate("edges") %>%
  filter(basin_id == 1294609) %>%
  # Use terminal nodes of the edge to find the component ID
  # Since all nodes in a component share one ID, we just need one
  mutate(edge_comp = .N()$comp_id[from]) %>% 
  pull(edge_comp) %>%
  unique()

# step 3: Filter the network to keep that entire component
stream <- nestor %>%
  activate("nodes") %>%
  filter(comp_id %in% target_id)

#####  DISPLAY



#(sub-basin shared by by Bulgaria and Greece)
g1 = st_as_sf(stream, "edges")
st_write(g1, "stream_target.shp")

# measure the length of the network
total_length <- stream %>%
  activate("edges") %>%
  st_as_sf() %>%      # Extract edges as an sf object
  st_length() %>%    # Calculate lengths
  sum()              # Sum them up

total_length
# 4164055 [m]

# access underlaying igraph object
#igobj = as.igraph(net)gg


####   here network indices can be calculated gg
# Example: Calculating betweenness centrality
edge_centrality_between = stream %>%
  activate("edges") %>%
  mutate(bc = centrality_edge_betweenness(weights = edge_length()))
#g1 = st_as_sf(edge_centrality_between, "edges")
#st_write(g1, "edge_centrality_betwee.shp")

###########################
###############  FISH

# read points species occurrences
fish_orig = read.csv("fish_all_species_snapped.csv")

# subcid in nestor
snes = stream  %>%
    activate("edges") %>%
    pull(subc_id) 

#### to get basin ID
#fish_bi = "https://nimbus.igb-berlin.de/index.php/s/zjWHEB7odjifdj8/download/fish_all_species_snapped.csv"
#basin_ids <- api_get_local_ids(csv_url = fish_bi,
#colname_lon = "longitude_snapped",
#colname_lat = "latitude_snapped",
#colname_site_id = "site_id",
#colname_subc_id = "subc_id",
#which_ids = "basin_id"
#)

# remove NAs
# convert to sf object
# Filter to only those within the network's bounding box
fish = fish_orig %>%
    drop_na(longitude_snapped, latitude_snapped) %>%
    filter(subc_id %in% snes) %>%
#    st_as_sf(  coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

# how many ocurrences there are? 300

#st_write(fish, "fish_nestor.shp")

###  use only one species as an example: Squalius orpheus
sqor = fish %>%
    filter(species == "Squalius orpheus" | species == "Squalius_orpheus") %>%
    distinct_at(vars(subc_id), .keep_all = TRUE)
# Simple feature collection with 20 features and 15 field
#st_write(sqor, "sqor_nestor.shp")

####  PLOT: visualize location of species occurrences

# calculated distances between fish occurrence locations assuming full connected network
distf = stream %>%
    activate("edges") %>%
    st_network_cost(from = sqor, to = sqor) %>%
    unclass() %>%
    melt()

#for 20 points there a total possible 190 connections
# formula = (n*(n-1))/2

# create data frame with conection between points and without duplicates or
# distance equal zero
dist_all = distf %>%
    distinct(
    # Create temporary standardized pairs
    min_val = pmin(Var1, Var2), 
    max_val = pmax(Var1, Var2), 
    .keep_all = TRUE
  )  %>%
    filter(value != 0)

### PLOT: boxplot of distances?

#write.table(distf, "distf.txt")


############################
##########  DAMS

# read points dams
dams_orig = read.csv("dams_snapped_points.csv")

# filter dams only for Nestor basin
dams = dams_orig %>%
    filter(subc_id %in% snes) %>%
#    st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)

#st_write(dams, "dams_nestor.shp")

# how many dams there are? 46 

# what type of dams are there?
levels(as.factor(dams$status))
# "existing" "planned" 

# first check existing dams
# extract only existing dams
dexist = dams[dams$status == "existing",]
#st_write(dexist, "dams_existing.shp",  layer_options = "OVERWRITE=trues )
dexist

# how many dams there are? 8
###  PLOT???

# break the graph

## first identify subcids where the dams are located. We need to remove those edges
# list of subcid to remove
rmsubc = unique(dexist$subc_id)

# remove edges with the specific subcids (where dams are located)
snet = stream %>%
    activate(edges) %>%
    filter(!(subc_id %in% rmsubc))

snet

components(snet)$no
components(snet)$csize # size refers to the number of nodes

### PLOT = showing each component in a different color

#dec = decompose(snet)
#g1 = as_sfnetwork(dec[[1]], directed=FALSE)
#g1 = st_as_sf(g1, "edges")
#st_write(g1, "g1.shp")

## calculate distances again with broken network by existing dams
diste = snet %>%
    activate("edges") %>%
    st_network_cost(from = sqor, to = sqor) %>%
    unclass() %>%
    melt()

####  how many conections are broken?
dist_existing = diste %>%
    filter(value != "Inf") %>%
    distinct(
    # Create temporary standardized pairs
    min_val = pmin(Var1, Var2), 
    max_val = pmax(Var1, Var2), 
    .keep_all = TRUE
  )  %>%
    filter(value != 0)

# how many remaining connections
nrow(dist_existing)
## 51 connections are broken
## 190 - 51 = 139 remaining

a = data.frame('connected' = distdf$value)
b = data.frame('disconnected' = diste$value)
boxplot(dplyr::bind_rows(a, b))
boxplot(dplyr::bind_rows(a, b), ylab="Distance (km)", cex.lab=1.5, cex.axis=2)

BASE_DIR   <- Sys.getenv("WORKFLOW_DATA", "/home/grigoropoulou/Documents/Postdoc/projects/workflow_paper/data")
NIMBUS_DIR <- Sys.getenv("NIMBUS_PATH", "/run/user/1000/gvfs/dav:host=nimbus.igb-berlin.de,ssl=true,user=grigoropoulou,prefix=%2Fremote.php%2Fwebdav/workflow_paper/data")
#' @title Fill gaps between suitable habitat patches in a stream network
#'
#' @description For a given set of suitable stream segments (e.g. from a
#' thresholded species distribution model), the function identifies short
#' unsuitable reaches that interrupt otherwise continuous suitable habitat
#' patches, and fills them in. This consolidates fragmented habitat predictions
#' into ecologically meaningful patches, under the assumption that fish can
#' move through short unsuitable reaches to access suitable habitat on either
#' side.
#'
#' A gap is defined as a sequence of consecutive unsuitable reaches between
#' two suitable patches. A gap is filled if its total length is less than the
#' specified threshold (sigma_mob, max_gap_m, or the minimum of both).
#'
#' @param g igraph object. A directed stream network graph. Edge direction
#'   should go from upstream to downstream (subc_id -> target).
#' @param suitable_ids numeric or character vector. The subc_ids of reaches
#'   classified as suitable habitat (above the suitability threshold).
#' @param edge_length named numeric vector. Reach lengths in metres, named
#'   by subc_id. Used to compute gap lengths.
#' @param sigma_mob numeric. Species-specific dispersal distance in metres
#'   (e.g. from fishmove). Gaps shorter than sigma_mob are filled.
#'   Optional if max_gap_m is provided.
#' @param max_gap_m numeric. Maximum gap length in metres to fill, regardless
#'   of species dispersal distance. Optional if sigma_mob is provided.
#'   If both sigma_mob and max_gap_m are provided, the minimum of the two
#'   is used as the gap threshold.
#'
#' @return A character vector of subc_ids of all suitable reaches after gap
#'   filling (original suitable reaches + filled gap reaches).
#'
#' @details
#' The gap-filling algorithm works as follows:
#' \enumerate{
#'   \item Build a subgraph of all unsuitable reaches.
#'   \item Find connected components of unsuitable reaches (potential gaps).
#'   \item For each component, check if it is flanked by suitable reaches
#'     on both sides (upstream and downstream).
#'   \item If the total length of the component is less than the gap threshold,
#'     fill it (reclassify as suitable).
#' }
#'
#' @importFrom igraph induced_subgraph components neighbors V E edge_attr
#'   as_ids is_directed
#' @importFrom data.table data.table rbindlist
#'
#' @author Afroditi Grigoropoulou
#'
#' @examples
#' \dontrun{
#' # Load stream network as graph
#' network_g <- read_geopackage("stream_network.gpkg", import_as = "graph")
#'
#' # Set edge weights to reach length
#' E(network_g)$weight <- reach_lengths[match(E(network_g)$subc_id,
#'                                             names(reach_lengths))]
#'
#' # Define suitable reaches (e.g. from thresholded SDM)
#' suitable <- network_dt %>%
#'   filter(idw_Salmo_farioides >= 0.7) %>%
#'   pull(subc_id)
#'
#' # Fill gaps shorter than sigma_mob
#' suitable_filled <- fill_habitat_gaps(
#'   g           = network_g,
#'   suitable_ids = suitable,
#'   edge_length  = setNames(E(network_g)$weight, V(network_g)$name),
#'   sigma_mob    = 12585,
#'   max_gap_m    = 5000
#' )
#' }
#'
#' @export

fill_habitat_gaps <- function(g,
                              suitable_ids,
                              edge_length,
                              sigma_mob  = NULL,
                              max_gap_m  = NULL) {

  # ---- Input checks ----
  if (!inherits(g, "igraph"))
    stop("g must be an igraph object.")

  if (!is_directed(g))
    stop("g must be a directed graph.")

  if (is.null(sigma_mob) && is.null(max_gap_m))
    stop("At least one of sigma_mob or max_gap_m must be provided.")

  # Determine gap threshold
  if (!is.null(sigma_mob) && !is.null(max_gap_m)) {
    gap_threshold <- min(sigma_mob, max_gap_m)
    message("  Using gap threshold = min(sigma_mob, max_gap_m) = ",
            round(gap_threshold), "m")
  } else if (!is.null(sigma_mob)) {
    gap_threshold <- sigma_mob
    message("  Using gap threshold = sigma_mob = ", round(gap_threshold), "m")
  } else {
    gap_threshold <- max_gap_m
    message("  Using gap threshold = max_gap_m = ", round(gap_threshold), "m")
  }

  # Convert suitable_ids to character for igraph compatibility
  suitable_ids  <- as.character(suitable_ids)
  all_node_ids  <- V(g)$name

  # Unsuitable reaches = all nodes not in suitable_ids
  unsuitable_ids <- setdiff(all_node_ids, suitable_ids)

  message("  Suitable reaches:   ", length(suitable_ids))
  message("  Unsuitable reaches: ", length(unsuitable_ids))

  if (length(unsuitable_ids) == 0) {
    message("  No unsuitable reaches — nothing to fill")
    return(suitable_ids)
  }

  # ---- Build undirected subgraph of unsuitable reaches ----
  # Use undirected for component detection so upstream/downstream
  # unsuitable sequences are treated as single gaps
  g_unsuitable <- induced_subgraph(g, unsuitable_ids)
  g_unsuitable_undir <- as_undirected(g_unsuitable, mode = "collapse")

  # Find connected components of unsuitable reaches
  comps <- components(g_unsuitable_undir)
  n_comps <- comps$no
  message("  Unsuitable connected components (potential gaps): ", n_comps)

  filled_ids <- character(0)
  n_filled   <- 0

  for (comp_id in seq_len(n_comps)) {

    # Nodes in this component
    comp_nodes <- names(which(comps$membership == comp_id))

    # Total gap length
    gap_length <- sum(edge_length[comp_nodes], na.rm = TRUE)

    # Check if gap is flanked by suitable reaches on both sides
    # A gap qualifies if at least one suitable reach is upstream
    # AND at least one suitable reach is downstream
    has_suitable_upstream   <- FALSE
    has_suitable_downstream <- FALSE

    for (node in comp_nodes) {
      # Upstream neighbours (edges pointing TO this node)
      up_neighbours <- as_ids(neighbors(g, node, mode = "in"))
      if (any(up_neighbours %in% suitable_ids))
        has_suitable_upstream <- TRUE

      # Downstream neighbours (edges pointing FROM this node)
      down_neighbours <- as_ids(neighbors(g, node, mode = "out"))
      if (any(down_neighbours %in% suitable_ids))
        has_suitable_downstream <- TRUE

      if (has_suitable_upstream && has_suitable_downstream) break
    }

    # Fill gap if: flanked on both sides AND shorter than threshold
    if (has_suitable_upstream && has_suitable_downstream &&
        gap_length <= gap_threshold) {
      filled_ids <- c(filled_ids, comp_nodes)
      n_filled   <- n_filled + 1
    }
  }

  message("  Gaps filled: ", n_filled, " (of ", n_comps, " components)")
  message("  Reaches added by gap filling: ", length(filled_ids))

  # Return all suitable reaches: original + filled gaps
  result <- unique(c(suitable_ids, filled_ids))
  message("  Total suitable reaches after gap filling: ", length(result))

  return(result)
}
get_subgraph_between_points <- function(graph, species_reach_ids, upstream_buffer = 3) {

  species_reach_ids <- as.character(species_reach_ids)

  # Keep only IDs that exist in graph
  occurrence_nodes <- species_reach_ids[species_reach_ids %in% igraph::V(graph)$name]

  if (length(occurrence_nodes) == 0) {
    warning("No occurrence nodes found in graph")
    return(NULL)
  }

  if (length(occurrence_nodes) == 1) {
    # Only one node — just take upstream buffer from it
    upstream <- igraph::ego(graph, order = upstream_buffer,
                            nodes = occurrence_nodes, mode = "in")
    return(igraph::induced_subgraph(graph, unique(unlist(upstream))))
  }

  # 1. Find all shortest paths between all pairs of occurrence nodes
  #    Use mode = "all" to traverse regardless of edge direction
  all_path_nodes <- c()

  for (i in seq_along(occurrence_nodes)) {
    paths <- suppressWarnings(igraph::shortest_paths(
      graph   = graph,
      from    = occurrence_nodes[i],
      to      = occurrence_nodes[-i],
      mode    = "all",       # undirected traversal — follows river network topology
      output  = "vpath"
    )
    )$vpath

    path_nodes <- unlist(lapply(paths, as.integer))
    all_path_nodes <- c(all_path_nodes, path_nodes)
  }

  all_path_nodes <- unique(all_path_nodes)

  # 2. Upstream buffer from every occurrence node
  upstream_nodes <- igraph::ego(
    graph = graph,
    order = upstream_buffer,
    nodes = occurrence_nodes,
    mode  = "in"
  )
  upstream_node_ids <- unique(unlist(lapply(upstream_nodes, as.integer)))

  # 3. Combine and return subgraph
  all_node_ids <- unique(c(all_path_nodes, upstream_node_ids))
  igraph::induced_subgraph(graph, all_node_ids)
}

# ###################### test
# library(sf)
# library(igraph)
# library(dplyr)
#
#
#
# output_dir <- "connectivity/species_subnetworks"
# dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
#
#
# test_ids <- occurrences |> filter(species == "Barbus_prespensis") |> pull(subc_id)
# test_sub <- get_subgraph_between_points(river_graph_current, test_ids)
# igraph::vcount(test_sub)  # should be much smaller than 131029
#
#
#
# # Clean species names for filenames (replace spaces with underscores)
# species_list <- unique(occurrences$species)
#
# for (sp in species_list) {
#
#   sp_filename <- gsub(" ", "_", sp)
#
#   # --- 1. Species occurrence points ---
#   sp_points <- occurrences |>
#     filter(species == sp) |>
#     st_as_sf(coords = c("longitude_snapped", "latitude_snapped"), crs = 4326)
#
#   st_write(
#     sp_points,
#     file.path(output_dir, paste0(sp_filename, "_occurrences.gpkg")),
#     delete_dsn = TRUE,
#     quiet = TRUE
#   )
#
#   # --- 2. Species subnetwork ---
#   sp_reach_ids <- occurrences |>
#     filter(species == sp) |>
#     pull(subc_id)
#
#   sp_subgraph <- get_subgraph_between_points(
#     graph             = river_graph_current,
#     species_reach_ids = sp_reach_ids,
#     upstream_buffer   = 3
#   )
#
#   sp_subc_ids <- as.integer(igraph::V(sp_subgraph)$name)
#
#   sp_network <- subcatchments |>
#     filter(subc_id %in% sp_subc_ids)
#
#   st_write(
#     sp_network,
#     file.path(output_dir, paste0(sp_filename, "_subnetwork.gpkg")),
#     delete_dsn = TRUE,
#     quiet = TRUE
#   )
#
#   message("Written: ", sp_filename)
# }
#
# subcs <- occurrences %>% filter(species=="Barbus_prespensis") %>% pull(subc_id)
# sub<-get_subgraph_between_points(
#   graph             = river_graph_current,
#   species_reach_ids = subcs,
#   upstream_buffer   = 3
# )
#
#
# species_subgraphs <- occurrences |>
#   split(~species) |>
#   lapply(function(df) {
#     get_subgraph_between_points(
#       graph             = river_graph_current,
#       species_reach_ids = df$subc_id,
#       upstream_buffer   = 3
#     )
#   })
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# pci_sparse.R
#
# Sparse implementation of the Population Connectivity Index (PCI)
# from riverconn::index_calculation (Baldan et al. 2022)
#
# Computes connectivity matrices only for occupied nodes (weight > 0),
# reducing memory from O(N²) to O(k²) where k = number of occupied
# subcatchments and N = total subcatchments in the basin.
# Produces identical results to riverconn::index_calculation.
#
# Validated against riverconn v0.3.31 on basins with up to ~800 nodes
# (see supplementary material).
#
# Limitations vs riverconn::index_calculation:
#   - Exponential dispersal kernel only (no threshold/leptokurtic)
#   - Symmetric directionality only (dir_fragmentation_type = "symmetric",
#     dir_distance_type = "symmetric")
#   - c_ij and B_ij always included (no flags to disable)
#   - index_type "sum" (CAFI) not implemented
#
# References:
#   Baldan, D., Cunillera-Montcusí, D., Funk, A., & Hein, T. (2022).
#     Introducing 'riverconn': an R package to assess river connectivity
#     indices. Environmental Modelling & Software, 156, 105470.
#
#   Jumani, S. et al. (2020). River fragmentation and flow alteration
#     metrics: a review. Environmental Research Letters, 15(12), 123009.
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

pci_sparse <- function(graph,
                       weight = "weight",
                       field_B = "length_reach",
                       param = 0.5,
                       index_type = "full",
                       index_mode = "to",
                       pass_u = "pass_u",
                       pass_d = "pass_d") {

  # ----------------------------------------------------------
  # Input validation
  # ----------------------------------------------------------
  if (!igraph::is_igraph(graph))
    stop("'graph' must be an igraph object")
  if (!(index_type %in% c("full", "reach")))
    stop("'index_type' must be 'full' or 'reach'")
  if (index_type == "reach" && !(index_mode %in% c("from", "to")))
    stop("'index_mode' must be 'from' or 'to'")
  if (!(weight %in% igraph::vertex_attr_names(graph)))
    stop("'weight' must be a vertex attribute in 'graph'")
  if (!(field_B %in% igraph::vertex_attr_names(graph)))
    stop("'field_B' must be a vertex attribute in 'graph'")
  if (!(pass_u %in% igraph::edge_attr_names(graph)))
    stop("'pass_u' must be an edge attribute in 'graph'")
  if (!(pass_d %in% igraph::edge_attr_names(graph)))
    stop("'pass_d' must be an edge attribute in 'graph'")

  # ----------------------------------------------------------
  # Extract vertex and edge attributes
  # ----------------------------------------------------------
  v_weights <- igraph::vertex_attr(graph, weight)   # binary presence (0/1)
  v_lengths <- igraph::vertex_attr(graph, field_B)   # reach lengths (for distance)
  n_nodes   <- igraph::vcount(graph)

  presence_idx <- which(v_weights > 0)
  n_presence   <- length(presence_idx)

  # ----------------------------------------------------------
  # Early exit if fewer than 2 occupied nodes
  # ----------------------------------------------------------
  if (n_presence < 2) {
    if (index_type == "full") {
      return(data.frame(num = 0, den = 0, index = 0))
    } else {
      return(data.frame(
        name  = igraph::V(graph)$name,
        num   = 0,
        den   = 0,
        index = 0
      ))
    }
  }

  # ----------------------------------------------------------
  # Get edge passabilities
  # ----------------------------------------------------------
  edge_pass_u <- igraph::edge_attr(graph, pass_u)
  edge_pass_d <- igraph::edge_attr(graph, pass_d)

  # ----------------------------------------------------------
  # Build sparse c_ij and B_ij matrices (presence × presence)
  #
  # c_ij: barrier passability along path
  #   For each edge on the path: pass = pass_u * pass_d (symmetric)
  #   c_ij = product of pass along all edges on the path
  #
  # B_ij: dispersal probability based on distance
  #   distance_ij = sum of field_B for all vertices on the path
  #   B_ij = param ^ distance_ij (exponential kernel)
  # ----------------------------------------------------------

  c_ij_sparse <- matrix(1, nrow = n_presence, ncol = n_presence)
  B_ij_sparse <- matrix(1, nrow = n_presence, ncol = n_presence)

  for (i in 1:n_presence) {
    origin_node <- presence_idx[i]

    paths_result <- igraph::shortest_paths(
      graph,
      from   = origin_node,
      to     = presence_idx,
      mode   = "all",
      output = "both"
    )

    for (j in 1:n_presence) {
      if (i == j) next

      path_edges <- paths_result$epath[[j]]
      path_verts <- paths_result$vpath[[j]]

      if (length(path_edges) == 0) {
        c_ij_sparse[i, j] <- 0
        B_ij_sparse[i, j] <- 0
      } else {
        # c_ij: product of symmetric passability along path
        c_ij_sparse[i, j] <- prod(
          edge_pass_u[path_edges] * edge_pass_d[path_edges]
        )

        # B_ij: exponential decay with distance
        path_node_ids <- as.integer(path_verts)
        distance_ij   <- sum(v_lengths[path_node_ids])
        B_ij_sparse[i, j] <- param ^ distance_ij
      }
    }

    rm(paths_result)
  }

  gc(verbose = FALSE)

  # ----------------------------------------------------------
  # Combine: agg_mat = c_ij * B_ij (element-wise)
  # ----------------------------------------------------------
  agg_mat <- c_ij_sparse * B_ij_sparse

  # ----------------------------------------------------------
  # Calculate PCI using binary weights
  # ----------------------------------------------------------
  w_presence <- v_weights[presence_idx]

  if (index_type == "full") {
    # Catchment-level PCI: w' * agg_mat * w / (sum(w))^2
    index_num <- as.numeric(t(w_presence) %*% agg_mat %*% w_presence)
    index_den <- sum(w_presence)^2
    index_val <- index_num / index_den

    result <- data.frame(
      num   = index_num,
      den   = index_den,
      index = index_val
    )

  } else if (index_type == "reach") {
    # Reach-level PCI: per-node connectivity score
    if (index_mode == "to") {
      index_num_sparse <- agg_mat %*% w_presence
    } else {
      index_num_sparse <- t(t(w_presence) %*% agg_mat)
    }

    index_den <- sum(w_presence)

    # Expand to full node list (zeros for unoccupied nodes)
    index_num_full <- numeric(n_nodes)
    index_num_full[presence_idx] <- as.numeric(index_num_sparse)

    index_full <- index_num_full / index_den

    result <- data.frame(
      name  = igraph::V(graph)$name,
      num   = index_num_full,
      den   = index_den,
      index = index_full
    )
  }

  return(result)
}
save_to_nimbus <- function(data, filename, save_function = NULL, ...) {
  # Create local temp path
  local_path <- file.path(tempdir(), basename(filename))

  # Save locally first
  if (is.null(save_function)) {
    # Auto-detect save function based on file extension
    ext <- tools::file_ext(filename)
    if (ext == "html") {
      # HTML widget
      htmlwidgets::saveWidget(data, local_path, selfcontained = TRUE)
    } else if (ext %in% c("gpkg", "shp", "geojson")) {
      # Spatial data
      # For GPKG, use layer name from filename if not provided
      if (ext == "gpkg" && !"layer" %in% names(list(...))) {
        layer_name <- tools::file_path_sans_ext(basename(filename))
        sf::st_write(data, local_path, layer = layer_name, delete_dsn = TRUE, quiet = TRUE, ...)
      } else {
        sf::st_write(data, local_path, delete_dsn = TRUE, quiet = TRUE, ...)
      }
    } else if (ext == "csv") {
      # CSV
      data.table::fwrite(data, local_path, ...)
    } else if (ext == "rds") {
      # RDS
      saveRDS(data, local_path, ...)
    } else {
      stop("Unknown file type. Please provide save_function argument.")
    }
  } else {
    # Use custom save function
    save_function(data, local_path, ...)
  }

  # Verify local file was created and has size
  if (!file.exists(local_path)) {
    stop("Local file was not created: ", local_path)
  }
  local_size <- file.info(local_path)$size
  if (local_size == 0) {
    stop("Local file has 0 bytes: ", local_path)
  }

  # Copy to Nimbus
  nimbus_dest <- filename

  # Create directory if needed
  dir.create(dirname(nimbus_dest), recursive = TRUE, showWarnings = FALSE)

  # Use system cp command for WebDAV mounts
  copy_result <- system2("cp", args = c(shQuote(local_path), shQuote(nimbus_dest)))
  if (copy_result != 0) {
    stop("Failed to copy file to Nimbus: ", nimbus_dest)
  }

  # Clean up
  unlink(local_path)

  message(sprintf("✓ Saved to Nimbus: %s", filename))
  invisible(nimbus_dest)
}
