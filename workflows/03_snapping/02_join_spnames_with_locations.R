#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 02_join_spnames_with_locations.R   (Module 3 -- Snapping)
#
# Merge the snapped network coordinates (from 01_snap_all_data.R) back onto
# the full species records. Snapping was done on unique locations only, so
# this step re-attaches species names (and GBIF taxonomy/date fields) to the
# snapped sub-catchment IDs, drops points that failed to snap, and writes a
# combined occurrence table for the SDM, env-space, and PCI modules.
#
# Workflow:
#   1. Load cleaned species records (HCMR, GBIF) and the snapped points
#   2. Join snapped coordinates + subc_id onto each source by site id
#   3. Drop unsnapped records
#   4. Save per-source files + one combined fish_all_species_snapped.csv
#
# INPUT:
#   - points_cleaned/fish/fish_basin_hcmr.csv          (from 01_clean_hcmr_fish.R)
#   - points_cleaned/fish/fish_gbif_clean.csv          (from 03_clean_gbif_fish.R)
#   - points_snapped/fish/all_snapped_fish_points.csv  (from 01_snap_all_data.R)
#
# OUTPUT:
#   - points_snapped/fish/fish_hcmr_with_species_snapped.csv
#   - points_snapped/fish/fish_gbif_with_species_snapped.csv
#   - points_snapped/fish/fish_all_species_snapped.csv  (combined, key output)
#
# LOCATION: workflows/03_snapping/02_join_spnames_with_locations.R
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

library(hydrographr)
library(data.table)
library(dplyr)

select <- dplyr::select

source("/home/grigoropoulou/Documents/PhD/scripts/hydrographr/workflows/helpers/config.R")
setwd(BASE_DIR)

# ============================================================
# STEP 1: Load data
# ============================================================

message("\n=== Merging snapped data with species information ===")

fish_hcmr   <- fread("points_cleaned/fish/fish_basin_hcmr.csv")
fish_gbif   <- fread("points_cleaned/fish/fish_gbif_clean.csv")
all_snapped <- fread("points_snapped/fish/all_snapped_fish_points.csv")

# Snapping combined both sources; split back out by origin to join correctly.
hcmr_snapped <- all_snapped %>% filter(source == "HCMR")
gbif_snapped <- all_snapped %>% filter(source == "GBIF")

# ============================================================
# STEP 2: Join snapped coordinates onto species records
# ============================================================

# HCMR: join by site name. filter() drops the few sites that did not snap
# (no snapped coordinate). Species names use underscores for downstream code.
fish_hcmr_snapped <- fish_hcmr %>%
  left_join(hcmr_snapped, by = c("Sites" = "site_id")) %>%
  filter(!is.na(longitude_snapped)) %>%
  mutate(species = gsub(" ", "_", species))

message(sprintf("HCMR: %d records, %d snapped to a sub-catchment",
                nrow(fish_hcmr_snapped),
                sum(!is.na(fish_hcmr_snapped$subc_id))))

# GBIF: gbifID is the site id; coerce to character so the join matches.
fish_gbif_snapped <- fish_gbif %>%
  mutate(gbifID = as.character(gbifID)) %>%
  left_join(gbif_snapped, by = c("gbifID" = "site_id")) %>%
  rename(decimalLongitude_original_gbif = decimalLongitude,
         decimalLatitude_original_gbif  = decimalLatitude) %>%
  filter(!is.na(longitude_snapped)) %>%
  mutate(species = gsub(" ", "_", species))

message(sprintf("GBIF: %d records, %d snapped to a sub-catchment",
                nrow(fish_gbif_snapped),
                sum(!is.na(fish_gbif_snapped$subc_id))))

# ============================================================
# STEP 3: Save per-source files
# ============================================================

fwrite(fish_hcmr_snapped, "points_snapped/fish/fish_hcmr_with_species_snapped.csv")
fwrite(fish_gbif_snapped, "points_snapped/fish/fish_gbif_with_species_snapped.csv")

# ============================================================
# STEP 4: Combine into one standardized table
# ============================================================

# Keep a shared column set; GBIF adds taxonomy + date fields, filled as NA
# for HCMR rows (fill = TRUE).
fish_all_combined <- rbind(
  fish_hcmr_snapped %>%
    select(site_id = Sites, species,
           longitude_original, latitude_original,
           longitude_snapped, latitude_snapped,
           subc_id, strahler, distance_metres, source) %>%
    mutate(dataset = "HCMR"),
  fish_gbif_snapped %>%
    select(site_id = gbifID, species, genus, family, order,
           longitude_original, latitude_original,
           longitude_snapped, latitude_snapped,
           subc_id, strahler, distance_metres, source,
           year, month, day) %>%
    mutate(dataset = "GBIF"),
  fill = TRUE
)

fwrite(fish_all_combined, "points_snapped/fish/fish_all_species_snapped.csv")

# ============================================================
# SUMMARY
# ============================================================

message("\nMerged files created:")
message("  - points_snapped/fish/fish_hcmr_with_species_snapped.csv")
message("  - points_snapped/fish/fish_gbif_with_species_snapped.csv")
message("  - points_snapped/fish/fish_all_species_snapped.csv  (", nrow(fish_all_combined), " records)")
message("\nNext: 03_snapping/03_extract_subbasin.R")
