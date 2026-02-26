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
BASE_DIR
setwd(BASE_DIR)

message("\n=== Merging Snapped Data with Species Information ===")

# Load original data with species
fish_hcmr <- fread("points_cleaned/fish/fish_greece_hcmr.csv")
fish_gbif <- fread("points_cleaned/fish/fish_gbif_clean.csv")

# Load snapped data
all_snapped <- fread("points_snapped/fish/all_snapped_fish_points.csv")

# Separate HCMR and GBIF from snapped data
hcmr_snapped <- all_snapped %>% filter(source == "HCMR")
gbif_snapped <- all_snapped %>% filter(source == "GBIF")

# Merge HCMR
fish_hcmr_snapped <- fish_hcmr %>%
  left_join(hcmr_snapped, by = c("Sites" = "site_id")) %>%
  rename(
    longitude_original_hcmr = longitude,
    latitude_original_hcmr = latitude
  ) %>% # exclude the 2 points that were not snapped
  filter(!is.na(longitude_snapped))


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
  filter(!is.na(longitude_snapped))

message(sprintf("GBIF: %d records with species", nrow(fish_gbif_snapped)))
message(sprintf("  Successfully snapped: %d (%.1f%%)",
                sum(!is.na(fish_gbif_snapped$subc_id)),
                100 * sum(!is.na(fish_gbif_snapped$subc_id)) / nrow(fish_gbif_snapped)))

# Save merged files
fwrite(fish_hcmr_snapped, "points_snapped/fish_hcmr_with_species_snapped.csv")
fwrite(fish_gbif_snapped, "points_snapped/fish_gbif_with_species_snapped.csv")

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

fwrite(fish_all_combined, "points_snapped/fish/fish_all_species_snapped.csv")

message("\n✓ Merged files created:")
message("  - points_snapped/fish_hcmr_with_species_snapped.csv")
message("  - points_snapped/fish_gbif_with_species_snapped.csv")
message("  - points_snapped/fish_all_species_snapped.csv")
